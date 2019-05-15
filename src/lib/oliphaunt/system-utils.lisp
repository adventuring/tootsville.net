(in-package :oliphaunt)



;;; ASDF Systems and dependencies

(defun asdf-system-dependencies (child)
  (list*
   (ignore-errors (slot-value (asdf:find-system child)
                              'asdf::load-dependencies))
   (ignore-errors (slot-value (asdf:find-system child)
                              'asdf::depends-on))
   (ignore-errors (slot-value (asdf:find-system child)
                              'asdf::sideway-dependencies))))

(defun asdf-dependency-system-name (parent system)
  (cond
    ((or (stringp system)
         (symbolp system)) system)
    ((and (listp system)
          (eql :version (first system))
          (or (stringp (second system))
              (symbolp (second system))))
     (second system))
    (t (warn "Unrecognized kind of dependency: ~s for ~a" system parent))))

(defun prerequisite-systems (&optional (child :romance-ii))
  (check-type child string-designator)
  (assert child)
  (if-let ((prereqs (remove-duplicates
                     (mapcar #'keywordify
                             (remove-if #'null
                                        (mapcar (curry #'asdf-dependency-system-name child)
                                                (asdf-system-dependencies child)))))))
    (remove-if
     (lambda (sys)
       (member sys
               #+sbcl '(:sb-grovel :sb-posix :sb-rotate-byte
                        :sb-grovel :sb-bsd-sockets)
               #-sbcl '()))
     (remove-duplicates (append (mapcan #'prerequisite-systems prereqs) prereqs)))))

(defun map-asdf-files (function module)
  (check-type function function)
  (check-type module asdf/component:module)
  (mapcan (lambda (child)
            (etypecase child
              (asdf/component:module (map-asdf-files function child))
              (asdf/component:file-component
               (list (funcall function
                              (slot-value child 'asdf/component::absolute-pathname))))))
          (asdf:component-children module)))



;;; Licenses relating to a system

(define-constant +license-words+
    '(:license :licence :copying :copyright)
  :test 'equal)


(defun manual-license-path (system)
  (merge-pathnames
   (make-pathname :directory '(:relative "doc" "legal" "licenses")
                  :name (string-downcase (string system))
                  :type "txt")
   (or #+romans romans-compiler-setup:*path/r2project*
       (asdf:system-source-directory :romance-ii))))

(defun sorted-prerequisite-systems (system)
  (sort (prerequisite-systems system)
        #'string<
        :key (compose #'string-upcase #'string)))

(defun asdf-system-sources (system)
  (make-pathname
   :directory (pathname-directory
               (or (and system (asdf:system-source-directory system))
                   #p"."))
   :name :wild :type :wild))

(defun find-manual-license-override (system)
  (let ((override-file (manual-license-path system)))
    (when (fad:file-exists-p override-file)
      override-file)))

(defun license-name-from-asdf (system)
  (ignore-errors (slot-value (asdf:find-system system) 'asdf::licence)))

(defun find-license-file-in-asdf-top-dir (asdf-dir)
  (loop
     for path in (directory asdf-dir)
     when (member (make-keyword (string-upcase
                                 (pathname-name path))) +license-words+)
     return (pathname path)))

(defun find-license-file-in-asdf-doc-dir (asdf-dir)
  (loop
     for path in (directory (merge-pathnames "doc/" asdf-dir))
     when (member (make-keyword (string-upcase
                                 (pathname-name path))) +license-words+)
     return (pathname path)))

(defun find-readme-file-in-asdf-dir (system asdf-dir)
  (loop
     for path in (directory asdf-dir)
     when (member (make-keyword (string-upcase
                                 (pathname-name path))) '(:readme))
     return (prog1 (list system (pathname path))
              (warn "No LICENSE for ~:(~A~), using README~%(in ~A)"
                    system asdf-dir))))

(defun find-some-license-info-for-system (system asdf-dir longp)
  (or
   (find-manual-license-override system)
   (unless longp
     (license-name-from-asdf system))
   (when asdf-dir
     (find-license-file-in-asdf-top-dir asdf-dir))
   (when asdf-dir
     (find-license-file-in-asdf-doc-dir asdf-dir))
   (when longp
     (license-name-from-asdf system))
   (find-readme-file-in-asdf-dir system asdf-dir)))

(defun find-copyrights (&optional (longp nil))
  (append
   (loop for system in (sorted-prerequisite-systems :romance-ii)
      for asdf-dir = (asdf-system-sources system)
      for license = (find-some-license-info-for-system system asdf-dir longp)
      when license collect (list system license)
      else collect (prog1 (list system nil)
                     (warn "No LICENSE for ~:(~A~)~%(in ~A );~%~TPlease find the license and insert it as ~a"
                           system asdf-dir (manual-license-path system))))
   (if longp
       (list (list :bullet2 (merge-pathnames
                             (make-pathname :directory '(:relative "doc" "legal" "licenses")
                                            :name "bullet2"
                                            :type "txt")
                             #+romans romans-compiler-setup:*path/r2project*)))
       (list (list :bullet2 "MIT")))))




;;; Copyright and licenses

(defun copyrights (&optional (longp nil))
  "Return a string with applicable copyright notices."

  (strcat
   "Romance Game System
Copyright © 1987-2015, Bruce-Robert Pocock;

This program is free software: you may use, modify, and/or distribute it
 *ONLY* in accordance with the terms of the GNU Affero General Public License
 (GNU AGPL).

 ★ Romance Ⅱ uses libraries which have their own licenses. ★

"
   (unless longp "(Abbreviated:)
")
   (loop for (package license) in (find-copyrights longp)
      collect
        (if longp
            (format nil "
————————————————————————————————————————————————————————————————————————
Romance Ⅱ uses the library ~@:(~A~)~2%"
                    package)
            (format nil "~% • ~:(~A~): " package))

      collect
        (typecase license
          (pathname (if longp
                        (alexandria:read-file-into-string license)
                        (first-paragraph-of license 2)))
          (string (if (or (< (length license) 75) longp)
                      license
                      (concatenate 'string (subseq license 0 75) "…")))
          (t (warn "Package ~A has no license?" package)
             "(see its documentation for license)")))
   (if longp
       "~|
————————————————————————————————————————————————————————————————————————

Romance Ⅱ itself is a program.

 Romance Game System Copyright © 1987-2015, Bruce-Robert Fenn Pocock;

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU Affero General Public License as
 published by the Free Software Foundation, either version 3 of the
 License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Affero General Public License for more details.

 You should have received a copy of the GNU Affero General Public
 License along with this program.  If not, see
 http://www.gnu.org/licenses/ ."
       ;; short version
       "
See COPYING.AGPL3 or run “romance --copyright” for details.
")))



;;; Warn about unimplemented functions

(defmacro warn-impl (symbol &optional message)
  `(warn "An implementation of ~a is needed for ~a on ~a.
~@[~%~a~%~]~
Please add an implementation to the file:
~a
git
 … with an appropriate #+(feature) tag to identify it, or contact the Romans
 development team, and we may be able to help. Your *FEATURES* contains the
 following:
~s
"
         ',symbol
         (lisp-implementation-type) (machine-type)
         ,message
         (or *compile-file-truename* *load-truename*)
         *features*))



;;; Information about the hardware

(fare-memoization:define-memo-function processor-count ()
  "Number of processor (cores) available."
  #+linux
  (progn
    (with-open-file (online "/sys/devices/system/cpu/online"
                            :direction :input
                            :if-does-not-exist :error)
      (let ((count 0))
        (loop for set = (read-line online nil nil)
           while set
           do (incf count (range-size set)))
        (the (integer 1 2000) count))))
  #-linux
  (error "I don't have code to check this on non-Linux hosts"))

(defun unembarassing (string)
  "Intel and AMD use these  embarassing ASCII7 character markup in things like
CPU names."
  (loop for ((from to)) on '(("\\(R\\)" "®") ("\\(tm\\)" "™") ("\\(TM\\)" "™"))
     do (setf string
              (cl-ppcre:regex-replace-all from string to)))
  string)



;;; Information about overall system status



(defun load-average ()
  "Load averages return as multiple-values.

Values are: load  averages over the past 1, 5,  and 10 minutes, followed
by  the  number of  actively-running  processes,  and the  total  number
of processes.

eg:

\(multiple-value-bind (load-average-1-minute
 load-average-5-minutes
 load-average-10-minutes
 number-of-processes-running
 total-number-of-processes)
 \(load-average))

… although commonly, only the primary value (load average over 1 minute)
will be of interest.

"
  (with-open-file (loadavg "/proc/loadavg"
                           :direction :input
                           :if-does-not-exist :error)
    (destructuring-bind (a1 a5 a10 ratio &rest _)
        (uiop:split-string (read-line loadavg) :separator " ")
      (declare (ignore _))
      (destructuring-bind (running total)
          (uiop:split-string ratio :separator "/")
        (values (the (real 0 *) (parse-number:parse-real-number a1))
                (the (real 0 *) (parse-number:parse-real-number a5))
                (the (real 0 *) (parse-number:parse-real-number a10))
                (the (integer 0 #.(expt 2 32)) (parse-number:parse-real-number running))
                (the (integer 0 #.(expt 2 32)) (parse-number:parse-real-number total)))))))



;;; Sysop functions

(defun stonith (&key host port pid)
  (cond
    ((and host port)
     (if (find host (mapcar #'network-interface-address (network-interfaces)))
         (if-let ((pid (find-pid-of-local-listener :host host :port port)))
           (stonith :pid pid)
           (error "Cannot find local listener on ~a:~d" host port))
         (drakma:http-request
          (format nil "http://~a:~a/maintenance/quit" host port))))
    (pid (signal-process :sigint pid))
    (t (error "Can't shoot the other node in the head without better information"))))





;;; Remote access and control

(defun run-ssh (&key host user identity-file command script (port 22))
  (check-type command (or string null))
  (check-type script (or string null))
  (assert (or command script))
  (assert host)
  (check-type host dns-name)
  (check-type user string)
  (assert (probe-file identity-file))

  (uiop/run-program:run-program (list "ssh" "-i" identity-file "-l" user host "echo t")))



(defun network-interfaces ()
  #+Linux (mapcar #'lastcar (mapcar #'pathname-directory
                                    (directory #p"/sys/class/net/*")))
  #-Linux (error 'unimplemented))

(defmethod network-interface-address ((interface-designator string))
  #+Linux
  (with-input-from-file (addr$ (make-pathname
                                :directory (list :absolute "sys" "class" "net"
                                                 interface-designator)
                                :name "address"))
    (read-line addr$))
  #-Linux (error 'unimplemented))
