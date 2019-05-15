;;;; -*- lisp -*-
;;;
;;;; ./servers/src/main.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This  program is  Free  Software: you  can  redistribute it  and/or
;;;; modify it under the terms of  the GNU Affero General Public License
;;;; as published by  the Free Software Foundation; either  version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

(in-package :Tootsville)



(defvar *acceptors* nil)

(defun find-acceptor (host port)
  "Find an active Acceptor running on the given HOST address and PORT"
  (dolist (acceptor *acceptors*)
    (when (and (typep acceptor 'Tootsville-REST-acceptor)
               (equal host
                      (hunchentoot:acceptor-address acceptor))
               (= port
                  (hunchentoot:acceptor-port acceptor)))
      (return-from find-acceptor acceptor))))

(defvar *async-tasks* nil)
(defvar *async-channel* nil)

(defun (setf thread-name) (name thread)
  #+sbcl (setf (sb-thread:thread-name thread) name))

(defun swank-connected-p ()
  (when (swank:connection-info) t))

(defun name-idle-threads-sequentially (count)
  (let ((n 0))
    (dolist (thread (all-threads))
      (when (string-equal "Idle Asynchronous Worker" (thread-name thread))
        (setf (thread-name thread)
              (format nil "Idle Asynchronous Worker № ~:d (of ~:d)"
                      (incf (the fixnum n)) count))))))

(defun init-async ()
  (let ((count-threads (max 1 (round (/ (processor-count) 2)))))
    (setf *async-tasks*
          (lparallel:make-kernel count-threads
                                 :name "Idle Asynchronous Worker"))
    (name-idle-threads-sequentially count-threads))
  (let ((lparallel:*kernel* *async-tasks*))
    (setf *async-channel* (lparallel:make-channel)))
  (unless lparallel:*kernel*
    (setf lparallel:*kernel* *async-tasks*)))

(defun run-async (function &key name)
  (unless *async-tasks*
    (init-async))
  (let ((lparallel:*kernel* *async-tasks*))
    (lparallel:submit-task
     *async-channel*
     (lambda ()
       (let ((idle-name (thread-name (current-thread))))
         (setf (thread-name (current-thread)) (or name
                                                  (format nil "Async: run ~s" function)))
         (unwind-protect
              (thread-pool-taskmaster:with-pool-thread-restarts
                  ((thread-name (current-thread)))
                (verbose:info '(:threadpool-worker :async-worker :worker-start)
                              "{~a}: working" (thread-name (current-thread)))
                (funcall function))
           (verbose:info '(:threadpool-worker :async-worker :worker-finish)
                         "{~a}: done" (thread-name (current-thread)))
           (setf (sb-thread:thread-name (current-thread)) idle-name)))))))

(defun background-gc ()
  "Start a garbage collection in a different thread."
  (run-async (lambda () (sb-ext:gc))
               :name "Garbage Collection"))

(defun start (&key (host "localhost") (port 5000) (fullp t))
  "Start a local Hunchentoot server.

HOST is an address of a live interface; PORT may be a port number.

The server will  be started running on port  5000 on local-loopback-only
addresses  (127.0.0.1  and  ::1).  If an  existing  server  is  running,
a restart will be presented to allow you to kill it (RESTART-SERVER)."
  (when fullp
    (load-config)
    (connect-databases)
    (power-on-self-test)
    (background-gc))
  (when-let ((previous (find-acceptor host port)))
    (restart-case (error "Server is already running on ~a port ~a" host port)
      (stop-previous ()
        :report "Stop it (restart)"
        (restart-case (stop previous)
          (ignore-error ()
            :report "Ignore error and try to start anyway ")))
      (change-port (port*)
        :report "Use a different port"
        (start :host host :port port*))))
  (setf hunchentoot:*log-lisp-errors-p* t
        hunchentoot:*log-lisp-backtraces-p* t
        hunchentoot:*log-lisp-warnings-p* t)
  (restart-case
      (push (let ((acceptor
                   (hunchentoot:start
                    (if (config :ssl)
                        (make-instance 'Tootsville-REST-SSL-Acceptor
                                       :ssl-certificate-file (config :ssl :certificate-file)
                                       :ssl-privatekey-file (config :ssl :private-key-file)
                                       :ssl-privatekey-password (config :ssl :private-key-password)
                                       :address host
                                       :port port)
                        (make-instance 'Tootsville-REST-Acceptor
                                       :address host
                                       :port port)))))
              (setf (hunchentoot:acceptor-name acceptor)
                    (format nil "Tootsville ~:[Non-TLS ~;~](~a port ~d)"
                            (config :ssl) host port))
              acceptor)
            *acceptors*)
    (change-port (port*)
      :report "Use a different port"
      (start :host host :port port*))
    (stonith ()
      :report "Shoot the other node in the head (kill listening process)"
      (stonith :host host :port port)
      (start :host host :port port))))



(defmethod usocket:socket-close ((socket null))
  (warn "Ignoring request to close NIL"))

(defun stop (&optional (acceptor (first *acceptors*)))
  "Stop the Hunchentoot server process started by `START'"
  (when acceptor
    (ignore-errors
      (hunchentoot:stop acceptor :soft t))
    (removef *acceptors* acceptor)))


;;; build date/timestamp

(defparameter *compiled* :never
  "A string representing the (fairly  precise) time at which the program
 was compiled.")

(defparameter *build-date* :never
  "A string representing  the year, month, and day at  which the program
 was compiled.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *compiled* (with-output-to-string (s)
                     (print-object (now) s))
        *build-date* (format-timestring nil (now)
                                        :format '(:year #\- :month #\- :day))))


;;; REPL

(defun start-repl ()
  "Starts a PREPL REPL."
  (restart-bind
      ((quit #'cl-user::exit
         :report-function (format *query-io* "Quit the REPL")))
    (let ((*package* (find-package :Oliphaunt-User)))
      (funcall (intern "REPL" (find-package :prepl))))))

;;; Swank

(defun start-swank (&optional (port (+ 46046 (* 2 (random 500)))))
  "Starts a SWANK server on PORT.

Writes  the   port  number   to  a  file   named  after   this  (parent)
process's PID."
  (asdf:load-system :swank)
  (v:info :swank "~&Starting Swank listener on port ~d" port)
  (swank:create-server :port port :dont-close t)
  (ensure-directories-exist "~/run/")
  (with-output-to-file (s (format nil "~~/run/~D.swank.port"
                                  (swank/backend:getpid))))
  port)



(defvar *verbose-logging-lock* (make-lock "Verbose logging lock"))

(defmethod verbose:format-message ((stream stream) (message v:message))
  (with-lock-held (*verbose-logging-lock*)
    (format stream "~&~a	{~a}	[~a: ~{~a~^, ~}]~%⯮	~a~%"
            (format-timestring nil (v:timestamp message)
                               :format
                               '((:year 4) #\- (:month 2) #\- (:day 2)
                                 #\Space
                                 (:hour 2) #\: (:min 2) #\: (:sec 2)
                                 #\Space
                                 :nsec))
            (thread-name (v:thread message))
            (v:level message)
            (mapcar #'symbol-munger:lisp->english (v:categories message))
            (v:content message))))



;;; Web servers

(defun start-hunchentoot (&key (host "localhost") (port 5000))
  "Start a Hunchentoot  server via `START' and fall through  into a REPL
to keep the process running."
  (start :host host :port port)
  (print "Hunchentoot server running. Evaluate (TOOTSVILLE:STOP) to stop, ~
or exit the REPL.")
  (start-repl))

(defun debugger ()
  (swank:set-default-directory (asdf:component-relative-pathname
                                (asdf:find-system  :Tootsville)))
  (swank:set-package :Tootsville)
  (start))

(defun destroy-all-listeners ()
  (map nil #'destroy-thread
       (remove-if-not (lambda (th) (search "Hunchentoot Listening on Address"
                                           (thread-name th)))
                      (all-threads))))

(defun destroy-all-idle-workers ()
  (let ((workers (remove-if-not (lambda (th) (search "Idle Web Worker"
                                                     (thread-name th)))
                                (all-threads))))
    (map nil #'destroy-thread workers)
    workers))

(defun destroy-all-web-tasks ()
  (destroy-all-listeners)
  (while (destroy-all-idle-workers)
    (sleep 1)))

(defparameter *trace-output-heartbeat-time* 90)

(defun start-production (&key host port)
  "Start a Hunchentoot  server via `START' and daemonize with Swank"
  (disable-sbcl-ldb)
  (set-up-for-daemon/start-logging)
  (v:info :starting "Starting on host interface ~a port ~a" host port)
  (start :host host :port port)
  (v:info :starting "Starting Swank")
  (start-swank)
  (loop
     (trace-output-heartbeat)
     (sleep *trace-output-heartbeat-time*)))


;;; Recompilation

(defvar *location-of-main* (or *load-pathname*
                               *compile-file-pathname*))

(defun rebuild-myself ()
  "Recompile the running server.

Hopefully you've already tested the changes?"
  (load (merge-pathnames
         #p"Tootsville.asd"
         (or (when *location-of-main*
               (merge-pathnames
                (make-pathname :directory '(:relative :up))
                (make-pathname :directory (pathname-directory
                                           *location-of-main*))))
             (merge-pathnames #p"servers/"
                              (user-homedir-pathname)))))
  (ql:quickload :Tootsville))



(defun connect-databases ()
  (dolist (thread (mapcar (lambda (n)
                            (make-thread n :name (symbol-munger:lisp->english n)))
                          '(connect-mixer connect-cache connect-maria)))
    (assert (join-thread thread))))
