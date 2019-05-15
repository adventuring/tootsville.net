;;;; -*- lisp -*-
;;;
;;;; ./servers/src/acceptor.lisp is part of Tootsville
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

(defclass Tootsville-REST-acceptor (hunchentoot:easy-acceptor)
  ((hunchentoot::taskmaster
    :initform (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))
  (:default-initargs
   :error-template-directory (config :templates :errors)
    :access-log-destination (config :log :access)
    :message-log-destination (config :log :message)))

(defmethod initialize-instance :after
    ((acceptor Tootsville-REST-acceptor) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value acceptor 'hunchentoot::taskmaster)
        (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))

(defun not-found-if-null (thing)
  "If THING is null, then abort with a 404 Not Found."
  (unless thing
    (verbose:info :not-found "{~a} 404: object not found"
                  (current-thread))
    (setf (hunchentoot:return-code*)
          hunchentoot:+http-not-found+)
    (hunchentoot:abort-request-handler))
  thing)

(defgeneric respond-to-error (condition)
  (:method ((error error))
    (hunchentoot:maybe-invoke-debugger error))
  (:method ((error unimplemented))
    (verbose:info :unimplemented
                  "Unimplemented function called: ~s" error)))

(defun request-accept-types ()
  (when-let (accept (assoc :accept (hunchentoot:headers-in*)))
    (mapcar (lambda (s) (string-trim +whitespace+ (the string s)))
            (split-sequence #\, (rest accept)))))

(defun template-match (template list)
  (if (every #'stringp template)
      (equalp template list)
      (loop for tmpl in template
         for el in list
         with result = nil
         do (etypecase tmpl
              (string (unless (string= tmpl el)
                        (return nil)))
              (symbol (push el result)))
         finally (return (nreverse result)))))

(defpost acceptor-template-matches-constants ()
  (template-match '("foo" "bar" "baz") '("foo" "bar" "baz")))
(defpost acceptor-template-unifies-variables ()
  (equalp '("42" "99")
          (template-match '("foo" :bar :baz) '("foo" "42" "99"))))

(defun strip-after-sem (s)
  (if-let ((sem (position #\; (the string s) :test #'char=)))
    (subseq s 0 sem)
    s))

(defun accept-type-equal (a b &key (allow-wildcard-p t))
  (let ((a (strip-after-sem a))
        (b (strip-after-sem b)))
    (or (string-equal a b)
        (and allow-wildcard-p
             (or (and (string-ends "/*" a)
                      (let ((slash (position #\/ a)))
                        (string-equal a b :end1 slash :end2 slash)))
                 (and (string-ends "/*" b)
                      (let ((slash (position #\/ b)))
                        (string-equal a b :end1 slash :end2 slash)))
                 (equal a "*/*")
                 (equal b "*/*"))))))

(defpost accept-type-matches-identically ()
  (accept-type-equal "text/html" "text/html"))
(defpost accept-type-matches-with-charset=utf-8 ()
  (accept-type-equal "text/html" "text/html;charset=utf-8"))
(defpost accept-type-matches-/* ()
  (accept-type-equal "text/html" "text/*"))
(defpost accept-type-matches-/*-with-charset=utf-8 ()
  (accept-type-equal "text/html" "text/*;charset=utf-8"))
(defpost accept-type-matches-*/* ()
  (accept-type-equal "text/html" "*/*"))
(defpost accept-type-does-not-match-/*-when-not-allow-wildcards-p ()
  (not (accept-type-equal "text/html" "text/*" :allow-wildcard-p nil)))

(defun find-user-for-headers (string)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 1)))
  (when string
    (if (string-begins "auth/Infinity/Alef/5.0 " (the string string))
        (destructuring-bind (provider token &rest _)
            (split-sequence #\Space (subseq string 23))
          (declare (ignore _) (type string provider token))
          (v:info :auth "Provider ~a asserts token ~a…"
                  provider (subseq token 0 (min (length token) 40)))
          (assert (string-equal provider "Firebase"))
          (ensure-user-for-plist
           (check-firebase-id-token token)))
        (progn (v:warn :auth "Unsupported ∞ auth, ~s"
                       (subseq (the string string)
                               (or (position #\Space string)
                                   (length string))))
               nil))))

(defun gracefully-report-http-client-error (c)
  (v:error :HTTP-client "Gracefully reporting error to HTTP client: ~a" c)
  (v:error :http-error "Condition ~s,~%backtrace ~s"
           c (ignore-errors (rollbar::find-appropriate-backtrace)))
  (rollbar:error! "Sending HTTP response to condition" 
                  :condition c)
  (if (wants-json-p)
      (encode-endpoint-reply
       (list (http-status-code c)
             (list :content-type "application/json; charset=utf-8")
             (if hunchentoot:*show-lisp-backtraces-p*
                 (to-json
                  (list :|error| (http-status-code c)
                        :|errorMessage| (princ-to-string c)
                        :|trace| (rollbar::find-appropriate-backtrace)))
                 (to-json
                  (list :|error| (http-status-code c)
                        :|errorMessage| (princ-to-string c))))))
      (encode-endpoint-reply
       (list (http-status-code c)
             (list :content-type "text/html; charset=utf-8")
             (pretty-print-html-error c)))))

(defmacro with-http-conditions (() &body body)
  `(handler-case (progn ,@body)
     (http-client-error (c)
       (gracefully-report-http-client-error c))
     (error (c)
       (gracefully-report-http-client-error c))))

(defun handle-options-request (uri-parts ua-accept)
  (v:info :request "Method is OPTIONS")
  (let ((method (make-keyword (hunchentoot:header-in* :Access-Control-Request-Method))))
    (if-let (match (find-best-endpoint method uri-parts ua-accept))
      (progn
        (setf (hunchentoot:return-code*) 200)
        (v:info :request "OPTIONS reply for ~s ~s ~s"
                method uri-parts ua-accept)
        (setf
         (hunchentoot:header-out :Access-Control-Allow-Methods)
         (string method)
         (hunchentoot:header-out :Access-Control-Allow-Headers)
         "Accept, Accept-Language, Content-Language, Content-Type, X-Infinity-Auth"
         (hunchentoot:header-out :Access-Control-Max-Age) 85000)
        (hunchentoot:send-headers)
        nil)
      (progn
        (v:info :request "No match for ~s ~s ~s"
                (make-keyword (hunchentoot:header-in* :access-control-request-method))
                uri-parts ua-accept)
        (error 'not-found :the "OPTIONS URI")))))

(defun set-http-default-headers ()
  (setf
   (hunchentoot:header-out :Access-Control-Allow-Origin)
   (or (hunchentoot:header-in* :Origin)
       "*")
   (hunchentoot:header-out :X-Tootsville-Machine) (machine-instance)
   (hunchentoot:header-out :X-Romance) (romance-ii-program-name/version)
   (hunchentoot:header-out :X-Lisp-Version)
   (format nil "~a/~a"
           (lisp-implementation-type)
           (lisp-implementation-version))))

(defun dispatch-endpoint (match)
  (destructuring-bind (endpoint &rest bindings) match
    (verbose:info :request "Calling ~s" match)
    (apply (fdefinition (endpoint-function endpoint)) bindings)))

(defun handle-normal-request (method uri-parts ua-accept)
  (if-let (match (find-best-endpoint method uri-parts ua-accept))
    (dispatch-endpoint match)
    (progn
      (verbose:info :request "No match for ~s ~{/~a~} accepting ~s"
                    method uri-parts ua-accept)
      (error 'not-found :the (format nil "The URI you requsted")))))

(defmethod hunchentoot:acceptor-dispatch-request
    ((acceptor Tootsville-REST-acceptor) request)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 1)))
  (verbose:info :request "Dispatching request ~s via acceptor ~s"
                request acceptor)
  (let* ((hunchentoot:*request* request)
         (*user* (find-user-for-headers (hunchentoot:header-in*
                                         "X-Infinity-Auth")))
         (*Toot* (find-active-Toot-for-user))
         (method (hunchentoot:request-method*))
         (uri-parts (split-sequence #\/
                                    (namestring
                                     (hunchentoot:request-pathname request))
                                    :remove-empty-subseqs t))
         (ua-accept (request-accept-types)))
    (with-http-conditions ()
      (set-http-default-headers)
      (if (eql :options method)
          (handle-options-request uri-parts ua-accept)
          (handle-normal-request method uri-parts ua-accept)))))

(defmethod hunchentoot:acceptor-status-message
    ((acceptor Tootsville-REST-Acceptor) HTTP-status-code
     &rest _ &key &allow-other-keys)
  (declare (ignore _))
  (unless (wants-json-p) (call-next-method))
  (when (< (the fixnum HTTP-status-code) 400) (call-next-method))

  (gracefully-report-HTTP-client-error
   (make-condition 'HTTP-client-error :status HTTP-status-code)))
