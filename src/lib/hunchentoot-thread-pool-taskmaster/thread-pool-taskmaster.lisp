(in-package #:thread-pool-taskmaster)

(declaim (optimize (safety 3) (speed 3)))

(defclass thread-pool-taskmaster (one-thread-per-connection-taskmaster)
  ((thread-pool :accessor taskmaster-thread-pool)
   (thread-pool-channel :accessor taskmaster-thread-pool-channel))
  (:default-initargs
   :worker-thread-name-format "Web Worker ~a")
  (:documentation
   "A taskmaster that uses a thread pool to dispatch incoming requests."))

(defconstant +threads-per-core+ 24
  "Must be an (UNSIGNED-BYTE 15) and non-zero.")

(defconstant +single-core-threads+ 32
  "More threads than otherwise expected on a single-core machine.")

(defconstant +max-queue-size-for-thread-pool+ #x100
  "What is the maximum size allowed for a thread pool?")

(declaim (type (integer (0) (#. (expt 2 15))) +threads-per-core+))
(declaim (type (integer (0) (#. (expt 2 15))) +single-core-threads+))

(defun swank-connected-p ()
  "Detect whether Swank is connected.

Used to determine whether to resignal errors."
  (and (find-package "SWANK")
       (ignore-errors
         (funcall (coerce (intern "CONNECTION-INFO" :swank) 'function)))))

(defun name-idle-threads-sequentially (taskmaster)
  (let ((n 0)
        (count (the fixnum (taskmaster-max-thread-count taskmaster))))
    (dolist (thread (all-threads))
      (when (string-equal "Idle Web Worker" (thread-name thread))
        (setf (thread-name thread)
              (format nil "Idle Web Worker № ~:d (of ~:d)"
                      (incf (the fixnum n)) count))))))

(defmethod initialize-instance :after ((taskmaster thread-pool-taskmaster)
                                       &rest initargs)
  (declare (ignore initargs))
  (setf (taskmaster-thread-pool taskmaster)
        (lparallel:make-kernel
         (taskmaster-max-thread-count taskmaster)
         :name "Idle Web Worker"))
  
  (name-idle-threads-sequentially taskmaster)
  
  (let ((lparallel:*kernel* (taskmaster-thread-pool taskmaster)))
    (setf (taskmaster-thread-pool-channel taskmaster) (lparallel:make-channel))))

(defmethod shutdown ((taskmaster thread-pool-taskmaster))
  "Idempotent. Shut down the Taskmaster."
  (when-let (pool (taskmaster-thread-pool taskmaster))
    (setf (taskmaster-thread-pool taskmaster) nil)
    ;; Haven't actually seen any errors, but seems wise to be safe here,
    ;; since we're about to lose the only reference to it.
    (ignore-errors (let ((lparallel:*kernel* (taskmaster-thread-pool taskmaster)))
                     (lparallel:end-kernel :wait t))))
  (call-next-method))

(define-memo-function cores*threads-per-core (cores)
  (declare (type (integer 0 #.(expt 2 15)) +threads-per-core+ cores))
  (if (= 1 cores)
      +single-core-threads+
      (the (unsigned-byte 63) (* (the (unsigned-byte 15) +threads-per-core+)
                                 (the (unsigned-byte 15) cores)))))

(defmethod taskmaster-max-thread-count ((taskmaster thread-pool-taskmaster))
  (cores*threads-per-core (processor-count)))

(defmethod taskmaster-thread-count ((taskmaster thread-pool-taskmaster))
  (if (taskmaster-thread-pool taskmaster)
      (taskmaster-max-thread-count taskmaster)
      0))

(defmethod taskmaster-max-accept-count ((taskmaster thread-pool-taskmaster))
  (the fixnum (1+ (the fixnum (cores*threads-per-core (processor-count))))))

(defparameter *mulligans* 5)

(defparameter *developmentp* nil)

(defmacro with-mulligan-handlers ((name mulligan) &body body)
  `(handler-bind
       (#+sbcl
        (Sb-Bsd-Sockets:Bad-File-Descriptor-Error
         (lambda (condition)
           (verbose:fatal '(:thread-pool-worker :peer-gone)
                          "Error signalled: worker ~a: ~
SB-BSD-Sockets:Bad-File-Descriptor-Error:~%~a"
                          ,name condition)
           (abort condition)))
        (error
         (lambda (condition)
           (verbose:fatal '(:thread-pool-worker :worker-error)
                          "Error signalled: worker ~a: ~:(~a~)~%~a"
                          ,name (class-of condition) condition)
           (cond
             (*developmentp*
              (signal condition))
             ((plusp (the fixnum ,mulligan))
              (verbose:info '(:thread-pool-worker :worker-mulligan)
                            "With ~r mulligan~:p left: Trying again (~a stopped by ~:(~a~) ~a)"
                            ,mulligan ,name (class-of condition) condition)
              (decf (the fixnum ,mulligan))
              (invoke-restart 'restart))
             (t
              (verbose:info '(:thread-pool-worker :work-abandoned)
                            "Out of mulligans, abandoning ~a" ,name)))))
        (condition
         (lambda (condition)
           (verbose:debug '(:thread-pool-worker :worker-signal :work-abandoned)
                          "Condition signalled: worker ~a signal ~:(~a~)~%~a~%~s"
                          ,name (class-of condition) condition
                          (ignore-errors (trivial-backtrace:backtrace-string)))
           (abort condition))))
     ,@body))

(defmacro with-pool-thread-restarts ((name) &body body)
  (let ((restart-top (gensym "RESTART-TOP-"))
        (mulligan (gensym "MULLIGAN-")))
    `(tagbody ,restart-top
        (let ((,mulligan *mulligans*))
          (restart-bind
              ((continue (lambda () (go ,restart-top))
                 :report-function (lambda (s)
                                    (princ (concatenate 'string "Restart " ,name) s)))
               (abort (lambda () (throw 'bazinga nil))
                 :report-function (lambda (s)
                                    (princ (concatenate 'string "Abandon " ,name) s))))
            (with-mulligan-handlers (,name ,mulligan)
              ,@body))))))

(defmacro named-thread-pool-runner ((&key (name "Thread pool worker")) &body body)
  #+sbcl
  (let ((idle-name (gensym "IDLE-NAME-"))
        (thread-name (gensym "THREAD-NAME-")))
    `(lambda ()
       (catch 'bazinga
         (let* ((,idle-name (thread-name (current-thread)))
                (,thread-name ,name))
           (setf (sb-thread:thread-name (current-thread)) ,thread-name)
           (unwind-protect
                (with-pool-thread-restarts (,thread-name)
                  (verbose:info '(:threadpool-worker :web-worker :worker-start) "~a working" ,thread-name)
                  ,@body)
             (verbose:info '(:threadpool-worker :web-worker :worker-finish) "~a done" ,thread-name)
             (setf (sb-thread:thread-name (current-thread)) ,idle-name))))))
  #-sbcl
  `(lambda () ,@body))

(defun client-as-string (socket)
  "A helper function which returns the client's address and port as a string
 and tries to act robustly in the presence of network problems.  This will
 also check the current HTTP request context to see if it's a forwarded
 connection, and report that, as well.

This version, unlike Hunchentoot's builtins, should work with IPv6 🤞"
  (if-let ((f-f (and (boundp 'hunchentoot::*request*)
                     (assoc :x-forwarded-for (hunchentoot::headers-in*)))))
    (format nil "~a (via ~a:~d; local ~a:~d)"
            (cdr f-f)
            (usocket::host-to-hostname (usocket:get-peer-address socket))
            (usocket:get-peer-port socket)
            (usocket::host-to-hostname (usocket:get-local-address socket))
            (usocket:get-local-port socket))
    (format nil "~a:~d (local: ~a:~d)"
            (usocket::host-to-hostname (usocket:get-peer-address socket))
            (usocket:get-peer-port socket)
            (usocket::host-to-hostname (usocket:get-local-address socket))
            (usocket:get-local-port socket))))

(defun make-thread-name (taskmaster socket)
  (declare (ignore taskmaster))
  (format nil "Web Worker serving ~a" (safe-client-as-string socket)))

(defun handle-incoming-connection% (taskmaster socket)
  (hunchentoot::increment-taskmaster-accept-count taskmaster)
  (verbose:info '(:web-worker :accepting) "{~a} processing ~s via ~a"
                (thread-name (current-thread)) (safe-client-as-string socket) (taskmaster-acceptor taskmaster))
  (handler-bind (#+sbcl (sb-int:closed-stream-error 
                         (lambda (c)
                           (v:info :disconnected "~s ~a" c)
                           (return-from handle-incoming-connection%))))
    (hunchentoot::process-connection (taskmaster-acceptor taskmaster) socket)))

(defun safe-client-as-string (socket)
  (or (ignore-errors (client-as-string socket))
      "Disconnected Client"))

(defmethod handle-incoming-connection ((taskmaster thread-pool-taskmaster)
                                       socket)
  (handler-bind
      ((error
        (lambda (cond)
          ;; need  to  bind  *ACCEPTOR*  so  that  LOG-MESSAGE*  can  do
          ;; its work.
          (let ((*acceptor* (taskmaster-acceptor taskmaster)))
            (ignore-errors
              (usocket:socket-close socket))
            (log-message* *lisp-errors-log-level*
                          "Error while assigning worker thread for ~
new incoming connection ~a: ~a"
                          (safe-client-as-string socket)
                          cond)))))
    (let ((lparallel:*kernel* (taskmaster-thread-pool taskmaster)))
      (lparallel:submit-task (taskmaster-thread-pool-channel taskmaster)
                             (named-thread-pool-runner
                                 (:name (make-thread-name taskmaster socket))
                               (handle-incoming-connection% taskmaster socket))))))

(defmethod start-thread ((taskmaster thread-pool-taskmaster)
                         thunk &key name)
  ;; XXX: if this happens, we should really toss it into add-job . . .
  (error "Thread-Pool-Taskmaster does not start new threads while running.
Tried to start a thread named ~a with ~s"
         name thunk))

(defmethod execute-acceptor ((taskmaster thread-pool-taskmaster))
  (declare (optimize (speed 1)))
  (verbose:info '(:taskmaster-acceptor) "Starting acceptor thread for ~a" taskmaster)
  (setf (hunchentoot::acceptor-process taskmaster)
        (make-thread
         (lambda ()
           (accept-connections (taskmaster-acceptor taskmaster)))
         :name (format nil "Hunchentoot Listening on ~
~:[all interfaces~;~:*Address ~a~], ~
Port ~d"
                       (acceptor-address (taskmaster-acceptor taskmaster))

                       (the (integer 1 65534)
                            (acceptor-port (taskmaster-acceptor taskmaster)))))))
