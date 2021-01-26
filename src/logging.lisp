(in-package :tootsville)

(defun trace-log-file (log-dir)
  "Get the pathname of the trace log file."
  (merge-pathnames (make-pathname :name "Tootsville.trace"
                                  :type "log")
                   log-dir))

(defun find-log-dir ()
  "Find the logging directory under `USER-HOMEDIR-PATHNAME'"
  (merge-pathnames #p"./logs/Tootsville/" (user-homedir-pathname)))

(defun standard-log-file (log-dir)
  "Get the pathname of the standard log file."
  (merge-pathnames (make-pathname :name "Tootsville.standard"
                                  :type "log")
                   log-dir))

(defun error-log-file (log-dir)
  "Get the pathname of the error log file."
  (merge-pathnames (make-pathname :name "Tootsville.error"
                                  :type "log")
                   log-dir))

(defun verbose-log-file (log-dir)
  "Get the pathname of the verbose log file."
  (merge-pathnames (make-pathname :name "Tootsville.verbose"
                                  :type "log")
                   log-dir))

(defun open-log-file (pathname)
  "Open PATHNAME for logging."
  (open pathname :direction :output
        :if-exists :append
        :if-does-not-exist :create
        :external-format :utf-8))



(defun greeting/daemon/error-output ()
  "Print a greeting to `*ERROR-OUTPUT*'."
  (format *error-output*
          "~%Error-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now)))
  (force-output *error-output*))

(defun greeting/daemon/log-output ()
  "Print a greeting to the verbose info log."
  (v:info :logging
          "~%Log-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now))))

(defun greeting/daemon/standard-output ()
  "Print a greeting to `*STANDARD-OUTPUT*'."
  (format t "~%Standard-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now)))
  (force-output))

(defun greeting/daemon/trace-output ()
  "Print a greeting to `*TRACE-OUTPUT*'"
  (format *trace-output*
          "~%Trace-Output: Tootsville daemon started at ~a"
          (local-time:format-rfc3339-timestring nil (local-time:now)))
  (force-output *trace-output*))

(defun set-up-for-daemon/standard-output (log-dir)
  "Set up the `*STANDARD-OUTPUT*' for logging."
  (setf *standard-output* (open-log-file (standard-log-file log-dir)))
  (greeting/daemon/standard-output))

(defun set-up-for-daemon/log-output (log-dir)
  "Set up the Verbose mode logging output file in LOG-DIR."
  (v:output-here (open-log-file (verbose-log-file log-dir)))
  (greeting/daemon/log-output))



(defun set-up-for-daemon/error-output (log-dir)
  "Set up the `*ERROR-OUTPUT*' for logging in LOG-DIR."
  (setf *error-output* (open-log-file (error-log-file log-dir)))
  (greeting/daemon/error-output))

(defun set-up-for-daemon/trace-output (log-dir)
  "Set up the `*TRACE-OUTPUT*' for logging in LOG-DIR."
  (setf *trace-output* (open-log-file (trace-log-file log-dir)))
  (greeting/daemon/trace-output))

(defun set-up-for-daemon/start-logging ()
  "Set up for daemon-mode logging."
  (let ((log-dir (find-log-dir)))
    (ensure-directories-exist log-dir)
    (set-up-for-daemon/log-output log-dir)
    (set-up-for-daemon/standard-output log-dir)
    (set-up-for-daemon/error-output log-dir)
    (set-up-for-daemon/trace-output log-dir)))

(defun trace-output-heartbeat ()
  "Output a heartbeat message with thread listing to `*TRACE-OUTPUT*'"
  (format *trace-output* "~&//* Still Alive (~a)" (now))
  (format *trace-output* "~{~&//  ~a~}" (bt:all-threads))
  (format *trace-output* "~&/**")
  (force-output *trace-output*))

(defun banner/query-io ()
  "Print a greeting banner to `*QUERY-IO*'"
  (write-string (tootsville-v-banner) *query-io*)
  (princ "This is the Query I/O channel



 Ⓣ Let's play in Tootsville!

 ♪Let's make some noise! ♪


" *query-io*)
  (finish-output *query-io*))

(defun banner/log ()
  "Print a greeting banner to the verbose log."
  (v:info :startup (tootsville-v-banner)))

(defun banner/standard-output ()
  "Print a greeting banner to `*STANDARD-OUTPUT*'"
  (format t "~&~|~%~a (© ~d)" (tootsville::romance-ii-program-name/version)
          (romance-ii-copyright-latest))
  (terpri *query-io*)
  (princ "This is the Standard Output channel" *query-io*)
  (finish-output))

(defun banner/error-output ()
  "Print a greeting banner to `*ERROR-OUTPUT*'"
  (format *error-output* "~&~|
~a Starting Tootsville Ⅴ, error log begins"
          (local-time:format-timestring nil (local-time:now))))

(defun banner/trace-output ()
  "Print a greeting banner to `*TRACE-OUTPUT*'"
  (format *error-output* "~&~|
~a Starting Tootsville Ⅴ, trace log begins"
          (local-time:format-timestring nil (local-time:now))))

(defun banner ()
  "Print greeting banners to the various output streams."
  (banner/log)
  (banner/query-io)
  (banner/standard-output)
  (banner/error-output)
  (banner/trace-output))
