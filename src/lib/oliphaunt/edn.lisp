(defpackage :edn-reader
  (:use :cl)
  (:export #:edn-begin
           #:edn-end
           #:read-edn
           #:with-edn))

(in-package :edn-reader)

(defun transform-primitive (value)
  (if (symbolp value)
      (cond
        ((string-equal (symbol-name value) "true") t)
        ((string-equal (symbol-name value) "false") nil)
        ((string-equal (symbol-name value) "null") nil)
        (t value))
      value))

(defun make-hash-table-with (&rest alist)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (key . value) in alist
       do (setf (gethash key table) value))
    table))

(defun read-next-object (separator delimiter
                         &optional (input-stream *standard-input*))
  (flet ((peek-next-char () (peek-char t input-stream t nil t))
         (discard-next-char () (read-char input-stream t nil t)))
    (if (and delimiter (char= (peek-next-char) delimiter))
        (progn
          (discard-next-char)
          nil)
        (let* ((object (read input-stream t nil t))
               (next-char (peek-next-char)))
          (cond
            ((char= next-char separator) (discard-next-char))
            ((and delimiter (char= next-char delimiter)) nil)
            (t (error "Unexpected next char: “~a” ~:*~s" next-char)))
          object))))

(defun read-separator (stream char)
  (declare (ignore stream))
  (error "Separator ~S shouldn't be read alone" char))

(defun read-delimiter (stream char)
  (declare (ignore stream))
  (error "Delimiter ~S shouldn't be read alone" char))

(defun read-left-bracket (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, 'read-separator)
    (loop
       for object = (read-next-object #\, #\] stream)
       while object
       collect (transform-primitive object) into objects
       finally (return `(vector ,@objects)))))

(defun stringify-key (key)
  (etypecase key
    (symbol (string-downcase (string key)))
    (string key)))

(defun read-left-brace (stream char)
  (declare (ignore char))
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\, 'read-separator)
    (set-macro-character #\Space 'read-separator)
    (set-macro-character #\Tab 'read-separator)
    (set-macro-character #\Newline 'read-separator)
    (set-macro-character #\Page 'read-separator)
    (loop
       for key = (read-next-object nil #\} stream)
       while key
       for value = (read-next-object nil #\} stream)
       collect `(cons ,key ,(transform-primitive value)) into pairs
       finally (return `(create-edn-hash-table ,@pairs)))))

(defun read-octothorpe (stream char)
  (declare (ignore char))
  (let ((next-char (peek-char t stream)))
    (cond
      ((char= #\_ next-char) (read-char stream) (read stream) nil)
      ((char= #\{ next-char) (read-char stream) (read-set stream))
      ((or (char<= #\a next-char #\z)
           (char<= #\A next-char #\Z))
       (let ((read-syntax (intern (string-upcase (string (read stream)))
                                  :keyword)))
         (case read-syntax

           (otherwise (warn "EDN extended read-syntax “~a” is not supported; ignoring: ~s"
                            read-syntax (read stream))
                      nil)))))))

(defun read-set (stream)
  (error "I cannot read a set. I suck."))

(defun read-backslash (stream char)
  (declare (ignore stream char)))

(defun read-java-string (stream char)
  (declare (ignore char))
  (push *readtable* *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (error "I cannot read a string. I suck.")
  (setq *readtable* (pop *previous-readtables*)))

(defvar *previous-readtables* nil)

(defmacro edn-begin ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push *readtable* *previous-readtables*)
     (setq *readtable* (copy-readtable))
     (set-macro-character #\[ 'read-left-bracket)
     (set-macro-character #\] 'read-delimiter)
     (set-macro-character #\{ 'read-left-brace)
     (set-macro-character #\} 'read-delimiter)
     (set-macro-character #\# 'read-octothorpe)
     (set-syntax-from-char #\| #\-)
     (set-syntax-from-char #\, #\Space)
     (dolist (char '(#\. $\* #\+ #\! #\_ #\? #\$ #\% #\& #\= #\< #\>))
       (set-syntax-from-char char #\-))
     (dolist (char '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\-))
       (set-syntax-from-char char #\-))
     (set-macro-character #\\ 'read-backslash)
     (set-macro-character #\" 'read-java-string)))

(defmacro edn-end ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setq *readtable* (pop *previous-readtables*))))

(defmacro with-edn (&body body)
  `(unwind-protect
        (progn
          (edn-begin)
          ,@body)
     (edn-end)))

(defun read-edn (stream)
  (with-edn (read stream)))



;;; TODO: Unit tests for reading and writing EDN notation needed
