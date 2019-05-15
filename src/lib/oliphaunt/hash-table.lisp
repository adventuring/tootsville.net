(in-package :oliphaunt)

(defmacro |hash| ((&rest params) plist-or-alist)
  (let ((alistp (and (consp (car plist-or-alist))
                     (atom (cdr (car plist-or-alist))))))
    `(let ((h (make-hash-table
               ,@(if (getf params :size)
                     params
                     (cons params (list :size (if alistp
                                                  (length plist-or-alist)
                                                  (/ (length plist-or-alist) 2))))))))
       ,(if alistp
            ;; let's assume it's an alist
            (loop for (key . value) in plist-or-alist
               collecting `(setf (gethash ,key h) ,value))
            ;; plist
            (loop for (key value) in plist-or-alist :by #'cddr
               collecting `(setf (gethash ,key h) ,value))))))

(defmacro dohash (((key value) hash-table) &body body)
  `(loop for ,key in (hash-table-keys ,hash-table)
      for ,value = (gethash ,key ,hash-table)
      do (progn ,@body)))
