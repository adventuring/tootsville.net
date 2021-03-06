;;;; -*- lisp -*-
;;;
;;;; src/db/memcached.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018-2021  The
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

(declaim (optimize (speed 3)))



(defun query-to-memcache-key (db prepared args)
  "Creates a key based on DB, PREPARED statement, and ARGS suitable for MemCacheD.

Currently uses `SHA1-HEX' of a particular stringified form"
  (declare (ignore db)) ; TODO?
  (let ((query (format nil "~a~{~a~^~}" prepared args)))
    (sha1-hex query)))

(defmacro with-memcached-query ((db query args &key (timeout (* 60 60 4))) &body body)
  "Execute BODY only if the QUERY's value is not found in MemCacheD."
  (let (($db (gensym "DB-"))
        ($query (gensym "QUERY-"))
        ($key (gensym "KEY-"))
        ($value (gensym "VALUE-"))
        ($args (gensym "ARGS-"))
        ($body (gensym "BODY-")))
    `(flet ((,$body () ,@body))
       (if cl-memcached:*memcache*
           (handler-case
               (let* ((,$db ,db) (,$query ,query) (,$args ,args)
                      (,$key (query-to-memcache-key ,$db ,$query ,$args))
                      (cl-memcached:*mc-default-encoding* :utf-8)
                      (*print-readably* t)
                      (*print-pretty* nil)
                      (*print-right-margin* 120))
                 (if-let (,$value (memcached-get-key ,$key))
                   (apply #'values (read-from-string ,$value))
                   (progn
                     #+(or) (v:warn :memcached "Cache miss on ~a" ,$key)
                     (let ((,$value (multiple-value-list (,$body))))
                       (cl-memcached:mc-store
                                ,$key
                                (trivial-utf-8:string-to-utf-8-bytes (format nil "~s" ,$value))
                                :timeout ,timeout
                                :command :set)
                       #+(or)
                       (v:warn :memcached "Caching ~:d values: ~a"
                               (length ,$VALUE)
                               (cl-memcached:mc-store
                                ,$key
                                (trivial-utf-8:string-to-utf-8-bytes (format nil "~s" ,$value))
                                :timeout ,timeout
                                :command :set))
                       (apply #'values ,$value)))))
             (cl-memcached::memcached-server-unreachable (c)
               (declare (ignore c))
               (,$body))
             (pooler::pool-item-creation-error (c)
               (declare (ignore c))
               (,$body)))
           (progn
             (run-async #'connect-cache)
             (,$body))))))



(defun connect-cache ()
  "Connect to MemCacheD.

Configuration comes from `DB-CONFIG' path (:cache (:ip :port :name)).

The pool size will be `PROCESSOR-COUNT', clamped to 3-15."
  (setf cl-memcached:*mc-use-pool* t)
  (dolist (server (car (db-config :cache)))
    (setf cl-memcached:*memcache*
          (cl-memcached:make-memcache
           :ip (extract server :ip)
           :port (or (extract server :port) 11211)
           :name (extract server :name)
           :pool-size (max 1 (floor (the (unsigned-byte 15) (processor-count))
                                    3))))
    )
  (length (the proper-list (db-config :cache))))



(defpost memcached-quick-test ()
  "Quick test provided by CL-MemCacheD library"
  (handler-case
      (cl-memcached::mc-quick-test)
    (cl-memcached::memcached-server-unreachable (c)
      (warn (princ-to-string (the condition c))))))

(defun memcached-get-key (key)
  (when-let (value (cl-memcached:mc-get (list key)))
    (map 'string #'code-char (lastcar (first value)))))

(defpost memcached-random-number-test ()
  "Store and fetch a random number"
  (handler-case
      (let ((n (princ-to-string (the (integer 0 *)
                                     (random (the (integer 0 *)
                                                  (expt 2 63))))))
            (key (format nil "post.~a.~a" (machine-instance) (cluster-name))))
        (cl-memcached:mc-set key n)
        (let ((m (memcached-get-key key)))
          (assert (equal n m) ()
                  "MemCacheD did not return the random number (~x) for key ~a"
                  n key))
        (cl-memcached:mc-del key))
    (cl-memcached::memcached-server-unreachable (c)
      (warn (princ-to-string c)))))



(defun powerset (list)
  "Create a powerset of the unordered elements of LIST.

@verbatim
(powerset '(:a :b :c))
((:A :B :C) (:B :C) (:A :C) (:C) (:A :B) (:B) (:A) NIL)
@end verbatim
"
  (if list
      (mapcan (lambda (el) (list (cons (car list) el) el))
              (powerset (cdr list)))
      '(())))

(defun erase-all-memcached-for (name &rest columns+values)
  (if cl-memcached:*memcache*
      (cl-memcached:mc-flush-all)
      #+ (or)
      (let ((db (second (database-for name)))
            (table (db-table-for name))
            (columns (plist-keys columns+values)))
        (loop for set in (powerset columns)
           for columns+values-subset = (loop for column in set
                                          collecting column
                                          collecting (getf columns+values column))
           do (cl-memcached:mc-del (query-to-memcache-key db table columns+values-subset))))
      (run-async #'connect-cache)))
