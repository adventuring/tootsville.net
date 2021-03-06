;;;; -*- lisp -*-
;;;
;;;; src/db/db-central.lisp is part of Tootsville
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



(defmacro ignore-not-found (&body body)
  "Ignore NOT-FOUND errors in BODY, and return a NIL instead."
  `(handler-case (progn ,@body)
     (not-found (c)
       (declare (ignore c))
       nil)))

(defun ensure-record (type &rest columns+values)
  (or (values (ignore-not-found (apply #'find-record type columns+values)) t)
      (values (apply #'make-record type columns+values) nil)))

(defmacro do-records ((record-var type &rest columns+values) &body body)
  "Apply BODY to each row as if `FIND-RECORDS' were called."
  (let (($record-var (gensym "RECORD-")))
    `(do-db-records-simply (,$record-var ,(db-table-for type) ,@columns+values)
       (let ((,record-var (load-record ',type ,$record-var))
             ),@body))))



(defvar pull-records-cache nil)

(defun pull-records (name)
  (copy-list (or (getf pull-records-cache name)
                 (setf (getf pull-records-cache name)
                       (mapcar (lambda (raw) (load-record name raw))
                               (db-select-records-simply (db-table-for name)))))))

(defun lisp-to-db-name (name)
  "Convert a Lispy name to an SQL-type one.

Particularly, changes CAPS-WITH-KEBABS to lower_with_snakes."
  (check-type name symbol)
  (cffi:translate-name-to-foreign (make-keyword (string name)) *package*))

(defvar *utc-timezone*
  (progn
    (local-time:reread-timezone-repository)
    (local-time:find-timezone-by-location-name "UTC"))
  "The UTC time zone.

The Universal Coördinated Time time zone.

For practical purposes,  this is essentially the same  as GMT (Greenwich
Mean Time) or Z (Zulu Time).")

(defun uuid-string-to-base64 (uuid-string)
  "Converts UUID-STRING into a UUID and gives its Base64 string value.

See also `UUID-TO-BASE64'."
  (subseq (the string
               (cl-base64:usb8-array-to-base64-string
                (uuid:uuid-to-byte-array
                 (uuid:make-uuid-from-string uuid-string))))
          0 22))

(defun uuid-to-base64 (uuid)
  "Convert UUID into a Base64 string.

Strips the trailing @code{==} that in invariant."
  (subseq (the string
               (cl-base64:usb8-array-to-base64-string
                (uuid:uuid-to-byte-array uuid)))
          0 22))

(defgeneric column-save-value (value type)
  (:documentation "Convert VALUE into the database's representation of TYPE")
  (:method (value (type (eql :string)))
    value)
  (:method (value (type (eql :keyword)))
    (and value (string value)))
  (:method (value (type (eql :yornp)))
    (if value "Y" "N"))
  (:method (value (type (eql :number)))
    value)
  (:method (value (type (eql :json)))
    (and value
         (with-output-to-string (*standard-output*)
           (%to-json value))))
  (:method (value (type (eql :uri)))
    (and value (etypecase value
                 (puri:uri (puri:render-uri value nil))
                 (string value))))
  (:method (value (type (eql :color24)))
    (and value (format nil "~6,'0x"
                       (color24-to-integer value))))
  (:method (value (type (eql :uuid)))
    (etypecase value
      (null nil)
      (string (uuid-string-to-base64 value))
      (uuid:uuid (uuid-to-base64 value))))
  (:method (value (type (eql :timestamp)))
    (and value (substitute #\Space #\Z (format-timestring nil value :timezone *utc-timezone*)))))

(defun base64-to-uuid (value)
  "Convert a BASE64 value into a UUID."
  (unless (emptyp value)
    (uuid:byte-array-to-uuid
     (cl-base64:base64-string-to-usb8-array
      (concatenate 'string value "==")))))

(let ((uuid (uuid:make-v4-uuid)))
  (assert (uuid:uuid= uuid (base64-to-uuid (uuid-to-base64 uuid))))
  (assert (equal (uuid-to-base64 uuid) (uuid-to-base64 uuid)))
  (assert (uuid:uuid= uuid (uri-to-uuid (uuid-to-uri uuid))))
  (assert (equalp (uuid:uuid-to-byte-array uuid)
                  (cl-base64:base64-string-to-usb8-array
                   (concatenate 'string (uuid-to-base64 uuid) "==")))))

(defgeneric column-load-value (value type)
  (:documentation "For a column of TYPE, interpret raw VALUE")
  (:method (value (type (eql :string)))
    value)
  (:method (value (type (eql :keyword)))
    (make-keyword value))
  (:method (value (type (eql :yornp)))
    (ecase (make-keyword value)
      (:y t) (:n nil)))
  (:method (value (type (eql :number)))
    (etypecase value
      (null nil)
      (integer value)
      (rational (format nil "~f" (coerce value 'float)))
      (real (format nil "~f" value))
      (t (error "Can't record number ~s?" value))))
  (:method (value (type (eql :json)))
    (and value
         (< 0 (length (the string value)))
         (jonathan.decode:parse value)))
  (:method (value (type (eql :uri)))
    (puri:parse-uri value))
  (:method (value (type (eql :color24)))
    (etypecase value
      (null nil)
      (integer (integer-to-color24 value))
      (string (parse-color24 value))))
  (:method (value (type (eql :uuid)))
    (base64-to-uuid value))
  (:method (value (type (eql :timestamp)))
    (let ((τ value))
      (etypecase τ
        (null nil)
        (integer (universal-to-timestamp τ))
        (string (if (equalp τ "0000-00-00")
                    nil
                    (parse-timestring (substitute #\T #\Space τ))))
        (vector (if (equalp τ #(48 48 48 48 45 48 48 45 48 48))
                                        ; 0000-00-00 in Unicode char-codes
                    nil
                    (parse-timestring
                     (substitute #\T #\Space (map 'string #'code-char τ)))))))))



(eval-when (:load-toplevel :execute :compile-toplevel)
  (defun column-load-mapping (column)
    "Map COLUMN from a database record into internal form.

Used in `DEFRECORD', qv."
    (let ((key (make-keyword (lisp-to-db-name (car column)))))
      (list (make-keyword (symbol-name (car column)))
            `(column-load-value (getf record ,key)
                                ,(make-keyword (symbol-name (second column)))))))

  (defun column-save-mapping (column)
    (let ((slot (first column)))
      `(column-save-value ,slot ,(make-keyword (symbol-name (second column))))))

  (defun column-normalizer (column)
    (let ((name (intern (symbol-name (car column)))))
      (list name
            (list 'and name
                  (ecase (make-keyword (symbol-name (second column)))
                    (:string `(typecase ,name (string ,name) (t (princ-to-string ,name))))
                    (:keyword `(make-keyword (string ,name)))
                    (:yornp `(when ,name t)) ; T or NIL
                    (:number `(etypecase ,name
                                (double-float (format nil "~f" ,name))
                                (number ,name)
                                (string (parse-number ,name))))
                    (:json name)        ; TODO
                    (:uri name)         ; TODO
                    (:color24 `(etypecase ,name
                                 (color24 ,name)
                                 (string (parse-color24 ,name))
                                 (integer (integer-to-color24 ,name))))
                    (:uuid `(etypecase ,name
                              (uuid:uuid ,name)
                              (vector (uuid:byte-array-to-uuid ,name))
                              (number (uuid:byte-array-to-uuid
                                       (integer-to-byte-vector ,name)))
                              (string (uuid:byte-array-to-uuid
                                       (cl-base64:base64-string-to-usb8-array
                                        (concatenate 'string ,name "=="))))))
                    (:timestamp `(etypecase ,name
                                   (timestamp ,name)
                                   (number (universal-to-timestamp ,name))
                                   (string (parse-timestring ,name))))))))))



(defun defrecord/load-record (name columns)
  `(defmethod load-record ((class (eql ',name)) record)
     (make-instance ',name
                    ,@(mapcan #'column-load-mapping columns))))

(defun arrange-columns+values-for-find (columns+values column-definitions)
  (when columns+values
    (loop for (column value) on columns+values by #'cddr
          for column-def = (assoc column column-definitions :test #'string=)
          do (unless column-def
               (error "Can't search on unknown column ~:(~a~); ~
columns are ~{~:(~a~)~^, ~}" column (mapcar #'car column-definitions)))
          append (list (lisp-to-db-name column)
                       (column-save-value value
                                          (make-keyword (symbol-name
                                                         (second column-def))))))))

(defun defrecord/find-record (name table columns)
  `(defmethod find-record ((class (eql ',name)) &rest columns+values)
     (or (refind-record ',name columns+values)
         (weakly-remember-record
          (load-record ',name (apply #'db-select-single-record
                                     ,table
                                     (arrange-columns+values-for-find
                                      columns+values ',columns)))))))

(defun defrecord/find-record/pull (name table columns)
  (declare (ignore table columns))
  `(defmethod find-record ((class (eql ',name)) &rest columns+values)
     (let ((all (apply #'find-records class columns+values)))
       (assert (= 1 (length all)))
       (first all))))

(defun defrecord/find-records (name table columns)
  `(defmethod find-records ((class (eql ',name)) &rest columns+values)
     (mapcar (lambda (record) (load-record ',name record))
             (apply #'db-select-records-simply ,table
                    (arrange-columns+values-for-find
                     columns+values ',columns)))))

(defun defrecord/find-records-by-sql (name database)
  `(defmethod find-records-by-sql ((class (eql ',name)) sql)
     (mapcar (lambda (record) (load-record ',name record))
             (db-select-all ,database sql))))

(defun defrecord/find-records/pull (name table columns)
  (declare (ignore table columns))
  `(defmethod find-records ((class (eql ',name)) &rest columns+values)
     (loop
       with solution = (pull-records ',name)
       for (column . value) on columns+values by #'cddr
       do (setf solution (remove-if-not (lambda (record)
                                          (equalp (slot-value record (intern (symbol-name column)))
                                                  value))
                                        solution))
       finally (return solution))))

(defun defrecord/before-save-normalize (name columns)
  `(defmethod before-save-normalize ((object ,name))
     (with-slots ,(mapcar #'car columns) object
       (setf ,@(mapcan #'column-normalizer columns)))))

(defun get-last-insert-id ()
  (let ((id-query (cl-dbi:prepare
                   *dbi-connection*
                   "SELECT LAST_INSERT_ID();")))
    (cl-dbi:execute id-query)
    (caar (cl-dbi:fetch-all id-query))))


(define-condition update-nil (condition) ())

(defun defrecord/save-record (name id-accessor database table columns )
  `(defmethod save-record ((object ,name))
     (with-dbi (,database)
       ,(when (string-equal (caar columns) "UUID")
          `(when (null (,id-accessor object))
             (setf (,id-accessor object) (uuid:make-v4-uuid))))
       (before-save-normalize object)
       (let* ((query (cl-dbi:prepare
                      *dbi-connection*
                      ,(format nil "INSERT INTO `~a` (~{`~a`~^, ~})~
~:* VALUES (~{?~*~^, ~}) ~
ON DUPLICATE KEY UPDATE  ~
~{`~a` = ?~^, ~};"
                               table
                               (mapcar (lambda (column)
                                         (lisp-to-db-name (car column)))
                                       (the proper-list columns))
                               (mapcar (lambda (column)
                                         (lisp-to-db-name (car column)))
                                       (the proper-list (rest columns)))))))
         (v:info :db "Saving ~a ∀ ~s=~s" (symbol-munger:lisp->english (type-of object))
                 ',(caar columns) (,id-accessor object))
         (with-slots ,(mapcar #'car columns) object
           (cl-dbi:execute query
                           (list ,@(mapcar #'column-save-mapping columns)
                                 ,@(mapcar #'column-save-mapping (rest columns))))))
       ,(when (string-equal (caar columns) "ID")
          `(setf (,id-accessor object) (get-last-insert-id))))
     object))

(defun defrecord/id-column-for (name columns id-column)
  (cond
    (id-column
     `(defmethod id-column-for ((type (eql ',name)))
        ',id-column))
    ((or (string-equal "ID" (caar columns))
         (string-equal "UUID" (caar columns)))
     `(defmethod id-column-for ((type (eql ',name)))
        ',(caar columns)))))

(defun defrecord/destroy-record (name id-accessor database table columns)
  (declare (ignore id-accessor))
  `(defmethod destroy-record ((object ,name))
     (with-dbi (,database)
       (let ((q (cl-dbi:prepare *dbi-connection*
                                ,(format nil "DELETE FROM ~a WHERE `~a` = ?"
                                         table
                                         (lisp-to-db-name (caar columns))))))
         (with-slots (,(caar columns)) object
           (cl-dbi:execute q (list ,(column-save-mapping (car columns)))))
         (let ((rows (cl-dbi:row-count *dbi-connection*)))
           (when (zerop rows)
             (signal 'update-nil))
           rows)))))

(defun defrecord/record= (name id-accessor)
  (let (($fname (intern (concatenate 'string (symbol-name name) "="))))
    `(defun ,$fname (a b &rest more)
       ,(format nil
                "Returns true if all arguments represent the same ~A record in the database.

Identity is determined by the ID column, ~A."
                (symbol-munger:lisp->english name)
                id-accessor)
       (if more
           (and (,$fname a b) (apply ',$fname a more))
           (,(case id-accessor
               (ID '=)
               (UUID 'UUID:UUID=)
               (t 'equal))
            (,id-accessor a) (,id-accessor b))))))

(defun defrecord/reload-record (name columns)
  (when (id-column-for name)
    (let ((id-accessor (intern (concatenate 'string (symbol-name name) "-"
                                            (symbol-name (id-column-for name))))))
      `(defmethod reload-record ((object ,name))
         (let ((instance (find-record ',name 
                                      ,(make-keyword (symbol-name (id-column-for name))) (,id-accessor object))))
           (setf ,@ (loop for column in columns
                          collecting (list (intern (concatenate 'string (symbol-name name) "-"
                                                                (symbol-name (car column))))
                                           'object)
                          collecting (list (intern (concatenate 'string (symbol-name name) "-"
                                                                (symbol-name (car column))))
                                           'instance)))
           object)))))

(defun defrecord/save-record-with-id-column (name database table columns)
  (when (id-column-for name)
    (let ((id-accessor (intern (concatenate 'string (symbol-name name) "-"
                                            (symbol-name (id-column-for name))))))
      `(,(defrecord/record= name id-accessor)
        ,(defrecord/save-record name id-accessor database table columns)
        ,(defrecord/destroy-record name id-accessor database table columns)))))

(defun defrecord/find-reference (name column)
  `(defmethod find-reference
       ((object ,name)
        (reference (eql ,(make-keyword (symbol-name (first column))))))
     (find-record ',(fourth column)
                  ,(make-keyword (id-column-for (fourth column)))
                  (,(intern (concatenate 'string (symbol-name name)
                                         "-"
                                         (symbol-name (first column))))
                    object))))

(defun defrecord/find-reference-columns (name columns)
  (when (find-if (lambda (column)
                   (< 2 (length (the proper-list column))))
                 (the proper-list columns))
    (cons 'progn
          (loop for column in columns
             when (and (= 4 (length (the proper-list column)))
                       (string-equal "REF" (third column)))
             collecting
               (defrecord/find-reference name column)))))

(defun defrecord/make-record (name)
  `(defmethod make-record ((class (eql ',name)) &rest columns+values)
     (let ((record (apply #'make-instance ',name columns+values)))
       (save-record record)
       record)))

(defun defrecord/column-to-json-pair (name basename column)

  (list (intern (symbol-munger:lisp->camel-case (first column)) :keyword)
        (list '%to-json
              (list (intern (concatenate 'string
                                         (symbol-name name) "-"
                                         (symbol-name (first column))))
                    basename))))

(defun defrecord/to-json (name columns)
  `(defmethod %to-json ((,name ,name))
     (%to-json
      (list :|isA| ,(symbol-munger:lisp->studly-caps name)
            ,@(mapcan
               (lambda (column)
                 (defrecord/column-to-json-pair name name column))
               columns)))))

(defun defrecord/invalidate-cache (name pull columns)
  (if pull
      `(defmethod invalidate-cache ((,name ,name))
         (setf (getf pull-records-cache ',name) nil))
      `(defmethod invalidate-cache ((,name ,name))
         (with-slots (,@(mapcar #'car columns)) ,name
           (erase-all-memcached-for
            ',name ,@(loop for column in columns
                           collect (make-keyword (string-downcase (car column)))
                           collect (car column)))))))



(defclass db-record () ())

(defmacro defrecord (name (database table &key pull id-column) &body columns)
  "Define a database-mapping object type NAME, for DATABASE and TABLE, with COLUMNS.

DATABASE  is the  symbolic name  of the  database, mapped  via `CONFIG';
eg, :friendly

TABLE is  the string table-name, exactly  as it exists in  the database;
eg, \"toots\"

PULL  is meant  to indicate  an infrequently-changed,  short table  (ie,
basically a  small enumeration) that  should be pulled into  local cache
up-front and referenced from there directly.

COLUMNS are a table of names, types, and foreign-key references, in the form:
 (LABEL TYPE &rest REFERENCE)

The LABEL  of a column is  mapped via `LISP-TO-DB-NAME'; it  is the Lisp
name which is essentially the same  as the SQL name, but with KEBAB-CASE
rather than snake_case.

When   present,  REFERENCE   is   the  symbol   REF   followed  by   the
record-type  (class) to  whose primary  key (ID  or UUID)  the reference
is made. NUMBER REF columns point to ID, UUID REF columns to UUID.

TYPE is one of the following:

@table @code
@item NUMBER
map to an integer or real column in the database
@item STRING
map to a CHAR, CHAR VARYING, or TEXT column, or ENUM
@item COLOR24
stored in the database as a 24-bit BINARY (3 bytes)
@item KEYWORD
map to a CHAR or CHAR VARYING column, or ENUM
@item UUID
stored as a 128-bit BINARY (16 bytes)
@item JSON
stored as a TEXT column, but parsed on loading via Jonathan
@item YORNP
a boolean, stored as (typically an enum) 'Y' or 'N'.
@item URI
stored as CHAR VARying or TEXT, parsed at load time as a PURI:URI.
@item TIMESTAMP
translates to a LOCAL-TIME:TIMESTAMP on loading.
@end table
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defclass ,name (db-record)
       (,@(mapcar
           (lambda (column)
             (destructuring-bind (col-name type &optional ref reference) column
               (declare (ignore ref reference))
               `(,col-name :type ,type :accessor 
                           ,(intern (concatenate 'string (symbol-name name) "-" (symbol-name col-name)))
                           :initarg ,(make-keyword (symbol-name col-name))
                           :initform nil)))
           columns)))
     (defmethod db-table-for ((class (eql ',name))) ,table)
     (defmethod database-for ((class (eql ',name))) (list :maria ,database))
     ,(defrecord/id-column-for name columns id-column)
     ,(defrecord/invalidate-cache name pull columns)
     ,(defrecord/make-record name)
     ,(defrecord/load-record name columns)
     ,(if (and nil pull)
          (defrecord/find-record/pull name table columns)
          (defrecord/find-record name table columns))
     ,(if (and nil pull)
          (defrecord/find-records/pull name table columns)
          (defrecord/find-records name table columns))
     ,(defrecord/find-records-by-sql name database)
     ,(defrecord/before-save-normalize name columns)
     ,@(defrecord/save-record-with-id-column name database table columns)
     ,(defrecord/reload-record name columns)
 ;;;,(defrecord/to-json name columns)
     ,(defrecord/find-reference-columns name columns)))

(defmethod save-record ((list cons))
  (warn "Got a list to SAVE-RECORD: ~s" list)
  (map nil #'save-record list))

(defgeneric reload-record (object)
  (:documentation "Reload the contents of OBJECT from the database"))
