(in-package :oliphaunt)

(defun sql-escape (string)
  "Simply replaces  ' with  '' in  strings (that's  paired/escape single
quotes). This is the  right thing to do for standard  SQL, but MySQL and
other servers might be naughtier than you'd like."
  (check-type string string)
  (regex-replace-all "\\'" string "''"))

(defgeneric ->sql (object)
  (:documentation "Convert an object into an SQL-escaped form.")
  (:method ((object (eql :null))) "NULL")
  (:method ((object (eql :true))) "TRUE")
  (:method ((object (eql :false))) "FALSE")
  (:method ((object (eql t))) "TRUE")
  (:method ((object string)) (concatenate 'string "'" (sql-escape object) "'"))
  (:method ((object integer)) (princ-to-string object))
  (:method ((object real)) (princ-to-string (* 1.0 object)))
  (:method ((object list)) (format nil "(~{~/sql/~^, ~})" object))
  (:method ((object null)) "NULL"))

(defun cl-user::sql (stream object colonp atp &rest parameters)
  "`FORMAT' ~/SQL/ printer. Handles strings, integers, and floating-point real numbers."
  (assert (not colonp) () "The colon modifier is not used for ~~/SQL/")
  (assert (not atp) () "The @ modifier is not used for ~~/SQL/")
  (assert (null parameters) ()
          "~~/SQL/ does not accept parameters; saw ~s" parameters)
  (princ (->sql object) stream))
