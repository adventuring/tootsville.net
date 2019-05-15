(in-package :oliphaunt)

;;; EDN output (to Clojure)
(defun schemey-record (record)
  "Convert  a  plist into  a  sort  that Scheme/Clojure  would  like,  probably with  some  crap  being translated  from
MySQL crap."
  (mapplist (key value) record
    (list (make-keyword (field-?-p key))
          (if (and (char= #\? (last-elt (string (field-?-p key))))
                   (member value '(1 0 t nil)))
              (case value
                ((0 nil) :false)
                ((1 t) :true))
              value))))

;;; NB.  JSON seems  easier to  work with,  with ClojureScript,  so this
;;; isn't actually being used right now.
(defvar *edn-pretty-indent* "  ")

(defgeneric ->edn (object)
  (:method ((object (eql t))) "true")
  (:method ((object (eql :true))) "true")
  (:method ((object (eql :false))) "false")
  (:method ((object null)) "nil")
  (:method ((object symbol)) (concatenate 'string #(#\:)
                                          (string-downcase (symbol-name object))))
  (:method ((object string)) (concatenate 'string
                                          #(#\")
                                          (regex-replace-all "\\\"" object "\\\"")
                                          #(#\")))
  (:method ((object integer)) (princ-to-string object))
  (:method ((object real)) (princ-to-string (* 1.0 object)))
  (:method ((object vector))
    (format nil
            (concatenate 'string
                         "[~<~%" *edn-pretty-indent* "~1:;~{~/edn/~>~^, ~}]")
            (coerce object 'list)))
  (:method ((object list))
    (if (plist-p object)
        (format nil (concatenate 'string
                                 "{~<~%" *edn-pretty-indent* "~1:;~{~/edn/ ~/edn/~>~^, ~}}")
                object)
        (->edn (coerce object 'vector)))))

(defun cl-user::edn (stream object colonp atp &rest parameters)
  "`FORMAT' ~/EDN/  formatter which handles  sexp→EDN output with  a few
optimizations.  One  “gotcha”  is  that most  lists  are  translated  to
vectors,  but lists  that  appear to  be plists  with  keyword keys  are
instead translated to Clojure maps."
  (assert (not colonp))
  (assert (not atp))
  (assert (null parameters))
  (let ((*edn-pretty-indent* (concatenate 'string *edn-pretty-indent* "     ")))
    (princ (->edn object) stream)))
