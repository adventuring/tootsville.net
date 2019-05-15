(in-package :oliphaunt)

(defun plist-json (plist)
  (labels ((pre-jsonify (plist)
             (loop for (key value) on plist by #'cddr
                appending (list (etypecase key
                                  (symbol (substitute #\_ #\- (string-downcase (symbol-name key))))
                                  (string key)
                                  (number key))
                                (if (consp value)
                                    (pre-jsonify value)
                                    value)))))
    (st-json:write-json-to-string
     (apply #'st-json:jso (pre-jsonify plist)))))


#+(or)
(defun json-plist (string)
  (labels ((post-plist (plist)
             (loop for (key value) on plist by #'cddr
                appending (list (etypecase key
                                  (string (keywordify key))
                                  (number key)
                                  (symbol key))
                                (if (consp value)
                                    (post-plist value)
                                    value)))))
    #. (warn "incomplete function ~s" '(post-plist (alist-plist (st-json::jso-alist |...| ))))))


(defmethod st-json:read-json ((nul null) &optional junk-allowed-p)
  (declare (ignore junk-allowed-p))
  nil)

(defmethod st-json:read-json ((list cons) &optional junk-allowed-p)
  (declare (ignore junk-allowed-p))
  (mapcar #'st-json:read-json list))


;;; JSON output
(defun jso-escape (string)
  "Escapes #\", #\', and #\newline in strings, to armour them for embedding in JSON"
  (regex-replace-pairs '(("\\\\" . "\\\\")
                         ("\\\"" . "\\\"")
                         ("\\n" . "\\n")) string))

(defvar *json-pretty-indent* "  ")

(defgeneric ->json (object)
  (:method ((object (eql :true))) "true")
  (:method ((object (eql :false))) "false")
  (:method ((object (eql t))) "true")
  (:method ((object null)) "null")
  (:method ((object symbol))
    (format nil "\"~a\"" (if (string= (string-upcase object) (string object))
                             (string-downcase object)
                             object)))
  (:method ((object string))
    (format nil "\"~a\"" object))
  (:method ((object integer))
    (princ-to-string object))
  (:method ((object real))
    (princ-to-string (* 1.0 object)))
  (:method ((object vector))
    (format nil (concatenate
                 'string
                 "[~{~<~%" *json-pretty-indent* "~1:;~/json/~>~^, ~}]")
            (coerce object 'list)))

  (:method ((object cons))
    (if (plist-p object)
        (format nil (concatenate
                     'string
                     "{~{~<~%" *json-pretty-indent* "~1:;~/json/: ~/json/~>~^, ~}}")
                object)
        (->json (coerce object 'vector))))
  (:method ((object t))
    (format nil "~a" object)))

(defun cl-user::json (stream object colonp atp &rest parameters)
  (assert (not colonp))
  (assert (not atp))
  (assert (null parameters))
  (let ((*json-pretty-indent* (concatenate 'string *json-pretty-indent* "     ")))
    (princ (->json object) stream)))
