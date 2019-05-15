(in-package :oliphaunt)

(defun html-escape (string)
  "Escapes < and & from strings for safe printing as HTML (text node) content."
  (regex-replace-pairs '(("\\&" . "&amp;")
                         ("\\<" . "&lt;")) (typecase string
                         (string string)
                         (t (princ-to-string string)))))

(defun condition->html (c)
  "Formats a condition object as (relatively) pretty HTML."
  (format nil "<section class=\"error\">
<h2> A condition of type ~/html/ was signaled. </h2>
<h3>~/html/</h3>
<ol class=\"backtrace\">
~/html/
</ol>
</section>"
          (type-of c)
          c
          (with-output-to-string (s)
            (uiop/image:print-backtrace :condition c :stream s))))

(defun cl-user::html (stream object colonp atp &rest parameters)
  "`FORMAT' ~/HTML/ formatter which escapes < and &."
  (assert (not colonp))
  (assert (not atp))
  (assert (null parameters))
  (etypecase object
    (string (princ (html-escape object) stream))
    (integer (princ (html-escape (format nil "~d" object)) stream))
    (condition (princ (condition->html object) stream))
    (t (princ (html-escape (princ-to-string object)) stream))))
