(in-package :oliphaunt)

;;; Handle reading Roman numerals (up to 4,999) to complement ~@:r

(defun roman-numeral (stream number atp colonp)
  (declare (ignore atp colonp))
  (write-string (presentation-roman-numeral (format nil "~@r" number)) stream))

(defun proper-roman-numeral (char)
  "Given an ASCII character, return the Unicode Roman numeral code-point
that     it     resembles;     eg,      for     #\C     this     returns
#\ROMAN_NUMERAL_ONE_HUNDRED."
  (case (char-upcase char)
    (#\I #\Ⅰ)
    (#\V #\Ⅴ)
    (#\G #\ↅ)
    (#\X #\Ⅹ)
    (#\L #\Ⅼ)
    (#\C #\Ⅽ)
    (#\D #\Ⅾ)
    (#\M #\Ⅿ)
    (otherwise nil)))

(defun presentation-roman-numeral (string)
  (regex-replace-pairs '(("ⅠⅠ" . "Ⅱ")
                         ("ⅡⅠ" . "Ⅲ")
                         ("ⅢⅠ" . "Ⅳ")
                         ("ⅠⅤ" . "Ⅳ")
                         ("ⅠⅤⅠ" . "Ⅴ")
                         ("ⅤⅠ" . "Ⅵ")
                         ("ⅤⅡ" . "Ⅶ")
                         ("ⅤⅢ" . "Ⅷ")
                         ("ⅠⅩ" . "Ⅸ")
                         ("ⅩⅠ" . "Ⅺ")
                         ("ⅩⅡ" . "Ⅻ")
                         ("ⅩⅩⅩⅩ" . "ⅩⅬ")
                         ("ⅩⅬⅩ" . "Ⅼ"))
                       (map 'string #'proper-roman-numeral string)))

(defun roman-numeral-value (char)
  "Return the numeric value of an Unicode Roman numeral."
  (case char
    (#\Ⅰ 1)
    (#\Ⅱ 2)
    (#\Ⅲ 3)
    (#\Ⅳ 4)
    (#\Ⅴ 5)
    (#\Ⅵ 6)
    (#\ↅ 6)
    (#\Ⅶ 7)
    (#\Ⅷ 8)
    (#\Ⅸ 9)
    (#\Ⅹ 10)
    (#\Ⅺ 11)
    (#\Ⅻ 12)
    (#\Ⅼ 50)
    (#\ↆ 50)
    (#\Ⅽ 100)
    (#\Ↄ 100)
    (#\Ⅾ 500)
    (#\Ⅿ 1000)
    (#\ↀ 1000)
    (#\ↁ 5000) ; NB. Neither OLIPHAUNT nor SBCL:FORMAT ~:@R will write 5000+
    (#\ↂ 10000)
    (#\ↇ 50000)
    (#\ↈ 100000)
    (nil nil)
    (otherwise (roman-numeral-value (proper-roman-numeral char)))))

(defun parse-roman-numeral (string)
  "Evaluate  a   string,  returning  its   value  as  a   Roman  number.
Assumes that the string follows typical  rules, and may yield results of
questionable value  on malformed  strings. Functions with  Unicode Roman
numeral codepoints  like #\ROMAN_NUMERAL_FIVE  as well as  Latin letters
that approximate them (as may be produced by `FORMAT' ~:@R)."
  (loop for char across string
     for position from 0
     for value = (roman-numeral-value char)
     for preceding = (when (plusp position)
                       (roman-numeral-value (elt string (1- position))))
     unless value do (error 'reader-error)
     summing (+ (if (and preceding (< preceding value))
                    (- (* 2 preceding))
                    0)
                value)))
