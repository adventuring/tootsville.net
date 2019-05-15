(in-package :oliphaunt)

;;; TODO: pick from :vas-string-metrics, :mk-string-metrics

(defgeneric ensure-string (object)
  (:method ((string string)) string)
  (:method ((vector vector))
    (typecase (elt vector 0)
      (character (coerce vector 'string))
      (number (map 'string #'code-char vector))
      (t (princ-to-string vector))))
  (:method (object)
    (princ-to-string object)))

(defun strcat (&rest strings)
  (reduce (curry #'concatenate 'string)
          (mapcar (lambda (element)
                    (typecase element
                      (cons (reduce #'strcat element))
                      (string element)
                      (t (princ-to-string element))))
                  (remove-if #'null strings))))

(define-constant +whitespace+
    (coerce #+sbcl #(;; Defined in ASCII
                     #\Space #\Tab #\Page #\Linefeed #\Return #\Null
                     ;; defined in ISO-8859-*
                     #\No-Break_Space #\Reverse-Linefeed
                     ;; defined in Unicode
                     #\Ogham_space_mark #\Mongolian_Vowel_Separator
                     #\En_quad #\Em_quad #\En_Space #\Em_Space
                     #\Three-per-Em_Space #\Four-per-Em_Space
                     #\Six-per-Em_Space #\Figure_Space #\Punctuation_Space
                     #\Thin_Space #\Hair_Space
                     #\Zero_Width_Space #\Narrow_No-Break_Space
                     #\Medium_Mathematical_Space #\Ideographic_Space
                     #\Zero_Width_No-Break_Space)
            #+ccl
            #(#\u+20 #\u+9 #\u+C #\u+A #\u+D #\u+0 #\u+A0 #\u+8D
              #\u+1680 #\u+180E #\u+2000 #\u+2001 #\u+2002 #\u+2003 #\u+2004 #\u+2005
              #\u+2006 #\u+2007 #\u+2008 #\u+2009 #\u+200A #\u+200B #\u+202F #\u+205F
              #\u+3000 #\u+FEFF)
            #-(or sbcl ccl) #.(error "Need Unicode help")
            'simple-string)
  :test #'equal
  :documentation "A list of all whitespace chars in Unicode. Superset of
 the ASCII whitespace chars we expect to encounter.")


(define-constant +inline-whitespace+
    (coerce #+sbcl #(;; Defined in ASCII
                     #\Space #\Tab
                     ;; defined in ISO-8859-*
                     #\No-Break_Space
                     ;; defined in Unicode
                     #\Ogham_space_mark #\Mongolian_Vowel_Separator
                     #\En_quad #\Em_quad #\En_Space #\Em_Space
                     #\Three-per-Em_Space #\Four-per-Em_Space
                     #\Six-per-Em_Space #\Figure_Space #\Punctuation_Space
                     #\Thin_Space #\Hair_Space
                     #\Zero_Width_Space #\Narrow_No-Break_Space
                     #\Medium_Mathematical_Space #\Ideographic_Space
                     #\Zero_Width_No-Break_Space)
            #+ccl #(#\u+20 #\u+9 #\u+A0 #\u+1680 #\u+180E #\u+2000 #\u+2001 #\u+2002
                    #\u+2003 #\u+2004 #\u+2005 #\u+2006 #\u+2007 #\u+2008 #\u+2009 #\u+200A
                    #\u+200B #\u+202F #\u+205F #\u+3000 #\u+FEFF)
            #-(or sbcl ccl) #.(error "Need Unicode help")
            'simple-string)
  :test #'equal
  :documentation "A list of all whitespace chars in Unicode that occur
 \"on a line,\" i.e. excluding Null, Carriage Return, Linefeed, and
 Page Break. Superset of the ASCII whitespace chars we expect
 to encounter.")

(defun blank-string-p (string)
  (check-type string string)
  (or (zerop (length string))
      (every (rcurry #'find +whitespace+) string)))

(deftype blank-string ()
  `(and string
        (satisfies blank-string-p)))

(define-constant +often-naughty-chars+
    (coerce #+sbcl #(#\\ #\! #\| #\# #\$ #\% #\& #\?
                     #\{ #\[ #\( #\) #\] #\} #\= #\^ #\~
                     #\' #\quotation_mark #\` #\< #\> #\*
                     #\Space #\Tab #\Page #\Linefeed #\Return #\Null
                     #\No-Break_Space #\Reverse-Linefeed
                     #\Zero_Width_No-Break_Space)
            #+ccl #(#\u+5C #\u+21 #\u+7C #\u+23 #\u+24 #\u+25 #\u+26 #\u+3F
                    #\u+7B #\u+5B #\u+28 #\u+29 #\u+5D #\u+7D #\u+3D #\u+5E
                    #\u+7E #\u+27 #\u+22 #\u+60 #\u+3C #\u+3E #\u+2A #\u+20
                    #\u+9 #\u+C #\u+A #\u+D #\u+0 #\u+A0 #\u+8D #\u+FEFF)
            #-(or sbcl ccl) #.(error "Need Unicode help")
            'simple-string)
  :test #'equal
  :documentation "A list of characters which often have special
meanings (e.g. in the shell) and should usually be replaced or escaped
in some contexts.")


;;; Escaping strings using various strategies
(defun escape-with-char (char escape-char)
  (check-type char character)
  (check-type escape-char character)
  (coerce (list escape-char char) 'string))

(defun escape-by-doubling (char &optional _)
  (declare (ignore _))
  (check-type char character)
  (coerce (list char char) 'string))

(defun escape-url-encoded (char &optional _)
  (declare (ignore _))
  (check-type char character)
  (cond
    ((or (char<= #\a char #\z)
         (char<= #\A char #\Z)
         (char<= #\0 char #\9)
         (find char "-_.," :test #'char=))
     (coerce (vector char) 'string))
    ((char= #\space char) "+")
    (t (format nil "~{%~2,'0x~}"
               (coerce (babel:string-to-octets (string char)) 'list)))))

(defun string-escape-uri-fragment (string)
  (reduce (curry #'concatenate 'string) (map 'list #'escape-url-encoded string)))

(defun escape-octal (char &optional _)
  (declare (ignore _))
  (check-type char character)
  (format nil "~{\\0~o~}" (coerce
                           (babel:string-to-octets (string char))
                           'list)))

(defun escape-java (char &optional _)
  (declare (ignore _))
  (check-type char character)
  (assert (< (char-code char) #x10000) (char)
          "Cannot Java-encode characters whose Unicode codepoint is
 above #xFFFF. Character ~@C (~:*~:C) has a codepoint of #x~x."
          char (char-code char))
  (case char
    (#\Linefeed "\\n")
    (#\Return "\\r")
    (#\Page "\\f")
    (#\\ "\\\\")
    (otherwise
     (format nil "\\u~4,'0x" (char-code char)))))

(defun escape-c-style (char &optional _)
  (declare (ignore _))
  (check-type char character)
  (assert (< (char-code char) #x100) (char)
          "Cannot C-encode characters whose Unicode codepoint is
 above #x7f yet. Character ~@C (~:*~:C) has a codepoint of #x~x."
          char (char-code char))
  (case char
    (#\Linefeed "\\n")
    (#\Return "\\r")
    (#\Page "\\f")
    (#\\ "\\\\")
    (otherwise
     (format nil "\\x~2,'0x" (char-code char)))))

(defun escape-html (char &optional _)
  (declare (ignore _))
  (check-type char character)
  (format nil "&~d;" (char-code char)))

(defun string-escape (string &optional
                               (escape-chars +often-naughty-chars+)
                               (escape-with #\\)
                               (escape-function #'escape-with-char))
  "Escape a string in some of the more popular forms.

The STRING is compared, character-by-character, against ESCAPE-CHARS. When a
character in STRING is a member of ESCAPE-CHARS, it is passed through ESCAPE-FUNCTION with the additional parameter ESCAPE-WITH.

When ESCAPE-CHARS is NIL, every character in STRING is escaped.

When ESCAPE-CHARS is a vector of two non-negative integers, then every
character is escaped whose codepoint does NOT fall between the inclusive
range of those two characters, UNLESS that character is also CHAR= to
ESCAPE-WITH.

When ESCAPE-CHARS is a string, then only the characters in that string
are escaped. The default is a set of typically-troublesome characters in
the ASCII range, including whitespace.

For example, to octal-escape all characters outside the range of printable ASCII characters, you might use:

\(string-escape string #(#x20 #x7e) #\\ #'escape-octal)
"
  (check-type string string)
  (check-type escape-chars (or null (vector (integer 0 *) 2) string))
  ;; (check-type escape-function (function (character t) string))
  (let ((output (make-array (length string) :adjustable t
                            :fill-pointer 0 :element-type 'character)))
    (flet ((encode-char (char)
             (doseq (out-char (the string (funcall escape-function char
                                                   escape-with)))
               (vector-push-extend out-char output))))
      (etypecase escape-chars
        (null (concatenate 'string
                           (mapcar (rcurry escape-function
                                           escape-with) string)))
        (string (loop for char across string
                   do (cond
                        ((find char escape-chars :test #'char=)
                         (encode-char char))
                        (t
                         (vector-push-extend char output)))))
        (vector (let ((range-start (code-char (elt escape-chars 0)))
                      (range-end (code-char (elt escape-chars 1))))

                  (loop for char across string
                     do (cond
                          ((or (not (char< range-start
                                           char
                                           range-end))
                               (char= escape-with char))
                           (encode-char char))
                          (t
                           (vector-push-extend char output))))))))
    output))


(assert (equal (string-escape "C:/WIN" "/" #\*) "C:*/WIN"))
(assert (equal (string-escape "BLAH 'BLAH' BLAH" "'" nil
                              #'escape-by-doubling)
               "BLAH ''BLAH'' BLAH"))
(assert (equal (string-escape "Foo&Bar" "&" nil
                              #'escape-url-encoded)
               "Foo%26Bar"))
(assert (equal (string-escape "☠" #(#x20 #x7e) nil
                              #'escape-url-encoded)
               "%E2%98%A0"))
(assert (equal (string-escape "boo$" "$" nil
                              #'escape-octal)
               "boo\\044"))
(assert (equal (string-escape "Blah <p>" "<&>" nil
                              #'escape-html)
               "Blah &60;p&62;"))


;;; Relatively complex human-language handling
(defvar *language*)
(defvar *dialect*)
(defvar *language-coding*)

(defun get-lang ()
  (unless (and (boundp '*language*)
               (symbol-value '*language*))
    (setf *language* (keywordify
                      (first (split-sequence
                              #\_ (uiop/os:getenv "LANG"))))))
  (unless (and (boundp '*dialect*)
               (symbol-value '*dialect*))
    (setf *dialect* (keywordify
                     (first (split-sequence
                             #\. (second (split-sequence
                                          #\_ (uiop/os:getenv "LANG"))))))))
  (unless (and (boundp '*language-coding*)
               (symbol-value '*language-coding*))
    (setf *language-coding* (keywordify
                             (second (split-sequence
                                      #\. (uiop/os:getenv "LANG"))))))
  (values *language*
          *dialect*
          *language-coding*))

(defun letter-case (string)
  "Determine the case of a STRING, by returning the symbol of the CL function that describes it. If the string is in all
upper-case,  returns 'STRING-UPCASE;  likewise, for  'STRING-DOWNCASE, or  'STRING-CAPITALIZE. If  none of  these apply,
returns 'IDENTITY."
  (cond ((equal string (string-upcase string))
         'string-upcase)
        ((equal string (string-downcase string))
         'string-downcase)
        ((equal string (string-capitalize string))
         'string-capitalize)
        (t 'identity)))

(defun irish-consonant-p (letter)
  (member (char-downcase letter)
          '(#\b #\c #\d #\f #\g #\j #\l #\m #\n #\p #\r #\s #\t #\v)))

(defun irish-vowel-p (letter)
  (member (char-downcase letter)
          '(#\ɑ #\a #\á #\e #\é #\i #\í #\ı #\o #\ó #\u #\ú)))

(defun substitute-map (map word)
  "Given a p-list MAP, where the keys are characters, replace them with the associated values of the p-list."
  (map 'string (lambda (char)
                 (if-let ((new (getf map char)))
                   new
                   char))
       word))

(defun string-begins (prefix string &key (test #'string-equal))
  "Returns a generalized Boolean indicating whether the given PREFIX matches the beginning of STRING.

TEST  is called  with STRING,  PREFIX, ':end1,  and  the length  of the  PREFIX.  This is  the signature  of (at  least)
`STRING-EQUAL' (the default) or `STRING='"
  (check-type string string)
  (check-type prefix string)
  (check-type test funcallable)
  (let ((prefix-length (length prefix)))
    (and (>= (length string) prefix-length)
         (funcall test string prefix
                  :end1 prefix-length))))

(assert (string-begins "foo" "foobar"))
(assert (string-begins "foo" "Foobar"))
(assert (not (string-begins "foo" "Foobar" :test #'string=)))

(defun string-ends (suffix string &key (test #'string-equal))
  "Returns a generalized Boolean indicating whether the given SUFFIX matches the end of STRING.

TEST is  called with STRING,  SUFFIX, ':start1, and  the length  of the string  less the length  of SUFFIX. This  is the
signature of (at least) `STRING-EQUAL' (the default) or `STRING='"
  (check-type string string)
  (check-type suffix string)
  (check-type test funcallable)
  (let ((suffix-length (length suffix)))
    (and (>= (length string) suffix-length)
         (funcall test string suffix
                  :start1 (- (length string) suffix-length)))))

(assert (string-ends "bar" "foobar"))
(assert (string-ends "bar" "FOOBAR"))
(assert (not (string-ends "bar" "FOOBAR" :test #'string=)))

(defun string-beginning (string prefix &key (test #'string-equal))
  (string-begins prefix string :test test))
(defun string-ending (string suffix &key (test #'string-equal))
  (string-ends suffix string :test test))

(defmacro string-ends-with-case (string &body clauses)
  (let ((s (gensym "STRING")))
    `(let ((,s ,string))
       (cond
         ,@(loop for pair in clauses
              for match = (car pair)
              for body = (cdr pair)
              collecting (cond
                           ((member match '(otherwise t))
                            `(t ,@body))
                           ((listp match)
                            `((member ,s ,(list 'quote (if (symbolp (car match))
                                                           (eval match)
                                                           match))
                                      :test #'string-ending)
                              ,@body))
                           (t `((string-ends ,match ,s)
                                ,@body))))))))




(defun string-fixed (string target-length &key (trim-p t)
                                               (pad-char #\Space))
  "Ensure that the string is precisely the length provided, right-padding with PAD-CHAR (Space).
If the string is too long, it will be truncated. Returns multiple
values: the trimmed string, and the difference in length of the new
string. If the second value is negative, the string was truncated."
  (check-type string string)
  (check-type target-length (integer 1 *))
  (check-type pad-char character)
  (let* ((trimmed (if trim-p
                      (string-trim +whitespace+ string)
                      string))
         (change-length (- target-length (length trimmed))))
    (values (cond
              ((plusp change-length)
               (concatenate 'string trimmed
                            (make-string change-length
                                         :initial-element pad-char)))
              ((minusp change-length)
               (subseq trimmed 0 target-length))
              (t
               trimmed))
            change-length)))



(assert (equal (string-fixed "Q" 5) "Q    "))
(assert (equal (string-fixed "  Q  " 5) "Q    "))
(assert (equal (string-fixed "  Q  " 5 :trim-p nil) "  Q  "))
(assert (equal (string-fixed "QJJJJJ" 5 ) "QJJJJ"))
(multiple-value-bind (string change)
    (string-fixed "Q" 5)
  (assert (= 4 change))
  (assert (equal "Q    " string)))
(multiple-value-bind (string change)
    (string-fixed "QJJJJJ" 5)
  (assert (= -1 change))
  (assert (equal "QJJJJ" string)))
(multiple-value-bind (string change)
    (string-fixed "QQQQQ" 5)
  (assert (= 0 change))
  (assert (equal "QQQQQ" string)))



(defpackage roman/$trash)

(defun intern$ (string)
  (intern string :roman/$trash))

(defun string-case/literals% (compare clauses)
  (let ((instance (gensym "STRING-CASE")))
    `(let ((,instance ,compare))
       (cond
         ,@(mapcar (lambda (clause)
                     (cond ((or (eql t (car clause))
                                (eql 'otherwise (car clause)))
                            `(t ,@(cdr clause)))
                           ((consp (car clause))
                            `((or ,@(mapcar (lambda (each)
                                              `(string= ,instance ,each))
                                            (car clause)))
                              ,@(cdr clause)))
                           (t
                            `((string= ,instance ,(car clause)) ,@(cdr clause)))))
                   clauses)))))


(defun string-case/interning% (compare clauses)
  (let ((instance (gensym "STRING-INTERNED")))
    `(let ((,instance (intern$ ,compare)))
       (case ,instance
         ,@(mapcar (lambda (clause)
                     (cond
                       ((or (eql t (car clause))
                            (eql 'otherwise (car clause)))
                        `(t ,@(cdr clause)))
                       ((consp (car clause))
                        `(,(mapcar #'intern$ (car clause))
                           ,@(cdr clause)))
                       (t
                        `(,(intern$ (car clause)) ,@(cdr clause)))))
                   clauses)))))


(defparameter *--interning-better-breakpoint* 25)

(let ((interning-better-breakpoint
       ;; Determine about how many cases there need to be, for interning to be
       ;; faster than STRING=
       (or
        (when (boundp '*--interning-better-breakpoint*)
          (if (numberp *--interning-better-breakpoint*)
              *--interning-better-breakpoint*
              (prog1
                  #+sbcl sb-ext:long-float-positive-infinity
                  #-sbcl most-positive-fixnum)))
        (flet ((make-random-string (string-length)
                 (format nil "~{~C~}"
                         (loop for char from 1 upto (1+ string-length)
                            collecting (code-char (+ 32 (random 95)))))))
          (format *trace-output* "~&;; timing STRING-CASE implementations …")
          (let ((trials
                 (loop for trial-count from 1 to (* 5 (+ 5 (random 5)))
                    for num-repeats = (* 1000 (+ 2 (random 4)))
                    collecting
                      (loop for num-cases from 1 by (1+ (random 2))
                         for stuff = (loop for case from 1 upto num-cases
                                        for string = (make-random-string (+ 4 (random 28)))
                                        collect (list string :nobody))
                         for expr/literal = (compile 'expr/literal
                                                     (list 'lambda '(x)
                                                           (funcall #'string-case/literals%
                                                                    'x stuff)))
                         for expr/interning = (compile 'expr/interning
                                                       (list 'lambda '(x)
                                                             (funcall #'string-case/interning%
                                                                      'x stuff)))
                         for test-string = (make-random-string 40)
                         for cost/literal = (progn
                                              (trivial-garbage:gc)
                                              (let ((start (get-internal-real-time)))
                                                (dotimes (i num-repeats)
                                                  (funcall expr/literal test-string))
                                                (- (get-internal-real-time) start)))
                         for cost/interning = (progn
                                                (trivial-garbage:gc)
                                                (let ((start (get-internal-real-time)))
                                                  (dotimes (i num-repeats)
                                                    (funcall expr/interning test-string))
                                                  (- (get-internal-real-time) start)))

                         ;; do (format  *trace-output* "~&;; STRING-CASE
                         ;;            Cost for ~D (~:D×): interning: ~F
                         ;;            literals:      ~F"      num-cases
                         ;;            num-repeats        cost/interning
                         ;;            cost/literal)
                         when (< cost/interning cost/literal)
                         return num-cases))))
            (let ((average (round (/ (apply #'+ trials) (length trials)))))
              (format *trace-output* "~&;;; STRING-CASE trials done; sweet spot is about ~R case~:P after ~R trial~:P"
                      average (length trials))
              average))))))

  (defmacro string-case (compare &body clauses)
    "Like a CASE expression, but using STRING= to campare cases.

Example:

\(STRING-CASE FOO ((\"A\" (print :A)) (\"B\" (print :B)) (t (print :otherwise)) "
    (if (> interning-better-breakpoint (length clauses))
        (string-case/literals% compare clauses)
        (string-case/interning% compare clauses))))



(defun keywordify (word)
  "Converts a C-style, possible camelCased or snake_cased name, into a keyword."
  (etypecase word
    (string
     (make-keyword
      (substitute #\- #\_
                  (symbol-name (cffi:translate-camelcase-name word)))))
    (symbol (make-keyword (string word)))))

(defun keyword* (word)
  "If WORD is all caps or all lower-case, interns it as an all-caps keyword. Otherwise, preserve its (mixed) case."
  (check-type word (or string symbol))
  (if (or (string-equal word (string-upcase word))
          (string-equal word (string-downcase word)))
      (make-keyword (string-upcase word))
      (make-keyword word)))

(defun lc-string-syms (token)
  "Returns one or more downcased strings taken from a token or list of tokens."
  (cond
    ((symbolp token)
     (string-downcase (symbol-name token)))
    ((consp token)
     (mapcar #'lc-string-syms token))
    (t (string-downcase (princ-to-string token)))))

(defmacro $$$ (&rest list-of-strings)
  "Typically used to provide a list  of strings as symbols; returns them
as downcased strings. Similar to Perl's qw// operator."
  (list 'quote (mapcar #'lc-string-syms list-of-strings)))

(defun char-string (char)
  "Coërce CHAR into a one-character-long string."
  (check-type char character)
  (coerce (vector char) 'string))

(defun join (joiner list-of-strings)
  "Join LIST-OF-STRINGS by injecting JOINER between each element."
  (let ((joiner (etypecase joiner
                  (string joiner)
                  (character (char-string joiner))
                  (symbol (symbol-name joiner)))))
    (reduce (lambda (a b)
              (concatenate 'string a joiner b))
            list-of-strings)))

(defun repeat (count object)
  "Creates a list with COUNT instances of OBJECT, or with the results of calling OBJECT COUNT times, if OBJECT satisfies
a `FUNCTIONP'"
  (if (functionp object)
      (loop repeat count collecting (funcall object))
      (make-list count :initial-element object)))

(defun regex-replace-pairs (pairs-alist string)
  "Given  PAIRS-ALIST,  in  which  each  CAR  is  a  regex,  and  each  CDR  is  a  replacement,  performs  sequentially
`REGEX-REPLACE-ALL' with each pair, returning STRING after all alterations have been peformed upon it."
  (check-type string string)
  (check-type pairs-alist cons)
  (loop
     for (from . to) in pairs-alist
     do (setf string (regex-replace-all from string to))
     finally (return string)))



(defun first-paragraph-of (file &optional (max-lines 10))
  (strcat
   (with-open-file (stream file :direction :input)
     (loop
        with seen = 0
        for line = (string-trim " #;/*" (read-line stream nil #\¶))

        for blank = (zerop (length line))

        until (or (and (> seen 1) blank)
                  (>= seen max-lines))

        unless blank do (incf seen)


        when (not blank)
        collect line
        and
        collect (when (= seen max-lines) (string #\…))
        and collect (string #\Newline)))))



(defun error-or-nil-p (x)
  (or (null x)
      (eql :error x)))
(deftype error-or-nil ()
  '(satisfies error-or-nil-p))



(defun 36r (figure)
  "Simply format FIGURE as a base-36 figure in upper-case, without a radix sigil.

eg, 10 → \"A\""
  (check-type figure (integer 0 *))
  (format nil "~@:(~36r~)" figure))

(defun parse-bignum (string &key (if-not-bignum nil))
  (check-type if-not-bignum error-or-nil)
  (let ((string (string-trim '(#\Space #\Tab) string)))
    (cond
      ((zerop (length string)) nil)

      ((and (= 1 (count #\. string))
            (every (lambda (ch)
                     (or (digit-char-p ch)
                         (eql #\. ch))) string))
       (let* ((decimal (position #\. string))
              (whole (parse-integer (subseq string 0 decimal)))
              (fraction$ (subseq string (1+ decimal))))
         (+ whole (/ (parse-integer fraction$) (expt 10 (length fraction$))))))

      ((every #'digit-char-p string)
       (parse-integer string))

      (t nil))))

(defun maybe-numeric (string)
  (if-let (numeric (parse-bignum string))
    numeric
    (let ((pos (- (length string) 3)))
      (if (and (plusp pos)
               (equal " kB" (subseq string pos)))
          (* 1024 (parse-integer (subseq string 0 pos)))
          string))))

(defun parse-decimal (string)
  "Parses a simple decimal  number. Accepts optional - sign (but  not +) and does not attempt  to understand such things
as, eg, scientific notation or the like."
  (check-type string string)
  (if (find #\. string)
      (let* ((decimal (search "." string))
             (units (subseq string 0 decimal))
             (fractional (subseq string (1+ decimal)))
             (negativep (eql #\- (elt string 0))))
        (* 1.0
           (+ (parse-integer units)
              (if (plusp (length fractional))
                  (* (parse-integer fractional)
                     (if negativep -1 1)
                     (expt 10 (- (length fractional))))
                  0))))
      (parse-integer string)))

(defun remove-commas (string)
  "Useful for parsing figures. Simply removes commas, and nothing more."
  (check-type string string)
  (remove-if (curry #'char= #\,) string))

(defun parse-money (string)
  "Parses a monetary amount, taking into account $ or ¢ signs. Assumes dollars, unless the string contains a ¢ sign."
  (check-type string string)
  (if (find #\¢ string :test #'char=)
      (* .01 (parse-decimal (remove-commas (string-trim " ¢" string))))
      (parse-decimal (remove-commas (string-trim " $" string)))))

(defun numeric (x)
  "If given a number, returns it; if given a string representing a number, returns the result of calling `PARSE-DECIMAL'
on it. "
  (check-type x (or string number))
  (etypecase x
    (number x)
    (string (parse-decimal x))))

(defun as-number (n)
  "Returns N as  a number; parses it  as money if it were  a string, so ¢ has  the desired effect. Does  not handle more
complex forms. If N is already a NUMBER, does nothing."
  (check-type n (or string number))
  (etypecase n
    (number n)
    (string (parse-money n))))


;;; String←file functions

(defun split-and-collect-line (line &optional (split-char #\Space)
                                              (filter #'string-upcase))
  (let ((split (position split-char line)))
    (when split
      (let ((key-part (subseq line 0 split)))
        (list (if (every #'digit-char-p key-part)
                  (parse-integer key-part)
                  (make-keyword
                   (funcall filter
                            (string-trim
                             '(#\Space #\Tab)
                             (substitute #\- #\_
                                         (substitute #\- #\(
                                                     (substitute #\Space #\)
                                                                 key-part)))))))
              (let* ((rest-of-line (subseq line (1+ split)))
                     (numeric (parse-bignum rest-of-line)))
                (if numeric
                    numeric
                    rest-of-line)))))))

(defun split-and-collect-file (file &optional (split-char #\Space)
                                              (filter #'string-upcase))
  (handler-bind
      ((simple-file-error (lambda (c)
                            (return-from split-and-collect-file c))))
    (with-open-file (reading file :direction :input)
      (loop for line = (read-line reading nil nil)
         while line
         when line
         append (split-and-collect-line line split-char filter)))))

(defun collect-file-lines (file &optional (record-end-char #\Newline))
  (let (lines
        (line (make-array 72 :element-type 'character :adjustable t :fill-pointer 0)))
    (with-open-file (reading file :direction :input)
      (loop for char = (read-char reading nil nil)
         while char
         do (if (char= record-end-char char)
                (progn (push (copy-seq line) lines)
                       (setf (fill-pointer line) 0))
                (vector-push-extend char line 16)))
      (when (plusp (length line))
        (push lines line)))
    (nreverse lines)))


(defun collect-file (file)
  (handler-bind
      ((simple-file-error (lambda (c)
                            (return-from collect-file c))))
    (let ((contents (string-trim '(#\Space #\Tab #\Linefeed #\Return)
                                 (alexandria:read-file-into-string file))))
      (maybe-numeric contents))))

(defun collect-file-tabular (file &optional (tab-char #\Tab) (record-char #\Newline))
  (let ((contents (alexandria:read-file-into-string file)))
    (mapcar (lambda (row)
              (mapcar #'maybe-numeric row))
            (mapcar (curry #'split-sequence tab-char)
                    (split-sequence record-char
                                    contents)))))

(defun maybe-alist-row (string &optional (= #\=))
  (cond ((and (every #'alpha-char-p string)
              (equal (string-downcase string) string))
         (make-keyword (string-upcase string)))

        ((find = string)
         (let ((=pos (position = string)))
           (cons (make-keyword (string-upcase (subseq string 0 =pos)))
                 (maybe-numeric (subseq string (1+ =pos))))))

        (t string)))

(defun maybe-alist-split (string &optional (= #\=) (record-char #\,))
  (mapcar (rcurry #'maybe-alist-row =)
          (split-sequence record-char string)))


(defun for-all (&rest predicates)
  (lambda (&rest args)
    (loop for predicate in predicates
       unless (apply predicate args)
       do (return nil)
       finally (return t))))

(defun for-any (&rest predicates)
  (lambda (&rest args)
    (loop for predicate in predicates
       when (apply predicate args)
       do (return t)
       finally (return nil))))

(defun membership (set)
  (rcurry #'member set))



(defun c-style-identifier-p (string)
  (and (let ((first (elt string 0)))
         (or (alpha-char-p first)
             (eql #\_ first)))
       (every (for-any #'alphanumericp
                       (membership '(#\_ #\$))) string)))

(deftype c-style-identifier ()
  '(satisfies c-style-identifier-p))
