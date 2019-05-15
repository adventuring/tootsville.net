(in-package :oliphaunt)

(require :prepl)
(require :local-time)
(require :trivial-garbage)

(defpackage :oliphaunt-user
  (:nicknames :romance2-user :romans-user :romance-ii-user :romance-user)
  (:use :cl :oliphaunt)
  (:export #:help #:hello #:bye))


;; Don't be  rude: We only  claim the nickname  USER if nobody  else has
;; lain claim to it before us.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "USER")
    (rename-package :oliphaunt-user
                    :oliphaunt-user
                    (cons "USER"
                          (package-nicknames :oliphaunt-user)))))

(defvar *user-ident* (make-hash-table
                      #+sbcl :weakness
                      #+sbcl :key :test 'equal))

(defun user-ident (&optional (new-name nil new-name-?))
  (prog1 (gethash (current-thread) *user-ident*)
    (when new-name-?
      (setf (gethash (current-thread) *user-ident*) new-name))))

(defconstant f nil
  "In USER, F means NIL for user-friendly'ish false values.")

(defgeneric repl-help (keyword)
  (:documentation "The REPL-HELP methods allow arbitrary text pages to
be added to the not-very-nice online help system.

TODO: Replace the online help system with a nice, hypertext
Info viewer."))

(defmethod repl-help :around (keyword)
  (format t (call-next-method)))

(defmethod repl-help ((keyword (eql :repl)))
  (prepl::help-cmd))

(defmethod repl-help ((keyword (eql :comm))) "
Romance Ⅱ: Operations Communications

Use these functions in package CAESAR for messaging. (e.g. to invoke
WHO you might type (CAESAR:WHO) or (IN-PACKAGE :CAESAR) (WHO) ...)

WHO: Who is online throughout the sysplex? (WHO :CLASS :OPER) by
default; (WHO :CLASS :AGENT) (WHO :CLASS :USER) (WHO :CLASS :ALL) for
various types of connection.

")
(defmethod repl-help ((keyword (eql :intro))) "

 Romance Ⅱ Read-Eval-Print-Loop (REPL) Help

CAUTION: Joining a live game world with this REPL is godlike power. Be
very careful.

 • For help with using the REPL, type :HELP or (HELP :REPL)

 • For copyright information, type (COPYRIGHTS) for brief,
 or (COPYRIGHTS T) for full details.

 • If you enter the debugger, and are presented with some restarts,
 choose a restart with :CONTINUE (restart) from the list
 presented. (The debugger prompt has a preceding number, usually [1],
 before your package prompt.) You can also use :ABORT to kill the
 function you had started and return to the REPL. — e.g. to choose
 restart [1], you would type: :CONTINUE 1 (with the leading \":\")

 • Use (HELP :COMM) to learn  about communicating with other operators
 through the REPL.

 • Use (HELP :SYSOP) to learn about systems administration tasks.

 • Use (BYE) to leave the REPL.
")

(defmethod repl-help ((keyword (eql :start)))
  "~| ★ Getting Started with the Romance Ⅱ REPL ★

This REPL (Read-Eval-Print Loop) is one way to interact directly with a
running instance of your Romance game system. Here, you can directly
interact with the various software objects (entities, components,
systems; functions, variables, classes, and more) which make up the game
server's environment.

The REPL is a part of the Common Lisp system in which Romance
runs. Usually, you'll interact with the REPL by entering a “form,” which
will begin with a symbol (word) representing a function. Most functions
expect some “arguments,” i.e. values, on which to operate, and return a
value to you. In Lisp's notation, each form is surrounded with
parentheses, like this:

 (help :intro)
 (+ 5 9)
 (string-downcase \"SOMETHING\")

~|

★Be Careful.★ You are in the live, running system. The actions you take
here can affect the game server running on this host.

The REPL has two main help functions you may want to use. This
one, (HELP) will tell you all about a Romance server feature when you
give it a keyword, e.g. (HELP :REPL), or about a symbol in the
environment, e.g. (HELP PRINT) or (HELP MACHINE-TYPE).

You can also discover what objects are defined in the system
using (APROPOS). (HELP APROPOS) will explain how, in brief. The most
general way is to provide a partial name, e.g. (APROPOS \"COPYRIGHT\").

~|

Symbol names are not case-sensitive, so HELP, help, Help, and heLP are
all the same. They're all treated internally as being UPPER-CASE. Symbol
names containing spaces, odd punctuation, or lower-case letters are
wrapped in | (the vertical pipe symbol), like this: |Symbol with
spaces|.

Strings are wrapped in \"double quotes\"; characters are prefixed with
#\ (e.g. #\C is the upper-case letter C, and #\Space is the space
character.) Numbers are entered without commas; e.g. 42,
123.45. Rational (fractional) numbers are entered as
numerator/denominator form only: e.g. 1½ is 3/2. (To compute this form,
though, you can enter the longer form: (+ 1 1/2) ⇒ 3/2) Numbers in
hexadecimal begin with #x (#xa0) and in octal with
#o (#o644). Complex (imaginary) numbers are entered in the form:
#c(real-part imaginary-part); e.g. #c(1 0) is just 1, but #c(1 2) is
1+2ｉ.

To quote a symbol, enter (quote symbol) or 'symbol. A keyword is
prefaced with a colon, :keyword. A symbol in another package that has
been exported is package:symbol; if not exported (private),
package::symbol instead. To get the value of a variable, just provide
the symbol naming it; to get the fuction named by a symbol,
use (function symbol) or #'symbol.

~|
This is a long document; you might need to scroll up to find the start." )

(defmethod repl-help ((keyword t))
  " Sorry: No help topic with that ID exists. Try (HELP :INTRO) for a
brief overview; (HELP :COMM), (HELP :REPL), or (HELP
:SYSOP) for other entry points.

If you're totally lost here, try (HELP :START)
")




(defun greeting-tod (&optional (instant (now)))
  (let ((sunrise (cons 6 0))
        (sunset (cons 18 0))
        (hour (timestamp-hour instant))
        (minute (timestamp-minute instant)))
    (concatenate
     'string
     (cond
       ((and
         (or (> hour (car sunrise))
             (and (= hour (car sunrise))
                  (>= minute (cdr sunrise))))
         (<= hour 12)) "Good morning")
       ((and
         (or (< hour (car sunset))
             (and (= hour (car sunset))
                  (< minute (cdr sunset))))
         (<= 12 hour)) "Good day")
       ((<= hour 23) "Good evening")
       (t "Hello, you nightowl"))
     (case (timestamp-day-of-week instant)
       ((0 6 7)
        ", and I hope you're having a relaxing week-end")
       (3 ", and happy hump day")
       (5 ", and TGIF")
       (otherwise "")))))




(in-package :oliphaunt-user)

(defun bye ()
  (invoke-restart (find-restart 'exit-module)))

(defun print-asdf-system-info (system)
  (format t "~&~|~%ASDF System ~A:~{~%  ~:(~A~): ~A~}"
          word
          (loop for fun in '(asdf:system-long-name
                             asdf:system-description
                             asdf:system-homepage
                             asdf:system-author
                             asdf:system-licence
                             asdf:system-mailto
                             asdf:system-maintainer
                             asdf:system-long-description
                             asdf:system-bug-tracker
                             asdf:system-definition-pathname
                             asdf:system-defsystem-depends-on
                             asdf:system-depends-on
                             asdf:system-weakly-depends-on
                             asdf:system-source-control
                             asdf:system-source-directory
                             asdf:system-source-file)
             for val = (funcall fun system)
             when val
             appending (list (subseq (symbol-name fun) 7)
                             val))))

(defun print-package-info (package)
  (format t "~&~|~% ~:@(~A~) is a package, which exports these symbols:~%"
          word)
  (let ((package-symbols
         (sort (loop for sym being the external-symbols of package
                  collect sym) #'string< :key #'symbol-name)))
    (when-let ((bound (remove-if-not #'boundp package-symbols)))
      (format t "~% Values bound in package ~A:~{~%   ~32A~^ ~32A~}~%"
              word
              bound))
    (when-let ((fbound (remove-if-not #'fboundp package-symbols)))
      (format t "~% Functions bound in package ~A:~{~%   ~32A~^ ~32A~}~%"
              word
              fbound))
    (when-let ((unbound (remove-if (lambda (sym)
                                     (or (fboundp sym)
                                         (boundp sym))) package-symbols)))
      (format t "~% Other symbols in package ~A:~{~%   ~32A~^ ~32A~}~%"
              word
              unbound))))

(defmacro help (&optional (word :intro))
  (typecase word
    (keyword
     (romans::repl-help word))
    (string
     (apropos word)
     (terpri)
     (ql:system-apropos word))
    (t (when (swank:connection-info)
         (ignore-errors
           (swank:eval-in-emacs
            `(ignore-errors
               (slime-hyperspec-lookup ,(string word))))))
       (when (symbolp word)
         (describe word)
         ;; (describe-object word t)
         (when (boundp word)
           (format t "~&Bound: The ~[current~;constant~] value is ~:D"
                   (constantp word)
                   (symbol-value word)))
         (dolist (doc-type '(variable function structure type setf t))
           (when-let ((info (without-warnings (documentation word doc-type))))
             (format t "~&~%Documentation of the ~(~A~) ~S:~%~A~%"
                     doc-type word info))))

       (when-let ((system (asdf:find-system word nil)))
         (print-asdf-system-info system))

       (when-let ((package (find-package (string word))))
         (print-package-info package)))))

(defmacro hello (name)
  (let ((named (typecase name
                 (symbol (join #\- (mapcar #'string-capitalize
                                           (split-sequence #\- (symbol-name name)))))
                 (string name)
                 (number (format nil "User ~:(~R~)" name))
                 (character (format nil "User ~:C" name)))))
    (romans::user-ident named)
    (format t "~&~a, ~a." (romans::greeting-tod) named)
    named))

(defun spy (&rest args)
  ;; package RAHAB isn't available yet at compile-time
  (apply (intern "SPY" (find-package "RAHAB")) args))
