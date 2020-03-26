;;;; -*- lisp -*-
;;;
;;;; src/write-docs.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  © 2018-2020  The
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

(in-package :tootsville)

(defun inform-declt-of-agplv3 ()
  "Adds the AGPLv3 to the list of licenses for DECLT."
  (let ((licenses (intern "*LICENSES*" (find-package :net.didierverna.declt))))
    (set licenses
         (cons (eval licenses)
               '((:agplv3
                  "The GNU Affero General Public License"
                  "This  program is  free  software; you  can redistribute  it
and/or  modify it  under  the terms  of the  GNU  Affero General  Public
License as  published by  the Free  Software Foundation;  either version
3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program; if not,  write to the Free Software Foundation,
Inc., 675 Mass Ave, Cambridge, MA 02139, USA."))))))

(defun write-docs/declt ()
  "Write out the documentation in TeΧinfo format using DECLT.

Note that DECLT  is not usually compiled into the  binary by default, so
this  may  have  to  download  DECLT  and/or  its  dependencies  through
Quicklisp when called."
  (format *trace-output* "~& Writing documentation…")
  
  (ql:quickload :net.didierverna.declt)
  (let ((source-dir (asdf:component-pathname (asdf:find-system :tootsville))))
    (inform-declt-of-agplv3)
    (ensure-directories-exist (merge-pathnames #p"doc/" source-dir))
    (funcall (intern "DECLT" (find-package :net.didierverna.declt))
             :tootsville
             :library "Tootsville Ⅴ (Romance Ⅱ)"
             :texi-file (merge-pathnames #p"doc/Tootsville.texi"
                                         source-dir)
             :info-file (merge-pathnames #p "doc/Tootsville"
                                         source-dir)
             :license :agplv3
             :declt-notice :short
             :hyperlinks nil
             :introduction (alexandria:read-file-into-string
                            (merge-pathnames #p"src/doc/introduction"
                                             source-dir))
             :conclusion (alexandria:read-file-into-string
                          (merge-pathnames #p"src/doc/conclusion"
                                           source-dir)))))

(defun sort-all-packages ()
  "A list of all packages to be included in the documentation, in order."
  (list
   (find-package :Tootsville)
   (find-package :Chœrogryllum)
   (find-package :Dreamhost)
   (find-package :Rollbar)
   (find-package :thread-pool-taskmaster)
   (find-package :Tootsville-User)
   (find-package :Common-Lisp)))

(defun write-docs ()
  "Write out the TεΧinfo documentation directly, without DECLT."
  (let ((source-dir (asdf:component-pathname (asdf:find-system :Tootsville)))
        (*print-right-margin* 72))
    (ensure-directories-exist (merge-pathnames #p"doc/" source-dir))
    (with-output-to-file (*standard-output*
                          (merge-pathnames #p "doc/Tootsville.texi" source-dir)
                          :if-exists :supersede)
      (format t "\\input texinfo @c -*-texinfo-*-
@setfilename Tootsville.info
@settitle Tootsville V / Romance II Programmers' Reference Guide
@headings double
@copying

Draft Edition.

Copyright @copyright{} ~d, the Corporation for Inter-World Tourism and 
Adventuring.

@quotation
Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3
or any later version published by the Free Software Foundation;
with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
A copy of the license is included in the section entitled ``GNU
Free Documentation License''.

A copy of the license is also available from the Free Software
Foundation Web site at @url{https://www.gnu.org/licenses/fdl.html}.
@end quotation

The document was typeset with @uref{http://www.textinto.org/, GNU @TeX{}info}.

@end copying

@titlepage
@title Tootsville V Programmers' Reference Guide
@subtitle Romance II System Documentation
@author Bruce-Robert Pocock <brpocock@@tootsville.org>
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@contents

@ifnottex
@node Top
@top Tootsville V Programmers' Reference Guide
@insertcopying

@menu
@c Use (texinfo-all-menus-update) in Emacs to write menus.
@end menu
@end ifnottex

@node Introduction
@chapter Introduction
~a"
              (romance-ii-copyright-latest)
              (read-file-into-string (asdf:system-relative-pathname 
                                      :Tootsville
                                      "src/doc/Introduction.texi")))
      (dolist (package (sort-all-packages))
        (format t "~2%@node ~a~%@chapter Package ~:*~a

@menu~%@end menu"
                (clean-docs (package-name package)))
        (let ((overview (asdf:system-relative-pathname
                         :Tootsville 
                         (format nil "src/doc/~a.texi"
                                 (string-downcase (package-name package))))))
          (when (probe-file overview)
            (format t "~2%@section Overview~2%~a~2%"
                    (read-file-into-string overview))))
        (format t "~2%@section Package Information")
        (when (package-use-list package)
          (format t "~%The package ~a uses these packages:
@itemize
~{@item
~a~%~}~
@end itemize"
                  (clean-docs (package-name package))
                  (sort (mapcar #'clean-docs
                                (mapcar #'package-name
                                        (package-use-list package)))
                        #'string-lessp)))
        (when (package-used-by-list package)
          (format t "~2%~a is used by these packages:
@itemize
~{@item
~a~%~}~
@end itemize"
                  (clean-docs (package-name package))
                  (sort (mapcar #'clean-docs
                                (mapcar #'package-name
                                        (package-used-by-list package)))
                        #'string-lessp)))
        (when (sb-ext:package-locked-p package)
          (format t "~2%The package is locked against modifications."))
        (when (package-nicknames package)
          (format t "~2%The package has these nicknames:
@itemize
~{@item
~a~%~}~
@end itemize"
                  (sort (mapcar #'clean-docs
                                (package-nicknames package))
                        #'string-lessp)))
        (let (symbols)
          (do-all-symbols (symbol)
            (when (and (eql (symbol-package symbol) package)
                       (symbol-has-definition-p symbol)
                       (or (eql (symbol-package symbol) (find-package :Tootsville))
                           (swank::symbol-external-p symbol)))
              (push symbol symbols)))
          (dolist (symbol (sort (remove-duplicates symbols) #'string<))
            (write-docs-for-symbol symbol))))
      (princ (read-file-into-string (asdf:system-relative-pathname 
                                     :Tootsville
                                     "src/doc/Conclusion.texi")))
      (terpri)
      (let ((client-docs (asdf:system-relative-pathname :Tootsville
                                                        "../tootsville.org/dist/doc.texi")))
        (when (probe-file client-docs)
          (princ (read-file-into-string client-docs))))
      (princ "@bye"))))

(defun symbol-has-definition-p (symbol)
  "True if SYMBOL has some definition that we print in documentation."
  (or (fboundp symbol)
      (boundp symbol)
      (ignore-errors (find-class symbol))))

(define-constant +teχinfo-accented-char-pairs+
    '(("ä" . "@\"a")
      ("ë" . "@\"e")
      ("ï" . "@\"i")
      ("ö" . "@\"o")
      ("ü" . "@\"u")
      ("ÿ" . "@\"y")
      ("á" . "@'a")
      ("é" . "@'e")
      ("í" . "@'i")
      ("ó" . "@'o")
      ("ú" . "@'u")
      ("ý" . "@'y")
      ("ç" . "@,{c}")
      ("ā" . "@=a")
      ("ē" . "@=e")
      ("ī" . "@=i")
      ("ō" . "@=o")
      ("ū" . "@=u")
      ("ȳ" . "@=y")
      ("à" . "@`a")
      ("è" . "@`e")
      ("ì" . "@`i")
      ("ò" . "@`o")
      ("ù" . "@`u")
      ("ỳ" . "@`y")
      ("ñ" . "@~n")
      ("æ" . "@ae{}")
      ("ð" . "@dh{}")
      ("œ" . "@oe{}")
      ("þ" . "@th{}")
      ("Ä" . "@\"A")
      ("Ë" . "@\"E")
      ("Ï" . "@\"I")
      ("Ö" . "@\"O")
      ("Ü" . "@\"U")
      ("Ÿ" . "@\"Y")
      ("Á" . "@'A")
      ("É" . "@'E")
      ("Í" . "@'I")
      ("Ó" . "@'O")
      ("Ú" . "@'U")
      ("Ý" . "@'Y")
      ("Ç" . "@,{C}")
      ("Ā" . "@=A")
      ("Ē" . "@=E")
      ("Ī" . "@=I")
      ("Ō" . "@=O")
      ("Ū" . "@=U")
      ("Ȳ" . "@=Y")
      ("À" . "@`A")
      ("È" . "@`E")
      ("Ì" . "@`I")
      ("Ò" . "@`O")
      ("Ù" . "@`U")
      ("Ỳ" . "@`Y")
      ("Ñ" . "@~N")
      ("Æ" . "@AE{}")
      ("Ð" . "@DH{}")
      ("Œ" . "@OE{}")
      ("Þ" . "@TH{}")
      ("°" . "@ordm{}")
      ("ª" . "@ordf{}")
      ("©" . "@copyright")
      ("\\(C\\)" . "@copyright")
      ("—" . "---")
      ("TεΧ" . "@TeX")
      ("TeΧ" . "@TeX")
      ("TeX" . "@TeX")
      ("∞" . "Infinity")
      ("•" . "@bullet")
      ("…" . "@dots{}")
      ("€" . "@euro")
      ("°" . "@textdegree")
      ("≤" . "@leq{}")
      ("<=" . "@leq{}")
      ("≥" . "@geq{}")
      (">=" . "@geq{}")
      ("®" . "@registeredsymbol{}")
      ("\\(R\\)" . "@registeredsymbol{}")
      ("→" . "@expansion{}")
      ("⇒" . "@result{}")
      ("≍" . "@equiv{}")
      ("===" . "@equiv{}")
      ("\\" . "@backslashchar{}")
      ("¡" . "@exclamdown{}")
      ("¿" . "@questiondown{}")
      ("([a-zA-Z])/([a-zA-Z])" . "\\1/@ \\2")
      ("⊕POST-" . "* Power-on Self-Test ")
      ("⊕Post-" . "* Power-on Self-Test "))
  :test #'equalp
  :documentation "Replacement sequences for TeΧinfo for special characters.")

(defun maybe-make-hyperlink (text)
  "If TEXT references a `SYMBOL-HAS-DEFINITION-P' symbol, make a reference.

Lisp  docstrings  containing `quoted  strings'  which  are the  name  of
a  symbol with  a definition  to be  documented will  be converted  into
references to the appropriate node in the documentation."
  (let ((symbol (intern text)))
    (if (symbol-has-definition-p symbol)
        (format nil "@ref{~a::~a}"
                (package-name (symbol-package symbol))
                (symbol-name symbol))
        text)))

(defun make-hyperlinks (string)
  "Convert any `quoted references' in STRING into hyperlinks.

Lisp  docstrings  containing `quoted  strings'  which  are the  name  of
a  symbol with  a definition  to be  documented will  be converted  into
references   to   the  appropriate   node   in   the  documentation   by
`MAYBE-MAKE-HYPERLINK'."
  (regex-replace-all 
   "\`(.*)'" string
   (lambda (TARGET-STRING START END
            MATCH-START MATCH-END
            REG-STARTS REG-ENDS)
     (format nil "`~a'"
             (maybe-make-hyperlink
              (subseq target-string (1+ match-start) (1- match-end)))))))

(defun clean-docs (docstring)
  "Clean up the contents of DOCSTRING for inclusion into the documentation."
  (etypecase docstring
    (proper-list (mapcar #'clean-docs docstring))
    (number (format nil "~a" docstring))
    (keyword (format nil "~s" docstring))
    (symbol (regex-replace-pairs 
             +teχinfo-accented-char-pairs+
             (string-capitalize docstring)))
    (string (regex-replace-pairs 
             +teχinfo-accented-char-pairs+
             (make-hyperlinks docstring)))))

(defun pretty-function-lambda-list (symbol)
  "Pretty-print an itemized list of the lambda list of SYMBOL's function.

Separates  positional,  optional,  and   keyword  arguments,  and  makes
a valiant effort towards &Rest arguments as well."
  (let ((λ-list (function-lambda-list symbol))
        (positionalp t)
        (optionalp nil)
        (index 1))
    (with-output-to-string (*standard-output*)
      (format t "~%@itemize")
      (loop for symbol in λ-list
         do 
           (progn
             (cond 
               ((eql symbol '&optional)
                (setf optionalp t)
                (format t "~%@end itemize
The following arguments are optional:
@itemize"))
               ((eql symbol '&key)
                (setf optionalp t
                      positionalp nil)
                (format t "~%@end itemize
The following are keyword arguments:
@itemize"))
               ((eql symbol '&rest)
                (setf optionalp t)
                (format t "~%@end itemize
All remaining arguments are collected into the &REST argument:
@itemize"))
               ((proper-list-p symbol)
                (cond
                  ((and optionalp (not positionalp))
                   (format t "~%@item~%The keyword argument ~a ~
has a default value of ~a~
~@[, and its presence or absence is recorded in ~a~]"
                           (clean-docs (first symbol))
                           (clean-docs (second symbol))
                           (and (= 3 (length symbol))
                                (clean-docs (third symbol)))))
                  ((and optionalp positionalp)
                   (format t "~%@item~%The optional ~:r argument, ~a, ~
has a default value of ~a"
                           index
                           (clean-docs (first symbol))
                           (clean-docs (second symbol)))
                   (incf index))
                  (t
                   (format t "~%@item~%The ~:r argument is a list of ~s"
                           index
                           symbol)
                   (incf index))))
               ((symbolp symbol)
                (cond
                  ((and optionalp (not positionalp))
                   (format t "~%@item~%Keyword argument ~a (with default nil)"
                           (clean-docs symbol)))
                  (optionalp
                   (format t "~%@item~%Optional ~:r argument ~a (with default nil)"
                           index
                           (clean-docs symbol))
                   (incf index))
                  (t
                   (format t "~%@item~%Required ~:r argument ~a"
                           index
                           (clean-docs symbol))
                   (incf index)))))))
      (format t "~%@end itemize~%"))))

(defun write-docs-for-symbol (symbol)
  "Write the documentation for any definitions associated with SYMBOL.

Currently identifies functions, macros,  global constants, special vars,
and types. This is, in effect, the TeΧinfo equivalent of `DESCRIBE'.

In fact, in the package ``Common-Lisp,'' this function instead defers to
`DESCRIBE' and performs  only minimal reformatting of  its output, since
SBCL's documentation is written in such a style that not much additional
styling is helpful."
  (format t "~%@vskip 48pt
@node ~a::~a~2%@section ~a~%" 
          (package-name (symbol-package symbol))
          symbol 
          (clean-docs symbol))
  (when (eql (symbol-package symbol) (find-package :common-lisp))
    ;; XXX would be nice to process `REFERENCES' in CL package docs
    (format t "~2%~a~2%"
            (regex-replace-pairs
             '(
               ("@" . "@@")
               ("{" . "@{")
               ("}" . "@}")
               ("
" . "

"))
             (with-output-to-string (*standard-output*)
               (describe symbol))))
    (return-from write-docs-for-symbol))
  (when (fboundp symbol)
    (format t "~2% ~a names a ~:[function~;macro~], taking ~[no arguments.~
~;one argument: ~a~
~:;the arguments: ~a~]

~a~2%"
            (clean-docs symbol)
            (macro-function symbol)
            (length (function-lambda-list symbol))
            (pretty-function-lambda-list symbol)
            (clean-docs (documentation symbol 'function)))
    #+sbcl
    (when-let (source-file (sb-int:debug-source-namestring
                            (sb-c::debug-info-source 
                             (sb-kernel:%code-debug-info
                              (sb-kernel:fun-code-header
                               (sb-kernel::%fun-fun
                                (symbol-function symbol))))))) 
      (format t "~&@vskip 18pt~%Source file: ~a~2%" (enough-namestring source-file))))
  (when (boundp symbol)
    (format t "~2% ~a names a ~:[special variable~;global constant~].~2%~a~2%"
            (clean-docs symbol)
            (constantp symbol)
            (clean-docs (documentation symbol 'variable)))
    (when (constantp symbol)
      (format t "The value of ~a is:~2%@verbatim~%~s~%@end verbatim"
              (clean-docs symbol)
              (symbol-value symbol))))
  (when (or (ignore-errors (find-class symbol))
            (documentation symbol 'type))
    (format t "~2% ~a names a type.~2%~a~2%"
            (clean-docs symbol)
            (clean-docs (documentation symbol 'type)))))
