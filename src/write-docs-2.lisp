;;;; -*- lisp -*-
;;;
;;;; src/write-docs.lisp is part of Tootsville
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

(in-package :tootsville)

(defun value-to-texi (symbol)
  "Pretty-print the value of SYMBOL to a string.

Used    for    values    of    constants   or    default    values    of
global (dynamic) variables."
  (let ((value (symbol-value symbol)))
    (cond
      ((null value) "NIL")
      ((typep value 'boolean) (princ-to-string value))
      ((typep value 'number)
       (format nil "~:d (#x~x)" value value))
      ((typep value 'string)
       (format nil "@verb{| ~s |}" value))
      ((typep value 'symbol)
       (format nil "@verb{| ~s |}" value))
      ((typep value 'hash-table)
       (format nil "the hash table:~2%@table @code~{~%~{@item ~a~%@code{~a}~}~}~%@end table"
               (loop for key being the hash-key of value
                     collecting (list key (gethash key value)))))
      (t (format nil "of type ~s" (type-of value))))))

(define-constant +doc-packages+
    '(Tootsville Tootsville-User Dreamhost
      Rollbar Thread-pool-taskmaster Chœrogryllum
      Twilio)
  :test 'equalp
  :documentation  "The packages  whose  symbols are  to  be included  in
  the manual.")

(defun double-@ (string)
  "Edit STRING to be safe in TeXInfo.

Escapes @@, @{, and @} characters and adds a space after ``/'' characters."
  (etypecase string
    (symbol (double-@ (string string)))
    (cons (double-@ (format nil "~{~a~^, ~}" string)))
    (string (regex-replace-all "/"
                               (regex-replace-all "([@{}])" string "@\\1")
                               "/ "))))

(defun texi-ref (string)
  "Given STRING is a TeXInfo text, replace any `single-quoted' links.

A single-quoted reference to a Lisp symbol will be replaced with a
hyperlink to that section of the manual, or given an annotation if it
is not in this manual.

References to ``Tootsville.'' are assumed to be valid Javascripts."
  (when (or (find #\{ string) (find #\} string))
    (setf string
          (regex-replace-pairs '(("^([^@]*)}" . "@}")
                                 ("^([^@]*){" "@{"))
                               string)))
  (regex-replace-all
   "`([^']*)'" string
   (lambda (target start end match-start match-end reg-starts reg-ends)
     (declare (ignore start end match-start match-end))
     (let ((quoted (subseq target
                           (elt reg-starts 0) (elt reg-ends 0))))
       (cond
         ((char= #\` (char quoted 0))
          (concatenate 'string "`" quoted "'"))
         ((and (< 11 (length quoted)) 
               (string= "Tootsville." (subseq quoted 0 11)))
          (concatenate 'string "@ref{" quoted "}"))
         ((find #\: quoted)
          (let ((package-name (subseq quoted 0 
                                      (position #\: quoted)))
                (symbol-name (subseq quoted
                                     (1+ (position #\: quoted :from-end t)))))
            (if (member package-name +doc-packages+ :test #'string-equal)
                (format nil "@ref{~a ~a}" 
                        package-name (double-@ symbol-name))
                (format nil "~a::~a (not in this manual)"
                        package-name (double-@ symbol-name)))))
         (t (if-let (sym (ignore-errors (find-symbol quoted :Tootsville)))
              (cond 
                ((member (package-name (symbol-package sym)) +doc-packages+ :test #'string-equal)
                 (format nil "@ref{~a ~a}" 
                         (package-name (symbol-package sym)) (double-@ sym)))
                ((equal "COMMON-LISP" (package-name (symbol-package sym)))
                 (format nil "~a (see the Common Lisp HyperSpec)" sym))
                (t (format nil "~a::~a (not in this manual)"
                           (package-name (symbol-package sym)) (double-@ sym))))
              (concatenate 'string "`" quoted "'"))))))))

(assert (equal (texi-ref "Link to `TEXI-REF' and `DOUBLE-@'")
               "Link to @ref{TOOTSVILLE TEXI-REF} and @ref{TOOTSVILLE DOUBLE-@@}"))
(assert (equal (texi-ref "Link to `Tootsville.javascript'")
               "Link to @ref{Tootsville.javascript}"))
(assert (equal (texi-ref "Link to `STRING'")
               "Link to STRING (see the Common Lisp HyperSpec)"))
(assert (equal (texi-ref "Link to `TIMESTAMP+'")
               "Link to LOCAL-TIME::TIMESTAMP+ (not in this manual)"))
(assert (equal (texi-ref "Just a ``quotation''")
               "Just a ``quotation''"))
(assert (equal (texi-ref "Just a `quotation'")
               "Just a `quotation'"))

(defun clean-symbols (src)
  (if (consp src)
      (if (proper-list-p src)
          (loop for element in src collecting (clean-symbols element))
          (format nil "~s" src))
      (format nil "~a" src)))

(defun write-class-docs (symbol metaobject s)
  "Write documentation for class named SYMBOL with metaobject METAOBJECT to stream S"
  (format s "~&@tindex ~:(~a~)" symbol)
  (let ((superclasses (mapcar #'class-name (sb-mop:class-direct-superclasses metaobject))))
    (princ (texi-ref
            (format nil "~2&@subsection Class~%~:(~a~) names a class, with ~r superclass~:*~[.~;~
: ~{~a~}.~;~
es: ~{~a and ~a~}~;~
es: ~{~a, ~a, and ~a~}~:;~
es: ~{~a~^, ~}.~]"
                    symbol (length superclasses)
                    (mapcar (lambda (class)
                              (format nil "`~a::~a'"
                                      (package-name (symbol-package class))
                                      (symbol-name class)))
                            superclasses)))
           s))
    (when-let ((doc (documentation metaobject t)))
      (format s "~2&~a~2%" doc))
    (if-let ((slots (ignore-errors (sb-mop::class-slots metaobject))))
      (progn
        (format s "~2&@subsection Slots")
        (format s "~2&Class ~:(~a~) has ~:d direct slot definition~:p:~2%@table @code~%"
                symbol (length slots))
        (dolist (slot slots)
          (format s "~&@item ~:(~a~)~%~a~%"
                  (sb-mop:slot-definition-name slot)
                  (or (if (or (eql 'sb-pcl::structure-effective-slot-definition
                                   (class-name (class-of slot)))
                              (eql 'sb-pcl::condition-effective-slot-definition
                                   (class-name (class-of slot))))
                          ""
                          (documentation slot t))
                      "(undocumented)")))
        (format s "~2&@end table"))
      (format s "~2&@subsection Slots~2%Class ~:(~a~) has no direct slots defined." symbol)))

(defun write-function-docs (symbol s)
  "Write documentation for the function (or macro) SYMBOL to S"
  (let ((kind (if (macro-function symbol) "macro" "function")))
    (format s "~&@findex ~:(~a~)~%@subsection ~:(~a~)" (double-@ symbol) kind)
    (if-let (docu (documentation symbol 'function))
      (format s "~2&~:(~a~) names a ~a, with lambda list~%~a:~2%~a"
              (double-@ symbol) kind (clean-symbols (function-lambda-list symbol))
              (texi-ref docu))
      (format s "~2&~:(~a~) names an undocumented ~a, with lambda list~%~a.~2%"
              symbol kind (clean-symbols (function-lambda-list symbol)))))
  (when-let (def (or (ignore-errors (sb-introspect:find-definition-source (coerce symbol 'function)))
                     (ignore-errors (sb-introspect:find-definition-source (macro-function symbol)))))
    (when-let (pathname (uiop:enough-pathname
                         (uiop:enough-pathname (sb-introspect:definition-source-pathname def) 
                                               (asdf:component-pathname (asdf:find-system :Tootsville)))
                         (user-homedir-pathname)))
      (format s "~2&\@subsection File~2%Defined in file ~a.~%@pnindex ~a.~a~2%"
              pathname (pathname-name pathname) (pathname-type pathname)))))

(defun write-setf-docs (symbol s)
  "Write documentation for the SetF function (SETF SYMBOL) to S"
  (format s "~&@findex ~:(~a~), SetF~%@findex SetF ~:(~a~)" symbol symbol)
  (format s "~%@subsection SetF Function")
  (if-let (docu (documentation symbol 'setf))
    (format s "~2&(SETF ~:(~a~)) names a function, with lambda list~%~a:~2%~a"
            symbol (clean-symbols (function-lambda-list (list 'setf symbol)))
            (texi-ref docu))
    (format s "~2&(SETF ~:(~a~)) names an undocumented function, with lambda list~%~a.~2%" symbol
            (clean-symbols (function-lambda-list (list 'setf symbol)))))
  (when-let (def (ignore-errors (sb-introspect:find-definition-source (coerce (list 'setf symbol) 'function))))
    (when-let (pathname (uiop:enough-pathname
                         (uiop:enough-pathname (sb-introspect:definition-source-pathname def) 
                                               (asdf:component-pathname (asdf:find-system :Tootsville)))
                         (user-homedir-pathname)))
      (format s "~2&@subsection File~2%Defined in file ~a.~%@pnindex ~a.~a~2%"
              pathname (pathname-name pathname) (pathname-type pathname)))))

(defun write-documentation (symbol s)
  "Writes the TeXinfo documentation for SYMBOL to stream S.

Ignores ``private''  functions, indicated by  a %  in the first  or last
position of the name."
  (when (char= #\% 
               (elt (symbol-name symbol) (1- (length (symbol-name symbol)))))
    (return-from write-documentation nil))
  (when (or (boundp symbol)
            (fboundp symbol)
            (fboundp (list 'setf symbol))
            (ignore-errors (find-class symbol))
            (documentation symbol 'structure)
            (documentation symbol 'type))
    (format s "~2&@page~%@node ~a ~a~%@section ~:(~a~)::~:(~a~)~%"
            (package-name (symbol-package symbol)) (double-@ (symbol-name symbol))
            (package-name (symbol-package symbol)) (double-@ (symbol-name symbol)))
    (when (fboundp symbol)
      (write-function-docs symbol s))
    (when (fboundp (list 'setf symbol))
      (write-setf-docs symbol s))
    (when (boundp symbol)
      (format s "~&@vindex ~:(~a~)" symbol)
      (if-let (docu (documentation symbol 'variable))
        (format s "~2&@subsection Variable~%~:(~a~) names a variable:~2%~a~2%Its value is ~a"
                symbol (texi-ref docu) (value-to-texi symbol))
        (format s "~2&@subsection Variable~%~:(~a~) names an undocumented variable with the value ~a"
                symbol (value-to-texi symbol))))
    (if-let (metaobject (ignore-errors (find-class symbol)))
      (write-class-docs symbol metaobject s)
      (dolist (kind '(structure type))
        (when-let (docu (documentation symbol kind))
          (format s "~&@tindex ~:(~a~)" symbol)
          (format s "~2&@subsection ~:(~a~)~%~:(~a~) names a ~a:~2%~a" kind symbol kind (texi-ref docu)))))))

(defun gather-all-symbols ()
  "Gathers all defined symbols in `+DOC-PACKAGES+'"
  (let (list)
    (do-all-symbols (symbol list)
      (when (member (symbol-package symbol)
                    (mapcar #'find-package +doc-packages+))
        (push symbol list)))
    (reverse (remove-duplicates list))))

(defun all-symbols-alphabetically ()
  "Finds all symbols from `GATHER-ALL-SYMBOLS' alphabetically"
  (sort (gather-all-symbols) #'string-lessp
        :key (lambda (symbol)
               (format nil "~a::~a"
                       (package-name (symbol-package symbol))
                       (symbol-name symbol)))))

(defun write-docs-header (docs source-dir)
  (format docs "\\input texinfo

@c Tootsville.texi — Reference manual
@setfilename Tootsville.info
@documentencoding UTF-8
@settitle The Book of Romance II for Tootsville V
@setchapternewpage odd
@documentdescription
The Book of Romance II for Tootsville V version ~a
@end documentdescription

@dircategory Common Lisp
@direntry
* Tootsville: (Tootsville). The Book of Romance II for Tootsville V
@end direntry

@copying
@quotation
Copyright @copyright{} 2008-2017 Bruce-Robert Pocock

Copyright @copyright{} 2018-2021 The Corporation for Inter-World Tourism
and Adventuring @url{https://ciwta.org/}

This manual is based upon manual-generating code taken from Declt 2.3.

Permission is granted to make and distribute verbatim copies of this
manual provided the copyright notice and this permission notice are
preserved on all copies.

@ignore

Permission is granted to process this  file through @TeX{} and print the
results,  provided the  printed  document carries  a copying  permission
notice identical to this one except for the removal of this paragraph
\(this paragraph not being relevant to the printed manual).

@end ignore

Permission is granted to copy and distribute modified versions of this
manual under the conditions for verbatim copying, provided also that the
section entitled ``Copying'' is included exactly as in the original.

Permission is granted to copy and distribute translations of this manual
into another language, under the above conditions for modified versions,
except that this permission notice may be translated as well.

@end quotation
@end copying

@c pathname index
@defindex pn
@c Infinity mode commands
@defindex ic
@c Operator commands
@defindex oc
@c Game Actions
@defindex ga
@c Javascript
@defindex js

@titlepage
@title The Book of Romance II
@subtitle A reference manual for Tootsville V, version ~:*~a.

@author Bruce-Robert Pocock <@email{BRPocock@@ciwta.org}>

@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@contents

@ifnottex
@node Top
@top The Book of Romance II

This is The Book of Romance II, describing the Romance II game core
and Tootsville V in particular. This manual is generated from the
docstrings found in the Tootsville package and supporting packages."
          (asdf:component-version (asdf:find-system :Tootsville))))

(defun write-docs ()
  "Write out the documentation in TeΧinfo format.

XXX this is a huge function that ought to be broken up more"
  (format *trace-output* "~& Writing documentation…")
  
  (let ((source-dir (asdf:component-pathname (asdf:find-system :tootsville))))
    (ensure-directories-exist (merge-pathnames #p"doc/" source-dir))
    (with-output-to-file (docs (merge-pathnames #p"doc/Tootsville.texi"
                                                source-dir)
                               :if-exists :supersede)
      (write-docs-header docs source-dir)
      (format docs "~2%@menu
* Copying:: The GNU Affero General Public License
* Introduction:: What Tootsville V (Romance II) is all about
* Definitions::
~{* Package ~a::~%~}~
* Javascript:: The front-end documentation
* Conclusion:: Time to go
* Indices:: Concepts, functions, variables, data types, and pathnames
@end menu

@insertcopying
@end ifnottex

@node Copying
@unnumbered Copying
@quotation

This program is  free software; you can redistribute  it and/or modify
it  under the  terms  of  the GNU  Affero  General  Public License  as
published by  the Free  Software Foundation; either  version 3  of the
License, or (at your option) any later version.

This program  is distributed in the  hope that it will  be useful, but
WITHOUT   ANY  WARRANTY;   without  even   the  implied   warranty  of
MERCHANTABILITY  or FITNESS  FOR A  PARTICULAR PURPOSE.   See the  GNU
Affero General Public License for more details.

You  should have  received a  copy of  the GNU  Affero General  Public
License along with  this program (one is found in  this book); if not,
write to the Free Software  Foundation, Inc., 675 Mass Ave, Cambridge,
MA 02139, USA.

@end quotation

@node Introduction
@chapter Introduction ~2%"
              
              (mapcar #'string-capitalize +doc-packages+))
      (princ (alexandria:read-file-into-string
              (merge-pathnames #p"src/doc/Introduction.texi" source-dir))
             docs)
      (format docs "

@node Definitions
@chapter Definitions

The following chapters provide documentation of symbols in each of the
main packages used by Tootsville. Many other libraries are relied upon
as well, whose documentation may not have been included here.

@menu
")
      (let ((defs (all-symbols-alphabetically))
            (last-package nil))
        (format docs "~{~%* ~{~a ~a~}::~}" (mapcar (lambda (def) 
                                                     (list (package-name (symbol-package def)) (symbol-name def)))
                                                   defs))
        (format docs "~&@end menu~2% ")
        (dolist (symbol defs)
          (unless (eql last-package (symbol-package symbol))
            (format docs "~2&@page
@node Package ~:(~a~),,Package ~:(~a~),Top
@chapter Package ~:(~a~)
@menu
@end menu~%"
                    (package-name (symbol-package symbol))
                    (or (and last-package (package-name last-package)) "")
                    (package-name (symbol-package symbol)))
            (setf last-package (symbol-package symbol))
            (let ((doc (documentation (symbol-package symbol) t)))
              (unless (emptyp doc)
                (format docs "~2&~a~2%" (texi-ref doc)))))
          (write-documentation symbol docs)))
      
      (princ (texi-ref (read-file-into-string (merge-pathnames #p"../tootsville.org/dist/doc.texi"
						   source-dir)))
	   docs)
      (format docs "

@node Credits
@chapter Credits

Tootsville is built upon a plethora of software. This is an attempt to
convey at least a partial enumeration of the credits.

First, the most directly responsible:

Tootsville is a production of the Corporation for Inter-World Tourism
and Adventuring, a not-for-profit corporation in the State of Florida,
United States.

~a

@node Major Support Software
@section Major Support Software

The following support software is used in the development of
Tootsville:

@itemize
@item
The Linux® Kernel
@item
The Gnu Operating System, by the Free Software Foundation
@item
The Fedora Distribution
@item
Emacs text editor and integrated development environment (IDE)
@item
Firefox web browser
@item
Chromium web browser
@item
Gimp graphics editor
@item
Inkscape graphics editor
@item
Blender graphics editor
@item
FFMPEG video and audio transcoder
@item
Audacity sound editor
@item
Rosegarden music editor
@end itemize

We'd also like to mention that we test with the following web browsers

@itemize
@item
Firefox for macOS and Microsoft Windows
@item
Epiphany (Gnome Web)
@item
Google Chrome for Linux, macOS, and Microsoft Windows
@item
Opera
@item
Microsoft Edge for macOS and Microsoft Windows
@item
Apple Safari for macOS
@end itemize

@node Systems
@section Systems

The following systems (libraries) are compiled into the Tootsville server

~a

@node SBCL Credits
@section The Steel Bank Common Lisp compiler

Tootsville is developed and compiled using the Steel Bank Common Lisp
compiler.

@verbatim
~a
@end verbatim
 "
              +credits+
              (all-credits)
              (or (ignore-errors (read-file-into-string #p"/usr/share/doc/sbcl/CREDITS"))
                  ""))
      
      (format docs "

@node Javascript Tools
@section Javascript Tools

The Javascript front-end to Tootsville is developed using the
following additional tools:

@itemize
@item
Babylon.JS 3D rendering library
@item
Google Closure Javascript compressor
@end itemize


")
      (princ (alexandria:read-file-into-string
              (merge-pathnames #p"src/doc/Conclusion.texi" source-dir))
             docs)
      
      (format docs "@node Indices
@appendix Indices
@menu
* Concept index::
* Function index::
* Variable index::
* Data type index::
* Pathname index::
* Javascript index::
* Infinity Mode commands::
* Operator commands::
* Game Actions::
@end menu


@c -------------
@c Concept index
@c -------------
@node Concept index
@appendixsec Concepts
@printindex cp

@page


@c --------------
@c Function index
@c --------------
@node Function index
@appendixsec Functions
@printindex fn

@page


@c --------------
@c Variable index
@c --------------
@node Variable index
@appendixsec Variables
@printindex vr

@page


@c ---------------
@c Data type index
@c ---------------
@node Data type index
@appendixsec Data types
@printindex tp

@page 

@c Pathname index
@node Pathname index
@appendixsec Pathnames
@printindex pn

@page

@c Infinity Mode commands
@node Infinity Mode commands
@appendixsec Infinity Mode commands
@printindex ic

@page

@c Operator commands
@node Operator commands
@appendixsec Operator commands
@printindex oc

@page

@c Game Actions
@node Game Actions
@appendixsec Game Actions
@printindex ga

@page 

@c Javascript index
@node Javascript index
@appendixsec Javascript
@printindex js

@page

``It's always dark at the beginning . . . ''

@bye

"))))
