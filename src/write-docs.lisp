;;;; -*- lisp -*-
;;;
;;;; src/write-docs.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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
  (list
   (find-package :Tootsville)
   (find-package :Chœrogryllum)
   (find-package :Rollbar)))

(defun write-docs ()
  "Write out the TεΧinfo documentation directly, without DECLT."
  (let ((source-dir (asdf:component-pathname (asdf:find-system :Tootsville))))
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
@chapter* Introduction
~a"
              *romance-ii-copyright-latest*
              (read-file-into-string (asdf:system-relative-pathname 
                                      :Tootsville
                                      "src/doc/Introduction.texi")))
      (dolist (package (sort-all-packages))
        (format t "~2%@node ~a~%@chapter Package ~:*~a

@menu~%@end menu"
                (string-capitalize (package-name package)))
        (let ((overview (asdf:system-relative-pathname
                         :Tootsville 
                         (format nil "src/doc/~a.texi"
                                 (string-downcase (package-name package))))))
          (when (probe-file overview)
            (format t "~2%@section Overview~2%~a~2%"
                    (read-file-into-string overview))))
        (format t "~2%@section Package Information
The package ~:(~a~) uses these packages:
@itemize
~{@item
~:(~a~)~%~}~
@end itemize"
                (package-name package)
                (sort (mapcar #'package-name (package-use-list package))
                      #'string-lessp))
        (format t "~2%~:(~a~) is used by these packages:
@itemize
~{@item
~:(~a~)~%~}~
@end itemize"
                (package-name package)
                (sort (mapcar #'package-name (package-used-by-list package))
                      #'string-lessp))
        (let (symbols)
          (do-all-symbols (symbol)
            (when (and (eql (symbol-package symbol) package)
                       (symbol-has-definition-p symbol)
                       (or (eql (symbol-package symbol) (find-package :Tootsville))
                           (swank::symbol-external-p symbol)))
              (push symbol symbols)))
          (dolist (symbol (sort (remove-duplicates symbols) #'string<))
            (write-docs-for-symbol symbol))))
      (terpri)
      (princ "@bye"))))

(defun symbol-has-definition-p (symbol)
  (or (fboundp symbol)
      (boundp symbol)
      (ignore-errors (find-class symbol))))


(defun clean-docs (docstring)
  (or docstring ""))

(defun write-docs-for-symbol (symbol)
  (format t "~2%@page~%@node ~:(~a::~a~)~2%@section ~:(~a~)~%" 
          (package-name (symbol-package symbol)) symbol symbol)
  (when (fboundp symbol)
    (format t "~2% ~:(~a~) names a ~:[function~;macro~], taking ~[no arguments~
~;the argument (~{~:(~a~)~})~
~:;the arguments: (~{~:(~a~)~^ ~})~].

~a"
            symbol
            (macro-function symbol)
            (length (function-lambda-list symbol))
            (function-lambda-list symbol)
            (clean-docs (documentation symbol 'function))))
  (when (boundp symbol)
    (format t "~2% ~:(~a~) names a ~:[special variable~;global constant~].~2%~a"
            symbol
            (constantp symbol)
            (clean-docs (documentation symbol 'variable))))
  (when (ignore-errors (find-class symbol))
    (format t "~2% ~:(~a~) names a class.~2%~a"
            symbol
            (clean-docs (documentation symbol 'type)))))
