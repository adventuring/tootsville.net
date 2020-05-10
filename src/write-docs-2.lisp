;;;; -*- lisp -*-
;;;
;;;; src/write-docs.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018-2020  The
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

(defun write-documentation (symbol s)
  (format s "~2&@node ~a::~a~%@section ~:(~a~)~%"
         (package-name (symbol-package symbol)) (symbol-name symbol)
         (symbol-name symbol))
  (dolist (kind '(function variable structure type setf))
    (when-let (docu (documentation symbol kind))
      (format s "~2&~:(~a~) names a ~(~a~):~2%~a" symbol kind docu))))

(defun gather-all-symbols ()
  (let (list)
    (do-all-symbols (symbol list)
      (when (member (symbol-package symbol)
                    (mapcar #'find-package '(Tootsville Tootsville-User Dreamhost
                                             Rollbar Thread-pool-taskmaster Chœrogryllum)))
        (push symbol list)))))

(defun write-docs ()
  "Write out the documentation in TeΧinfo format."
  (format *trace-output* "~& Writing documentation…")

  (let ((source-dir (asdf:component-pathname (asdf:find-system :tootsville))))
    (ensure-directories-exist (merge-pathnames #p"doc/" source-dir))
    (with-output-to-file (docs (merge-pathnames #p"doc/Tootsville.texi"
                                                source-dir)
                               :if-exists :supersede)
      (format docs "\\input texinfo

@c Tootsville.texi — Reference manual
@setfilename Tootsville.info
@documentencoding UTF-8
@settitle The Book of Romance II for Tootsville V
@setchapternewpage odd
@documentdescription
The Book of Romance II for Tootsville V version ~a
@end documentdescription
@c Lisp files
@macro lispfileindex{name}
@cindex @t{\name\}
@cindex Lisp File, @t{\name\}
@cindex File, Lisp, @t{\name\}
@end macro
@c JavaScript files
@macro jsfileindex{name}
@cindex @t{\name\}
@cindex JavaScript File, @t{\name\}
@cindex File, JavaScript, @t{\name\}
@end macro

@c Constants
@macro constantsubindex{name}
@vindex @r{Constant, }\name\
@end macro

@c Special variables
@macro specialsubindex{name}
@vindex @r{Special Variable, }\name\
@end macro

@c Symbol macros
@macro symbolmacrosubindex{name}
@vindex @r{Symbol Macro, }\name\
@end macro

@c Slots
@macro slotsubindex{name}
@vindex @r{Slot, }\name\
@end macro

@c Macros
@macro macrosubindex{name}
@findex @r{Macro, }\name\
@end macro

@c Compiler Macros
@macro compilermacrosubindex{name}
@findex @r{Compiler Macro, }\name\
@end macro

@c Functions
@macro functionsubindex{name}
@findex @r{Function, }\name\
@end macro

@c Methods
@macro methodsubindex{name}
@findex @r{Method, }\name\
@end macro

@c Generic Functions
@macro genericsubindex{name}
@findex @r{Generic Function, }\name\
@end macro

@c Setf Expanders
@macro setfexpandersubindex{name}
@findex @r{Setf Expander, }\name\
@end macro

@c Method Combinations
@macro shortcombinationsubindex{name}
@tpindex @r{Short Method Combination, }\name\
@tpindex @r{Method Combination, Short, }\name\
@end macro

@macro longcombinationsubindex{name}
@tpindex @r{Long Method Combination, }\name\
@tpindex @r{Method Combination, Long, }\name\
@end macro

@c Conditions
@macro conditionsubindex{name}
@tpindex @r{Condition, }\name\
@end macro

@c Structures
@macro structuresubindex{name}
@tpindex @r{Structure, }\name\
@end macro

@c Types
@macro typesubindex{name}
@tpindex @r{Type, }\name\
@end macro

@c Classes
@macro classsubindex{name}
@tpindex @r{Class, }\name\
@end macro

@dircategory Common Lisp
@direntry
* Tootsville: (Tootsville). The Book of Romance II for Tootsville V
@end direntry

@copying
@quotation
Copyright @copyright{} 2008-2017 Bruce-Robert Pocock

Copyright @copyright{} 2018-2020 The Corporation for Inter-World Tourism
and Adventuring @url{https://ciwta.org/}

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

@titlepage
@title The Book of Romance II
@subtitle A reference manual for Tootsville V, version ~:*~a.

@author Bruce-Robert Pocock <@email{BRPocock@@ciwta.org}>

@page
@quotation
This manual is based upon materials taken from Declt 2.3.
@end quotation
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@contents

@ifnottex
@node Top, Copying, (dir), (dir)
@top The Book of Romance II

This is The Book of Romance II, describing the Romance II game core and
Tootsville V in particular. This manual is generated from the docstrings found
in the Tootsville package and supporting packages.

@menu
* Copying:: The GNU Affero General Public License
* Introduction:: What Tootsville Ⅴ (Romance Ⅱ) is all about
* Definitions:: The symbols documentation
* Conclusion:: Time to go
* Indices:: Concepts, functions, variables and data types
@end menu

@insertcopying
@end ifnottex

@node Copying, Introduction, Top, Top
@unnumbered Copying
@quotation
This  program is  free  software; you  can redistribute  it
and/or  modify it  under  the terms  of the  GNU  Affero General  Public
License as  published by  the Free  Software Foundation;  either version
3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program; if not,  write to the Free Software Foundation,
Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
@end quotation

@node Introduction, Definitions, Copying, Top
@chapter Introduction ~2%"
              (asdf:component-version (asdf:find-system :Tootsville)))
      (princ (alexandria:read-file-into-string
              (merge-pathnames #p"src/doc/Introduction.texi" source-dir))
             docs)
      (format docs "

@node Definitions, Conclusion, Introduction, Top
@chapter Definitions

@menu
")
      
      (let ((defs (gather-all-symbols)))
        (format docs "~{~%* ~a::~}" defs)
        (format docs "~&@end menu~2% ")
        (dolist (symbol defs)
          (write-documentation symbol docs)))
      
      (format docs "

@node Credits, Conclusion, Definitions, Top
@chapter Credits

Tootsville is built upon a plethora of software. This is an attempt to convey at least a partial enumeration of the credits.

First, the most noticeable:

~a

~a

@subsection The Steel Bank Common Lisp compiler

@verbatim
~a
@end verbatim
 "
              +credits+
              (all-credits)
              (read-file-into-string "/usr/share/doc/sbcl/CREDITS"))
      
      (format docs "

@node Conclusion, Indices, Credits, Top
@chapter Conclusion


")
      (princ (alexandria:read-file-into-string
              (merge-pathnames #p"src/doc/Conclusion.texi" source-dir))
             docs)
      
      (format docs "@node Indices, , Conclusion, Top
@appendix Indices
@menu
* Concept index::
* Function index::
* Variable index::
* Data type index::
@end menu


@c -------------
@c Concept index
@c -------------
@node Concept index, Function index, Indices, Indices
@appendixsec Concepts
@printindex cp

@page


@c --------------
@c Function index
@c --------------
@node Function index, Variable index, Concept index, Indices
@appendixsec Functions
@printindex fn

@page


@c --------------
@c Variable index
@c --------------
@node Variable index, Data type index, Function index, Indices
@appendixsec Variables
@printindex vr

@page


@c ---------------
@c Data type index
@c ---------------
@node Data type index, , Variable index, Indices
@appendixsec Data types
@printindex tp

@bye

"))))
