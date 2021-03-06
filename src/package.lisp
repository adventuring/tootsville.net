;;;; -*- lisp -*-
;;;
;;;; src/package.lisp is part of Tootsville
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

(cl:in-package :cl-user)
(in-package :cl-user)

#+sbcl
(require 'sb-introspect)

(defpackage Tootsville
  (:documentation  "Let's make some noise!

The  Tootsville  package  is  the  main  container  for  all  Tootsville
V (Romance II) server functions.

In  future, some  of the  concerns  (e.g. specific  facilities like  the
friendly database  accessors or  the metronome) may  be broken  out into
their own packages, but for  simplicity of early development, everything
is in this one big ball of mud.
")
  (:use :alexandria :cl :local-time :bordeaux-threads
        :oliphaunt)
  (:shadowing-import-from :cl-fad
                          #:copy-file #:copy-stream ; conflicts with Alexandria.
                          #:directory-pathname-p)
  (:import-from :trivial-backtrace #:print-backtrace)
  (:import-from :sb-introspect
                #:function-lambda-expression #:function-lambda-list)
  (:import-from :uiop #:run-program)
  (:import-from :jonathan.encode #:%to-json #:to-json)
  (:export
   #:*application-root*
   #:*compiled*
   #:config
   #:connection-settings
   #:debugger
   #:db
   #:entry
   #:journal
   #:middleware
   #:power-on-self-test
   #:print-help
   #:rebuild-myself
   #:render-json
   #:start
   #:start-hunchentoot
   #:start-repl
   #:start-swank
   #:stop
   #:three-chars-in-a-row-p
   #:two-chars-in-a-row-p
   #:wants-json-p
   #:with-connection
   #:write-docs
   ))
