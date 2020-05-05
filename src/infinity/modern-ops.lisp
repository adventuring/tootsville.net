;; -*- lisp -*-

;;; src/infinity/modern-ops.lisp is part of Tootsville
;;;
;;; Copyright ©  2008-2017, Bruce-Robert  Pocock; Copyright  © 2009,2010
;;; Res  Interactive LLC;  Copyright  © 2018-2020,  the Corporation  for
;;; Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;; This program is Free Software: you can redistribute it and/or modify
;;; it  under the  terms of  the GNU  Affero General  Public License  as
;;; published by the  Free Software Foundation; either version  3 of the
;;; License, or (at your option) any later version.
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


(in-package :Tootsville)

(define-operator-command quick-reload (words u r)
  "Quicklisp reload of the Tootsville package from disk."
  (asdf:load-asd (asdf:system-source-file :Tootsville))
  (ql:quickload :Tootsville)
  (return (format nil "Now running Tootsville.net server version ~a"
                  (asdf:component-version (asdf:find-system :Tootsville)))))

(define-operator-command push-script (words u r)
  "Instruct clients to load a new script file"
  (broadcast (list :|from| "newScript"
                   :|status| t
                   :|script| (first words))))
