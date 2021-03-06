;;;; -*- lisp -*-
;;;
;;;; src/utils.lisp is part of Tootsville
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

(in-package :Tootsville)

;;; Obscure compiler features?

#+sbcl
(sb-alien:define-alien-routine ("disable_lossage_handler" disable-sbcl-ldb)
    sb-alien:void)
#-sbcl
(defun disable-sbcl-ldb ())

#+sbcl
(sb-alien:define-alien-routine ("enable_lossage_handler" enable-sbcl-ldb)
    sb-alien:void)
#-sbcl
(defun enable-sbcl-ldb ())



(defun bool-sort (a b)
  "Sort Boolean values"
  (declare (ignore b))
  a)

(defun sinus (x range)
  "Give the Y value at X in a sinus curve"
  (sin (/ (mod x range) (/ range pi))))

(defun divisible-by-200-p (n)
  "Is N evenly divisible by 200?"
  (zerop (mod n 200)))

(defun split-plist (plist)
  "Split a PLIST into two lists, of keys and values."
  (loop for (key value) on plist by #'cddr
     collect key into keys
     collect value into values
     finally (return (list keys values))))



(defun sync ()
  (dolist (stream (list *standard-output* *error-output* *trace-output* *query-io*))
    (finish-output stream))
  (uiop:run-program "sync")
  (values))

(defun chdir (new-path)
  (setf *default-pathname-defaults* (merge-pathnames new-path)))

