;;;; -*- lisp -*-
;;;
;;;; src/types/world-types.lisp    is    part    of
;;;; Tootsville
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

(in-package :Tootsville)

(defun world-moniker-p (moniker)
  (and (symbolp moniker)
       (member moniker '(:CHOR :MOON :OTHM :PINK :ORBIT))))

(defun world-name (moniker)
  (ecase moniker
    (:chor "Chœorgryllum")
    (:moon "The Moon")
    (:othm "The Other Moon")
    (:pink "The Pink Moon")
    (:orbit "Orbit around Chœrogryllum")))

(deftype world-moniker ()
  '(satisfies world-moniker-p))
