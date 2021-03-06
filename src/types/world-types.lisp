;;;; -*- lisp -*-
;;;
;;;; src/types/world-types.lisp is part of Tootsville
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

(defun world-moniker-p (moniker)
  "cv. `MAP-PLACES'

The monikers for the worlds are the hard list:

@table @code
@item CHOR
Chœrogryllum (the planet on which Tootsville is found).
@item MOON
The moon.
@item OTHM
The other moon.
@item PINK
The pink moon.
@item ORBIT
In orbit of Chœrogryllum, but not on any moon.
@end table"
  (and (symbolp moniker)
       (member moniker '(:CHOR :MOON :OTHM :PINK :ORBIT))))

(deftype world-moniker ()
  "cv `MAP-PLACES'"
  '(satisfies world-moniker-p))

(defconstant +moon-year+ 3600000)
(defconstant +other-moon-year+ 583243)
(defconstant +pink-moon-year+ 452398723)

(defconstant +moon-day+ (* 18 60 59))
(defconstant +other-moon-day+ (* 2 18 60 58))
(defconstant +pink-moon-day+ (* 8 18 60 56))
