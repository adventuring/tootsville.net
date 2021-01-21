;;;; -*- lisp -*-
;;;
;;;; src/characters/named/mayor-louis.lisp is part of Tootsville
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



(define-character Mayor-Louis Shaddow)

(define-reply (Mayor-Louis nil)
  (robot-match ("louis")
    (robot-say robot "Don't tell anybody that I'm secretly a Shaddow agent.")
    (do-after (9)
      (robot-say robot "I'm trying to help Shade escape from his prison in Shaddow Falls.")))
  (robot-match ("shade")
    (robot-say robot "When Zap and his friends discovered my treachery,")
    (do-after (4)
      (robot-say robot "… they trapped Shade in a prison in Shaddow Falls."))
    (do-after (10)
      (robot-say robot "Now Zap and his do-gooder friends are in charge."))
    (do-after (15)
      (robot-say robot "Us Shaddows will figure out how to free Shade and take over again."))))
               
