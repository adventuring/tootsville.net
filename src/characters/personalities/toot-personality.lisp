;;;; -*- lisp -*-
;;;
;;;; src/characters/personalities/toot-personality.lisp is part of
;;;; Tootsville
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



(define-personality Toot)

(define-personality Basic-8 Toot)

(define-reply (Basic-8 nil)
  (robot-match ("you from earth" "you from (chœrogryllum|choerogryllum)"
                "you from this planet")
    (robot-set-mode from-earth)
    (robot-say robot "I'm from Earth, originally, but now I live here on Chœrogryllum.")))

(define-reply (Basic-8 from-earth)
  (robot-match ("from earth")
    (robot-say robot "I grew up on Earth as an elephant, before coming through the Mist to Tootsville")))
                

(define-reply (Toot nil)
  (robot-match ((string-downcase (Toot-name (Toot robot))))
    (robot-set-mode how-are-you)
    (robot-say robot "Hello, ~a. How are you?" (Toot-name speaker))))

(define-reply (Toot how-are-you)
  (robot-match ("good")
    (robot-say robot "I'm glad to hear that"))
  (robot-match ("you\\?" "yourself\\?" "how are you" "you doing\\?" "howdy do")
    (robot-say robot "I'm doing well, thanks for asking")))

(define-reply (Toot nil)
  (robot-match ("\\bfuck" "\\bshit" "\\bdamn" "\\bgoddamn?\\b" )
    (robot-say robot "Maybe you should not use that kind of language around here.")))
