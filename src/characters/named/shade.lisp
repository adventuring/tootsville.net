;;;; -*- lisp -*-
;;;
;;;; src/characters/named/shade.lisp is part of Tootsville
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



(defmethod initialize-robot ((robot robot) (Toot-name (eql :shade)))
  (initialize-robo-Toot robot))

(robo-Toot-heard* (shade nil)
  (robo-match ("shade")
    (robo-set-mode how-are-you)
    (robo-Toot-say robot "Hello, ~a. How are you?" (Toot-name speaker))))

(robo-Toot-heard* (shade how-are-you)
  (robo-match ("good")
    (robo-Toot-say robot "I'm glad to hear that"))
  (robo-match ("you\\?" "yourself\\?" "how are you" "you doing\\?" "howdy do")
    (robo-Toot-say robot "I'm doing well, thanks for asking")))
  
