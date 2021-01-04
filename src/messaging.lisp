;;;; -*- lisp -*-
;;;
;;;; src/messaging.lisp is part of Tootsville
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



(defun broadcast (message &key near except)
  "Broadcast MESSAGE to all ∞ Mode listeners connected who are near NEAR.

NEAR is a Toot  character who is the epicenter of  the message, which is
currently ignored.

EXCEPT is  a user  or Toot who  does not need  to receive  the broadcast
message (usually the originator)"
  (declare (ignore near))
  (ws-broadcast *infinity-websocket-resource* message 
                :except (user-stream except))
  (tcp-broadcast message)
  (robot-broadcast message near :except except))

(defun unicast (message &optional (user (active-player)))
  "Send MESSAGE directly to USER (which may be a Person or Toot)"
  (if-let ((client (user-stream user)))
    (with-websocket-disconnections (client)
      (ws-unicast message client))
    (if (robotp user)
      (robot-unicast message user)
      (v:warn :stream "Unable to transmit unicast message to ~a: not connected"
              user))))

