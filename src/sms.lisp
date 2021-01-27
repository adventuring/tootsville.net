;;;; -*- lisp -*-
;;;
;;;; src/sms.lisp is part of Tootsville
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




(defun Toot-SMS-messages (Toot &key (from nil) (limit 100))
  "Find TOOT's SMS message starting with FROM, up to LIMIT.

FROM can be a UUID or an index from 0."
  (error 'unimplemented))

(defun SMS-message-index (Toot UUID)
  "Find the SQL position of UUID in a TOOT's mailbox"
  (error 'unimplemented))

(defun send-SMS-message (&key from to body uuid)
  "Send the SMS message from FROM to TO with body BODY. On success or error, reference UUID.

If online, both FROM and TO will receive notifications.

FROM and TO may be Toot designators, or TO may be a list of Toot
designators.

See `INFINITY-SEND-MAIL-MESSAGE'."
  (error 'unimplemented))
