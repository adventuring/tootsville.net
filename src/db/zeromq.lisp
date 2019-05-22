;;;; -*- lisp -*-
;;;
;;;; src/db/couch.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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

(declaim (optimize (speed 3)))

(defvar *gossip-sdp-context*)
(defvar *gossip-sdp-push*)
(defvar *gossip-sdp-pull*)

(defun connect-zeromq ()
  (setf *gossip-sdp-context* (zeromq:ctx-new)
        *gossip-sdp-push* (zeromq:socket *gossip-sdp-context* :push)
        *gossip-sdp-pull* (zeromq:socket *gossip-sdp-context* :pull))
  (let ((port (or (getf (db-config :zeromq) :port) 5010)))
    (zeromq:bind *gossip-sdp-push* (format nil "tcp://127.0.0.1:~d" port))
    (zeromq:connect *gossip-sdp-pull* (format nil "tcp://127.0.0.1:~d" port))
    port))

(defun enqueue-sdp-offer (sdp)
  (check-type sdp string)
  (let* ((uuid (uuid:make-v4-uuid))
         (uuid-bytes (uuid:uuid-to-byte-array uuid)))
    (zeromq:send *gossip-sdp-push* (format nil "~4,'0x" (+ 16 (length sdp))))
    (zeromq:msg-send *gossip-sdp-push* (zeromq:make-msg :size 16 :data uuid-bytes))
    (zeromq:send *gossip-sdp-push* sdp)
    uuid))

(defun dequeue-sdp-offer ()
  (ignore-errors
    (let* ((length (parse-integer (zeromq:recv *gossip-sdp-pull* 4)
                                  :radix 16))
           (string (zeromq:recv *gossip-sdp-pull* length)))
      (list :|uuid| (subseq string 0 16)
            :|offer| (subseq string 16)))))

