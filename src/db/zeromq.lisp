;;;; -*- lisp -*-
;;;
;;;; src/db/couch.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018-2020  The
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

(defvar *gossip-sdp-lock* (make-lock "Gossip SDP queue lock"))

#+ (or)
(defun enqueue-sdp-offer (sdp)
  (check-type sdp string)
  (let* ((uuid (uuid:make-v4-uuid))
         (uuid-msg (zeromq:make-msg :data (uuid:uuid-to-byte-array uuid)))
         (bytes (flexi-streams:string-to-octets sdp
                                                :external-format :utf-8))
         (message (zeromq:make-msg :data bytes)))
    (with-lock-held (*gossip-sdp-lock*)
      (zeromq:send *gossip-sdp-push* "SDPO")
      (zeromq:msg-send *gossip-sdp-push* uuid-msg)
      (zeromq:msg-send *gossip-sdp-push* message))
    uuid))

#+ (or)
(defun dequeue-sdp-offer ()
  (when (string= "SDPO" (with-lock-held (*gossip-sdp-lock*)
                          (zeromq:recv *gossip-sdp-pull* 4)))
    (let ((uuid-data (zmq:make-msg))
          (offer-data (zmq:make-msg)))
      (with-lock-held (*gossip-sdp-lock*)
        (zmq:msg-recv *gossip-sdp-pull* uuid-data)
        (zmq:msg-recv *gossip-sdp-pull* offer-data))
      (handler-case
          (let* ((uuid (uuid:byte-array-to-uuid
                        (zmq:msg-data-as-array uuid-data)))
                 (offer (flexi-streams:octets-to-string
                         (zmq:msg-data-as-array offer-data)
                         :external-format :utf-8)))
            (list :|uuid| uuid
                  :|offer| offer))
        (babel-encodings:end-of-input-in-character (_)
          (declare (ignore _))
          nil)))))

(defvar *simple-sdp-queue* nil)

(defun enqueue-sdp-offer (offer)
  (let ((uuid (uuid:make-v4-uuid)))
    (appendf *simple-sdp-queue* (cons (cons uuid offer) nil))
    uuid))

(defun dequeue-sdp-offer ()
  (and *simple-sdp-queue*
       (destructuring-bind (uuid . offer)
           (pop *simple-sdp-queue*)
         (list :|uuid| uuid
               :|offer| offer))))
