;;;; -*- lisp -*-
;;;
;;;; src/tcp-stream.lisp is part of Tootsville
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



(defvar *tcp-listener* nil)
(defvar *tcp-clients* (make-hash-table :test 'equalp))
(defvar *tcp-peer-traffic* 0)

(defclass tcp-client ()
  ((socket :initarg :socket :accessor tcp-client-socket)
   (buffer :initarg :buffer :accessor tcp-client-buffer)
   (expected-length :initarg :expected-length :accessor tcp-client-expected-length)
   (peer :initarg :peer :accessor tcp-client-peer)))

(defun tcp-unicast (message tcp-client)
  "Writes MESSAGE to TCP-CLIENT.

MESSAGE is encoded with a SOH (start of heading, ASCII value 1),
followed by the length of the message in base-36, then STX (start of
text, ASCII value 2), the message itself, and a final ETX (end of
text, ASCII value 3)."
  (tcp-bandwidth-record message)
  (format (usocket:socket-stream tcp-client) 
          "~36r~a" (length message) message))

(defun tcp-format-error (tcp-client)
  "Send a format error to TCP-CLIENT.

This is the character EM, End of Medium, ASCII value 25 (decimal)."
  (write-char #\EM tcp-client))

(defun tcp-stream-authenticate (client auth$)
  "Private server-to-server messaging authentication.

Tunnelled over SSH, so a simple non-cryptographically-secure
authentication is all that's performed here.

TODO: This is not implemented."
  (declare (ignore auth$))
  (tcp-format-error client)
  (error 'unimplemented))

(defun tcp-reply (message tcp-client)
  (tcp-unicast message tcp-client))

(defun tcp-broadcast (message)
  (let ((message (ensure-message-is-characters message)))
    (dolist (client (hash-table-values *tcp-clients*))
      (tcp-unicast message client))))

(defun tcp-bandwidth-record (message &optional (multiplier 1))
  (incf *tcp-peer-traffic* (* multiplier (length message))))

(defun tcp-handle-peer-request (message peer)
  (with-simple-restart (continue "Restart ∞ request processor")
    (tcp-bandwidth-record message)
    (tcp-reply (call-infinity-from-stream (jonathan.decode:parse message))
               peer)))

(defun tcp-process-packet (packet tcp-client)
  (if-let (peer (tcp-client-peer tcp-client))
    (tcp-handle-peer-request (jonathan.decode:parse packet) peer)
    (tcp-stream-authenticate tcp-client packet)))

(defun tcp-socket-input (tcp-client)
  (block nil
    (with-slots (socket expected-length buffer) tcp-client
      (let ((soh (read-char (usocket:socket-stream socket))))
        (unless (char= soh #\SOH)
          (return (tcp-format-error tcp-client))))
      (let ((length ""))
        (loop for char = (read-char (usocket:socket-stream socket))
           until (or (char= #\STX char)
                     (> (length length) 3)) ; #36rZZZ ≈ 45½k
           do (appendf length char))
        (let ((len (parse-integer length :radix 36)))
          (setf expected-length len
                buffer (make-array len :element-type 'character))))
      (loop for i from 0 below expected-length
         do (setf (aref buffer i) (read-char (usocket:socket-stream socket))))
      (let ((etx (read-char (usocket:socket-stream socket))))
        (unless (char= etx #\ETX)
          (return (tcp-format-error tcp-client))))
      (tcp-process-packet buffer tcp-client))))

(defun find-client-for-socket (socket)
  (gethash (usocket:get-peer-address socket) *tcp-clients*))

(defun start-tcp-listener (&optional (host "::1") (port 2773))
  "Start listening for TCP peers on interface HOST and PORT.

The default PORT is 2773."
  (setf *tcp-listener* (usocket:socket-listen host port :backlog 256))
  (loop
     (lparallel::pmapcar 
      (lambda (socket) 
        (if (eq socket *tcp-listener*)
            (let ((client (usocket:socket-accept socket
                                                 :element-type 'character)))
              (setf (gethash (usocket:get-peer-address socket)
                             *tcp-clients*)
                    (make-tcp-client 
                     :socket client
                     :buffer nil
                     :expected-length nil
                     :peer nil))
              (v:info :stream "TCP connection from ~a" client))
            (handler-case
                (tcp-socket-input (find-client-for-socket socket)))))
      (usocket:wait-for-input (cons *tcp-listener* *tcp-clients*) :ready-only t))))

(defun server-list ()
  "A list of all servers active in the current cluster."
  (cons (machine-instance)
        (mapcar #'tcp-client-peer (hash-table-values *tcp-clients*))))
