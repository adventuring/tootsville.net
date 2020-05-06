;;;; -*- lisp -*-
;;;
;;;; src/websockets.lisp is part of Tootsville
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



(defvar *websocket-server*)

(defmacro with-websocket-disconnections ((client) &body body)
  `(handler-case
       (progn ,@body)
     (sb-int:broken-pipe (c)
       (v:warn :stream "Disconnect detected on ~a for ~a"
               (stream-error-stream c) ,client)
       (force-close-hunchensocket ,client))
     (error (c)
       (v:error :stream "Error ~a sending ~:d character~:p to ~:a"
                c (length message) ,client))))

(defclass websocket-acceptor (hunchensocket:websocket-acceptor)
  ((hunchentoot::taskmaster
    :initform (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))
  (:default-initargs
   :access-log-destination (config :log :websocket :access)
    :message-log-destination (config :log :websocket :message)
    :port 5004))

(defclass websocket-ssl-acceptor (hunchensocket:websocket-ssl-acceptor)
  ((hunchentoot::taskmaster
    :initform (make-instance 'thread-pool-taskmaster:thread-pool-taskmaster)))
  (:default-initargs
   :access-log-destination (config :log :websocket :access)
    :message-log-destination (config :log :websocket :message)
    :port 5004
    :ssl-certificate-file (ssl-certificate)
    :ssl-privatekey-file (ssl-private-key)))

(defclass infinity-websocket-resource (hunchensocket:websocket-resource)
  ()
  (:default-initargs :client-class 'ws-client))

(defclass ws-client (hunchensocket:websocket-client)
  ((user :accessor user-account :initform nil)))

(defmethod print-object ((user ws-client) s)
  (format s "#<WS-Client from ~a~:[ without user~;~:* for ~a~]>"
          (when (slot-boundp user 'hunchensocket::input-stream)
            (slot-value (slot-value user 'hunchensocket::input-stream)
                        'sb-impl::name))
          (when (slot-boundp user 'user)
            (user-account user))))

(defvar *infinity-websocket-resource* (make-instance 'infinity-websocket-resource))

(defun find-infinity-websocket-resource (request)
  (let ((uri (hunchentoot:request-uri request)))
    (when (string-equal uri "/infinity/alef-null")
      *infinity-websocket-resource*)))

(pushnew 'find-infinity-websocket-resource hunchensocket:*websocket-dispatch-table*)

(defun broadcast (message &key near)
  "Broadcast MESSAGE to all ∞ Mode listeners connected who are near NEAR.

NEAR is a Toot  character who is the epicenter of  the message, which is
currently ignored."
  (declare (ignore near))
  (ws-broadcast *infinity-websocket-resource* message)
  (tcp-broadcast message))

(defun unicast (message &optional (user *user*))
  "Send MESSAGE directly to USER (which may be a Person or Toot)"
  (let ((client (user-stream user)))
    (with-websocket-disconnections (client)
      (ws-unicast message client))))

(defun streams-near (&optional (Toot *Toot*))
  "Find the streams assocated with users whose Toots are near TOOT."
  (declare (ignore Toot))
  (error 'unimplemented))

(defmethod user-stream ((null null))
  nil)

(defmethod user-stream ((user person))
  "Find the stream associated with USER"
  (dolist (client (remove-if-not #'user-account
                                 (hunchensocket:clients *infinity-websocket-resource*)))
    (when (uuid:uuid= (person-uuid (user-account client))
                      (person-uuid user))
      (return-from user-stream client)))
  nil)

(defmethod user-stream ((Toot Toot))
  "Find the stream associated with USER"
  (user-stream (find-reference Toot :player))
  nil)

(defun force-close-hunchensocket (client)
  "Attempt to destroy the connection to CLIENT"
  (ignore-errors
    (with-slots (clients lock) *infinity-websocket-resource*
      (bt:with-lock-held (lock)
        (with-slots (write-lock) client
          (bt:with-lock-held (write-lock)
            (setq clients (remove client clients))
            (setq write-lock nil))))))
  (hunchensocket:client-disconnected *infinity-websocket-resource* client))

(defun ws-broadcast (res message &key except)
  "Low-level broadcast MESSAGE to all WebSocket clients of resource RES.

You almost certainly don't want to call this --- you want `BROADCAST'."
  (let ((message (typecase message
                   (string message)
                   (cons (jonathan:to-json message)))))
    (lparallel:pmapcar
     (lambda (client)
       (unless (eql client except)
         (with-websocket-disconnections (client)
           (hunchensocket:send-text-message client message))))
     (hunchensocket:clients res))
    (v:info :stream "Broadcast to ~a: ~:d character~:p" res (length message))))

(defun ws-unicast (message user)
  "Low-level unicast MESSAGE to USER over WebSockets"
  (let ((message (typecase message
                   (string message)
                   (cons (jonathan:to-json message))
                   (hash-table (jonathan:to-json message))
                   (nil nil)))
        (user-stream (etypecase user
                       (ws-client user)
                       (person (user-stream user)))))
    (if user-stream
        (progn
          (hunchensocket:send-text-message user-stream message)
          (v:info :stream "Unicast to ~a: ~:d character~:p" user-stream (length message)))
        (v:warn :stream "Unable to send ~:d character~:p to ~a" (length message) user))))

(defmethod hunchensocket:client-connected ((res infinity-websocket-resource) client)
  (v:info :stream "WebSocket connection on ~a from ~a" res client))

(defmethod hunchensocket:client-disconnected ((res infinity-websocket-resource) client)
  (v:info :stream "WebSocket disconnection on ~a from ~a" res client)
  (when *Toot*
    (ws-broadcast *infinity-websocket-resource*
                  (list :|from| "bye"
                        :|status| t
                        :|u| (Toot-uuid *Toot*)
                        :|n| (Toot-name *Toot*))
                  :except client)))

(defun ws-reply (message ws-client)
  (let ((text (jonathan.encode:to-json (lastcar message))))
    (unless (equal text "[]")
      (v:info :stream "Unicast text reply to ~a, ~:d character~:p" 
              ws-client (length text))
      (hunchensocket:send-text-message ws-client text))))

(defmethod hunchensocket:text-message-received ((res infinity-websocket-resource)
                                                user message)
  (if-let (*user* (user-account user))
    (ws-reply (call-infinity-from-stream message)
              user)
    (websocket-authenticate user message)))

(defun who-is-connected ()
  "All users currently connected"
  (remove-if #'null
             (mapcar #'user-account
                     (hunchensocket:clients *infinity-websocket-resource*))))

(defun connected-Toots ()
  "All Toots currently connected"
  (remove-if #'null
             (mapcar #'player-Toot (who-is-connected))))

(defun find-user-for-json (json)
  "Find a user based on submitted authentication JSON"
  (if (and json
           (string= "∞/ℵ₀" (getf json :|auth|)))
      (let ((provider (getf json :|provider|))
            (token (getf json :|token|)))
        (when (and provider token)
          (v:info :auth "Provider ~:(~a~) asserts token (… ~a)"
                  provider (subseq token (max 0 (- (length token) 50))
                                   (length token)))
          (assert (string-equal provider "Firebase"))
          (ensure-user-for-plist
           (check-firebase-id-token token))))
      (progn (v:warn :auth "Unsupported ∞ JSON auth, ~s" json)
             nil)))

(defun login-ok-message ()
  "Produce a logOK message for successful login"
  (jonathan:to-json
   (list :|_cmd| "logOK"
         :|status| t
         :|auth| "accepted auth, ∞ mode ℵ₀ protocol (Tootsville/5.0)"
         :|greet| "Welcome to Tootsville! Let's make some noise."
         :|motd| *motd*
         :|userAccount| (person-info *user*))))

(defun login-failed-message ()
  "Produce a logOK for failed login"
  (jonathan:to-json
   (list :|_cmd| "logOK"
         :|status| :false
         :|auth| "rejected auth, ∞ mode ℵ₀ protocol (Tootsville/5.0)"
         :|motd| *motd*
         :|error| "Authentication attempt was rejected. Either your software is trying to\
use the ∞ mode $Eden-CHAP login method, which is not currently supported by the \
Tootsville 5 demo version, or it is submitting invalid credentials using mode ℵ₀.")))

(defun Toot-join-message (&optional (Toot *Toot*) (world "CHOR"))
  "Send joinOK message for TOOT"
  (list :|from| "joinOK"
        :|status| t
        :|uLs| (toot-uuid Toot)
        :|n| (toot-name Toot)
        :|r| world))

(defun ws-kick (client)
  "Kick a WebSocket connected user off-line"
  (unicast (list :|from| "admin"
                 :|status| t
                 :|title| "Whoops!"
                 :|message| "You've signed in from another location.")
           *user*)
  (unicast (list :|from| "goToWeb"
                 :|status| t
                 :|url| "https://Tootsville.org/")
           *user*)
  (force-close-hunchensocket client))

(defun websocket-authenticate (client auth$)
  (let ((auth (jonathan.decode:parse auth$)))
    (if-let (*user* (find-user-for-json auth))
      (progn
        (v:info :stream "User authenticated as ~a on ~a" *user* client)
        (when-let (other-client (user-stream *user*))
          (ws-kick other-client))
        (setf (user-account client) *user*)
        (hunchensocket:send-text-message client (login-ok-message)))
      (hunchensocket:send-text-message client (login-failed-message)))))

(defun listen-for-websockets ()
  (setf *websocket-server*
        (make-instance (if (enable-ssl-p)
                           'websocket-ssl-acceptor
                           'websocket-acceptor)))
  (hunchentoot:start *websocket-server*))

(defun stop-listening-for-websockets ()
  (when *websocket-server*
    (hunchentoot:stop *websocket-server*)
    (setf *websocket-server* nil)))



(defun admin-message (title message
                      &key (label title))
  (broadcast (list :|from| "admin"
                   :|status| t
                   :|title| title
                   :|label| label
                   :|message| message)))

(defun private-admin-message (title message
                              &key (label title)
                                   (user *user*))
  (let ((*user* user))
    (if *user*
        (unicast (list :|from| "admin"
                       :|status| t
                       :|title| title
                       :|label| label
                       :|message| message))
        (v:info :admin "Admin message to nobody: “~a”: “~a” (~a)"
                title message label))))

(defun Toot-speak (speech &key (Toot *Toot*) vol)
  "Broadcast a public message of SPEECH from TOOT at volume VOL."
  (broadcast (list :|from| "pub"
                   :|u| (Toot-name Toot)
                   :|t| speech
                   :|x| vol
                   :|id| (Toot-UUID Toot))
             :near Toot))
