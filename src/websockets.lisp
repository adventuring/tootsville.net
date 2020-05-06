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

(defmacro with-disconnect-catcher (() &body body)
  `(handler-case
       (progn ,@body)
     (sb-int:broken-pipe (c)
       (v:warn "Surprise disconnection on WebSocket ignored: ~a" c))))

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
  (:default-initargs :client-class 'stream-user))

(defclass stream-user (hunchensocket:websocket-client)
  ((user :accessor user-account :initform nil)))

(defmethod print-object ((user stream-user) s)
  (format s "#<Stream-User ~@[for ~a~]>"
          (when (slot-boundp user 'user)
            (user-account user))))

(defvar *infinity-websocket-resource* (make-instance 'infinity-websocket-resource))

(defun find-infinity-websocket-resource (request)
  (let ((uri (hunchentoot:request-uri request)))
    (when (string-equal uri "/infinity/alef-null")
      *infinity-websocket-resource*)))

(pushnew 'find-infinity-websocket-resource hunchensocket:*websocket-dispatch-table*)

(defun broadcast (message)
  "Broadcast MESSAGE to all ∞ Mode listeners connected."
  (ws-broadcast *infinity-websocket-resource* message))

(defun unicast (message &optional (user *user*))
  (ws-unicast *infinity-websocket-resource* message user))

(defun unicast-to-toot (message &optional (toot *Toot*))
  (unicast message (find-record 'person :uuid (toot-player toot))))

(defun force-close-hunchensocket (client)
  (ignore-errors
    (with-slots (clients lock) *infinity-websocket-resource*
      (bt:with-lock-held (lock)
        (with-slots (write-lock) client
          (bt:with-lock-held (write-lock)
            (setq clients (remove client clients))
            (setq write-lock nil))))))
  (hunchensocket:client-disconnected *infinity-websocket-resource* client))

(defun ws-broadcast (res message)
  (let ((message (typecase message
                   (string message)
                   (cons (jonathan:to-json message)))))
    (lparallel:pmapcar (lambda (peer)
                         (handler-bind
                             ((sb-int:broken-pipe (lambda (c)
                                                    (v:warn :stream "Disconnect detected on ~a for ~a"
                                                            (stream-error-stream c) peer)
                                                    (force-close-hunchensocket peer))))
                           (hunchensocket:send-text-message peer message)))
                       (hunchensocket:clients res))
    (v:info :stream "Broadcast to ~a: ~:d character~:p" res (length message))))

(defun ws-unicast (res message user)
  (let ((message (typecase message
                   (string message)
                   (cons (jonathan:to-json message))
                   (nil nil))))
    (dolist (peer (hunchensocket:clients res))
      (when (and (user-account peer)
                 (uuid:uuid= (person-uuid (user-account peer))
                             (person-uuid user)))
        (hunchensocket:send-text-message peer message)
        (v:info :stream "Unicast to ~a: ~:d character~:p" peer (length message))
        (return-from ws-unicast)))))

(defmethod hunchensocket:client-connected ((res infinity-websocket-resource) user)
  (v:info :stream "WebSocket connection on ~a from ~a" res user))

(defmethod hunchensocket:client-disconnected ((res infinity-websocket-resource) user)
  (v:info :stream "WebSocket disconnection on ~a from ~a" res user)
  (when *Toot*
    (broadcast (list :|from| "bye"
                     :|status| t
                     :|u| (Toot-uuid *Toot*)
                     :|n| (Toot-name *Toot*)))))

(defun ws-reply (message stream-user)
  (let ((text (jonathan.encode:to-json (lastcar message))))
    (unless (equal text "[]")
      (v:info :stream "Unicast text reply to ~a, ~:d character~:p" stream-user (length text))
      (hunchensocket:send-text-message stream-user text))))

(defmethod hunchensocket:text-message-received ((res infinity-websocket-resource) user message)
  (if-let (*user* (user-account user))
    (ws-reply (call-infinity-from-stream message)
              user)
    (websocket-authenticate user message)))

(defun who-is-connected ()
  (remove-if #'null
             (mapcar #'user-account
                     (hunchensocket:clients *infinity-websocket-resource*))))

(defun connected-Toots ()
  (remove-if #'null
             (mapcar #'player-Toot (who-is-connected))))

(defun find-user-for-json (json)
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

(defun log-ok-message ()
  (jonathan:to-json
   (list :|_cmd| "logOK"
         :|status| t
         :|auth| "accepted auth, ∞ mode ℵ₀ protocol (Tootsville/5.0)"
         :|greet| "Welcome to Tootsville! Let's make some noise."
         :|motd| *motd*
         :|userAccount| (person-info *user*))))

(defun login-failed-message ()
  (jonathan:to-json
   (list :|_cmd| "logOK"
         :|status| :false
         :|auth| "rejected auth, ∞ mode ℵ₀ protocol (Tootsville/5.0)"
         :|motd| *motd*
         :|error| "Authentication attempt was rejected. Either your software is trying to\
use the ∞ mode $Eden-CHAP login method, which is not currently supported by the \
Tootsville 5 demo version, or it is submitting invalid credentials using mode ℵ₀.")))

(defun Toot-join-message (&optional (Toot *Toot*) (room "@Tootsville"))
  (list :|from| "joinOK"
        :|status| t
        :|uLs| (toot-name Toot)
        :|r| room))

(defun ws-kick (client)
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

(defun websocket-authenticate (user auth$)
  (let ((auth (jonathan.decode:parse auth$)))
    (if-let (*user* (find-user-for-json auth))
      (progn 
        (dolist (client (hunchensocket:clients *infinity-websocket-resource*))
          (when-let (account (user-account client))
            (when (uuid:uuid= (person-uuid account) (person-uuid *user*))
              (ws-kick client))))
        (setf (user-account user) *user*)
        (hunchensocket:send-text-message user (log-ok-message)))
      (hunchensocket:send-text-message user (login-failed-message)))))

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

(defun toot-speak (speech &key (toot *toot*) vol)
  (broadcast (list :|from| "pub"
                   :|u| (toot-name toot)
                   :|t| speech
                   :|x| vol
                   :|id| (toot-uuid toot))))
