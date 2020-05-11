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



(defconstant +pre-login-max-time+ 5
  "How many seconds does a client have to authenticate?

Clients  which  fail   to  authenticate  within  the   time  limit  will
be disconnected.")

(defconstant +pre-login-max-commands+ 10
  "How many commands may a client issue before logging in?

This includes  the authentication packet.  Clients must sign  in without
issuing an exorbitant number of commands or they will be disconnected. ")

(defvar *client* nil)

(defvar *websocket-server* nil
  "The Hunchentoot/Hunchensocket server object for WebSockets.")

(defvar *websocket-maintenance-thread* nil
  "A thread which culls dead connections and periodically logs statistics about active WebSockets connections.")

(defvar *ws-connections* 0
  "The number of times that someone has connected ever. @emph{NOT} the same as @emph{active} connections.")
(defvar *ws-sign-ins* 0
  "The number of times that someone has authenticated (signed in) ever.")
(defvar *ws-chars-received* 0
  "Total payload characters read.")
(defvar *ws-chars-broadcast* 0
  "Total payload characters broadcast.

NB you'd have to  multiply this by connected clients to  get a real idea
of the bandwidth involved.")
(defvar *ws-chars-unicast* 0
  "Total payload characters unicast to anyone.")
(defvar *ws-surprise-disconnects* 0
  "Number of times someone has dropped a connection without a proper disconnection sequence.")

(defconstant +ws-idle-seconds+ 300
  "How long before we treat a connection as “idle” and start sending Are You There?")

(defvar *ws-traffic-commands* (make-hash-table :test 'equal))
(defvar *ws-traffic-from* (make-hash-table :test 'equal))
(defvar *ws-traffic-other* 0)

(defun ws-stats-reset-all ()
  (setf *ws-connections* 0
        *ws-sign-ins* 0
        *ws-chars-received* 0
        *ws-chars-unicast* 0
        *ws-chars-broadcast* 0
        *ws-surprise-disconnects* 0
        *ws-traffic-commands* (make-hash-table :test 'equal)
        *ws-traffic-from* (make-hash-table :test 'equal)
        *ws-traffic-other* 0))

(defun ws-bandwidth-by-source ()
  (format nil "Commands:
~{~a: ~:d character~:p~^;~%~}.

Replies:
~{~a: ~:d character~:p~^;~%~}.

Other:
~:d byte~:p."
          (alist-plist
           (sort (hash-table-alist *ws-traffic-commands*)
                 #'> :key 'cdr))
          (alist-plist
           (sort (hash-table-alist *ws-traffic-from*)
                 #'> :key 'cdr))
          *ws-traffic-other*))

(defun ws-bandwidth-record (packet &optional (multiplier 1))
  "Record bandwidth used by this PACKET.

For broadcasts, multiply by MULTIPLIER."
  (etypecase packet
    (cons (if-let (from (getf packet :|from|))
            (incf (gethash from *ws-traffic-from* 0)
                  (* multiplier (length (to-json packet))))
            (if-let (c (getf packet :|c|))
              (incf (gethash c *ws-traffic-commands* 0)
                    (* multiplier (length (to-json packet))))
              (if-let (c (getf packet :|_cmd|))
                (incf (gethash c *ws-traffic-commands* 0)
                      (* multiplier (length (to-json packet))))
                (incf *ws-traffic-other* 
                      (* multiplier (length (to-json packet))))))))
    (hash-table (if-let (from (gethash :|from| packet))
                  (incf (gethash from *ws-traffic-from* 0)
                        (* multiplier (length (to-json packet))))
                  (if-let (c (gethash :|c| packet))
                    (incf (gethash c *ws-traffic-commands* 0)
                          (* multiplier (length (to-json packet))))
                    (if-let (c (gethash :|_cmd| packet))
                      (incf (gethash c *ws-traffic-commands* 0)
                            (* multiplier (length (to-json packet))))
                      (incf *ws-traffic-other* 
                            (* multiplier (length (to-json packet))))))))
    (t (incf *ws-traffic-other* (* multiplier (length (princ-to-string packet)))))))

(defun ws-stats ()
  "Returns a string with some nifty statistics about WebSockets"
  (let* ((clients (hunchensocket:clients *infinity-websocket-resource*))
         (user-clients (remove-if-not #'user-account clients))
         (active-clients (remove-if-not (lambda (client)
                                          (> (last-active client) (- (get-universal-time) +ws-idle-seconds+)))
                                        clients)))
    (format nil "WebSockets: Connections: ~:d;
Sign-Ins: ~:d (~:d%);
Chars: Received: ~:d, Sent: Broadcast: ~:d, Unicast: ~:d;
Surprise Disconnects: ~:d;
Active Clients: ~:d;
Signed-In Clients: ~:d (~:d%);
Active Clients (~:d secs): ~:d (~:d%)."
            *ws-connections*
            *ws-sign-ins* (when (plusp *ws-connections*)
                            (round (* 100 (/ *ws-sign-ins* *ws-connections*))))
            *ws-chars-received* *ws-chars-broadcast* *ws-chars-unicast*
            *ws-surprise-disconnects*
            (length clients)
            (length user-clients) (when (plusp (length clients))
                                    (round (* 100 (/ (length user-clients)
                                                     (length clients)))))
            +ws-idle-seconds+
            (length active-clients) (when (plusp (length clients))
                                      (round (* 100 (/ (length active-clients)
                                                       (length clients))))))))

(defmacro with-websocket-disconnections ((client) &body body)
  "Handle errors caused by surprise disconnections by CLIENT."
  `(handler-case
       (progn ,@body)
     (sb-int:broken-pipe (c)
       (v:warn :stream "Disconnect detected on ~a for ~a"
               (stream-error-stream c) ,client)
       (force-close-hunchensocket ,client)
       (incf *ws-surprise-disconnects*))
     ;; (error (c)
     ;;   (v:error :stream "Error ~a sending ~:d character~:p to ~:a"
     ;;            c (length message) ,client))
     ))

(defclass websocket-acceptor (hunchensocket:websocket-acceptor)
  ()
  (:default-initargs
   :access-log-destination (config :log :websocket :access)
    :message-log-destination (config :log :websocket :message)
    :port 5004))

(defclass websocket-ssl-acceptor (hunchensocket:websocket-ssl-acceptor)
  ()
  (:default-initargs
   :access-log-destination (config :log :websocket :access)
    :message-log-destination (config :log :websocket :message)
    :port 5004
    :request-class 'hunchensocket::websocket-request
    :reply-class 'hunchensocket::websocket-reply
    :ssl-certificate-file (ssl-certificate)
    :ssl-privatekey-file (ssl-private-key)))

(defclass infinity-websocket-resource (hunchensocket:websocket-resource)
  ()
  (:default-initargs :client-class 'ws-client))

(defclass ws-client (hunchensocket:websocket-client)
  ((user :accessor user-account :initform nil)
   (Toot :accessor Toot :initform nil)
   (random-key :accessor random-key :initform nil)
   (pre-login-commands :accessor pre-login-commands :initform +pre-login-max-commands+)
   (connect-time :reader connect-time :initform (get-universal-time))
   (last-active :accessor last-active :initform (get-universal-time))))

(defmethod print-object ((user ws-client) s)
  (format s "#<WS-Client from ~a~:[ without user~;~:* for ~a~]>"
          (when (slot-boundp user 'hunchensocket::input-stream)
            (princ-to-string (slot-value user 'hunchensocket::input-stream)))
          (when (slot-boundp user 'user)
            (user-account user))))

(defvar *infinity-websocket-resource* (make-instance 'infinity-websocket-resource))

(defun find-infinity-websocket-resource (request)
  (let ((uri (hunchentoot:request-uri request)))
    (when (or (string-equal uri "/infinity/alef-null")
              (string-equal uri "/Tootsville"))
      *infinity-websocket-resource*)))

(pushnew 'find-infinity-websocket-resource hunchensocket:*websocket-dispatch-table*)

(defun broadcast (message &key near except)
  "Broadcast MESSAGE to all ∞ Mode listeners connected who are near NEAR.

NEAR is a Toot  character who is the epicenter of  the message, which is
currently ignored.

EXCEPT is  a user  or Toot who  does not need  to receive  the broadcast
message (usually the originator)"
  (declare (ignore near))
  (ws-broadcast *infinity-websocket-resource* message 
                :except (user-stream except))
  (tcp-broadcast message))

(defun unicast (message &optional (user (active-player)))
  "Send MESSAGE directly to USER (which may be a Person or Toot)"
  (if-let ((client (user-stream user)))
    (with-websocket-disconnections (client)
      (ws-unicast message client))
    (v:warn :stream "Unable to transmit unicast message to ~a: not connected"
            user)))

(defun streams-near (&optional (Toot *Toot*))
  "Find the streams assocated with users whose Toots are near TOOT."
  (declare (ignore Toot))
  (error 'unimplemented))

(defmethod user-stream ((null null))
  nil)

(defmethod user-stream ((client ws-client))
  client)

(defmethod user-stream ((user person))
  "Find the stream associated with USER"
  (or (gethash (uuid:uuid-to-byte-array (person-uuid User)) *ws-client-for-user*)
      (dolist (client (remove-if-not #'user-account
                                     (hunchensocket:clients *infinity-websocket-resource*)))
        (when (uuid:uuid= (person-uuid (user-account client))
                          (person-uuid user))
          (return-from user-stream client))))
  nil)

(defmethod user-stream ((Toot Toot))
  "Find the stream associated with TOOT"
  (or (gethash (uuid:uuid-to-byte-array (Toot-uuid Toot)) *ws-client-for-Toot*)
      (dolist (client (remove-if-not #'Toot
				     (hunchensocket:clients *infinity-websocket-resource*)))
	(when (uuid:uuid= (Toot-uuid (Toot client))
			  (Toot-uuid Toot))
	  (return-from user-stream client))))
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
  (ws-bandwidth-record message (length (hunchensocket:clients res)))
  (let ((message (ensure-message-is-characters message)))
    (lparallel:pmapcar
     (lambda (client)
       (unless (eql client except)
         (with-websocket-disconnections (client)
           (hunchensocket:send-text-message client message)
           (incf *ws-chars-broadcast* (length message)))))
     (hunchensocket:clients res))
    (v:info :stream "Broadcast to ~a: ~:d character~:p" res (length message))))

(defun ensure-message-is-characters (message)
  "Convert MESSAGE into a string of characters, probably as JSON."
  (typecase message
    (string message)
    (cons (to-json message))
    (hash-table (to-json message))
    (nil nil)))

(defun websockets-unicast-low-level% (message user-stream)
  (hunchensocket:send-text-message user-stream message)
  (v:info :stream "Unicast to ~a: ~:d character~:p" user-stream (length message))
  (incf *ws-chars-unicast* (length message)))

(defun ws-unicast (message user)
  "Low-level unicast MESSAGE to USER over WebSockets"
  (ws-bandwidth-record message)
  (let ((message (ensure-message-is-characters message))
        (user-stream (etypecase user
                       (ws-client user)
                       (person (user-stream user)))))
    (if user-stream
        (websockets-unicast-low-level% message user-stream)
        (v:warn :stream "Unable to send ~:d character~:p to ~a" (length message) user))))

(defmethod hunchensocket:client-connected ((res infinity-websocket-resource) client)
  (v:info :stream "WebSocket connection on ~a from ~a" res client)
  (incf *ws-connections*))

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
  "Send a reply MESSAGE to a WebSocket WS-CLIENT from an Infinity handler."
  (ws-bandwidth-record message)
  (let ((text (ensure-message-is-characters (lastcar message))))
    (unless (zerop (length text))
      (v:info :stream "Unicast text reply to ~a, ~:d character~:p" 
              ws-client (length text))
      (incf *ws-chars-unicast* (length text))
      (hunchensocket:send-text-message ws-client text))))

(defmethod hunchensocket::process-connection :around ((acceptor websocket-acceptor) socket)
  (handler-bind
      ((sb-sys:io-timeout 
        (lambda (c)
          (v:warn "Ignoring ~a" c))))
    (call-next-method)))

(defun disconnect-no-login (client)
  "Disconnect client for failing to log in within the required time or number of commands."
  (ws-unicast 
   (list :|from| "preLogin"
         :|status| :false
         :|error| (format nil "Did not log in within ~:d second~:p or ~:d command~:p; disconnecting."
                          +pre-login-max-time+
                          +pre-login-max-commands+))
   client)
  (hunchensocket:close-connection client))

(defun ws-to-infinity (client message)
  (let ((*user* (if (eql t (user-account client))
                    nil
                    (user-account client)))
        (*Toot* (Toot client))
        (*client* client))
    (ws-reply (with-simple-restart (continue "Restart ∞ request processor")
                (let ((json (jonathan.decode:parse message)))
                  (ws-bandwidth-record json)
                  (call-infinity-from-stream json)))
              client)))

(defun ws-without-login (client message)
  (if (or (zerop (decf (pre-login-commands client)))
          (< +pre-login-max-time+ (- (get-universal-time)
                                     (connect-time client))))
      (disconnect-no-login client)) 
  (websocket-authenticate client message))

(defmethod hunchensocket:text-message-received ((res infinity-websocket-resource)
                                                client message)
  (incf *ws-chars-received* (length message))
  (setf (last-active client) (get-universal-time))
  (with-websocket-disconnections (client)
    (if (user-account client)
        (ws-to-infinity client message)
        (ws-without-login client message))))

(defconstant +unix-time-in-universal+
  2208988800
  "The number of seconds from Universal Time Epoch to Unix Epoch.")

(defun ayt-idle-users ()
  (let ((server-time (* 1000 (- (get-universal-time) +unix-time-in-universal+)))
        (idle-time (* 1000 (- (- (get-universal-time) +ws-idle-seconds+)
                              +unix-time-in-universal+))))
    (dolist (client (remove-if (lambda (client)
                                 (< (last-active client) idle-time))
                               (hunchensocket:clients *infinity-websocket-resource*)))
      (ws-unicast (list :|from| "ayt"
                        :|status| t
                        :|serverTime| server-time
                        :|idleTime| idle-time)
                  client))))


(defun who-is-connected ()
  "All users currently connected"
  (remove-if #'null
             (mapcar #'user-account
                     (hunchensocket:clients *infinity-websocket-resource*))))

(defun connected-Toots ()
  "All Toots currently connected"
  (remove-if #'null
             (mapcar #'Toot (hunchensocket:clients *infinity-websocket-resource*))))

(defun find-user-for-json (json)
  "Find a user based on submitted authentication JSON"
  (if (and json
           (string= "Auth/∞/ℵ₀" (getf json :|c|)))
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

(defun user-online-p (user)
  "Is USER actively connected right now?"
  (user-stream user))

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

(defvar *ws-client-for-user* 
  (make-hash-table :test 'equal :weakness :value))

(defvar *ws-client-for-Toot* 
  (make-hash-table :test 'equal :weakness :value))

(defun ws-sign-in-user (client &optional (user *user*))
  "Sign in USER on CLIENT connection.

The full procedure comes about from `WS-PERFORM-SIGN-IN.' This function
only handles the low-level bookkeeping."
  (setf (user-account client) user
        (gethash (uuid:uuid-to-byte-array (person-uuid user)) *ws-client-for-user*) client)
  (post-sign-in user)
  (incf *ws-sign-ins*))

(defun ws-kick-other-streams-for-user (&optional (user *user*))
  "`WS-KICK' any stream on which USER is signed in."
  (when-let (other-client (user-stream user))
    (ws-kick other-client)))

(defun ws-perform-sign-in (client &optional (user *user*))
  "Perform signing in USER on CLIENT and side-effects.

Calls `WS-SIGN-IN-USER' and `WS-KICK-OTHER-STREAMS-FOR-USER'

Sends logOK message and Toots List"
  (v:info :stream "User authenticated as ~a on ~a" user client)
  (ws-kick-other-streams-for-user user)
  (ws-sign-in-user client user)
  (ws-unicast (login-ok-message) client)
  (ws-unicast (second (toot-list-message)) client))

(defun infinity-pre-login (c auth client)
  "Handle ∞ mode pre-login commands.

Commands supported:

@table @code

@item getApple

See `INFINITY-GET-APPLE'

@item login

See `INFINITY-LOGIN'

@end table

Commands ignored with error returns for compatibility with version 1.2:

@table @code

@item batch

@item finger

@item getZoneList

@end table

@subsection Changes from 1.0 to 1.2

I don't actually have a record as to when these commands were added, but
@code{batch} and @code{finger} were added in either 1.1 or 1.2.

@code{batch} was  used for scripting  server events from  shell scripts,
and @code{finger} was used by the Toot Viewer application to obtain Toot
public information without  logging in. Any new Toot Viewer  can use the
REST interface for that purpose.

@subsection Changes from 1.2 to 2.0

Several  commands are  no longer  supported.  Note also  changes to  the
@code{getApple} protocol  at `INFINITY-GET-APPLE' and the  new Alef-null
external  authentication at  `GET-USER-FOR-JSON'. Due  to reliance  upon
external  authentication  services,   Romance  now  @emph{only}  accepts
password-based  CHAP  authentication  (the  @code{$Eden}/@code{getApple}
protocol) from  child Toots. This  is not much  of a change,  since most
Toots  in  Tootsville  IV  were  children, but  on  the  other  hand  it
represents  a  major   change  in  expecting  more   adult  players  and
registration of parents.

@code{batch}  is no  longer  supported. The  REST  interfaces serve  the
same purpose with far less complexity.

@code{finger} is no longer supported  in this way. See `INFINITY-FINGER'
for      a      REST      endpoint     with      authentication,      or
`ENDPOINT-GET-/toots/toot-name→json'  for  a  REST interface  to  obtain
public Toot information without authentication.

@code{getZoneList} is  a weird one,  because it was never  functional in
pre-login mode, but was  added to be ignored due to a  bug in the Virgil
client that sometimes sent it before logging in due to asynchronous code
execution. It  used to return  a fake  zone list with  only @code{$Eden}
before login, but now returns a @code{status: false} instead.
"
  (string-case c
    ("getApple"
     (infinity-get-apple (getf auth :|d|) client))
    ("login"
     (infinity-login (getf auth :|d|) client))
    ("batch" (list :|from| "batch" :status :false))
    ("finger" (list :|from| "finger" :status :false))
    ("getZoneList" (list :|from| "getZoneList" :status :false))
    (otherwise (list :|from| "c" :status :false))))

(defun websocket-authenticate (client auth$)
  "CLIENT wishes to authenticate using AUTH$, a string containing JSON data.

AUTH$ must be a packet in one of the following forms:

@itemize

@item

It may be a direct login using a known authentication provider, in which
case  it will  contain a  key @code{Auth/∞/ℵ₀}  (that is,  auth infinity
alef-null) and be passed to `FIND-USER-FOR-JSON' for processing.

@item

It  may  be  a  Toot-based  login  (now  for  children  only)  and  send
a @code{getApple} request followed by  a @code{login} request. These are
handled by `INFINITY-GET-APPLE'  (qv for dotails of  this mechanism) and
`INFINITY-LOGIN'.

@item

For  compatibility, a  few other  packet types  may be  ignored by  this
function  but   are  no   longer  processed.   See  `INFINITY-PRE-LOGIN'
for details.

@end itemize

The client is required to sign in  within a few seconds and can issue no
more     than    a     few     commands     before    being     dropped.
See `+PRE-LOGIN-MAX-TIME+' and `+PRE-LOGIN-MAX-COMMANDS+'.
"
  
  (incf (pre-login-commands client))
  (let ((auth (jonathan.decode:parse auth$)))
    (ws-bandwidth-record auth)
    (if-let (*user* (find-user-for-json auth))
      (ws-perform-sign-in client)
      (if-let (c (getf auth :|c|))
        (infinity-pre-login c auth client)
        (ws-unicast (login-failed-message) client)))))

(defun websockets-maintenance ()
  "Maintain websockets.

Sends Are You There packets to idle users."
  (ayt-idle-users))

(defun ws-maintenance-thread ()
  "This is the main function for the websockets maintenance thread.

It     calls     `WEBSOCKETS-MAINTENANCE',     qv,    at     half     of
+WS-IDLE-SECONDS+ intervals."
  (loop while *websocket-server*
     do (progn 
          (websockets-maintenance)
          (sleep (/ +ws-idle-seconds+ 2)))))

(defun listen-for-websockets ()
  "Start listening for websocket connections."
  (setf *websocket-server*
        (make-instance (if (enable-ssl-p)
                           'websocket-ssl-acceptor
                           'websocket-acceptor)))
  (setf *websocket-maintenance-thread*
        (make-thread #'ws-maintenance-thread
                     :name "WebSockets maintenance thread"))
  (hunchentoot:start *websocket-server*))

(defun stop-listening-for-websockets ()
  "Stop listening for websocket connections and disable the maintenance thread."
  (when *websocket-server*
    (hunchentoot:stop *websocket-server*)
    (setf *websocket-server* nil)
    (join-thread *websocket-maintenance-thread*)))



(defun admin-message (title message
                      &key (label title))
  "Send a broadcast admin MESSAGE with TITLE and LABEL.
 
Also logs the contents to the console."
  (broadcast (list :|from| "admin"
                   :|status| t
                   :|title| title
                   :|label| label
                   :|message| message))
  (format t "Admin message (broadcast): “~a”: “~a” (~a)"
          title message label))

(defun active-player ()
  (or *user* *Toot*))

(defun private-admin-message (title message
                              &key (label title)
                                   (user (active-player)))
  "Send a unicast admin MESSAGE to USER with TITLE and LABEL.

Instead logs the contents to the console if USER is not connected.

Note that the current Tootsville V client does not make use of LABEL."
  (let ((*user* user))
    (if (and *user* (user-stream *user*))
        (unicast (list :|from| "admin"
                       :|status| t
                       :|title| title
                       :|label| label
                       :|message| message))
        (format t "Admin message to ~a: “~a”: “~a” (~a)"
                (or *user* "nobody")
                title message label))))

(defun Toot-speak (speech &key (Toot *Toot*) vol)
  "Broadcast a public message of SPEECH from TOOT at volume VOL."
  (broadcast (list :|from| "pub"
                   :|u| (Toot-name Toot)
                   :|t| speech
                   :|x| vol
                   :|id| (Toot-UUID Toot))
             :near Toot))

(defun return-new-apple (client)
  "Used by `INFINITY-GET-APPLE' to send CLIENT a new apple value."
  (let ((random-key (cl-base64:integer-to-base64-string
                     (ironclad:random-bits (random 32768
                                                   )))))
    (setf (random-key client) random-key)
    (list :|from| "getApple"
          :|status| t
          :|apple| random-key)))

(defun infinity-get-apple (client packet$)
  "Get the apple to get into, or out of, $Eden.

@subsection Apple-based authentication

In the modern usage, the user who wishes to get authenticated connects
a stream (ie, WebSocket) connection and sends a packet like this:

@verbatim
{ c: \"getApple\" }
@end verbatim

There are no @code{d} data required.

The response from the server will be something like

@verbatim
{ from: \"getApple\",
  status: true,
  apple: \"an opaque string\" }
@end verbatim

The default action is to create a new apple value on each call. However,
the   client   can   control   this  with   an   additional   parameter,
@code{replace}.

@table @code

@item supersede

The  default. A  new apple  will be  returned, superseding  any previous
value,  regardless as  to whether  any  previous value  had been  given.
Previous apple values became irrelevant / no longer can be used.

@item never

If an apple value  has been issued, do not replace it.  An error will be
returned on subsequent calls to @code{getApple}.

@item replace

Assert that  there must have been  a previous apple issued,  and replace
it. If no previous apple had been issued, an error will be returned.

@end table

The  @code{apple} value  will be  a valid  UTF-8 string  without control
characters of no more than 4kiB, but no other assertions about it can be
assumed by a conforming client.

In the  case of an error  from @code{getApple}, a returned  error packet
will look like

@verbatim
{ from: \"getApple\", status: false,
  error: \"error message text\" }
@end verbatim

Upon receiving  a valid  apple string,  the client  will submit  a login
packet (see: `INFINITY-LOGIN') like:

@verbatim
{ c: \"login\",
  d: { userName: \"a-Toot-name\",
       password: \"a-secret-sha1-hex-string\",
       zone: \"$Eden\" } }
@end verbatim

The @code{pass} submitted is a hash created by:

@enumerate

@item

Concatenate the @code{apple} value with the @code{child-code} for the
Toot being signed-in.

@item

Take this concatenated string, and take the SHA1 hash of it.

@item

Take the hex value of that SHA1 hash

@end enumerate

The login packet will return @code{from: \"login\", status: true} if the
password is successful.

@subsection New in 1.1

This mechanism for logins was introduced in 1.1

@subsection Changes from 1.1 to 1.2

1.2 switched all communications to  JSON, removing XML equivalent legacy
commands used by SmartFox Server's protocol.

@subsection Changes from 1.2 to 2.0

@itemize

@item

Apple values may now potentially reach 4kiB; the former limit was 256 characters.

@item

Apple values are UTF-8, not ASCII-67 (7-bit) characters.

@item

Apple values will not contain control characters.

@item

The login zone is only @code{$Eden}; there are no other zones.

@end itemize

WRITEME"
  (let ((packet (jonathan.decode:parse packet$)))
    (if-let (replace (getf packet :|replaceP|))
      (string-case replace
        ("never" 
         (if (random-key client)
             (list :|from| "getApple"
                   :|status| :false
                   :|error| "Already got an apple")
             (return-new-apple client)))
        ("supersede"
         (return-new-apple client))
        ("replace"
         (if (random-key client)
             (return-new-apple client)
             (list :|from| "getApple"
                   :|status| :false
                   :|error| "Did not get an apple to replace"))))
      (return-new-apple client))))

(defun infinity-login (client packet$)
  "Notification of a new player in the game.

See `INFINITY-GET-APPLE' for an overview of the login process.

Response: logOK or @{ from: \"login\", status: false, err:
\"login.fail\", msg: reason @}

@subsection Usage

The input packet must have 3 data elements:

@table @code

@item userName

The name of the Toot character signing in

@item password

The SHA1  hex hash of  the concatenated  apple and password  values (see
`INFINITY-GET-APPLE' for details)

@item zone

Must always be @code{$Eden} exactly.

@end table

@subsection Changes from 1.2

In 1.2, users  would log in to  zone @code{$Eden}, then log  in again to
a specific zone.  Now, @code{$Eden} is just a placeholder  and there are
no sharded zones.
"
  (let ((packet (jonathan.decode:parse packet$)))
    (let ((user-name (getf packet :|userName|))
          (password (getf packet :|password|))
          (zone (getf packet :|zone|)))
      (if (equal zone "$Eden")
          (if-let (random-key (random-key client))
            (if-let (Toot (find-record 'Toot :name user-name))
              (if-let (child-code (Toot-child-code Toot))
                (if (equal password 
                           (sha1-hex (concatenate 'string 
                                                  random-key
                                                  child-code)))
                    (progn
                      (setf (user-account client) t
                            (Toot client) Toot)
                      (play-with-Toot Toot)
                      (list :|from| "login"
                            :|status| t
                            :|zone| "@Tootsville"))
                    ;; TODO  factor out failure packet code
                    (list :|from| "login"
                          :|status| :false
                          :|err| "login.fail"
                          :|msg| "I gave you an apple, and you made applesauce."
                          :|err2| "applesauce"))
                (list :|from| "login"
                      :|status| :false
                      :|err| "login.fail"
                      :|msg| (format nil "~a is not a child Toot" (Toot-name Toot))
                      :|err2| "not-child"))
              (list :|from| "login"
                    :|status| :false
                    :|err| "login.fail"
                    :|msg| (format nil "There is no Toot named ~a" user-name)
                    :|err2| "no-Toot"))
            (list :|from| "login"
                  :|status| :false
                  :|err| "login.fail"
                  :|msg| "Your software did not obtain an apple to seed its password"
                  :|err2| "no-apple"))
          (list :|from| "login"
                :|status| :false
                :|err| "login.fail"
                :|msg| (format nil "There is no Zone named ~a" zone)
                :|err2| "no-zone")))))


;;; Work-around for timeout buglet in Hunchensockets

(defun hunchensocket::read-frame-from-client (client)
  "Read a text or binary message from CLIENT."
  (handler-case
      (with-slots (hunchensocket::input-stream) client
        (hunchensocket::read-frame hunchensocket::input-stream))
    (sb-sys:io-timeout (c)
      (throw 'hunchensocket::websocket-done c))))
