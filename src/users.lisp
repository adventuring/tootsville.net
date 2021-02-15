;;;; -*- lisp -*-
;;;
;;;; src/users.lisp is part of Tootsville
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



(defvar *user* nil
  "The currently-signed-in user, if any")

(defun email-lhs (address)
  (when address
    (subseq address 0 (position #\@ address))))

(defmacro ignore-duplicates (&body body)
  `(restart-case
       (handler-bind
           ((dbi.error:<dbi-database-error>
             (lambda (c)
               (when (= 1062 (slot-value c 'DBI.ERROR::ERROR-CODE))
                 (invoke-restart 'continue)))))
         ,@body)
     (continue () nil)))

(defun find-user-for-credentials (credentials)
  (loop for (provider ids) on credentials by #'cddr
     do (dolist (id ids)
          (when-let (credential (ignore-not-found (find-record 'credential
                                                               :id-token id
                                                               :provider provider)))
            (v:info :Login "Found user UUID ~a by credential ~s from ~a"
                    (credential-person credential) id provider)
            (return-from find-user-for-credentials
              (find-reference credential :person)))))
  (v:info :Login "No existing match for credentials ~s" credentials)
  nil)

(defun associate-credentials (person credentials)
  (loop for (provider ids) on credentials by #'cddr
     do (dolist (id ids)
          (ensure-record 'credential
                         :person (person-uuid person)
                         :id-token id
                         :uid id
                         :provider provider))))

(defun find-person-by-url (url &optional more)
  (when-let (link (handler-case
                      (find-record 'person-link
                                   :url (reduce (curry #'concatenate 'string)
                                                (list url more)))
                    (not-found (c)
                      (declare (ignore c))
                      nil)))
    (find-record 'person :uuid (person-link-person link))))

(defun person-links-to-email (email)
  (find-records 'person-link
                :url
                (concatenate 'string
                             "mailto:" email)))

(defun all-links-to-same-person-p (links)
  (let ((first (person-link-person (first links))))
    (every (lambda (link)
             (uuid:uuid= (person-link-person link) first))
           (rest links))))

(defun ensure-user-for-plist (plist)
  "Find or create the user described by PLIST and return them.

PLIST  can  have  keys  that  align to  a  DB.PERSON  or  their  contact
infos (eg,  email) and is expected  to have been validated  already (eg,
come from a trusted authentication provider like Google Firebase)."
  (let ((person
         (or (find-user-for-credentials (getf plist :credentials))
             (when-let (email (and (getf plist :email-verified-p)
                                   (getf plist :email)))
               (when-let (links (person-links-to-email email))
                 (when-let (link (when (all-links-to-same-person-p links)
                                   (first links)))
                   (find-reference link :person))))
             (progn
               (make-record
                'person
                :display-name (or (getf plist :name)
                                  (email-lhs (getf plist :email)))
                :given-name (or (getf plist :given-name)
                                (getf plist :name)
                                (email-lhs (getf plist :email)))
                :surname (or (getf plist :surname) "")
                :gender :X
                :lang "en_US")))))
    (ensure-record 'person-link
                   :person (person-uuid person)
                   :rel :contact
                   :url (concatenate 'string "mailto:"
                                     (getf plist :email))
                   :label "email"
                   :provenance "Provided by Firebase login")
    (associate-credentials person (getf plist :credentials))
    (when-let (picture (getf plist :picture))
      (ensure-record 'person-link
                     :person (person-uuid person)
                     :rel :photo
                     :url picture
                     :label "portrait"
                     :provenance "Provided by Firebase login"))
    (when-let (email (and (getf plist :email-verified-p)
                          (getf plist :email)))
      (update-gravatar person email))
    (v:info :login "Person for session is ~a (~a)"
            (person-display-name person) (person-uuid person))
    (incf (gethash (uuid:uuid-to-byte-array (person-uuid person))
                   *infinity-users* 0))
    person))

(defun update-gravatar (person email)
  (if-let ((gravatar (ignore-not-found
                       (find-record 'person-link
                                    :person (person-uuid person)
                                    :rel :photo
                                    :provenance "Provided by Gravatar"))))
    (setf (person-link-url gravatar)
          (gravatar-image-url email
                              :size 256
                              :rating :pg
                              :default :identicon))
    (make-record 'person-link
                 :person (person-uuid person)
                 :rel :photo
                 :label "Gravatar"
                 :provenance "Provided by Gravatar"
                 :url (gravatar-image-url email
                                          :size 256
                                          :rating :pg
                                          :default :identicon))))



(defmacro with-user (() &body body)
  `(progn (unless *user*
            (error 'unidentified-player-error))
          ,@body))



;;; User details

(defun user-display-name (&optional (person *user*))
  (person-display-name person))

(defun user-given-name (&optional (person *user*))
  (person-given-name person))

(defun user-surname (&optional (person *user*))
  (person-surname person))

(defun user-email (&optional (person *user*))
  "Finds an email address for PERSON of type CONTACT."
  (when-let (mails (remove-if-not
                    (lambda (record)
                      (eql :mailto (puri:uri-scheme (person-link-url record))))
                    (find-records 'person-link
                                  :person (person-uuid person)
                                  :rel :CONTACT)))
    (subseq (puri:render-uri (person-link-url (random-elt mails)) nil) 7)))

(defun user-face (&optional (person *user*))
  "Finds a portrait URI for PERSON"
  (when-let (portraits (find-records 'person-link
                                     :person (person-uuid person)
                                     :rel :photo))
    (random-elt portraits)))

(defun user-id (&optional (person *user*))
  (person-uuid person))

(defun user->alist (user)
  (list (cons :|displayName| (user-display-name user))
        (cons :|givenName|   (user-given-name user))
        (cons :|surname|     (user-surname user))
        (cons :|face|        (user-face user))
        (cons :|uuid|        (user-id user))))


(defun player-childp (&optional (player *user*))
  (or (< (or (when-let (dob (person-date-of-birth player)) (legal-age dob))
             (person-age player)
             1)
         13)
      (when-let (Toot (when-let (stream (user-stream player)) 
                        (Toot stream)))
        (Toot-child-code Toot))))

(defun player-adultp (&optional (player *user*))
  (and (not (player-childp player))
       (>= (or (legal-age (person-date-of-birth player))
               (person-age player))
           18)))

(defun player-Toots (&optional (player *user*))
  (find-records 'Toot :player (person-uuid player)))

(defun find-player-or-die ()
  "Ensure that a recognized player is connected."
  (unless *user* (error 'unidentified-player-error)))

(defvar *403.json-bytes*
  (flexi-streams:string-to-octets "{\"error\":\"player-not-found\",
\"note\":\"You are not signed in to the web services\",
\"login\":\"https://play.Tootsville.org/play/\"}"))



(defun assert-my-character (Toot-name &optional (user *user*))
  "Signal a security error if TOOT-NAME is not owned by USER"
  (check-type Toot-name Toot-name)
  (unless (find-record 'toot
                       :player (person-uuid user)
                       :name Toot-name)
    (error 'not-your-Toot-error :name Toot-name)))



;;; Copied from CL-Gravatar, but fixed for my version of Drakma (newer?)

;;; TODO: post patch upstream

#| CL-Gravatar

Copyright 2011 Greg Pfeil <greg@technomadic.org> 

Licensed under the Apache License,  Version 2.0 (the "License") you may 
not use this file except in  compliance with the License. You may obtain 
a copy of the License at               

http://www.apache.org/licenses/LICENSE-2.0 

Unless  required by  applicable law  or agreed  to in  writing, software 
distributed  under the  License  is  distributed on  an  "AS IS"  BASIS, 
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
See  the License  for the  specific language  governing permissions  and 
limitations under the License. |#

(define-constant +gravatar-base-uri+ (puri:uri "https://secure.gravatar.com/")
  :test #'puri:uri=
  :documentation "Why would we ever _not_ use SSL?")

(defun gravatar-hash (email)
  "Computes the Gravatar hash of an EMAIL address."
  (string-downcase (format nil "~{~2,'0x~}"
                           (coerce (md5:md5sum-sequence
                                    (string-downcase (string-trim '(#\space)
                                                                  email)))
                                   'list))))

(defun gravatar-image-url (email &key size default force-default-p rating)
  "DEFAULT may be either a URL to your own image, or one of :404, :mm,
:identicon, :monsterid, :wavatar, or :retro. RATING may be one of :g, :pg,
:r, or :x."
  (let ((parameters ()))
    (when size (push `("s" . ,(format nil "~d" size)) parameters))
    (typecase default
      (keyword (push `("d" . ,(string-downcase default)) parameters))
      (string (push `("d" . ,default) parameters)))
    (when force-default-p (push '("f" . "y") parameters))
    (when rating (push `("r" . ,(string-downcase rating)) parameters))
    (puri:merge-uris (format nil "avatar/~a~@[?~a~]"
                             (gravatar-hash email)
                             (drakma::alist-to-url-encoded-string
                              parameters
                              :utf-8 #'drakma:url-encode))
                     +gravatar-base-uri+)))

#| --------------- end Gravatar code |#



(defun person-is-patron-p (person)
  "Returns true if PERSON is a patron of CIWTA.

Currently just Bruce-Robert Pocock, Gene Cronk, Zephyr Salz, and Ali Dolan."
  ;; just me ☹
  (member (uuid-to-uri (person-uuid person)) 
          '("SAsJFzx6TRO1W6pWEFxeAA==" ; Bruce-Robert
            "Vra1I1adQaGXZKZeA82vlg==" ; Gene
            "kd7Q14M2S7iFxsH09Soaqw==" ; Bruce-Robert also
            "hcuB9HuoStysFdReBb+efA==" ; Zephyr
            )))

(defun get-rollbar-person (&optional (person *user*))
  "Return PERSON information for Rollbar error reporting"
  (when person
    (list :|person|
          (list :|uid| (princ-to-string (person-uuid person))
                :|username| (format nil "~@[Toot: ~a, ~]Person: ~a"
                                    (when-let (Toot (when-let (stream (user-stream person))
                                                      (Toot stream)))
                                      (Toot-name Toot))
                                    (person-display-name person))
                :|email| (person-first-email person)))))



(defun player-alert (person &rest message)
  "Sends an asynchronous notification alert MESSAGE to PERSON"
  (when-let (client (user-stream person))
    (unicast (list :|from| "alert"
                   :|status| t
                   :|alert| message)
             person)))

(defun person-age* (&optional (user *user*))
  "Get a person's age in years."
  (or (when-let (dob (person-date-of-birth user))
        (timestamp-whole-year-difference (now) dob))
      (person-age user)))

(defun reasonable-name-char-p (char)
  "Is CHAR a character that can reasonably appear in a person's name?"
  (or (alpha-char-p char)
      (find char "-'`\",.()‘’“” " :test #'char=)))

(defun reasonable-name-p (name)
  "Does NAME appear to be a reasonable name for a person?"
  (and (every #'reasonable-name-char-p name)
       (not (emptyp name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun list-of-string= (a b)
    (and (listp a) (listp b)
         (loop for aa in a
            for bb in b
            always (string= aa bb)))))

(assert (list-of-string= '("Foo" "Bar") '("Foo" "Bar")))
(assert (not (list-of-string= '("Foo" "Bar") '("FOO" "BAR"))))

(define-constant +supported-languages+
    '("en" "en-US")
  :test #'list-of-string=)

(defun person-info (&optional (user *user*))
  "Creates a JSON-like PList of information about USER.

Its contents are:

@table @code
@item uuid
The person's UUID
@item displayName
The person's name, formatted for display.
@item patronP
True if this person is a patron of the CIWTA project.
@item gender
One of ☿ (unknown/other),♀ (female), or ♂ (male).
@item givenName
The person's given name.
@item surname
The person's surname.
@item language
The person's spoken language
@item sensitiveP
If true, this is a Sensitive Player
@item dateOfBirth
The person's date of birth, in an ISO format string
@item age
The person's age in years
@end table"
  (list :|uuid| (person-uuid user)
        :|displayName| (person-display-name user)
        :|patronP| (or (person-is-patron-p user) :false)
        :|gender| (ecase (person-gender user)
                    (:X "☿")
                    (:F "♀")
                    (:M "♂"))
        :|givenName| (person-given-name user)
        :|surname| (person-surname user)
        :|language| (person-lang user)
        :|sensitiveP| (or (person-sensitivep user) :false)
        :|dateOfBirth| (person-date-of-birth user)
        :|age| (person-age* user)))

(defmethod print-object ((user person) s)
  (format s "#<User ~a ~a ~a>"
          (person-uuid user)
          (or (person-display-name user)
              (and (or (person-given-name user)
                       (person-surname user))
                   (format nil "~a ~a"
                           (or (person-given-name user)
                               (case (person-gender user)
                                 (:m "Mr")
                                 (:f "Ms")
                                 (otherwise "Mx")))
                           (or (person-surname user) "")))
              "(No name)")
          (or (when (person-is-patron-p user)
                " (*Patron)")
              "")))

(defun url-to-string (url)
  "Converts URL to a string, if it is not already."
  (etypecase url
    (string url)
    (puri:uri (with-output-to-string (s)
                (puri:render-uri url s)))))

(defun person-first-email (&optional (user *user*))
  "Gives one possible eMail address associated with USER.

Uses the first, alphabetically speaking."
  (when-let (first-mailto (first
                           (sort
                            (mapcar #'url-to-string
                                    (mapcar #'person-link-url
                                            (remove-if-not
                                             (lambda (link)
                                               (and (eql :contact (person-link-rel link))
                                                    (equal :mailto (puri:uri-scheme (person-link-url link)))))
                                             (find-records 'person-link :person (person-uuid user)))))
                            #'string<)))
    (subseq first-mailto 7 #| length of "mailto:" |#)))



(defmacro with-local-user ((email) &body body)
  "Set *USER* to the user with EMAIL locally"
  `(if-let (links (person-links-to-email ,email))
     (if (= 1 (length links))
         (let ((*user* (find-reference (first links) :person)))
           ,@body)
         (error "Multiple users share email ~a" ,email))
     (error "No users have email ~a" ,email)))

(defmacro with-local-Toot ((Toot) &body body)
  "Set *TOOT* to the Toot named TOOT."
  `(let ((*Toot* (find-record 'Toot :name ,Toot)))
     ,@body))



(defconstant +builder-toot-hard-hat-template+ 2494
  "This item grants a player the ability to run operator commands.")

(defun builder-Toot-p (&optional (Toot *Toot*))
  "Determine whether TOOT is a Builder Toot who can run operator commands"
  (Toot-has-item-p +builder-Toot-hard-hat-template+ Toot))



(defun send-parent-child-login-email (request)
  "Send a parent child's REQUEST to play via email."
  (let* ((Toot (find-reference request :Toot))
         (*user* (find-reference Toot :player)))
    (cl-smtp:send-email
     (config :email :noreply :smtp)
     (format nil "\"~a\" <~a>" 
             (config :email :noreply :from)
             (config :email :noreply :from-address))
     (format nil "\"~a\" <~a>"
             (user-display-name)
             (person-first-email *user*))
     (format nil "~a, your child wants to play as ~:(~a~)"
             (user-given-name)
             (Toot-Name Toot))
     (format nil "~a:

Your child wanto to play in Tootsville using the ~:(~a~) 
account that you created for them.

You can approve or deny this request for one hour by visiting:
 
https://www.tootsville.org/play/#child-request=~a

If you don't approve, your child cannot play online.

\(This is an automated email that you receive as a player
of Tootsville who has created a child account. Write to 
support@tootsville.org if you have any questions.)"
             (user-display-name)
             (Toot-name Toot)
             (child-request-uuid request))
     :ssl :tls
     :authentication (list (config :email :noreply :from-address)
                           (config :email :noreply :password)))))

(defun send-parent-child-login-request (request)
  "Send a parent a child's REQUEST to play as a popup in game."
  (unicast (list :|from| "prompt"
                 :|status| t
                 :|id| (format nil "child-request-~a" (child-request-uuid request))
                 :|label| "Child login"
                 :|label_en_US| "Child login"
                 :|title| "Child login permission"
                 :|msg| (format nil 
                                "Your child wants to play on Tootsville as “~a.” Is that OK?" 
                                (Toot-name (find-reference request :Toot)))
                 :|replies|
                 (list :|deny| (list :|label| "No" 
                                     :|label_en_US| "No"
                                     :|type| "neg")
                       :|1hour|
                       (list :|label| "For 1 Hour"
                             :|label_en_US| "For 1 Hour"
                             :|type| "aff")
                       :|2hours|
                       (list :|label| "For 2 Hours"
                             :|label_en_US| "For 2 Hours"
                             :|type| "aff")
                       :|4hours|
                       (list :|label| "For 4 Hours"
                             :|label_en_US| "For 4 Hours"
                             :|type| "aff")))
           (find-reference (find-reference request :Toot) :Player)))

(defun parent-grant-permission (request &key (hours 168)
                                             (via "web"))
  "The parent *USER* grants REQUEST for HOURS via VIA.

This sets the approval time to `NOW'  and allows HOURS of play time from
`NOW'. The `CHILD-REQUEST-RESPONSE' of REQUEST  is set to an explanation
that  *USER* approved  the  request  via VIA.  VIA  can contain  further
comments, which will be presented in the UI.

Returns NIL."
  (let ((Toot (find-reference request :Toot)))
    (assert (uuid:uuid= (person-uuid *user*) (Toot-player Toot)) (*user*)
            "~a can not grant access to ~a" *user* Toot)
    (v:info :child "~a grants access to ~a for ~:d hour~:P"
            *user* Toot hours)
    (setf (child-request-allowed-at request) (now)
          (child-request-denied-at request) nil
          (child-request-allowed-for request) hours
          (child-request-response request) (format nil "~a approved via ~a"
                                                   (Person-display-name *user*)
                                                   via))
    (save-record request)
    (unicast (from-avatars (list :|childChanged| Toot)))
    (ws-approve-Toot Toot request))
  nil)

(defun parent-deny-permission (request &key (via "web"))
  "The parent who was given REQUEST has denied permission via VIA.

The child who placed REQUEST is @i{not} being given permission to play
in Tootsville.

Returns NIL"
  (let ((Toot (find-reference request :Toot)))
    (assert (uuid:uuid= (person-uuid *user*) (Toot-player Toot)) (*user*)
            "~a can not deny access to ~a" *user* Toot)
    (v:info :child "~a denies access to ~a" *user* Toot)
    (setf (child-request-allowed-at request) nil
          (child-request-denied-at request) (now)
          (child-request-allowed-for request) 0
          (child-request-response request) (format nil "~a denied via ~a"
                                                   (Person-display-name *user*)
                                                   via))
    (save-record request)
    (unicast (from-avatars (list :|childChanged| Toot)))
    (ws-deny-Toot Toot request))
  nil)

(defun login-child (Toot)
  "Start a login request for TOOT, if one is not already pending.

WRITEME"
  (let ((request (if-let (requests (pending-child-requests-by-Toot Toot))
                   (and (setf (child-request-placed-at (first requests)) (now))
                        (first requests))
                   (make-record 'child-request
                                :uuid (uuid:make-v4-uuid)
                                :Toot (Toot-UUID Toot)
                                :placed-at (now)
                                :response ""))))
    (if-let (*user* (let ((user (find-reference Toot :player)))
                      (and (user-online-p user) user)))
      (send-parent-child-login-request request)
      (send-parent-child-login-email request)))
  (list 200 (list :|message| "Waiting for permission…")))

(defun pending-child-approval-request (user)
  (dolist (Toot (player-Toots user))
    (when-let (requests (pending-child-requests-by-Toot Toot))
      (return-from pending-child-approval-request (first requests)))))

(defun pending-child-requests-by-Toot (Toot)
  (find-records-by-sql 
   'child-request 
   (format nil "
SELECT * FROM child_requests
WHERE toot='~a'
AND (allowed_at IS NULL
    AND denied_at IS NULL)
AND placed_at > CURRENT_TIMESTAMP - INTERVAL 1 HOUR
"
           (column-save-value (Toot-UUID Toot) :uuid))))

(defun child-request-allowed-until (request)
  (when (child-request-allowed-at request)
    (timestamp+ (child-request-allowed-at request)
                (child-request-allowed-for request)
                :hour)))

(defun answered-child-requests-by-Toot (Toot)
  "Recent requests by TOOT to play which have been answered and not expired yet."
  (remove-if
   (lambda (request)
     (and (child-request-allowed-at request)
          (timestamp< (child-request-allowed-until request) (now))))
   (find-records-by-sql 
    'child-request 
    (format nil "
SELECT * FROM child_requests
WHERE toot='~a'
AND ((allowed_at IS NOT NULL
           AND allowed_at > CURRENT_TIMESTAMP - INTERVAL 168 HOUR)
  OR (denied_at IS NOT NULL
         AND denied_at > CURRENT_TIMESTAMP - INTERVAL 1 HOUR))
ORDER BY placed_at ASC
"
            (column-save-value (Toot-UUID Toot) :uuid)))))



(defun post-sign-in (user)
  "Perform housekeeping after an user signs in. 

This might  include sending  a pending  child prompt."
  (when (not (eql t user))
    (when-let (request (pending-child-approval-request user))
      (send-parent-child-login-request request))))

(defun reap-uninteresting-child-requests ()
  "Remove uninteresting requests from the child_requests table.

Normally run by the metronome periodically."
  (v:info :child-request "Reaping denied requests (2 hours): ~s"
          (db-select-all :friendly
                         "DELETE FROM child_requests WHERE denied_at IS NOT NULL AND denied_at < CURRENT_TIMESTAMP - INTERVAL 2 HOUR"))
  (v:info :child-request "Reaping ignored requests (2 hours): ~s"
          (db-select-all :friendly
                         "DELETE FROM child_requests WHERE allowed_at IS NULL AND placed_at < CURRENT_TIMESTAMP - INTERVAL 2 HOUR"))
  (v:info :child-request "Reaping approved requests (1 week): ~s"
          (db-select-all :friendly
                         "DELETE FROM child_requests WHERE allowed_at IS NOT NULL AND placed_at < CURRENT_TIMESTAMP - INTERVAL 7 DAY")))
