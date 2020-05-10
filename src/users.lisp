;;;; -*- lisp -*-
;;;
;;;; src/users.lisp is part of Tootsville
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
      (when-let (Toot (player-Toot *user*))
        (Toot-child-code Toot))))

(defun player-adultp (&optional (player *user*))
  (>= (or (legal-age (person-date-of-birth player))
          (person-age player))
      18))

(defun player-Toots (&optional (player *user*))
  (find-records 'Toot :player (person-uuid player)))

(defun find-player-or-die ()
  "Ensure that a recognized player is connected."
  (unless *user* (error 'unidentified-player-error)))

(defvar *403.json-bytes*
  (flexi-streams:string-to-octets "{\"error\":\"player-not-found\",
\"note\":\"You are not signed in to the web services\",
\"login\":\"https://play.Tootsville.org/login/\"}"))



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


(defun person-is-patron-p (person)
  "Returns true if PERSON is a patron of CIWTA.

Currently just me."
  ;; just me ☹
  (string= (uuid-to-uri (person-uuid person)) "SAsJFzx6TRO1W6pWEFxeAA=="))

(defun get-rollbar-person (&optional (person *user*))
  "Return PERSON information for Rollbar error reporting"
  (when person
    (list :|person|
          (list :|uid| (princ-to-string (person-uuid person))
                :|username| (format nil "~@[Toot: ~a, ~]Person: ~a"
                                    (when-let (Toot (ignore-not-found (player-Toot person)))
                                      (ignore-not-found (Toot-name Toot)))
                                    (person-display-name person))
                :|email| (user-email)))))



(defun player-alert (person &rest message)
  "Sends an asynchronous notification alert MESSAGE to PERSON"
  (when-let (client (user-stream person))
    (unicast (list :|from| "alert"
                   :|status| t
                   :|alert| message)
             person)))



(defun player-Toot (&optional (person *user*))
  "Find the Toot which PERSON is playing as (or was most recently)"
  (when-let (p-t-link (find-record 'player-Toot :player (person-uuid person)))
    (ignore-not-found (find-record 'Toot :UUID (player-Toot-Toot p-t-link)))))

(defun (setf player-Toot) (Toot person)
  (if-let (p-t-link (ignore-not-found
                      (find-record 'player-Toot :player (person-uuid person))))
    (progn (setf (player-Toot-Toot p-t-link) (Toot-uuid Toot))
           (save-record p-t-link))
    (save-record (make-record 'player-Toot :player (person-uuid person)
                              :Toot (Toot-uuid Toot))))
  (setf (Toot-last-active Toot) (now))
  (save-record Toot)
  Toot)



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



(defun user-plane (&optional (user *user*))
  "Find the world in which USER's Toot is playing."
  (Toot-world (find-active-toot-for-user user)))

(defmethod print-object ((user person) s)
  (format s "#<User ~a ~a (~a)~a>"
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
          (when-let (Toot (ignore-not-found (player-Toot user)))
            (Toot-name Toot))
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



(defun builder-Toot-p (&optional (Toot *Toot*))
  (Toot-has-item-p +builder-Toot-hard-hat-template+ *Toot*))



(defun send-parent-child-login-request (Toot)
  (unicast (list :|from| "prompt"
                 :|id| (format nil "child-request-~a" (Toot-UUID Toot))
                 :|label| "Child login"
                 :|label_en_US| "Child login"
                 :|msg| (format nil 
                                "Your child wants to play on Tootsville as “~a.” Is that OK?" 
                                (Toot-name Toot))
                 :|replies| (list :|affirm| (list :|label| "Yes"
                                                  :|label_en_US| "Yes"
                                                  :|type| "aff")
                                  :|deny| (list :|label| "No" 
                                                :|label_en_US| "No"
                                                :|type| "neg")
                                  :|1hour| (list :|label| "For 1 Hour"
                                                 :|label_en_US| "For 1 Hour"
                                                 :|type| "aff")))))

(defun login-child (Toot)
  "Start a login request for TOOT, if one is not already pending.

WRITEME"
  (with-dbi (:friendly)
    (let ((make-table (dbi:prepare *dbi-connection* "
CREATE TABLE IF NOT EXISTS child_requests
\( toot CHAR (22) NOT NULL,
  placed_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP (),
  allowed_at TIMESTAMP NULL,
  denied_at TIMESTAMP NULL,
  allowed_for INTEGER NULL,
  response VARCHAR (160)
)")))
      (dbi:execute make-table)))
  (save-record 
   (make-record 'child-request
                :Toot (Toot-UUID Toot)
                :placed-at (now)))
  (unless (user-online-p (Toot-player Toot))
    (return-from login-child
      (list 400 (list :|error| "Your parent or guardian is not online, and we can not send them an email."))))
  (let ((*user* (find-reference Toot :player)))
    (send-parent-child-login-request Toot))
  (list 200 (list :|message| "Waiting for permission…")))

(defun Toot-with-pending-parent-approval (user)
  (dolist (Toot (player-Toots user))
    (when-let (request (ignore-not-found (find-record 'child-request 
                                                      :allowed-at nil
                                                      :denied-at nil
                                                      :Toot (Toot-UUID Toot))))
      (return-from Toot-with-pending-parent-approval Toot))))



(defun post-sign-in (user)
  "Perform housekeeping after an user signs in. 

This might  include sending  a pending  child prompt."
  (when-let (Toot (Toot-with-pending-parent-approval user))
    (send-parent-child-login-request Toot)))
