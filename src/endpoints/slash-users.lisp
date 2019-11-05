;;;; -*- lisp -*-
;;;
;;;; src/endpoints/slash-users.lisp is part of Tootsville
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

(defun plist-to-English (plist)
  (format nil "~{~a~^~%~}"
          (loop for (key value) on plist by #'cddr
             collect (format nil "~:(~a~): ~s"
                             (symbol-munger:camel-case->english key) value))))

(defendpoint (get "/users/me" "text/plain")
  "Provides information about your user account, in plain text form."
  (with-user ()
    (plist-to-English (person-info))))

(defendpoint (get "/users/me" "application/json")
  "Provides information about your user account.

Requires player authentication.

@subsection{Status: 200 OK}

You will receive some information about your user account.

The top-level keys of the JSON object are:

@table
@item toots
The names of the Toot characters that you own.
@item logins

The authentication services which you can use. Each one has its own data
elements below it.

@end table

@subsubsection{Google Auth}

Under  logins/google, we  put  your  real name,  Gmail  address, URI  of
portrait (if you have  uploaded one), and a string token  that we use to
represent you to Google. TODO document this better

@subsubsection{Child Auth}

Child accounts will have some tokens here that help us … TODO

@subsection{Status: 401 Authorization Required}

@subsection{Status: 403 Authorization Failed}

"
  (with-user ()
    (list 200 (person-info))))

(defendpoint (put "/users/me" "application/json")
  "Makes changes to an user account.

Input JSO: { field: \"field\", newValue: \"x\" }

Fields and value formats:

displayName (fullName)

givenName

surname (familyName)

sensitive (sensitiveP)

Must be \"true\" or \"false\" (as a string)

lang (language)

Must be a supported ISO language string; e.g. \"en_US\"

gender

Must  be  one of  \"☿\"  \"♀\"  \"♂\",  where \"☿\"  is  gender-neutral.
Selects pronouns; respectively, \"they,\" \"she,\" or \"he.\"

dob (dateOfBirth)

Format in RFC-3339 timestamp format; eg, \"1990-05-21T00:00:00-0400\" or
just \"1990-05-21\"

@subsection{Status: 201 Created}

XXX is there a better status for updates?

@subsection{Status: 401 Authorization Required}

@subsection{Status: 403 Authorization Failed}

@subsection{Status: 405 Not Allowed}

@subsection{Status: 422 }

"
  (with-user ()
    (with-posted-json (key new-value)
      (check-arg-type key string)
      (check-arg-type new-value string)
      (ecase (make-keyword (string-upcase (symbol-munger:camel-case->lisp-name key)))
        ((:full-name :display-name)
         (setf (person-display-name *user*)
               (with-errors-as-http (422 "Display Name")
                 (reasonable-name-p new-value))))
        (:given-name
         (setf (person-given-name *user*)
               (with-errors-as-http (422 "Given Name")
                 (reasonable-name-p new-value))))
        ((:family-name :surname)
         (setf (person-surname *user*)
               (with-errors-as-http (422 "Surname")
                 (reasonable-name-p new-value))))
        ((:sensitive :sensitive-p)
         (setf (person-sensitivep *user*)
               (with-errors-as-http (422 "Sensitive User flag")
                 (ecase (make-keyword (string-upcase new-value))
                   (:true t)
                   (:false nil)))))
        ((:lang :language)
         (with-errors-as-http (422 "Human language")
           (assert (member (the string new-value) +supported-languages+
                           :test #'string=)))
         (setf (person-lang *user*) new-value))
        (:gender
         (with-errors-as-http (422 "Gender")
           (assert (member (the string new-value)
                           '("☿" "♀" "♂") :test #'string=)))
         (setf (person-gender *user*) (ecase (char new-value 0)
                                        (#\☿ :X)
                                        (#\♀ :F)
                                        (#\♂ :M))))
        ((:dob :date-of-birth)
         (with-errors-as-http (422 "Date of birth")
           (setf (person-date-of-birth *user*) (parse-timestring new-value))
           (assert (<= 13 (person-age*) 125) (new-value)))))
      (save-record *user*))
    (list 202 (person-info))))

(defendpoint (patch "/users/me" "application/json")
  "Alters information about your user account.

Requires player authentication.

Requires a body with fields to be changed, and their new values. TODO.

@subsection{Status: 200 OK}

@subsection{Status: 401 Authorization Required}

@subsection{Status: 403 Authorization Failed}

@subsection{Status: 405 Not Allowed}

"
  (with-user ()
    (error 'unimplemented)))

(defendpoint (get "/users/me/toots" "text/plain")
  "Enumerate all Toot characters available to you."
  (with-user ()
    (list 200
          (list :Last-Modified (header-time
                                (universal-to-timestamp
                                 (loop for Toot in (player-Toots)
                                    maximizing
                                      (the fixnum
                                           (timestamp-to-universal
                                            (or (Toot-last-active Toot) (now))))))))
          (format nil "~{~:(~a~)~^~%~}"
                  (mapcar #'Toot-name
                          (sort (player-Toots)
                                #'timestamp<
                                :key (lambda (Toot)
                                       (or (Toot-last-active Toot)
                                           (universal-to-timestamp 0)))))))))

(defendpoint (get "/users/me/toots" "application/json" 1)
  "Enumerate all Toot characters available to you."
  (with-user ()
    (list 200
          (list :Last-Modified (header-time
                                (universal-to-timestamp
                                 (loop for Toot in (player-Toots)
                                    maximizing
                                      (the fixnum
                                           (timestamp-to-universal
                                            (or (Toot-last-active Toot) (now))))))))
          (list :|toots| (mapcar #'Toot-name
                                 (sort (player-Toots)
                                       #'timestamp<
                                       :key (lambda (Toot)
                                              (or (Toot-last-active Toot)
                                                  (universal-to-timestamp 0)))))))))

(defendpoint (get "/users/me/toots/:toot-name" "text/plain")
  "Gives detailed information about your Toot character TOOT-NAME."
  (with-user ()
    (assert-my-character Toot-name)
    (let ((Toot (find-Toot-by-name Toot-name)))
      (list 200
            (list :Last-Modified (header-time (Toot-last-active Toot)))
            (plist-to-English (Toot-info Toot))))))

(defendpoint (get "/users/me/toots/:toot-name" "application/json")
  "Gives detailed information about your Toot character TOOT-NAME.

This Toot  must be owned by  you (the logged-in user).  You will receive
details about your  own Toot, like inventory, that are  not available to
other players.

Requires player authentication.

@subsection{Status: 200 OK}

@subsection{Status: 401 Authorization Required}
No user credentials were passed.

@subsection{Status: 403 Authorization Failed}
The user credentials presented were not recognized.

@subsection{Status: 404 Not Found}

"
  (with-user ()
    (assert-my-character Toot-name)
    (let ((Toot (find-Toot-by-name Toot-name)))
      (list 200
            (list :Last-Modified (header-time (Toot-last-active Toot)))
            (Toot-info Toot)))))

(defendpoint (delete "/users/me/toots/:toot-name" "application/json")
  "Permanently destroys the Toot character TOOT-NAME.

This Toot  must be owned by  you (the logged-in user).

Any inventory held by  the Toot, or property owned by  the Toot, will be
released to the public domain. Players should transfer items or property
prior to deleting a Toot.

For  a time  after  a Toot's  deletion, their  name  remains locked  (to
prevent immediate impersonation).

Requires player authentication.

@subsection{Status:  202 Toot  deletion in  progress}

The  Toot  will  be  deleted,  but   it  may  not  have  completed  yet.
A subsequent, identical request can confirm.

@subsection{Status: 204 Toot deleted}

The Toot has  been deleted. Repeated calls will return  the same status,
for the duration of the name lock on the Toot.

@subsection{Status: 401 Authorization Required}

No user credentials were passed.

@subsection{Status: 403 Authorization Failed}

The user credentials presented were not recognized.

@subsection{Status: 404 Not Found}

The Toot named does not exist.

@subsection{Status: 405 Not Allowed}

The Toot named is  one that you have permission to use,  but are not the
main owner of. This is usually a child account.

"
  (with-user ()
    (assert-my-character toot-name)
    (error 'unimplemented)))

(defendpoint (post "/users/me/play-with/:toot-name" "application/json")
  "Begin playing with the Toot named TOOT-NAME.

@table
@item Toot-Name
The name of the Toot character to play with.
@end table

@subsection{Status: 200 OK}

You are now in control of this Toot. The Toot's info will be returned.

@subsection{Status: 401 Authorization Required}

No user credentials were passed.

@subsection{Status: 403 Authorization Failed}

The user credentials presented were not recognized.

@subsection{Status: 404 Not Found}

The Toot named does not exist.

@subsection{Status: 405 Not Allowed}

The Toot named is  one that you have permission to use,  but are not the
main owner of. This is usually a child account.

"
  (with-user ()
    (assert-my-character Toot-name)
    (let ((Toot (find-Toot-by-name Toot-name)))
      (setf (player-Toot *user*) Toot)
      (list 200
            ()
            (list :|toot| (Toot-info Toot)
                  :|player| (person-info *user*))))))
