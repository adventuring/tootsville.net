;;;; -*- lisp -*-
;;;
;;;; src/endpoints/slash-users.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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

Requires the user  to pass some external,  trusted authentication source
information, like an OAuth2 login.

@subsection{Status: 201 Created}

XXX is there a better status for updates?

@subsection{Status: 401 Authorization Required}

@subsection{Status: 403 Authorization Failed}

@subsection{Status: 405 Not Allowed}

"
  (with-user ()
    (with-posted-json (field new-value)
      (with-errors-as-http (400)
        (check-type field string)
        (check-type new-value string))
      (ecase (make-keyword (symbol-munger:camel-case->lisp-name field))
        (:display-name
         (setf (person-display-name *user*) (with-errors-as-http (422)
                                              (reasonable-name new-value))))
        (:given-name
         (setf (person-given-name *user*) (with-errors-as-http (422)
                                            (reasonable-name new-value))))
        (:surname
         (setf (person-surname *user*) (with-errors-as-http (422)
                                         (reasonable-name new-value))))
        (:language
         (with-errors-as-http (422)
           (assert (member (the string new-value) +supported-languages+ :test #'string=)))
         (setf (person-lang *user*) new-value))
        (:gender
         (with-errors-as-http (422)
           (assert (member (the string new-value) '("☿" "♀" "♂") :test #'string=)))
         (setf (person-gender *user*) (ecase (char new-value 0)
                                        (#\☿ "X")
                                        (#\♀ "F")
                                        (#\♂ "M"))))
        (:date-of-birth
         (with-errors-as-http (422)
           (setf (person-date-of-birth *user*) new-value)
           (assert (<= 13 (person-age*) 125) (new-value)
                   "Person's age must be between 13 and 125."))))
      (save-record *user*))
    (list 200 (person-info))))

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

(defendpoint (get "/users/me/toots" "application/json" 1)
  "Enumerate all Toot characters available to you."
  (with-user ()
    (dolist (Toot (player-Toots))
      (v:info :Toots "Player ~a has Toot ~a" (person-display-name *user*)
              (Toot-name Toot)))
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
    (let ((Toot (find-record 'Toot :name Toot-name)))
      (list 200
            () ;; (list :Last-Modified (Toot-last-active Toot))
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
    (let ((Toot (find-record 'Toot :name Toot-name)))
      (if-let (p-t-link (ignore-not-found 
                          (find-record 'player-Toot :player (person-uuid *user*))))
        (setf (player-Toot-Toot p-t-link) (Toot-uuid Toot))
        (save-record (make-record 'player-Toot :player (person-uuid *user*) 
                                  :Toot (Toot-uuid Toot))))
      (setf (Toot-last-active Toot) (get-universal-time))
      (list 200
            ()
            (list :|toot| (Toot-info Toot)
                  :|player| (person-info *user*))))))
