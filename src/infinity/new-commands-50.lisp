(in-package :Tootsville)

;;; infinity/new-commands-50.lisp is part of Tootsville
;;;
;;; Copyright  ©  2020,  the  Corporation for  Inter-World  Tourism  and
;;; Adventuring (ciwta.org).
;;;
;;; This program is Free Software: you can redistribute it and/or modify
;;; it  under the  terms of  the GNU  Affero General  Public License  as
;;; published by the  Free Software Foundation; either version  3 of the
;;; License, or (at your option) any later version.
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


;; This file contains commands which were added in 5.0

(definfinity toot-list (nil u recipient/s)
  "Enumerates all Toots owned by the user.

@subsection 200 OK

Returns  an object  with  @code{status: true,  from: \"tootList\"},  and
a  key @code{toots}  under which  is  the list  of Toot  names owned  by
this user.

This  list can  be reformatted  (into hash  values) and  passed back  to
`INFINITY-FINGER' for details."
  (list 200
        (list :|status| t
              :|from| "tootList"
              :|toots| (mapcar #'Toot-name
                               (sort (player-Toots)
                                     #'timestamp>
                                     :key (lambda (Toot)
                                            (or (Toot-last-active Toot)
                                                (universal-to-timestamp 0))))))))

(defun plist-with-index (list)
  (loop for i from 0
     for el in list
     appending (list i el)))

(definfinity play-with ((character) u r)
  "Choose a Toot as your active CHARACTER in the game. "
  (if-let (toot (find-record 'Toot :name character))
    (if (uuid:uuid= (toot-player toot) (person-uuid *user*))
        (prog2
            (setf (player-toot *user*) Toot)
            (list 200
                  (list :|status| t
                        :|from| "playWith"
                        :|playWith| character
                        :|uuid| (toot-uuid toot)
                        :|player| (list :|uuid| (person-uuid *user*)
                                        :|name| (person-display-name *user*)
                                        :|email| (person-first-email *user*))))
          (broadcast (Toot-join-message))
          (let ((everyone (connected-Toots)))
            (dolist (Toot everyone)
              (unicast (Toot-join-message Toot)))
            (unicast (from-avatars (plist-with-index everyone))))
          (dolist (key (hash-table-keys *transient-vars*))
            (let* ((user (gethash key *transient-vars*))
                   (wtl (getf user :wtl)))
              (when wtl
                (destructuring-bind (course . facing) wtl
                  (unicast (list :|status| t
                                 :|from| "wtl"
                                 :|course| course
                                 :|facing| facing
                                 :|u| key
                                 :|n| (toot-name (find-record 'Toot :UUID key))))))))
          (broadcast (list :|status| t
                           :|from| "avatars"
                           :|avatars| (list :|joined| (Toot-info Toot)))))
        (list 403
              (v:warn :toot-security "Attempt by ~a to access ~a" *user* Toot)
              (list :|status| nil
                    :|from| "playWith"
                    :|error| "Not your Toot")))
    (list 404
          (v:warn :toot-security "Attempt by ~a to access non-existent ~a" *user* Toot)
          (list :|status|
                :|from| "playWith"
                :|error| "No such Toot"))))

(definfinity wtl ((course facing) u r)
  "Walk the line"
  (set-transient :wtl (cons course facing))
  (broadcast (list :|status| t
                   :|from| "wtl"
                   :|course| course
                   :|facing| facing
                   :|u| (toot-uuid *toot*)
                   :|n| (toot-name *toot*)))
  (list 204 nil))
