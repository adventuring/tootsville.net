;;;; -*- lisp -*-
;;;
;;;; src/endpoints/slash-login.lisp is part of Tootsville
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

;;;; login.lisp — Login page services
(in-package :Tootsville)

(defendpoint (post "/login/child" "application/json")
  "Child login submission.

See `LOGIN-CHILD' for details of the child login protocol."
  (with-posted-json (name code)
    (let* ((Toot (ignore-errors (find-record 'Toot :name name)))
           (correctp (when (and Toot (Toot-childp Toot))
                       (string-equal code (Toot-child-code Toot)))))
      (v:info :child-login "Login request from ~a with ~:[incorrect~;correct~] code" 
              name correctp)
      (if correctp
          (list 200 (login-child Toot))
          (list 403 (if Toot
                        (list :|error| "Wrong secret code")
                        (list :|error| (format nil "No Toot is named “~a”" name))))))))
