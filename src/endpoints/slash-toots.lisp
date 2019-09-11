;;;; -*- lisp -*-
;;;
;;;; src/endpoints/slash-toots.lisp is part of Tootsville
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

(defendpoint (get "/toots/:toot-name/avatar" "application/json" 1)
  "Get the avatar info for TOOT-NAME"
  (check-arg-type Toot-name Toot-name)
  (if-let (Toot (find-Toot-by-name Toot-name))
    (list 200
          `(:last-modified ,(header-time (or (Toot-last-active Toot) (now))))
          (Toot-info Toot))
    (list 404 nil nil)))

(defendpoint (get "/toots/:toot-name" "application/json" 1)
  "Get public info about TOOT-NAME"
  (check-arg-type toot-name toot-name)
  (with-user ()
    (let ((Toot (find-toot-by-name toot-name)))
      (list 200
            `(:last-modified ,(header-time))
            (Toot-info Toot)))))

(defendpoint (post "/toots" "application/json")
  "Create a new Toot.

Input JSON must have the following fields:
     tootName, baseColor, padColor, pattern, patternColor, tShirtColor

Responds with 201 (Created); or 409 (Conflict)  if the name is in use or
for some other reason the value can't  be entered; 422 if the Toot name,
color  or pattern  name(s)  given are  not valid.  (400  if the  request
is malformed.)"
  (with-user ()
    (with-posted-json (Toot-name base-color pad-color pattern pattern-color
                                 t-shirt-color)
      (with-errors-as-http (422)
        (check-type Toot-name Toot-name)
        (check-type base-color Toot-base-color)
        (check-type pad-color Toot-pad-color)
        (check-type pattern pattern)
        (check-type pattern-color Toot-pattern-color)
        (check-type t-shirt-color initial-t-shirt-color))
      (with-errors-as-http (409)
        (assert (not (find-record 'Toot :name Toot-name)))
        (let ((Toot
               (make-record 'Toot
                            :name toot-name
                            :pattern (pattern-id (find-record 'pattern
                                                              :name pattern))
                            :base-color (parse-color24 base-color)
                            :pattern-color (parse-color24 pattern-color)
                            :pad-color (parse-color24 pad-color)
                            :player (person-uuid *player*)
                            :avatar-scale-x 1.0
                            :avatar-scale-y 1.0
                            :avatar-scale-z 1.0
                            :avatar 1   ; UltraToot
                            :note (concatenate 
                                   'string
                                   "New Toot registered via web from "
                                   (hunchentoot:remote-addr*))))
              (t-shirt 
               (make-record 'item
                            :base-color (parse-color24 t-shirt-color)
                            :alt-color (color24-rgb 0 0 0)
                            :template 1       ; Solid color basic T-shirt
                            :avatar-scale-x 1.0
                            :avatar-scale-y 1.0
                            :avatar-scale-z 1.0)))
          (save-record Toot)
          (save-record t-shirt)
          (save-record
           (make-record 'inventory-item
                        :item (item-uuid t-shirt)
                        :person (person-uuid *player*)
                        :Toot (Toot-uuid Toot)
                        :equipped :y))
          (list 200 ))))))

(defendpoint (put "/toots/:toot-name/child-p" "application/json")
  "Set CHILD-P flag for TOOT-NAME.

Input JSON: { set: true } or { set: false }"
  (check-arg-type Toot-name Toot-name)
  (with-user ()
    (let ((Toot (find-Toot-by-name Toot-name)))
      (assert-my-character Toot))
    (list 200
          `(:last-modified ,(header-time (Toot-last-active Toot)))
          (if-let (toot (find-toot-by-name toot-name))
            (toot-info toot)
            `(:is-a "toot"
                    :name ,(string-capitalize toot-name)
                    :avatar "ultraToot"
                    :child-p nil
                    :sensitive-p t
                    :online-p t
                    :last-seen ,(local-time:format-timestring
                                 nil (local-time:now))
                    :exists-p "maybe?")))))
