;;;; -*- lisp -*-
;;;
;;;; ./servers/src/db/friendly.lisp is part of Tootsville
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

(defrecord person (:friendly "people")
  (uuid uuid)
  (display-name string)
  (given-name string)
  (surname string)
  (date-of-birth timestamp)
  (age number)
  (sensitivep yornp)
  (child-code string)
  (gender keyword)
  (lang keyword))

(defrecord parent-child (:friendly "parent_child")
  (parent uuid ref person)
  (child uuid ref person))

(defrecord credential (:friendly "credentials")
  (uuid uuid)
  (person uuid ref person)
  (uid string)
  (provider string)
  (id-token string)
  (auth-token string)
  (refresh-token string)
  (json-info json))

(defrecord person-link (:friendly "person_links")
  (uuid uuid)
  (person uuid ref person)
  (rel keyword)
  (url uri)
  (label string)
  (provenance string))

(defrecord login (:friendly "logins")
  (uuid uuid)
  (person uuid ref person)
  (credential uuid ref credential)
  (start timestamp)
  (renewed timestamp)
  (last-seen timestamp)
  (origin string))

(defrecord contact (:friendly "contacts")
  (uuid uuid)
  (owner uuid ref person)
  (contact uuid ref person)
  (starredp yornp)
  (added timestamp)
  (last-used timestamp))

(defrecord sms (:friendly "sms")
  (uuid uuid)
  (sender uuid ref Toot)
  (destination uuid ref Toot)
  (message string)
  (mmsp yornp))


(defrecord avatar (:friendly "avatars" :pull t)
  (id number)
  (moniker string)
  (avatar-scale-x number)
  (avatar-scale-y number)
  (avatar-scale-z number))

(defrecord pattern (:friendly "patterns" :pull t)
  (id number)
  (name string))

(defrecord Toot (:friendly "toots")
  (uuid uuid)
  (name string)
  (pattern number ref pattern)
  (base-color color24)
  (pattern-color color24)
  (pad-color color24)
  (avatar number ref avatar)
  (player uuid ref person)
  (last-active timestamp)
  (note string)
  (avatar-scale-x number)
  (avatar-scale-y number)
  (avatar-scale-z number))

(defmethod save-record :before ((Toot Toot))
  (setf (Toot-last-active Toot) (now)))

(defrecord wear-slot (:friendly "wear-slots")
  (id number)
  (name string)
  (alternate number)
  (avatar-point keyword)
  (valence number)
  (obstruct-point keyword)
  (obstruct-min number)
  (obstruct-max number))

(defrecord avatar-slot (:friendly "avatar_slots")
  (id number)
  (avatar number ref avatar)
  (slot keyword)
  (valence number))

(defrecord item-template (:friendly "item_templates" :pull t)
  (id number)
  (name string)
  (default-base-color color24)
  (default-alt-color color24)
  (avatar string)
  (energy-kind keyword)
  (energy-max number)
  (on-zero keyword)
  (wear-slot number ref wear-slot)
  (weight number)
  (avatar-scale-x number)
  (avatar-scale-y number)
  (avatar-scale-z number))

(defrecord item (:friendly "items")
  (uuid uuid)
  (base-color color24)
  (alt-color color24)
  (template number ref item-template)
  (energy number)
  (avatar-scale-x number)
  (avatar-scale-y number)
  (avatar-scale-z number)
  (x number)
  (y number)
  (z number)
  (latitude number)
  (longitude number)
  (altitude number)
  (world keyword))

(defrecord inventory-item (:friendly "inventory" :id-column item)
  (item uuid ref item)
  (person uuid ref person)
  (Toot uuid ref Toot)
  (equipped keyword))


(defrecord music (:friendly "music" :pull t)
  (id number)
  (title string)
  (artist string)
  (genre string)
  (license string)
  (moniker string))

(defrecord character-music (:friendly "character_music"
                                      :pull t)
  (music number ref music)
  (Toot uuid ref Toot))

(defrecord locale-music (:friendly "locale_music"
                                   :pull t)
  (music number ref music)
  (x number)
  (y number)
  (z number)
  (radius number))



(defrecord lot (:friendly "lots")
  (x1 number)
  (x2 number)
  (y1 number)
  (y2 number)
  (z1 number)
  (z2 number)
  (ownership keyword)
  (owner-toot uuid ref Toot)
  (world keyword))
