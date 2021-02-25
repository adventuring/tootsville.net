;;;; -*- lisp -*-
;;;
;;;; src/db/friendly.lisp is part of Tootsville
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

(defrecord person (:friendly "people")
  (uuid uuid)
  (display-name string)
  (given-name string)
  (surname string)
  (date-of-birth timestamp)
  (age number)
  (sensitivep yornp)
  (gender keyword)
  (lang keyword))

(defmethod save-record :before ((person person))
  (when (null (person-given-name person))
    (setf (person-given-name person) ""))
  (when (null (person-surname person))
    (setf (person-surname person) ""))
  (when (emptyp (person-display-name person))
    (setf (person-display-name person) (format nil "~a ~a"
                                               (person-given-name person)
                                               (person-surname person)))))

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

(defmethod print-object ((link person-link) s)
  (format s "#<Person-Link ~a ~a —~a—→ ~a (~a from ~a)>"
          (person-link-uuid link)
          (person-link-person link)
          (person-link-rel link)
          (person-link-url link)
          (person-link-label link)
          (person-link-provenance link)))

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
  (owner uuid ref Toot)
  (contact uuid ref Toot)
  (starredp yornp)
  (added timestamp)
  (last-used timestamp))

(defmethod print-object ((contact contact) s)
  (format s "#<Contact ~a:~a~@[ ★~]>"
          (contact-owner contact) (contact-contact contact) (contact-starredp contact)))

(defrecord ignored (:friendly "ignored")
  (uuid uuid)
  (owner uuid ref Toot)
  (ignored uuid ref Toot))

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
  (child-code string)
  (last-active timestamp)
  (note string)
  (avatar-scale-x number)
  (avatar-scale-y number)
  (avatar-scale-z number))

(defmethod print-object ((Toot Toot) s)
  (format s "#<Toot ~a ~a>" (Toot-name Toot) (Toot-UUID Toot)))

(defmethod save-record :before ((Toot Toot))
  (setf (Toot-last-active Toot) (now)))

(defrecord wear-slot (:friendly "wear_slots")
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
  (description string)
  (trade keyword) ; Y/es, N/o, or X/hidden
  (default-base-color color24)
  (default-alt-color color24)
  (avatar string)
  (energy-kind keyword) ; COUNTABLE or UNCOUNTABLE, or null
  (energy-max number)
  (on-zero keyword) ; VANISH or EMPTY
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
  (facing number)
  (latitude number)
  (longitude number)
  (altitude number)
  (world keyword ref worlds)
  (effect keyword)
  (attributes string)
  (special-texture string))

(defmethod print-object ((item item) s)
  (format s "#<Item (~a) @ (~d, ~d, ~d) on ~a (~d, ~d) + ~d>"
          (item-template item)
          (item-x item) (item-y item) (item-z item)
          (item-world item)
          (item-latitude item) (item-longitude item)
          (item-altitude item)))

(defrecord item-tag (:friendly "item_tags")
  (item number ref item-templace)
  (tag keyword))

(defmethod print-object ((item-tag item-tag) s)
  (format s "#<Item Template ~d Tag ~a>"
          (item-tag-item item-tag)
          (item-tag-tag item-tag)))

(defrecord inventory-item (:friendly "inventory" :id-column item)
  (item uuid ref item)
  (person uuid ref person)
  (Toot uuid ref Toot)
  (equipped keyword))

(defmethod print-object ((item inventory-item) s)
  (format s "#<Inventory Item ~a owned by ~a (~a)~@[ (equipped)~]>"
          (inventory-item-item item)
          (inventory-item-person item)
          (inventory-item-Toot item)
          (inventory-item-equipped item)))

(defmethod inventory-item-equippedp ((item inventory-item))
  (inventory-item-equipped item))

(defrecord store-item (:friendly "store_items")
  (uuid uuid)
  (template number ref item-template)
  (qty number)
  (price number)
  (currency keyword))

(defmethod print-object ((item store-item) s)
  (format s "#<Store Item ~:d × qty ~:d @ ~a ~:d>"
          (store-item-template item)
          (store-item-qty item)
          (store-item-currency item)
          (store-item-price item)))

(defmethod store-item-quantity ((item store-item))
  (store-item-qty item))


(defrecord music (:friendly "music" :pull t)
  (id number)
  (title string)
  (artist string)
  (file string)
  (link string)
  (license string)
  (moniker string))

(defrecord character-music (:friendly "character_music"
                            :pull t)
  (music number ref music)
  (Toot uuid ref Toot))

(defrecord locale-music (:friendly "locale_music"
                         :pull t)
  (music number ref music)
  (latitude number)
  (longitude number)
  (altitude number)
  (world string))

(defmethod save-record ((object locale-music))
  (with-dbi (:friendly)
    (before-save-normalize object)
    (let* ((query
             (dbi.driver:prepare *dbi-connection*
                                 "INSERT INTO locale_music (music, latitude, longitude, altitude, world) values (?, ?, ?, ?, ?) on duplicate key update  music =  ?;")))
      (verbose:info :db "saving Locale Music for (~d,~d) +~d @~a"
                    (locale-music-latitude object) (locale-music-longitude object)
                    (locale-music-altitude object) (locale-music-world object))
      (with-slots (latitude longitude altitude world music)
          object
        (dbi.driver:execute query
                            (list (column-save-value music :number)
                                  (column-save-value latitude :number)
                                  (column-save-value longitude :number)
                                  (column-save-value altitude :number)
                                  (column-save-value world :keyword)
                                  (column-save-value music :number))))))
  object)



(defrecord lot (:friendly "lots")
  (x1 number)
  (x2 number)
  (y1 number)
  (y2 number)
  (z1 number)
  (z2 number)
  (ownership keyword)
  (owner-toot uuid ref Toot)
  (world keyword ref worlds))



(defrecord world (:friendly "worlds"
                  :pull t)
  (moniker keyword)
  (name string))

(defrecord mist (:friendly "mist")
  (world keyword ref worlds)
  (latitude-1 number)
  (longitude-1 number)
  (altitude-1 number)
  (latitude-2 number)
  (longitude-2 number)
  (altitude-2 number)
  (definedp yornp))



(defrecord terrain-height (:friendly "terrain_heights")
  (world keyword ref worlds)
  (latitude number)
  (longitude number)
  (terrain string))



(defrecord child-request (:friendly "child_requests")
  (uuid uuid)
  (Toot uuid ref Toot)
  (placed-at timestamp)
  (allowed-at timestamp)
  (denied-at timestamp)
  (allowed-for number)
  (response string))



(defrecord place (:friendly "places")
  (uuid uuid)
  (world keyword ref worlds)
  (latitude number)
  (longitude number)
  (altitude number)
  (shape string)
  (kind keyword)
  (attributes string)
  (appearance string))



(defrecord Toot-quiesced (:friendly "toots_quiesced")
  (Toot uuid ref Toot)
  (world keyword ref worlds)
  (latitude number)
  (longitude number)
  (altitude number)
  (wtl string)
  (d3 string)
  (emotion keyword)
  (observed timestamp)
  (peer-address string)
  (attribs string))

(defmethod id-column-for ((type (eql 'Toot-quiesced)))
  'Toot)

(defmethod print-object ((q Toot-quiesced) s)
  (format s "#<Toot-Quiesced ~a>" (Toot-quiesced-Toot q)))



(defrecord quaestor-event (:friendly "quaestor_events")
  (uuid uuid)
  (source uuid)
  (started-by uuid ref Toot)
  (started-at timestamp)
  (ended-at timestamp)
  (completedp yornp)
  (peanuts number)
  (fairy-dust number)
  (item uuid ref items)
  (score number)
  (medal keyword)
  (kind keyword))



(defrecord staff-journal-entry (:friendly "staff_journal_entries")
  (uuid uuid)
  (written-by uuid)
  (written-at timestamp)
  (entry string))

(defrecord staff-journal-reference (:friendly "staff_journal_references")
  (entry uuid)
  (person uuid))



(defrecord named-spot (:friendly "named_spots")
  (name string)
  (world keyword)
  (latitude number)
  (longitude number)
  (altitude number)
  (x number)
  (y number)
  (z number)
  (badgedp yornp))
