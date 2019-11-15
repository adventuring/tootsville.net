;;;; -*- lisp -*-
;;;
;;;; src/items.lisp is part of Tootsville
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




(defun create-item (template-id)
  "Create an item as an instance of the given TEMPLATE-ID."
  (let ((template (find-record 'item-template :id template-id)))
    (make-record 'item
                 :template template-id
                 :base-color (item-template-default-base-color template)
                 :alt-color (item-template-default-alt-color template)
                 :avatar-scale-x (item-template-avatar-scale-x template)
                 :avatar-scale-y (item-template-avatar-scale-y template)
                 :avatar-scale-z (item-template-avatar-scale-z template)
                 :x 0 :y 0 :z 0
                 :latitude 0 :longitude 0 :altitude -1000
                 :world :chor)))

(defun grant-item (template-id recipient)
  "Create a new instance of TEMPLATE-ID and give it to RECIPIENT."
  (let ((item (create-item template-id))
        (player (etypecase recipient
                  (Toot (Toot-player recipient))
                  (string (Toot-player (find-record 'Toot :name recipient)))
                  (person recipient)))
        (Toot  (etypecase recipient
                 (Toot recipient)
                 (string (find-record 'Toot :name recipient))
                 (person nil))))
    (player-alert player :inventory :get item)
    (make-record 'inventory-item
                 :item (item-uuid item)
                 :person (person-uuid player)
                 :Toot (Toot-uuid Toot)
                 :equipped :N)))

(defun gift-item (item giver recipient)
  "Transfer the ownership of ITEM from GIVER to RECIPIENT."
  (when *user*
    (unless (eql *user* giver)
      (error 'not-allowed)))
  (let ((giver-player (etypecase giver
                        (Toot (Toot-player giver))
                        (string (Toot-player (find-record 'Toot :name giver)))
                        (person giver)))
        (giver-Toot  (etypecase giver
                       (Toot giver)
                       (string (find-record 'Toot :name giver))
                       (person (player-Toot giver))))
        (recipient-player (etypecase recipient
                            (Toot (Toot-player recipient))
                            (string (Toot-player (find-record 'Toot :name recipient)))
                            (person recipient)))
        (recipient-Toot  (etypecase recipient
                           (Toot recipient)
                           (string (find-record 'Toot :name recipient))
                           (person (player-Toot recipient)))))
    (player-alert recipient-player :inventory :get item)
    (player-alert giver-player :inventory :drop item)
    (let ((inventory (find-record 'inventory
                                  :item (item-uuid item)
                                  :person (person-uuid giver-player)
                                  :Toot (Toot-uuid giver-Toot))))
      (setf (inventory-item-equipped inventory) :N
            (inventory-item-person inventory) (person-uuid recipient-player)
            (inventory-item-Toot inventory) (toot-uuid recipient-Toot))
      inventory)))

(defun vanish-item (item)
  "ITEM ceases to exist."
  (v:info '(:vanish :item) "VANISH item ~a" item)
  (destroy-record item))



(defun item-lose-energy (item amount)
  "Decrease the energy of ITEM by AMOUNT (stopping at zero).

If the item's  energy reaches zero, the effect of  its :On-Zero flag will
occur; either it will remain :EMPTY, or :VANISH.

If ITEM's Energy-Kind is :COUNTABLE, then AMOUNT must be an integer."
  (when (eql :countable (item-template-energy-kind (item-template item)))
    (assert (integerp amount)))
  (cond
    ((> (item-energy item) amount)
     (decf (item-energy item) amount))
    (t
     (setf (item-energy item) 0)
     (ecase (item-template-on-zero (item-template item))
       (:vanish (progn
                  (vanish-item item)
                  (return-from item-lose-energy)))
       ;; TODO ... on-zero cases
       )))
  (save-record item))

(defun item-gain-energy (item amount)
  "Increate the energy of ITEM by AMOUNT (stopping at its :Energy-Max).

If ITEM's Energy-Kind is :COUNTABLE, then AMOUNT must be an integer."
  (when (eql :countable (item-template-energy-kind (item-template item)))
    (assert (integerp amount)))
  (incf (item-energy item) amount)
  (save-record item))



(defun don-item (item slot)
  "Equip ITEM on its owning Toot in SLOT.

If this conflicts with any other equipped items, remove them."
  (declare (ignore item slot))
  (error 'unimplemented))

(defun doff-item (item)
  "Un-equip ITEM."
  (declare (ignore item))
  (error 'unimplemented))

(defun drop-item (item)
  "Drop ITEM and cease to own it."
  (declare (ignore item))
  (error 'unimplemented))

(defun take-item (item recipient)
  "RECIPIENT becomes the new owner of ITEM.

The RECIPIENT Toot must  be close enough to pick up  ITEM, and ITEM must
be in the world, and not owned by any other player."
  (declare (ignore item))
  (when *user*
    (unless (eql *user* recipient)
      (error 'not-allowed))
    (error 'unimplemented)))


(defun item-info (item)
  "Describe ITEM in a form suitable for JSON representation.

The keys are:

@table @code
@item uuid
The item's globally unique identifier
@item baseColor
If the item's avatar has a layer named \"Base\", it should be recolored to this 
color.
@item altColor
If the item's avatar has a layer named \"Alt\", it should be recolored to this
color.
@item template
The description of the item's template, as per `ITEM-TEMPLATE-INFO'.
@item energy
The amount of energy that this item has, if any. 
@item avatarScale
The scaling of the avatar in each of @code{x}, @code{y}, and @code{z} 
dimentions.
@item position
The position of the avatar within the 200m cube: @code{x}, @code{y},
and @code{z}.
@item location
The 200m cube in which the avatar is located, by @code{latitude},
@code{longitude}, @code{altitude}, and @code{world}.
@end table"
  (list :|uuid| (item-uuid item)
        :|baseColor| (item-base-color item)
        :|altColor| (item-alt-color item)
        :|template| (item-template-info (find-record 'item-template 
                                                     :id (item-template item)))
        :|energy| (item-energy item)
        :|avatarScale| (list :|x| (item-avatar-scale-x item)
                             :|y| (item-avatar-scale-y item)
                             :|z| (item-avatar-scale-z item))
        :|position| (list :|x| (item-x item)
                          :|y| (item-y item)
                          :|z| (item-z item))
        :|location| (list :|latitude| (item-latitude item)
                          :|longitude| (item-longitude item)
                          :|altitude| (item-altitude item)
                          :|world| (item-world item))))

(defun item-template-info (template)
  "Information about the item TEMPLATE in a form suitable for JSON.

@table @code
@item id
The unique ID of this item template
@item name
The unique name of this item template, in a form suitable for
displaying to the user (in English).
@item avatar
The avatar model file. The full name of the model will be 
@code{https://jumbo.tootsville.org/Assets/Models/5/}@i{avatar}@code{.babylon}
@item energyKind
The kind of energy counting that applies to item instances of this template.
Values are @code{:COUNTABLE} or @code{:UNCOUNTABLE}, or @code{NIL}.
When an item has countable energy, it represents discrete quanta of energy 
which should be presented to the player via a counter. When an item has
uncountable energy, ithas energy which exists on a continuum, and should be
presented as a gauge representing the fraction of @code{energyMax} which
is present.
@item energyMax
The maximum amount of energy.
@item onZero
When the item's energy reaches zero, what becomes of it? 
Values are @code{:VANISH} or @code{:EMPTY}.
The item can either vanish (be destroyed) when its energy reaches zero, or
it can simply remain empty awaiting potential recharging.
@item wearSlot
As per `WEAR-SLOT-INFO'.
@item weight
The mass of the item in grams.
@end table

Not  presented via  this  interface, item  templates  also have  default
colors (base  and alternate) and  avatar scaling values;  however, these
are only interesting at the moment of item creation.

"
  (list :|id| (item-template-id template)
        :|name| (item-template-name template)
        :|avatar| (item-template-avatar template)
        :|energyKind| (item-template-energy-kind template)
        :|energyMax| (item-template-energy-max template)
        :|onZero| (item-template-on-zero template)
        :|wearSlot| (when (item-template-wear-slot template)
                      (wear-slot-info (find-record 'wear-slot
                                                   :id (item-template-wear-slot template))))
        :|weight| (item-template-weight template)))

(defun wear-slot-info (slot)
  "Decribes a wearable item SLOT in suitable form for JSON.

A ``wear  slot'' represents  a place  on the  character avatar  model on
which an item, such as an  article of clothing, can be donned (mounted).
Each  named avatar  mounting  point  can have  wear  slots of  different
valences, which are layers building outwards from the skin to the sky.

For example, in the  case of the HEAD slots, valence  10 is \"Hair\", 20
is \"Headscarf\", and 30 is \"Hat\".

Not all wear slots are necessarily present on every avatar. If an avatar
lacks the @code{avatarPoint} named, then  all valences on that point are
absent,  and  no  item  whose  template specifies  that  wear  slot  can
be donned.

@table @code
@item id
The unique ID of this wear slot
@item name
The unique name of this wear slot, in a form suitable for presentation to
the end user (in English).
@item alternate
If this slot has a mirror, alternate slot that corresponds to it, each should
reciprocally point to the other's ID here. Usually NIL.
@item avatarPoint
The name of the point on the character avatar on which a model of an
item donned in this wear slot is joined to the character. This is limited to
8 characters and is traditionally in CAPS.
@item valence
The valence of the slot represents its depth in stacking order on the avatar
access point.
@item obstructs
If this slot obstructs another slot from having an item mounted on it, then
it must be identified here. The obstruction will be against the named avatar
mount point @code{point}, and affect valences from @code{min} through
@code{max}, inclusive.
@end table"
  (list :|id| (wear-slot-id slot)
        :|name| (wear-slot-name slot)
        :|alternate| (wear-slot-alternate slot)
        :|avatarPoint| (wear-slot-avatar-point slot)
        :|valence| (wear-slot-valence slot)
        :|obstructs| (list :|point| (wear-slot-obstruct-point slot)
                           :|min| (wear-slot-obstruct-min slot)
                           :|max| (wear-slot-obstruct-max slot))))
