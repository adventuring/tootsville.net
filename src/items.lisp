;;;; -*- lisp -*-
;;;
;;;; src/items.lisp is part of Tootsville
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




(defun create-item (template-id)
  "Create an item as an instance of the given TEMPLATE-ID."
  (let ((template (find-record 'item-template :id template-id)))
    (make-record 'item
                 :uuid (uuid:make-v4-uuid)
                 :template template-id
                 :base-color (item-template-default-base-color template)
                 :alt-color (item-template-default-alt-color template)
                 :avatar-scale-x (item-template-avatar-scale-x template)
                 :avatar-scale-y (item-template-avatar-scale-y template)
                 :avatar-scale-z (item-template-avatar-scale-z template)
                 :x 0 :y 0 :z 0
                 :facing 0
                 :latitude 0 :longitude 0 :altitude -1000
                 :world :inv)))

(defun grant-item (template-id recipient)
  "Create a new instance of TEMPLATE-ID and give it to RECIPIENT."
  (let ((item (create-item template-id))
        (player (etypecase recipient
                  (Toot (find-reference recipient :player))
                  (string (find-reference (find-record 'Toot :name recipient)
                                          :player))
                  (person recipient)))
        (Toot (etypecase recipient
                (Toot recipient)
                (string (find-record 'Toot :name recipient))
                (person nil))))
    (make-record 'inventory-item
                 :item (item-uuid item)
                 :person (person-uuid player)
                 :Toot (Toot-uuid Toot)
                 :equipped :n)))

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
                       (string (find-record 'Toot :name giver))))
        (recipient-player (etypecase recipient
                            (Toot (Toot-player recipient))
                            (string (Toot-player (find-record 'Toot :name recipient)))
                            (person recipient)))
        (recipient-Toot  (etypecase recipient
                           (Toot recipient)
                           (string (find-record 'Toot :name recipient)))))
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



(defmethod ensure-inventory-item ((item inventory-item))
  item)

(defmethod ensure-inventory-item ((item uuid:uuid))
  (find-record 'inventory-item :UUID item))

(defmethod ensure-inventory-item ((item string))
  (find-record 'inventory-item :UUID (UUID:make-uuid-from-string item)))

(defmethod ensure-wear-slot ((slot wear-slot))
  slot)

(defmethod ensure-wear-slot ((slot integer))
  (find-record 'wear-slot :id slot))

(defmethod ensure-wear-slot ((slot string))
  (find-record 'wear-slot :id (parse-integer slot)))

(defun avatar-has-slot-p (avatar slot)
  (ignore-not-found (find-record 'avatar-slot
                                 :avatar (avatar-id avatar)
                                 :slot (wear-slot-name slot))))



(defun don-item (inventory-item &optional wear-slot)
  "Equip INVENTORY-ITEM on its owning Toot in SLOT.

If this conflicts with any other equipped items, remove them."
  (let* ((inventory-item (ensure-inventory-item inventory-item))
         (item (find-reference inventory-item :item))
         (item-template (find-reference item :template))
         (Toot (find-reference inventory-item :Toot))
         (avatar (find-reference Toot :avatar))
         (wear-slot (ensure-wear-slot
                     (if wear-slot
                         wear-slot
                         (item-template-wear-slot item-template))))
         (person (find-reference inventory-item :person)))
    (assert (UUID:UUID= (Toot-player Toot) (person-UUID person))
            (Toot person)
            "Item ~s: Toot ~s is not owned by person ~s"
            inventory-item Toot person)
    (assert (avatar-has-slot-p avatar wear-slot)
            (avatar wear-slot)
            "Avatar ~s does not have a slot ~s" avatar wear-slot)
    (assert (= (wear-slot-id wear-slot) (item-template-wear-slot item-template))
            (item-template wear-slot)
            "Item ~s: template ~s cannot be worn in slot ~s (needs slot #~d)"
            inventory-item item-template wear-slot (item-template-wear-slot item-template))
    (doff-any-conflicting-item wear-slot Toot)
    (setf (inventory-item-equipped inventory-item) :Y))
  t)

(defun doff-item-in-slot (wear-slot Toot)
  (when-let (found (find-record 'inventory-item
                                :Toot (Toot-UUID Toot)
                                :wear-slot (wear-slot-id wear-slot)))
    (doff-item found)))

(defun doff-any-conflicting-item (wear-slot Toot)
  (let ((doffedp nil))
    (when-let (point (wear-slot-obstruct-point wear-slot))
           (let ((min (wear-slot-obstruct-min wear-slot))
                 (max (wear-slot-obstruct-max wear-slot)))
             (do-records (other-slot wear-slot :avatar-point point)
               (when (<= min (wear-slot-valence other-slot) max)
                 (let ((doffed-this-item-p (doff-item-in-slot other-slot Toot)))
                   (unless doffedp
                     (setf doffedp doffed-this-item-p)))))))
   doffedp))

(defun doff-item (inventory-item)
  "Un-equip ITEM."
  (when (not (eql :N (inventory-item-equipped inventory-item)))
    (setf (inventory-item-equipped inventory-item) :N)
    (save-record inventory-item)
    inventory-item))

(defun drop-item (inventory-item)
  "Drop ITEM and cease to own it."
  (let ((item (inventory-item-item inventory-item))
        (Toot (find-reference inventory-item 'Toot)))
    (destroy-record inventory-item)
    (destructuring-bind (world latitude longitude altitude) (Toot-position Toot)
      (destructuring-bind (x y z) (current-position Toot)
        (setf (item-x item) x
              (item-y item) y
              (item-z item) z
              (item-latitude item) latitude
              (item-longitude item) longitude
              (item-altitude item) altitude
              (item-world item) world)))
    (save-record item)
    item))

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
  "Describes ITEM in a JSON structure.

This structure has the following keys:

@table @code
@item uuid
The universally-unique ID of this particular item
@item baseColor
The base color of this item (if any). This is a primary color
texture that may be applied to the model. See `PARSE-COLOR24'
for the syntax. The model must have a material named @code{base}
for this color to apply to it (case-insensitive).
See `Tootsville.FurnitureBuilder.makeFurnitureColorizeMaterial'
for details.
@item altColor
The alternate color of this item (if any). This is a secondary 
color texture that may be applied to the model. See
`PARSE-COLOR24' for the syntax. The model must have a material
named @code{alt} for this color to apply to it
(case-insensitive).
See `Tootsville.FurnitureBuilder.makeFurnitureColorizeMaterial'
for details.
@item specialTexture
The special texture image which may be applied to this image 
(if any). Some item models have a material named @code{map} which
is textured with this image on a per-item basis. This is often
used for e.g. signs and things which share geometry but have one
surface that displays something unique. See
`Tootsville.FurnitureBuilder.setMaterialTexture' for details.
@item template
The Item Template of which this individual item is an instance.
This is a table in the form described at `ITEM-TEMPLATE-INFO'.

@item energy
For items with an @code{energyKind} of @code{COUNTABLE} or
@code{UNCOUNTABLE}, this indicates the number of (@code{COUNTABLE})
discrete energy units remaining or (@code{UNCOUNTABLE}) the
portion of @code{energyMax} remaining (which should be surfaced to
the user as a percentage or the like).
@item scale
The item's scaling factors in each of the @code{x}, @code{y}, and @code{z}
dimensions, as compared to the size of the raw asset in the avatar
model file.
@item position
The item's relative position in @code{x}, @code{y}, and @code{z}
coördinates
@item facing
The angle in which the item is facing in radians. Clients should also
support, for compatibility, the eight cardinal directions given as the
strings @code{N}, @code{NE}, @code{E}, @code{SE}, @code{S}, @code{SW},
@code{W}, or @code{NW}.
@item world
The world in which the item is located
@item location
The location of the item within the world in @code{lat}-itude, 
@code{long}-itude, and @code{alt}-itude.

@end table
"
  (let ((item (ensure-item item)))
    (list :|uuid| (item-uuid item)
          :|baseColor| (item-base-color item)
          :|altColor| (item-alt-color item)
          :|specialTexture| (or (item-special-texture item) :null)
          :|template| (item-template-info (find-reference item :template))
          :|energy| (item-energy item)
          :|scale| (list :|x| (item-avatar-scale-x item)
                         :|y| (item-avatar-scale-y item)
                         :|z| (item-avatar-scale-z item))
          :|position| (list :|x| (item-x item)
                            :|y| (item-y item)
                            :|z| (item-z item))
          :|facing| (item-facing item)
          :|world| (item-world item)
          :|location| (list :|lat| (item-latitude item)
                            :|long| (item-longitude item)
                            :|alt| (item-altitude item)))))

(defun item-template-info (template)
  "Provides a JSON structure describing the item TEMPLATE given.

This structure has the following keys:

@table @code

@item id
The unique ID (integer) of this item template

@item name
The unique name of this item template. This may be user-visible.

@item description
A description, which may be surfaced to the user, of this item.

@item trade
This is one of the following values (case-insensitive):

@table @code

@item Y
Yes, this item can be traded (given away or dropped).

@item N
No, this item cannot be traded (given away or dropped).

@item X 
As @code{N}, but also, this item @i{should not} be visible to
the player holding it (e.g. in inventory).  This code represents the
value ``hidden''.

@end table

@item avatar
The item avatar (model) representing this item.

@item energyKind
The kind of energy (if any) used by this item. Values 
may be ``null,'' if the item does not consume any particular
kind of energy, or @code{COUNTABLE} if the item uses a form of
energy that is counted in discrete units, or @code{UNCOUNTABLE}
if the item's energy is recorded as a fluid percentage of
its maximum value. This should be used by the client to
provide either a counter, a gauge (meter), or no affordance
indicating the energy level, as appropriate.

@item energyMax
The maximum amount of energy that this kind of item can
possess.

@item onZero
When this item's energy reaches zero, does it @code{VANISH} from the
game, or remain @code{EMPTY} awaiting a refill?

@item gauge
Linked to @code{energyKind}, should a guage or counter be
displayed? True for @code{EnergyKind} of @code{COUNTABLE}
or @code{UNCOUNTABLE}, false for null.

@item wearSlot
The ID of the wearable item slot into which this item can
be equipped, if any.

@item weight
The weight of instances of this template

@end table
"
  
  (list :|id| (item-template-id template)
        :|name| (item-template-name template)
        :|description| (or (item-template-description template) "")
        :|trade| (string-upcase (item-template-trade template))
        :|avatar| (item-template-avatar template)
        :|energyKind| (item-template-energy-kind template)
        :|energyMax| (item-template-energy-max template)
        :|onZero| (item-template-on-zero template)
        :|gauge| (case (item-template-energy-kind template)
                   (:nil :false)
                   (otherwise t))
        :|wearSlot| (item-template-wear-slot template)
        :|weight| (item-template-weight template)
        :|scale| (list :|x| (item-template-avatar-scale-x template)
                       :|y| (item-template-avatar-scale-y template)
                       :|z| (item-template-avatar-scale-z template))))



(defun Toot-inventory (&optional (Toot *Toot*)
                       &key privatep)
  "The inventory of TOOT, possibly including PRIVATEP items.

When PRIVATEP is false (default), only the inventory items which
are equipped will be enumerated.

Returns a list of ITEM objects."
  (mapcar (lambda (i-i-i)
            (find-record 'item :uuid (inventory-item-item i-i-i))) 
          (find-records 'inventory-item :Toot (Toot-UUID Toot))))

(defun Toot-has-item-p (item-template-id &optional (Toot *Toot*))
  "A generalize boolean indicating whether TOOT has any item based upon ITEM-TEMPLATE-ID

Calls `TOOT-INVENTORY' to benefit from caching."
  (member item-template-id (mapcar #'item-template (Toot-inventory Toot))))

(defun item-owned-by-p (item &optional (Toot *Toot*))
  "A generalized boolean indicating whether ITEM is owned by TOOT.

Calls `TOOT-INVENTORY' to benefit from caching."
  (member item (Toot-inventory Toot)))




(defun store-info (store-item)
  "Returns a structure describing STORE-ITEM.

This structure is a JSON-style Plist with the keys:

@table @code
@item id
The unique store item ID. This is currently a UUID.
@item template
The `ITEM-TEMPLATE-INFO' of this item
@item qty
The quantity (integer) of these items available in the store.
@item price
The price (in @code{currency} units) of the item
@item currency
The currency indicator. This will generally be one of
@table @code
@item X-TVPN
Tootsville peanuts; or
@item X-FADU
Fairy dust
@end table
@end table
"
  (list :|id| (store-item-uuid store-item)
        :|template| (item-template-info (find-reference store-item :template))
        :|qty| (store-item-qty store-item)
        :|price| (store-item-price store-item)
        :|currency| (store-item-currency store-item)))



(defun place-string (place)
  "Formats PLACE in the encoding for the client.

The PLACE is encoded into a string in the form:

@example
kind:shape|appearance|attributes
@end example
"
  (format nil "~a:~a|~a|~a"
          (place-kind place)
          (place-shape place)
          (place-appearance place)
          (place-attributes place)))

(defun wear-slot-info (wear-slot)
  "Provides a JSON-style Plist describing WEAR-SLOT.

@table @code
@item id
The unique ID of this wear-slot.
@item name
The (potentially user-visible) name of this wear-slot.
@item alternate
If this wear-slot has an alternate slot associated with it,
this will be the wear-slot-ID of the alternate slot.
@item avatarPoint
The moniker of the point on the avatar to which an item
in this slot is mounted.
@item valence
The valence level of this wear-slot on that avatarPoint.
Multiple items mounted on one wear-slot can exist in
valence levels.
@item obstruct
If wearing an item in this slot obstructs the character
from also wearing items in certain other slots:
@table @code
@item point
The @code{avatarPoint} which is obstructed,
@item min
The minimum valence level obstructed,
@item max
and the maximum valence level obstructed.
@end table
@end table
"
  (list :|id| (wear-slot-id wear-slot)
        :|name| (wear-slot-name wear-slot)
        :|alternate| (wear-slot-alternate wear-slot)
        :|avatarPoint| (wear-slot-avatar-point wear-slot)
        :|valence| (wear-slot-valence wear-slot)
        :|obstruct| (list :|point| (wear-slot-obstruct-point wear-slot)
                          :|min| (wear-slot-obstruct-min wear-slot)
                          :|max| (wear-slot-obstruct-max wear-slot))))

(defun place-string-circle (radius x-center z-center segments)
  "Defines a place-string for a circle of RADIUS centered at X-CENTER, Z-CENTER with SEGMENTS precision.

An n-sided (SEGMENTS-sided) regular polygon approximating a circle
will be created at (X-CENTER, Z-CENTER) and returned as the path
segments string used by the client; i.e. a list of the form
@code{x,y,z~x,y,z~x,y,z} with @code{~} delimiters between coördinate
lists joined by @code{,}."
  (format nil "~{~{~8,6f,0,~8,6f~}~^~~~}" 
          (loop for i from 1.0 below segments 
                collecting (list
                            (+ x-center (* radius (cos (* 2 pi (/ i segments)))))
                            (+ z-center (* radius (sin (* 2 pi (/ i segments)))))))))



(defun vitem-grant-item (item recipient)
  "RECIPIENT receives an item from ITEM.

As per the VITEM placement command; see also 
`TOOTSVILLE-USER::PLACE'. "
  (grant-item (item-template-id item) recipient))

(defconstant +snowball-item+ 100)

(defun grant-snowballs (recipient &optional (count 6))
  "RECIPIENT receives COUNT snowballs.

As     per    the     SNOWBALL    placement     command;    see     also
`TOOTSVILLE-USER::PLACE'."
  (dotimes (i count)
    (grant-item +snowball-item+ recipient)))

(defun swing-door (item)
  "Swing the door open or shut (toggle)"
  (error 'unimplemented))

(defgeneric %item-click-effect (item effect clicker mods x y z)
  (:documentation "Low-level mapping of EFFECT to a handler")
  (:method (item (effect (eql :vitem)) clicker mods x y z)
    (vitem-grant item clicker))
  (:method (item (effect (eql :snowballs)) clicker mods x y z)
    (grant-snowballs clicker))
  (:method (item (effect (eql :shop)) clicker mods x y z)
    (ask-buy-item item clicker))
  (:method (item (effect (eql :swing-door)) clicker mods x y z)
    (swing-door item)))

(defun item-accept-click (item clicker mods &optional x y z)
  "CLICKER has clicked on ITEM with MODS in effect at item-relative X Y Z"
  (let ((item (ensure-item item)))
    (when-let (effect (item-effect item))
      (%item-click-effect item effect clicker mods x y z))))

(defun item-template-tags (template)
  "Returns the set of tags associated with TEMPLATE.

TEMPLATE can be an `ITEM-TEMPLATE' or the ID number for one."
  (let ((template-id (etypecase template
                       (number template)
                       (item-template (item-template-id template)))))
    (mapcar #'item-tag-tag (find-records 'item-tag
                                         :item template-id))))

(defun ensure-item (designator)
  (etypecase designator
    (string (find-record 'item :uuid designator))
    (uuid:uuid (find-record 'item :uuid designator))
    (item designator)))
