;;;; -*- lisp -*-
;;;
;;;; src/items.lisp is part of Tootsville
;;;
;;;; Copyright © 2008-2017 Bruce-Robert Pocock; © 2010, Res Interactive,
;;;; LLC;  ©  2018,2019  The  Corporation for  Inter-World  Tourism  and
;;;; Adventuring (ciwta.org).
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


(defun item-owner (item)
  ;;FIXME
  )

(defun item-equipped-p (item)
  ;; FIXME
  )

(defun item-template-can-trade-p (item-template)
  "If true, this item can be traded, dropped, or give away.

If false, once lodged into a player's inventory, it is stuck there forever.

Item templates  have a property  `ITEM-TEMPLATE-TRADE' which can  be any
of :Y,  :N, or :X. When  :Y, the item  can be traded, dropped,  or given
away freely. This is the usual case and describes almost all items.

Certain items cannot be dropped or  given away. These items have a TRADE
value of  :N or :X.  The difference  between :N and  :X is that  an item
with :N  as its TRADE  value is still  visible normally in  the player's
inventory, and can be equipped if it's  such an item. An item with :X as
its TRADE value will be invisible to  its owner, so it cannot be used in
any way.
"
  (eql :y (item-template-trade item-template)))

(defun item-template-visible-to-owner-p (item-template)
  "If true, the owner of this item can tell that they own it.

If false, the item is invisible to  its owner and @i{de facto} will also
be untradeable (see `ITEM-TEMPLATE-CAN-TRADE-P' for a discussion)."
  (not (eql :x (item-template-trade item-template))))



(defmethod currency-name (code)
  (:method (code (eql :x-tvpn))
    "Tootsville Magic Peanuts")
  (:method (code (eql :x-fadu))
    "Fairy Dust"))

(defmethod currency-symbol (code)
  (:method (code (eql :x-tvpn)) "🥜")
  (:method (code (eql :x-fadu)) "⁂"))


(defun item-info (item)
  "Describe ITEM in a form suitable for JSON representation.

The keys are:

@table @code
@item uuid, slot
The item's globally unique identifier.
The alias @code{slot} is a legacy name which is derprecated, and frankly,  
confusing. (In Romance 1.x, the inventory was considered to have a sequence
of integer inventory slots, into which items were inserted.)
@item baseColor, color
If the item's avatar has a layer named \"Base\", it should be recolored to this
color.
@item altColor
If the item's avatar has a layer named \"Alt\", it should be recolored to this
color.
@item template
The description of the item's template, as per `ITEM-TEMPLATE-INFO'.
@item energy, health
The amount of energy that this item has, if any. 
@item avatarScale
The scaling of the avatar in each of @code{x}, @code{y}, and @code{z} 
dimentions.
@item rarity
Always returns FIXME
@item position and x,y,z
The position of the avatar within the 200m cube
identified by @code{location}: @code{x}, @code{y},
and @code{z}.
See `ITEM-X', `ITEM-Y', `ITEM-Z'.
Note that these are subject to the physics engine and may not always be 
current and accurate when obtained from the server.
@item location
The 200m cube in which the avatar is located, by @code{latitude},
@code{longitude}, @code{altitude}, and @code{world}. 
See `ITEM-LATITUDE', `ITEM-LONGITUDE', `ITEM-ALTITUDE', `ITEM-WORLD'
@item itemType, title
The name of the item's template. See `ITEM-TEMPLATE-NAME'
@item itemID
The unique ID of the item's template.
See `ITEM-TEMPLATE-ID', `ITEM-TEMPLATE'
@item inRoom
Deprecated. Always returns ``@code{@@Tootsville}'' now.
@item ownerID
The UUID of the Toot who owns this item, if any. If there is no owner, 
returns the string ``@code{-1}''.
See `ITEM-OWNER'
@item isActive
If this item is equipped by its owner, then this value is @code{true}.
See `ITEM-IS-EQUIPPED-P'
@item equipType
The name of the wear-slot into which this item can be worn, if any. 
See `WEAR-SLOT-NAME' by way of `ITEM-TEMPLATE-WEAR-SLOT'
by way of `ITEM-TEMPLATE'.
@end table

@subsection Changes from 1.2 to 2.0

@itemize
@item
@code{inRoom} is no longer supported, in favor of @code{location}.
@item
@code{x}, @code{y}, and @code{z} may not always be correct as the item may be
subject to the physics engine.
@item
As announced previously, @code{healthType} now is presented as 
@code{continuous} or @code{dicrete} rather than @code{C} or @code{D}.
@end itemize

@subsection Deprecated

@table @code
@item slot
Deprecated in favor of the less confusing and more direct name @code{uuid}.
@item inRoom
Already useless, use @code{location} instead.
@item x,y,z
Use the versions in @code{position} or obtain current data from the mesh.
@item itemType, itemID, title
Use the information in @code{template}; see `ITEM-TEMPLATE-INFO'
@item color
Use @code{baseColor}.
@item rarity, facing
These no longer have meaningful values.
@item health, healthType
Use @code{energy}, @code{template.energyType}
@item equipType
@end table
"
  (let ((item-template (find-record 'item-template 
                                    :id (item-template item))))
    (list :|uuid| (item-uuid item)
          :|ownerID| (or (item-owner item) "-1")
          :|baseColor| (item-base-color item)
          :|altColor| (item-alt-color item)
          :|template| (item-template-info item-template)
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
                            :|world| (item-world item))
          :|isActive| (item-equipped-p item)
          ;; DEPRECATED and may be removed in a future release
          :|equipType| (wear-slot-name
                        (find-record 'wear-slot
                                     :id (item-template-wear-slot item-template)))
          :|health| (item-energy item)
          :|healthType| (if (eql :uncountable (item-template-energy-kind item-template))
                            "continuous"
                            "discrete")
          :|rarity| FIXME
          :|facing| "S"
          :|color| (item-base-color item)
          :|itemType| (item-template-name item-template)
          :|title| (item-template-name item-template)
          :|itemID| (item-template-id item-template)
          :|slot| (item-uuid item)
          :|inRoom| "@Tootsville"
          :|x| (item-x item)
          :|y| (item-y item)
          :|z| (item-z item))))

(defun store-item-info (store-item)
  "Describes a store item in JSON-compatible format.

@table @code
@item id
The unique ID for this instance of this item at this store.
@item title
The title of the item; see `ITEM-TEMPLATE-NAME'.
@item price
The price to purchase an instance of this item. See `STORE-ITEM-PRICE'.
@item currency
The display name of the type of currency in which the price has been specified.
Typically one of ``Tootsville Magic Peanuts'' or ``Fairy Dust''.
See `CURRENCY-NAME' for `STORE-ITEM-CURRENCY'
@item currCode
The ISO-4217 currency code for @code{currency}. See
  `STORE-ITEM-CURRENCY'.
For Tootsville, we use @code{X-TVPN} for Tootsville Magic Peanuts, or
@code{X-FADU} for Fairy Dust.
@item currSym
The display symbol for the currency. Tootsville Magic Peanuts use a peanut 
emoji, while Fairy Dust uses an asterism. See `CURRENCY-SYMBOL'
for `STORE-ITEM-CURRENCY'.
@item qty
The quantity of this item available. If zero, this item is not available.
If an unlimited number of this item are available, this will be instead
the Infinity sign as a literal string. See `STORE-ITEM-QTY'
@item desc
A description about the item. See `ITEM-TEMPLATE-DESCRIPTION'.
@item template
Information about the item's template; see `ITEM-TEMPLATE-INFO'.
@end table

@subsection Changes from 1.2 to 2.0

@itemize
@item
Item templates are now distinct from store items.
@item
Added @code{template} and @code{qty}
@item
The Tootsville Magic Peanuts currency was previously
 ``Tootsville@sup{TM} Peanuts'' and had a currency symbol of
``þ'' (The Icelandic letter Thorn).
@item
Currency Codes are now uniformly ALL CAPS.
@end itemize
"
  (let ((currency (store-item-currency store-item))
        (item-template (find-reference store-item :item-template)))
    (list :|id| (store-item-id store-item)
          :|title| (item-template-name item-template)
          :|qty| (or (store-item-qty store-item) "∞")
          :|price| (store-item-price store-item)
          :|currency| (currency-name currency)
          :|currSym| (currency-symbol currency)
          :|currCode| (string currency)
          :|desc| (item-template-description item-template)
          :|template| (item-template-info item-template))))

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


