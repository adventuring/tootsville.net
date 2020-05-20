;;;; -*- lisp -*-
;;;
;;;; src/items.lisp is part of Tootsville
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
  (let ((template (find-record 'item-template :id (item-template item))))
    (list :|uuid| (item-uuid item)
          :|baseColor| (item-base-color item)
          :|altColor| (item-alt-color item)
          :|template| (list :|id| (item-template-id template)
                            :|name| (item-template-name template)
                            :|description| (item-template-description template)
                            :|trade| (string-upcase (item-template-trade template))
                            :|avatar| (item-template-avatar template)
                            :|energyKind| (item-template-energy-kind template)
                            :|energyMax| (item-template-energy-max template)
                            :|onZero| (item-template-on-zero template)
                            :|gauge| (case (item-template-energy-kind template)
                                       (:nil :false)
                                       (otherwise t))
                            :|wearSlot| (item-template-wear-slot template)
                            :|weight| (item-template-weight template))
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
  (list :|id| (item-template-id template)
        :|name| (item-template-name template)
        :|description| (item-template-description template)
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
  (mapcar (lambda (i-i-i)
            (find-record 'item :uuid (inventory-item-item i-i-i))) 
          (find-records 'inventory-item :Toot (Toot-UUID Toot))))

(defun Toot-has-item-p (item-template-id &optional (Toot *Toot*))
  (member item-template-id (mapcar #'item-template (Toot-inventory Toot))))



(defun store-info (store-item)
  (list :|id| (store-item-id store-item)
        :|template| (item-template-info (find-reference store-item :template))
        :|qty| (store-item-qty store-item)
        :|price| (store-item-price store-item)
        :|currency| (store-item-currency store-item)))



(defun place-string (place)
  (format nil "~a:~a|~a|~a"
          (place-kind place)
          (place-shape place)
          (place-appearance place)
          (place-attributes place)))

(defun wear-slot-info (wear-slot)
  (list :|id| (wear-slot-id wear-slot)
        :|name| (wear-slot-name wear-slot)
        :|alternate| (wear-slot-alternate wear-slot)
        :|avatarPoint| (wear-slot-avatar-point wear-slot)
        :|valence| (wear-slot-valence wear-slot)
        :|obstruct| (list :|point| (wear-slot-obstruct-point wear-slot)
                          :|min| (wear-slot-obstruct-min wear-slot)
                          :|max| (wear-slot-obstruct-max wear-slot))))

(defun place-string-circle (radius x-center z-center segments)
  (format nil "~{~{~8,6f,0,~8,6f~}~^~~~}" 
          (loop for i from 1.0 below segments 
             collecting (list
                         (+ x-center (* radius (cos (* 2 pi (/ i segments)))))
                         (+ z-center (* radius (sin (* 2 pi (/ i segments)))))))))
