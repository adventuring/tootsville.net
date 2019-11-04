;;;; -*- lisp -*-
;;;
;;;; src/toots.lisp is part of Tootsville
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

(defvar *Toot* nil
  "The Toot that the active user, is currectly using.")

;;; Toot character data.

(defun find-Toot-by-name (Toot-name)
  (check-type Toot-name Toot-name)
  (find-record 'Toot :name Toot-name))


(defun Toot-childp (Toot)
  (player-childp (find-reference Toot :player)))

(defun Toot-item-info (inv)
  (let* ((item (find-reference inv :item))
         (template (find-reference item :template)))
    (list :|equipped| (inventory-item-equipped inv)
          :|uuid| (item-uuid item)
          :|baseColor| (item-base-color item)
          :|energy| (item-energy item)
          :|template| (item-template-id template)
          :|name| (item-template-name template)
          :|defaultBaseColor| (item-template-default-base-color template)
          :|avatar| (item-template-avatar template)
          :|energyKind| (item-template-energy-kind template)
          :|onZero| (item-template-on-zero template)
          :|wearSlot| (item-template-wear-slot template)
          :|weight| (item-template-weight template))))

(defun inventory-item-equipped-p (item)
  "Is the inventory item equipped at all?"
  (ecase (inventory-item-equipped item)
    ((:Y :A) t)
    (:N nil)))

(defun Toot-inventory (Toot &optional privatep)
  (remove-if (lambda (item)
               (and privatep
                    (not (inventory-item-equipped-p item))))
             (find-records 'inventory-item :Toot (Toot-uuid Toot))))

(defun Toot-info (Toot &optional privatep)
  (list :|name| (Toot-name Toot)
        :|uuid| (Toot-UUID Toot)
        :|note| (or (Toot-note Toot) "")
        :|avatar| (avatar-moniker (find-reference Toot :avatar))
        :|baseColor| (color24-name (Toot-base-color Toot))
        :|pattern| (string-downcase (pattern-name
                                     (find-reference Toot :pattern)))
        :|patternColor| (color24-name (Toot-pattern-color Toot))
        :|padColor| (color24-name (Toot-pad-color Toot))
        :|childP| (if (Toot-childp Toot) :true :false)
        :|sensitiveP| (if (or (Toot-childp Toot)
                              (person-sensitivep
                               (find-reference Toot :player)))
                          :true :false)
        :|lastSeen| (Toot-last-active Toot)
        :|equip| (apply #'vector
                        (mapcar #'Toot-item-info
                                (Toot-inventory Toot privatep)))))

(defun find-active-Toot-for-user (&optional (user *user*))
  (when user
    (when-let (record (ignore-not-found (find-record 'player-Toot :player (person-uuid user))))
      (values (ignore-not-found (find-record 'Toot :uuid (player-Toot-Toot record)))
              record))))

(defun link-active-Toot-to-user (Toot &optional (user *user*))
  (multiple-value-bind (old-Toot player-Toot) (find-active-Toot-for-user user)
    (when old-Toot
      (unless (Toot= Toot old-Toot)
        (destroy-record player-Toot))))
  (make-record 'player-Toot :player (person-uuid user) :Toot (Toot-uuid Toot)))

(defun every-Toot-name ()
  (sort (mapcar #'Toot-name (find-records 'Toot)) #'string-lessp)) 
