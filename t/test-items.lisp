;;;; -*- lisp -*-
;;;
;;;; t/test-items.lisp is part of Tootsville
;;;
;;;; Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2024 The
;;;; Corporation for Inter-World Tourism and Adventuring (CIWTA.org); © 2025
;;;; Interworldly Adventuring, LLC
;;;
;;;; This program is Free Software: you can redistribute it and/or
;;;; modify it under the terms of the GNU Affero General Public License
;;;; as published by the Free Software Foundation; either version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;; Affero General Public License for more details.
;;;
;;;; You should have received a copy of the GNU Affero General Public
;;;; License along with this program. If not, see
;;;; <https://www.gnu.org/licenses/>.

(in-package :Tootsville.test)

;;; Item system tests

(in-suite items-tests)

(test item-creation
  "Test item creation functionality"
  (with-test-database
    (let* ((item-data (list :name "Test Sword" :type "weapon" :damage 10))
           (item (Tootsville::create-item item-data)))
      (is (not (null item)) "Should create item")
      (assert-item-valid item)
      (is (equal "Test Sword" (getf item :name)) "Should have correct name")
      (is (equal "weapon" (getf item :type)) "Should have correct type"))))

(test item-validation
  "Test item data validation"
  (with-test-database
    (let ((invalid-item (list :name "" :type "invalid-type")))
      (signals validation-error (Tootsville::validate-item invalid-item) "Should signal validation error for invalid item")
      
      (let ((valid-item (list :name "Valid Item" :type "tool")))
        (is (Tootsville::validate-item valid-item) "Should validate correct item")))))

(test item-retrieval
  "Test item retrieval by ID"
  (with-test-database
    (let* ((item (make-test-item))
           (retrieved-item (Tootsville::get-item-by-id (getf item :uuid))))
      (is (not (null retrieved-item)) "Should retrieve item by ID")
      (is (uuid:uuid= (getf item :uuid) (getf retrieved-item :uuid)) "Should have matching UUID"))))

(test item-update
  "Test item update functionality"
  (with-test-database
    (let* ((item (make-test-item))
           (updated-item (copy-list item)))
      (setf (getf updated-item :name) "Updated Item Name")
      (let ((result (Tootsville::update-item updated-item)))
        (is (not (null result)) "Should update item")
        (is (equal "Updated Item Name" (getf result :name)) "Should have updated name")))))

(test item-deletion
  "Test item deletion"
  (with-test-database
    (let* ((item (make-test-item))
           (item-id (getf item :uuid)))
      (Tootsville::delete-item item-id)
      (is (null (Tootsville::get-item-by-id item-id)) "Should delete item"))))

(test item-search
  "Test item search functionality"
  (with-test-database
    (let* ((item1 (make-test-item :name "Sword of Power" :type "weapon"))
           (item2 (make-test-item :name "Shield of Protection" :type "armor"))
           (search-results (Tootsville::search-items "Sword")))
      (is (= 1 (length search-results)) "Should find matching items")
      (is (equal "Sword of Power" (getf (first search-results) :name)) "Should find correct item"))))

(test item-inventory-management
  "Test item inventory management"
  (with-test-database
    (let* ((user (make-test-user))
           (item (make-test-item))
           (inventory (Tootsville::get-user-inventory (getf user :uuid))))
      (Tootsville::add-item-to-inventory (getf user :uuid) (getf item :uuid))
      (let ((updated-inventory (Tootsville::get-user-inventory (getf user :uuid))))
        (is (= (1+ (length inventory)) (length updated-inventory)) "Should add item to inventory")))))

(test item-trading
  "Test item trading between users"
  (with-test-database
    (let* ((user1 (make-test-user))
           (user2 (make-test-user))
           (item (make-test-item)))
      (Tootsville::add-item-to-inventory (getf user1 :uuid) (getf item :uuid))
      (Tootsville::trade-item (getf user1 :uuid) (getf user2 :uuid) (getf item :uuid))
      
      (is (not (member (getf item :uuid) (Tootsville::get-user-inventory (getf user1 :uuid)) :key #'getf :test #'uuid:uuid=)) "Should remove item from user1")
      (is (member (getf item :uuid) (Tootsville::get-user-inventory (getf user2 :uuid)) :key #'getf :test #'uuid:uuid=) "Should add item to user2"))))

(test item-durability
  "Test item durability system"
  (with-test-database
    (let* ((item (make-test-item :type "weapon"))
           (initial-durability 100))
      (setf (getf item :durability) initial-durability)
      (Tootsville::use-item (getf item :uuid))
      (let ((used-item (Tootsville::get-item-by-id (getf item :uuid))))
        (is (< (getf used-item :durability) initial-durability) "Should reduce durability when used")))))

(test item-stacking
  "Test item stacking functionality"
  (with-test-database
    (let* ((stackable-item (make-test-item :type "consumable" :stackable t :max-stack 10))
           (stacked-items (Tootsville::stack-items stackable-item 5)))
      (is (= 5 (length stacked-items)) "Should create correct number of stacked items")
      (is (every (lambda (item) (getf item :stackable)) stacked-items) "Should maintain stackable property"))))

(test item-crafting
  "Test item crafting system"
  (with-test-database
    (let* ((recipe (list :name "Iron Sword" :ingredients (list (list :item "iron" :quantity 3) (list :item "wood" :quantity 1))))
           (crafted-item (Tootsville::craft-item recipe)))
      (is (not (null crafted-item)) "Should craft item from recipe")
      (is (equal "Iron Sword" (getf crafted-item :name)) "Should have correct crafted name"))))

(test item-enchantment
  "Test item enchantment system"
  (with-test-database
    (let* ((item (make-test-item :type "weapon"))
           (enchantment (list :type "fire" :power 5))
           (enchanted-item (Tootsville::enchant-item (getf item :uuid) enchantment)))
      (is (not (null enchanted-item)) "Should enchant item")
      (is (getf enchanted-item :enchantments) "Should have enchantments property"))))

(test item-error-handling
  "Test item error handling"
  (with-test-database
    (let ((invalid-item-id (uuid:make-v4-uuid)))
      (signals item-error (Tootsville::get-item-by-id invalid-item-id) "Should signal error for non-existent item")
      
      (let ((invalid-item-data (list :name "" :type "invalid")))
        (signals validation-error (Tootsville::create-item invalid-item-data) "Should signal validation error for invalid data")))))

(test item-performance
  "Test item operations performance"
  (with-test-database
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (make-test-item :name (format nil "PerfItem~a" i)))
      (let ((end-time (get-internal-real-time)))
        (is (< (- end-time start-time) 1000) "Should create 100 items within reasonable time")))))

(test item-concurrency
  "Test item operations under concurrent access"
  (with-test-database
    (let ((results (make-array 10 :initial-element nil)))
      (dotimes (i 10)
        (setf (aref results i) (make-test-item :name (format nil "ConcurrentItem~a" i))))
      (is (= 10 (length (remove nil results))) "Should handle concurrent item creation"))))

(test item-security
  "Test item security measures"
  (with-test-database
    (let* ((user1 (make-test-user))
           (user2 (make-test-user))
           (item (make-test-item)))
      (Tootsville::add-item-to-inventory (getf user1 :uuid) (getf item :uuid))
      
      ;; Attempt to access item without permission
      (signals security-error (Tootsville::get-item-by-id (getf item :uuid) :user-id (getf user2 :uuid)) "Should prevent unauthorized access"))))

(test item-audit-trail
  "Test item audit trail functionality"
  (with-test-database
    (let* ((item (make-test-item))
           (audit-entry (Tootsville::log-item-action (getf item :uuid) "create" (getf item :uuid))))
      (is (not (null audit-entry)) "Should create audit entry")
      (is (getf audit-entry :item-id) "Should include item ID in audit")
      (is (getf audit-entry :action) "Should include action in audit")
      (is (getf audit-entry :timestamp) "Should include timestamp in audit"))))

(test item-cleanup
  "Test item cleanup and maintenance"
  (with-test-database
    (let* ((item (make-test-item))
           (item-id (getf item :uuid)))
      (Tootsville::mark-item-for-cleanup item-id)
      (let ((cleanup-items (Tootsville::get-items-for-cleanup)))
        (is (member item-id cleanup-items :test #'uuid:uuid=) "Should mark item for cleanup")))))

(test item-export-import
  "Test item export and import functionality"
  (with-test-database
    (let* ((item (make-test-item))
           (exported-data (Tootsville::export-item (getf item :uuid)))
           (imported-item (Tootsville::import-item exported-data)))
      (is (not (null exported-data)) "Should export item data")
      (is (not (null imported-item)) "Should import item data")
      (is (equal (getf item :name) (getf imported-item :name)) "Should preserve item name during export/import"))))
