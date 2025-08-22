;;;; -*- lisp -*-
;;;
;;;; t/test-world.lisp is part of Tootsville
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

;;; World management tests

(in-suite world-tests)

(test world-creation
  "Test world creation functionality"
  (with-test-database
    (let* ((world-data (list :name "Test World" :type "CHOR" :gravity 9.81))
           (world (Tootsville::create-world world-data)))
      (is (not (null world)) "Should create world")
      (assert-world-valid world)
      (is (equal "Test World" (getf world :name)) "Should have correct name")
      (is (equal "CHOR" (getf world :type)) "Should have correct type"))))

(test world-types
  "Test different world types"
  (with-test-database
    (let ((world-types '("CHOR" "MOON" "OTHM" "PINK")))
      (dolist (world-type world-types)
        (let* ((world-data (list :name (format nil "~a World" world-type) :type world-type))
               (world (Tootsville::create-world world-data)))
          (is (not (null world)) (format nil "Should create ~a world" world-type))
          (is (equal world-type (getf world :type)) (format nil "Should have correct type ~a" world-type)))))))

(test world-retrieval
  "Test world retrieval by ID"
  (with-test-database
    (let* ((world (make-test-world))
           (retrieved-world (Tootsville::get-world-by-id (getf world :uuid))))
      (is (not (null retrieved-world)) "Should retrieve world by ID")
      (is (uuid:uuid= (getf world :uuid) (getf retrieved-world :uuid)) "Should have matching UUID"))))

(test world-update
  "Test world update functionality"
  (with-test-database
    (let* ((world (make-test-world))
           (updated-world (copy-list world)))
      (setf (getf updated-world :name) "Updated World Name")
      (let ((result (Tootsville::update-world updated-world)))
        (is (not (null result)) "Should update world")
        (is (equal "Updated World Name" (getf result :name)) "Should have updated name")))))

(test world-deletion
  "Test world deletion"
  (with-test-database
    (let* ((world (make-test-world))
           (world-id (getf world :uuid)))
      (Tootsville::delete-world world-id)
      (is (null (Tootsville::get-world-by-id world-id)) "Should delete world"))))

(test world-player-management
  "Test world player management"
  (with-test-database
    (let* ((world (make-test-world))
           (user (make-test-user)))
      (Tootsville::add-player-to-world (getf world :uuid) (getf user :uuid))
      (let ((players (Tootsville::get-world-players (getf world :uuid))))
        (is (member (getf user :uuid) players :test #'uuid:uuid=) "Should add player to world"))
      
      (Tootsville::remove-player-from-world (getf world :uuid) (getf user :uuid))
      (let ((players (Tootsville::get-world-players (getf world :uuid))))
        (is (not (member (getf user :uuid) players :test #'uuid:uuid=)) "Should remove player from world")))))

(test world-physics
  "Test world physics settings"
  (with-test-database
    (let* ((world (make-test-world :type "MOON"))
           (physics (Tootsville::get-world-physics (getf world :uuid))))
      (is (not (null physics)) "Should get world physics")
      (is (getf physics :gravity) "Should have gravity setting")
      (is (getf physics :atmosphere) "Should have atmosphere setting"))))

(test world-weather
  "Test world weather system"
  (with-test-database
    (let* ((world (make-test-world :type "CHOR"))
           (weather (Tootsville::get-world-weather (getf world :uuid))))
      (is (not (null weather)) "Should get world weather")
      (is (getf weather :current) "Should have current weather")
      (is (getf weather :forecast) "Should have weather forecast"))))

(test world-time
  "Test world time system"
  (with-test-database
    (let* ((world (make-test-world))
           (time (Tootsville::get-world-time (getf world :uuid))))
      (is (not (null time)) "Should get world time")
      (is (getf time :current) "Should have current time")
      (is (getf time :day-night-cycle) "Should have day/night cycle"))))

(test world-bounds
  "Test world boundary system"
  (with-test-database
    (let* ((world (make-test-world))
           (bounds (Tootsville::get-world-bounds (getf world :uuid))))
      (is (not (null bounds)) "Should get world bounds")
      (is (getf bounds :min-x) "Should have minimum X")
      (is (getf bounds :max-x) "Should have maximum X")
      (is (getf bounds :min-y) "Should have minimum Y")
      (is (getf bounds :max-y) "Should have maximum Y"))))

(test world-portals
  "Test world portal system"
  (with-test-database
    (let* ((world1 (make-test-world :type "CHOR"))
           (world2 (make-test-world :type "MOON"))
           (portal (Tootsville::create-portal (getf world1 :uuid) (getf world2 :uuid))))
      (is (not (null portal)) "Should create portal")
      (is (equal (getf world1 :uuid) (getf portal :source-world)) "Should have correct source world")
      (is (equal (getf world2 :uuid) (getf portal :destination-world)) "Should have correct destination world"))))

(test world-error-handling
  "Test world error handling"
  (with-test-database
    (let ((invalid-world-id (uuid:make-v4-uuid)))
      (signals world-error (Tootsville::get-world-by-id invalid-world-id) "Should signal error for non-existent world")
      
      (let ((invalid-world-data (list :name "" :type "INVALID")))
        (signals validation-error (Tootsville::create-world invalid-world-data) "Should signal validation error for invalid data")))))

(test world-performance
  "Test world operations performance"
  (with-test-database
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 10)
        (make-test-world :name (format nil "PerfWorld~a" i)))
      (let ((end-time (get-internal-real-time)))
        (is (< (- end-time start-time) 1000) "Should create 10 worlds within reasonable time")))))

(test world-concurrency
  "Test world operations under concurrent access"
  (with-test-database
    (let ((results (make-array 10 :initial-element nil)))
      (dotimes (i 10)
        (setf (aref results i) (make-test-world :name (format nil "ConcurrentWorld~a" i))))
      (is (= 10 (length (remove nil results))) "Should handle concurrent world creation"))))

(test world-security
  "Test world security measures"
  (with-test-database
    (let* ((world (make-test-world))
           (user1 (make-test-user))
           (user2 (make-test-user)))
      (Tootsville::add-player-to-world (getf world :uuid) (getf user1 :uuid))
      
      ;; Attempt to access world without permission
      (signals security-error (Tootsville::get-world-players (getf world :uuid) :user-id (getf user2 :uuid)) "Should prevent unauthorized access"))))

(test world-audit-trail
  "Test world audit trail functionality"
  (with-test-database
    (let* ((world (make-test-world))
           (audit-entry (Tootsville::log-world-action (getf world :uuid) "create" (getf world :uuid))))
      (is (not (null audit-entry)) "Should create audit entry")
      (is (getf audit-entry :world-id) "Should include world ID in audit")
      (is (getf audit-entry :action) "Should include action in audit")
      (is (getf audit-entry :timestamp) "Should include timestamp in audit"))))

(test world-backup-restore
  "Test world backup and restore"
  (with-test-database
    (let* ((world (make-test-world))
           (world-id (getf world :uuid)))
      (Tootsville::backup-world world-id)
      (let ((backup (Tootsville::get-world-backup world-id)))
        (is (not (null backup)) "Should create world backup")
        (Tootsville::restore-world backup)
        (is (not (null (Tootsville::get-world-by-id world-id))) "Should restore world")))))

(test world-statistics
  "Test world statistics collection"
  (with-test-database
    (let* ((world (make-test-world))
           (stats (Tootsville::collect-world-statistics (getf world :uuid))))
      (is (listp stats) "Should collect statistics")
      (is (getf stats :player-count) "Should include player count")
      (is (getf stats :item-count) "Should include item count")
      (is (getf stats :uptime) "Should include uptime"))))

(test world-validation
  "Test world data validation"
  (with-test-database
    (let* ((world (make-test-world)))
      (is (Tootsville::validate-world world) "Should validate correct world")
      
      (let ((invalid-world (copy-list world)))
        (setf (getf invalid-world :name) "")
        (is (not (Tootsville::validate-world invalid-world)) "Should reject invalid world")))))

(test world-export-import
  "Test world export and import"
  (with-test-database
    (let* ((world (make-test-world))
           (exported (Tootsville::export-world (getf world :uuid)))
           (imported (Tootsville::import-world exported)))
      (is (not (null exported)) "Should export world")
      (is (not (null imported)) "Should import world")
      (is (equal (getf world :name) (getf imported :name)) "Should preserve world name during export/import"))))


