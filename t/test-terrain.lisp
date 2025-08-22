;;;; -*- lisp -*-
;;;
;;;; t/test-terrain.lisp is part of Tootsville
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

;;; Terrain system tests

(in-suite terrain-tests)

(test terrain-generation
  "Test terrain generation functionality"
  (with-test-database
    (let* ((world (make-test-world :type "CHOR"))
           (terrain (Tootsville::generate-terrain (getf world :uuid))))
      (is (not (null terrain)) "Should generate terrain")
      (is (getf terrain :world-id) "Should have world ID")
      (is (getf terrain :height-map) "Should have height map"))))

(test terrain-world-types
  "Test terrain generation for different world types"
  (with-test-database
    (let ((world-types '("CHOR" "MOON" "OTHM" "PINK")))
      (dolist (world-type world-types)
        (let* ((world (make-test-world :type world-type))
               (terrain (Tootsville::generate-terrain (getf world :uuid))))
          (is (not (null terrain)) (format nil "Should generate terrain for ~a" world-type))
          (is (equal world-type (getf terrain :world-type)) (format nil "Should have correct world type ~a" world-type)))))))

(test terrain-height-calculation
  "Test terrain height calculation"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid)))
           (coordinates (list :x 100 :y 200))
           (height (Tootsville::get-terrain-height terrain coordinates)))
      (is (numberp height) "Should return numeric height")
      (is (>= height 0) "Should return non-negative height"))))

(test terrain-feature-placement
  "Test terrain feature placement"
  (with-test-database
    (let* ((world (make-test-world :type "CHOR"))
           (terrain (Tootsville::generate-terrain (getf world :uuid)))
           (features (Tootsville::place-terrain-features terrain)))
      (is (listp features) "Should return list of features")
      (is (every #'listp features) "Should have valid feature data"))))

(test terrain-collision-detection
  "Test terrain collision detection"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid)))
           (position (list :x 50 :y 50 :z 10))
           (collision (Tootsville::check-terrain-collision terrain position)))
      (is (booleanp collision) "Should return boolean collision result"))))

(test terrain-pathfinding
  "Test terrain pathfinding"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid)))
           (start (list :x 0 :y 0))
           (end (list :x 100 :y 100))
           (path (Tootsville::find-terrain-path terrain start end)))
      (is (listp path) "Should return path as list")
      (is (>= (length path) 2) "Should have at least start and end points"))))

(test terrain-texture-mapping
  "Test terrain texture mapping"
  (with-test-database
    (let* ((world (make-test-world :type "CHOR"))
           (terrain (Tootsville::generate-terrain (getf world :uuid)))
           (textures (Tootsville::generate-terrain-textures terrain)))
      (is (listp textures) "Should return texture data")
      (is (every #'stringp (mapcar #'car textures)) "Should have texture names"))))

(test terrain-optimization
  "Test terrain optimization"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid)))
           (optimized (Tootsville::optimize-terrain terrain)))
      (is (not (null optimized)) "Should optimize terrain")
      (is (<= (getf optimized :polygon-count) (getf terrain :polygon-count)) "Should reduce polygon count"))))

(test terrain-caching
  "Test terrain caching functionality"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid))))
      (Tootsville::cache-terrain terrain)
      (let ((cached (Tootsville::get-cached-terrain (getf world :uuid))))
        (is (not (null cached)) "Should cache terrain")
        (is (equal (getf terrain :world-id) (getf cached :world-id)) "Should retrieve cached terrain")))))

(test terrain-error-handling
  "Test terrain error handling"
  (with-test-database
    (let ((invalid-world-id (uuid:make-v4-uuid)))
      (signals terrain-error (Tootsville::generate-terrain invalid-world-id) "Should signal error for invalid world")
      
      (let ((invalid-coordinates (list :x -1000 :y -1000)))
        (signals validation-error (Tootsville::validate-coordinates invalid-coordinates) "Should signal validation error for invalid coordinates")))))

(test terrain-performance
  "Test terrain generation performance"
  (with-test-database
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 10)
        (let ((world (make-test-world :type "CHOR")))
          (Tootsville::generate-terrain (getf world :uuid))))
      (let ((end-time (get-internal-real-time)))
        (is (< (- end-time start-time) 5000) "Should generate 10 terrains within reasonable time")))))

(test terrain-memory-management
  "Test terrain memory management"
  (with-test-database
    (let ((terrains (loop for i from 1 to 50 collect (make-test-world))))
      (dolist (world terrains)
        (Tootsville::generate-terrain (getf world :uuid)))
      (Tootsville::cleanup-unused-terrain)
      (is (<= (length (Tootsville::active-terrain-cache)) 50) "Should manage terrain memory properly"))))

(test terrain-serialization
  "Test terrain serialization"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid)))
           (serialized (Tootsville::serialize-terrain terrain))
           (deserialized (Tootsville::deserialize-terrain serialized)))
      (is (not (null serialized)) "Should serialize terrain")
      (is (not (null deserialized)) "Should deserialize terrain")
      (is (equal (getf terrain :world-id) (getf deserialized :world-id)) "Should preserve world ID"))))

(test terrain-versioning
  "Test terrain versioning system"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain-v1 (Tootsville::generate-terrain (getf world :uuid)))
           (terrain-v2 (Tootsville::update-terrain-version terrain-v1)))
      (is (not (null terrain-v2)) "Should create new terrain version")
      (is (> (getf terrain-v2 :version) (getf terrain-v1 :version)) "Should increment version number"))))

(test terrain-backup-restore
  "Test terrain backup and restore"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid))))
      (Tootsville::backup-terrain terrain)
      (let ((backup (Tootsville::get-terrain-backup (getf world :uuid))))
        (is (not (null backup)) "Should create terrain backup")
        (Tootsville::restore-terrain backup)
        (is (not (null (Tootsville::get-terrain-by-world (getf world :uuid)))) "Should restore terrain")))))

(test terrain-statistics
  "Test terrain statistics collection"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid)))
           (stats (Tootsville::collect-terrain-statistics terrain)))
      (is (listp stats) "Should collect statistics")
      (is (getf stats :area) "Should include area")
      (is (getf stats :elevation-range) "Should include elevation range")
      (is (getf stats :feature-count) "Should include feature count"))))

(test terrain-validation
  "Test terrain data validation"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid))))
      (is (Tootsville::validate-terrain terrain) "Should validate correct terrain")
      
      (let ((invalid-terrain (copy-list terrain)))
        (setf (getf invalid-terrain :height-map) nil)
        (is (not (Tootsville::validate-terrain invalid-terrain)) "Should reject invalid terrain")))))

(test terrain-export-import
  "Test terrain export and import"
  (with-test-database
    (let* ((world (make-test-world))
           (terrain (Tootsville::generate-terrain (getf world :uuid)))
           (exported (Tootsville::export-terrain terrain))
           (imported (Tootsville::import-terrain exported)))
      (is (not (null exported)) "Should export terrain")
      (is (not (null imported)) "Should import terrain")
      (is (equal (getf terrain :world-id) (getf imported :world-id)) "Should preserve world ID during export/import"))))
