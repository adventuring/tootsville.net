;;;; -*- lisp -*-
;;;
;;;; t/test-utils.lisp is part of Tootsville
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

;;; Utility function tests

(in-suite utils-tests)

(test string-utilities
  "Test string utility functions"
  (is (equal "HELLO" (Tootsville::string-upcase "hello")) "Should convert to uppercase")
  (is (equal "hello" (Tootsville::string-downcase "HELLO")) "Should convert to lowercase")
  (is (equal "Hello World" (Tootsville::string-capitalize "hello world")) "Should capitalize string")
  (is (equal "test" (Tootsville::string-trim "  test  ")) "Should trim whitespace"))

(test number-utilities
  "Test number utility functions"
  (is (= 10 (Tootsville::clamp 5 10 20)) "Should clamp minimum value")
  (is (= 20 (Tootsville::clamp 25 10 20)) "Should clamp maximum value")
  (is (= 15 (Tootsville::clamp 15 10 20)) "Should not clamp value in range")
  (is (= 5.5 (Tootsville::round-to 5.47 1)) "Should round to specified decimal places"))

(test list-utilities
  "Test list utility functions"
  (let ((test-list '(1 2 3 4 5)))
    (is (equal '(5 4 3 2 1) (Tootsville::reverse-list test-list)) "Should reverse list")
    (is (= 3 (Tootsville::list-length test-list)) "Should get list length")
    (is (equal '(2 4) (Tootsville::filter-even test-list)) "Should filter even numbers")))

(test hash-table-utilities
  "Test hash table utility functions"
  (let ((test-hash (make-hash-table :test 'equal)))
    (setf (gethash "key1" test-hash) "value1")
    (setf (gethash "key2" test-hash) "value2")
    (is (equal "value1" (Tootsville::safe-gethash "key1" test-hash)) "Should safely get hash value")
    (is (null (Tootsville::safe-gethash "nonexistent" test-hash)) "Should return nil for missing key")))

(test date-time-utilities
  "Test date and time utility functions"
  (let ((current-time (get-universal-time)))
    (is (numberp (Tootsville::timestamp-to-universal current-time)) "Should convert timestamp")
    (is (stringp (Tootsville::format-timestamp current-time)) "Should format timestamp")
    (is (numberp (Tootsville::time-difference current-time (+ current-time 3600))) "Should calculate time difference")))

(test uuid-utilities
  "Test UUID utility functions"
  (let ((uuid1 (uuid:make-v4-uuid))
        (uuid2 (uuid:make-v4-uuid)))
    (is (uuid:uuid-p uuid1) "Should create valid UUID")
    (is (stringp (Tootsville::uuid-to-string uuid1)) "Should convert UUID to string")
    (is (uuid:uuid-p (Tootsville::string-to-uuid (Tootsville::uuid-to-string uuid1))) "Should convert string back to UUID")
    (is (uuid:uuid= uuid1 uuid1) "Should compare UUIDs correctly")
    (is (not (uuid:uuid= uuid1 uuid2)) "Should detect different UUIDs")))

(test json-utilities
  "Test JSON utility functions"
  (let ((test-data (list :name "test" :value 42 :nested (list :key "value"))))
    (is (stringp (Tootsville::to-json test-data)) "Should convert to JSON")
    (let ((parsed (Tootsville::from-json (Tootsville::to-json test-data))))
      (is (equal (getf test-data :name) (getf parsed :name)) "Should preserve data through JSON conversion"))))

(test validation-utilities
  "Test validation utility functions"
  (is (Tootsville::valid-email-p "test@example.com") "Should validate correct email")
  (is (not (Tootsville::valid-email-p "invalid-email")) "Should reject invalid email")
  (is (Tootsville::valid-url-p "https://example.com") "Should validate correct URL")
  (is (not (Tootsville::valid-url-p "not-a-url")) "Should reject invalid URL"))

(test encryption-utilities
  "Test encryption utility functions"
  (let ((test-data "secret message")
        (key "test-key"))
    (let ((encrypted (Tootsville::encrypt-data test-data key)))
      (is (stringp encrypted) "Should encrypt data")
      (let ((decrypted (Tootsville::decrypt-data encrypted key)))
        (is (equal test-data decrypted) "Should decrypt data correctly")))))

(test compression-utilities
  "Test compression utility functions"
  (let ((test-data "This is a test string that should be compressed"))
    (let ((compressed (Tootsville::compress-data test-data)))
      (is (stringp compressed) "Should compress data")
      (let ((decompressed (Tootsville::decompress-data compressed)))
        (is (equal test-data decompressed) "Should decompress data correctly")))))

(test file-utilities
  "Test file utility functions"
  (let ((test-path "/tmp/test-file.txt"))
    (Tootsville::write-file test-path "test content")
    (is (Tootsville::file-exists-p test-path) "Should create file")
    (is (equal "test content" (Tootsville::read-file test-path)) "Should read file content")
    (Tootsville::delete-file test-path)
    (is (not (Tootsville::file-exists-p test-path)) "Should delete file")))

(test network-utilities
  "Test network utility functions"
  (is (stringp (Tootsville::get-local-ip)) "Should get local IP address")
  (is (numberp (Tootsville::ping-host "localhost")) "Should ping localhost")
  (is (Tootsville::port-available-p 8080) "Should check port availability"))

(test logging-utilities
  "Test logging utility functions"
  (Tootsville::log-message :info "Test info message")
  (Tootsville::log-message :error "Test error message")
  (Tootsville::log-message :debug "Test debug message")
  (is (Tootsville::log-level-enabled-p :info) "Should enable info logging")
  (is (Tootsville::log-level-enabled-p :error) "Should enable error logging"))

(test configuration-utilities
  "Test configuration utility functions"
  (let ((config (list :database "test.db" :port 8080 :debug t)))
    (Tootsville::save-config config "test-config.json")
    (let ((loaded-config (Tootsville::load-config "test-config.json")))
      (is (equal (getf config :database) (getf loaded-config :database)) "Should load configuration correctly"))
    (Tootsville::delete-file "test-config.json")))

(test cache-utilities
  "Test cache utility functions"
  (let ((cache (Tootsville::create-cache)))
    (Tootsville::cache-set cache "key1" "value1" 60)
    (is (equal "value1" (Tootsville::cache-get cache "key1")) "Should get cached value")
    (Tootsville::cache-delete cache "key1")
    (is (null (Tootsville::cache-get cache "key1")) "Should delete cached value")))

(test error-utilities
  "Test error utility functions"
  (let ((error-condition (make-condition 'error :message "Test error")))
    (is (stringp (Tootsville::format-error error-condition)) "Should format error")
    (is (listp (Tootsville::get-error-stack-trace error-condition)) "Should get stack trace")
    (is (Tootsville::should-retry-p error-condition) "Should determine if retry is needed")))

(test performance-utilities
  "Test performance utility functions"
  (let ((start-time (get-internal-real-time)))
    (Tootsville::sleep-ms 10)
    (let ((end-time (get-internal-real-time)))
      (is (>= (- end-time start-time) 0.01) "Should sleep for specified milliseconds")))
  
  (let ((result (Tootsville::measure-time (lambda () (sleep 0.01)))))
    (is (numberp result) "Should measure execution time")
    (is (>= result 0.01) "Should return accurate timing")))

(test security-utilities
  "Test security utility functions"
  (let ((password "testpassword"))
    (let ((hashed (Tootsville::hash-password password)))
      (is (stringp hashed) "Should hash password")
      (is (Tootsville::verify-password password hashed) "Should verify password correctly")
      (is (not (Tootsville::verify-password "wrongpassword" hashed)) "Should reject wrong password")))
  
  (let ((token (Tootsville::generate-token)))
    (is (stringp token) "Should generate token")
    (is (Tootsville::validate-token token) "Should validate token")))

(test math-utilities
  "Test mathematical utility functions"
  (is (= 15 (Tootsville::calculate-distance '(0 0) '(3 4))) "Should calculate distance")
  (is (= 90 (Tootsville::calculate-angle '(0 0) '(0 1))) "Should calculate angle")
  (is (Tootsville::point-in-bounds-p '(5 5) '(0 0) '(10 10)) "Should check if point is in bounds")
  (is (not (Tootsville::point-in-bounds-p '(15 15) '(0 0) '(10 10))) "Should detect point outside bounds"))

(test random-utilities
  "Test random number utility functions"
  (let ((random-int (Tootsville::random-int 1 100)))
    (is (and (>= random-int 1) (<= random-int 100)) "Should generate random integer in range"))
  
  (let ((random-float (Tootsville::random-float 0.0 1.0)))
    (is (and (>= random-float 0.0) (<= random-float 1.0)) "Should generate random float in range"))
  
  (let ((random-choice (Tootsville::random-choice '(a b c))))
    (is (member random-choice '(a b c)) "Should choose random element from list")))

(test validation-error-handling
  "Test validation error handling"
  (signals validation-error (Tootsville::validate-required-field nil "field") "Should signal error for nil field")
  (signals validation-error (Tootsville::validate-required-field "" "field") "Should signal error for empty field")
  (is (Tootsville::validate-required-field "valid" "field") "Should validate non-empty field"))

(test utility-performance
  "Test utility function performance"
  (let ((start-time (get-internal-real-time)))
    (dotimes (i 1000)
      (Tootsville::uuid-to-string (uuid:make-v4-uuid)))
    (let ((end-time (get-internal-real-time)))
      (is (< (- end-time start-time) 1000) "Should convert 1000 UUIDs within reasonable time"))))

(test utility-error-recovery
  "Test utility function error recovery"
  (let ((result (Tootsville::safe-execute (lambda () (error "Test error")))))
    (is (null result) "Should handle errors gracefully"))
  
  (let ((result (Tootsville::safe-execute (lambda () "success"))))
    (is (equal "success" result) "Should return successful result")))


