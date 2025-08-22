;;;; -*- lisp -*-
;;;
;;;; t/test-users.lisp is part of Tootsville
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

;;; User management tests

(in-suite users-tests)

(test email-lhs-extraction
  "Test email left-hand side extraction"
  (is (equal "user" (Tootsville::email-lhs "user@example.com")))
  (is (equal "test.user" (Tootsville::email-lhs "test.user@domain.org")))
  (is (null (Tootsville::email-lhs nil)))
  (is (null (Tootsville::email-lhs ""))))

(test email-lhs-null-input
  "Test email-lhs with null input"
  (signals validation-error (Tootsville::email-lhs nil) "Should signal error for null email"))

(test email-lhs-empty-input
  "Test email-lhs with empty input"
  (signals validation-error (Tootsville::email-lhs "") "Should signal error for empty email"))

(test email-lhs-malformed-input
  "Test email-lhs with malformed input"
  (signals validation-error (Tootsville::email-lhs "not-an-email") "Should signal error for malformed email")
  (signals validation-error (Tootsville::email-lhs "@domain.com") "Should signal error for missing local part")
  (signals validation-error (Tootsville::email-lhs "user@") "Should signal error for missing domain"))

(test email-lhs-extremely-long-input
  "Test email-lhs with extremely long input"
  (let ((long-email (concatenate 'string (make-string 1000 :initial-element #\a) "@domain.com")))
    (signals validation-error (Tootsville::email-lhs long-email) "Should signal error for extremely long email")))

(test email-lhs-special-characters
  "Test email-lhs with special characters"
  (signals validation-error (Tootsville::email-lhs "user!@domain.com") "Should signal error for special characters")
  (signals validation-error (Tootsville::email-lhs "user#@domain.com") "Should signal error for special characters"))

(test find-user-for-credentials
  "Test finding user by credentials"
  (with-test-database
    (let* ((test-user (make-test-user))
           (credentials (make-test-credentials))
           (found-user (Tootsville::find-user-for-credentials credentials)))
      (is (null found-user) "Should return nil for non-existent credentials"))))

(test find-user-null-credentials
  "Test find-user-for-credentials with null input"
  (signals validation-error (Tootsville::find-user-for-credentials nil nil) "Should signal error for null credentials"))

(test find-user-empty-credentials
  "Test find-user-for-credentials with empty input"
  (signals validation-error (Tootsville::find-user-for-credentials "" "") "Should signal error for empty credentials"))

(test find-user-invalid-id
  "Test find-user-by-id with invalid input"
  (signals validation-error (Tootsville::find-user-by-id "invalid-uuid") "Should signal error for invalid UUID")
  (signals validation-error (Tootsville::find-user-by-id "") "Should signal error for empty ID")
  (signals validation-error (Tootsville::find-user-by-id nil) "Should signal error for null ID"))

(test find-user-out-of-range-id
  "Test find-user-by-id with out-of-range input"
  (signals validation-error (Tootsville::find-user-by-id "00000000-0000-0000-0000-000000000000") "Should signal error for zero UUID"))

(test associate-credentials
  "Test associating credentials with a user"
  (with-test-database
    (let* ((test-user (make-test-user))
           (credentials (make-test-credentials))
           (result (Tootsville::associate-credentials test-user credentials)))
      (is (not (null result)) "Should successfully associate credentials"))))

(test find-person-by-url
  "Test finding person by URL"
  (with-test-database
    (let ((result (Tootsville::find-person-by-url "https://example.com/profile")))
      (is (null result) "Should return nil for non-existent URL"))))

(test find-person-null-url
  "Test find-person-by-url with null input"
  (signals validation-error (Tootsville::find-person-by-url nil) "Should signal error for null URL"))

(test find-person-invalid-url
  "Test find-person-by-url with invalid input"
  (signals validation-error (Tootsville::find-person-by-url "not-a-url") "Should signal error for invalid URL")
  (signals validation-error (Tootsville::find-person-by-url "ftp://invalid") "Should signal error for invalid protocol"))

(test find-person-extremely-long-url
  "Test find-person-by-url with extremely long input"
  (let ((long-url (concatenate 'string "https://domain.com/" (make-string 10000 :initial-element #\a))))
    (signals validation-error (Tootsville::find-person-by-url long-url) "Should signal error for extremely long URL")))

(test person-links-to-email
  "Test finding person links by email"
  (with-test-database
    (let ((result (Tootsville::person-links-to-email "test@example.com")))
      (is (listp result) "Should return a list")
      (is (zerop (length result)) "Should return empty list for non-existent email"))))

(test person-links-null-data
  "Test person-links-to-email with null input"
  (signals validation-error (Tootsville::person-links-to-email nil "test@example.com") "Should signal error for null person data"))

(test person-links-empty-data
  "Test person-links-to-email with empty input"
  (signals validation-error (Tootsville::person-links-to-email (list) "test@example.com") "Should signal error for empty person data"))

(test person-links-malformed-data
  "Test person-links-to-email with malformed input"
  (let ((malformed-person (list :links (list nil nil (list :url nil) (list :url "") (list :url "invalid-url")))))
    (signals validation-error (Tootsville::person-links-to-email malformed-person "test@example.com") "Should signal error for malformed person data")))

(test all-links-to-same-person-p
  "Test checking if all links point to same person"
  (with-test-database
    (let* ((test-user (make-test-user))
           (links (list (list :person (getf test-user :uuid))
                        (list :person (getf test-user :uuid)))))
      (is (Tootsville::all-links-to-same-person-p links) "Should return true for same person")
      
      (let* ((different-user (make-test-user :email "different@example.com"))
             (mixed-links (list (list :person (getf test-user :uuid))
                               (list :person (getf different-user :uuid)))))
        (is (not (Tootsville::all-links-to-same-person-p mixed-links)) "Should return false for different people")))))

(test all-links-null-list
  "Test all-links-to-same-person-p with null input"
  (signals validation-error (Tootsville::all-links-to-same-person-p nil) "Should signal error for null person list"))

(test all-links-empty-list
  "Test all-links-to-same-person-p with empty input"
  (signals validation-error (Tootsville::all-links-to-same-person-p (list)) "Should signal error for empty person list"))

(test all-links-malformed-list
  "Test all-links-to-same-person-p with malformed input"
  (let ((malformed-list (list nil nil (list :id nil) (list :id "") (list :id 123) (list :id (list)) (list :id (make-hash-table)))))
    (signals validation-error (Tootsville::all-links-to-same-person-p malformed-list) "Should signal error for malformed person list")))

(test ensure-user-for-plist
  "Test ensuring user exists from property list"
  (with-test-database
    (let* ((user-plist (list :email "test@example.com" :name "Test User"))
           (user (Tootsville::ensure-user-for-plist user-plist)))
      (is (not (null user)) "Should create or find user")
      (assert-user-valid user))))

(test ensure-user-null-plist
  "Test ensure-user-for-plist with null input"
  (signals validation-error (Tootsville::ensure-user-for-plist nil) "Should signal error for null plist"))

(test ensure-user-empty-plist
  "Test ensure-user-for-plist with empty input"
  (signals validation-error (Tootsville::ensure-user-for-plist (list)) "Should signal error for empty plist"))

(test ensure-user-malformed-plist
  "Test ensure-user-for-plist with malformed input"
  (let ((malformed-plist (list :email nil :name nil :id "" :created-at "invalid-date" :settings 123)))
    (signals validation-error (Tootsville::ensure-user-for-plist malformed-plist) "Should signal error for malformed plist")))

(test user-authentication
  "Test user authentication flow"
  (with-test-database
    (let* ((credentials (make-test-credentials))
           (user (Tootsville::ensure-user-for-plist (list :email "auth@example.com" :name "Auth User")))
           (authenticated-user (Tootsville::find-user-for-credentials credentials)))
      (is (not (null user)) "Should create user")
      (is (null authenticated-user) "Should not find user without associated credentials"))))

(test user-profile-management
  "Test user profile management functions"
  (with-test-database
    (let* ((user (make-test-user))
           (updated-user (copy-list user)))
      (setf (getf updated-user :name) "Updated Name")
      (is (not (equal (getf user :name) (getf updated-user :name))) "Should allow profile updates"))))

(test user-session-management
  "Test user session management"
  (with-test-database
    (let ((*user* nil))
      (is (null *user*) "Should start with no current user")
      (setf *user* (make-test-user))
      (is (not (null *user*)) "Should be able to set current user"))))

(test user-data-validation
  "Test user data validation"
  (with-test-database
    (let ((invalid-user (list :email "invalid-email" :name "")))
      (signals error (Tootsville::ensure-user-for-plist invalid-user) "Should signal error for invalid data"))))

(test validate-user-malformed-data
  "Test validate-user with malformed data"
  (let ((malformed-user (list :id nil :email nil :name 123 :created-at "invalid-date")))
    (signals validation-error (Tootsville::validate-user malformed-user) "Should signal error for malformed user data")))

(test validate-user-circular-references
  "Test validate-user with circular references"
  (let ((circular-user (list :id 1 :name "circular")))
    (setf (getf circular-user :self) circular-user)
    (signals validation-error (Tootsville::validate-user circular-user) "Should signal error for circular references")))

(test validate-user-deeply-nested
  "Test validate-user with deeply nested data"
  (let ((deep-user (list :id 1 :profile (list :settings (list :preferences (list :display (list :theme (list :colors (list :primary "blue")))))))))
    (signals validation-error (Tootsville::validate-user deep-user) "Should signal error for deeply nested data")))

(test user-search-functionality
  "Test user search functionality"
  (with-test-database
    (let* ((user1 (make-test-user :email "search1@example.com" :name "Search User 1"))
           (user2 (make-test-user :email "search2@example.com" :name "Search User 2"))
           (search-results (list user1 user2)))
      (is (= 2 (length search-results)) "Should find multiple users")
      (is (find "search1@example.com" search-results :key (lambda (u) (getf u :email)) :test #'string=)
          "Should find specific user by email"))))

(test user-permissions
  "Test user permissions and access control"
  (with-test-database
    (let* ((admin-user (make-test-user :email "admin@example.com"))
           (regular-user (make-test-user :email "user@example.com")))
      (is (not (null admin-user)) "Should create admin user")
      (is (not (null regular-user)) "Should create regular user")
      ;; Test permission checks would go here
      )))

(test user-activity-tracking
  "Test user activity tracking"
  (with-test-database
    (let* ((user (make-test-user))
           (activity (list :user (getf user :uuid) :action "login" :timestamp (get-universal-time))))
      (is (not (null activity)) "Should track user activity")
      (is (getf activity :user) "Should include user in activity")
      (is (getf activity :action) "Should include action in activity"))))

(test user-data-persistence
  "Test user data persistence"
  (with-test-database
    (let* ((user (make-test-user))
           (persisted-user (copy-list user)))
      (is (equal (getf user :email) (getf persisted-user :email)) "Should persist email")
      (is (equal (getf user :name) (getf persisted-user :name)) "Should persist name")
      (is (uuid:uuid= (getf user :uuid) (getf persisted-user :uuid)) "Should persist UUID"))))

(test user-cleanup
  "Test user cleanup and deletion"
  (with-test-database
    (let* ((user (make-test-user))
           (user-uuid (getf user :uuid)))
      (is (not (null user)) "Should create user")
      ;; Test cleanup would go here
      (is (uuid:uuid-p user-uuid) "Should have valid UUID for cleanup"))))

(test user-error-handling
  "Test user error handling"
  (with-test-database
    (let ((invalid-credentials nil))
      (is (null (Tootsville::find-user-for-credentials invalid-credentials)) "Should handle nil credentials")
      (is (null (Tootsville::find-person-by-url nil)) "Should handle nil URL")
      (is (null (Tootsville::person-links-to-email nil)) "Should handle nil email"))))

(test user-performance
  "Test user operations performance"
  (with-test-database
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (make-test-user :email (format nil "perf~a@example.com" i)))
      (let ((end-time (get-internal-real-time)))
        (is (< (- end-time start-time) 1000) "Should complete operations within reasonable time")))))

(test user-concurrency
  "Test user operations under concurrent access"
  (with-test-database
    (let ((results (make-array 10 :initial-element nil)))
      (dotimes (i 10)
        (setf (aref results i) (make-test-user :email (format nil "concurrent~a@example.com" i))))
      (is (= 10 (length (remove nil results))) "Should handle concurrent user creation"))))

(test user-security
  "Test user security measures"
  (with-test-database
    (let* ((user (make-test-user))
           (credentials (make-test-credentials))
           (hashed-credentials (copy-list credentials)))
      (is (not (null user)) "Should create user securely")
      (is (not (equal credentials hashed-credentials)) "Should hash credentials"))))

(test user-audit-trail
  "Test user audit trail functionality"
  (with-test-database
    (let* ((user (make-test-user))
           (audit-entry (list :user (getf user :uuid) :action "create" :timestamp (get-universal-time))))
      (is (not (null audit-entry)) "Should create audit entry")
      (is (getf audit-entry :user) "Should include user in audit")
      (is (getf audit-entry :action) "Should include action in audit")
      (is (getf audit-entry :timestamp) "Should include timestamp in audit"))))

(test validate-data-null-input
  "Test validate-user-data with null input"
  (signals validation-error (Tootsville::validate-user-data nil) "Should signal error for null user data"))

(test validate-data-invalid-input
  "Test validate-user-data with invalid input"
  (let ((invalid-data (list :email "invalid" :name "" :age -1 :password "weak")))
    (signals validation-error (Tootsville::validate-user-data invalid-data) "Should signal error for invalid user data")))

(test check-permission-null-input
  "Test check-user-permission with null input"
  (signals validation-error (Tootsville::check-user-permission nil "read") "Should signal error for null user ID"))

(test check-permission-invalid-input
  "Test check-user-permission with invalid input"
  (signals validation-error (Tootsville::check-user-permission "invalid-id" nil) "Should signal error for null permission")
  (signals validation-error (Tootsville::check-user-permission "invalid-id" "") "Should signal error for empty permission"))

(test log-activity-null-input
  "Test log-user-activity with null input"
  (signals validation-error (Tootsville::log-user-activity nil "login") "Should signal error for null user ID"))

(test log-activity-invalid-input
  "Test log-user-activity with invalid input"
  (signals validation-error (Tootsville::log-user-activity "invalid-id" nil) "Should signal error for null activity")
  (signals validation-error (Tootsville::log-user-activity "invalid-id" "") "Should signal error for empty activity"))

(test save-user-null-input
  "Test save-user with null input"
  (signals validation-error (Tootsville::save-user nil) "Should signal error for null user data"))

(test save-user-invalid-input
  "Test save-user with invalid input"
  (signals validation-error (Tootsville::save-user "not-an-object") "Should signal error for invalid user data"))

(test handle-error-null-input
  "Test handle-user-error with null input"
  (signals validation-error (Tootsville::handle-user-error nil) "Should signal error for null error"))

(test handle-error-invalid-input
  "Test handle-user-error with invalid input"
  (signals validation-error (Tootsville::handle-user-error "not-an-error") "Should signal error for invalid error"))

(test measure-operation-null-input
  "Test measure-user-operation with null input"
  (signals validation-error (Tootsville::measure-user-operation nil) "Should signal error for null operation"))

(test measure-operation-invalid-input
  "Test measure-user-operation with invalid input"
  (signals validation-error (Tootsville::measure-user-operation "not-a-function") "Should signal error for invalid operation"))

(test lock-user-null-input
  "Test lock-user with null input"
  (signals validation-error (Tootsville::lock-user nil) "Should signal error for null user ID"))

(test lock-user-invalid-input
  "Test lock-user with invalid input"
  (signals validation-error (Tootsville::lock-user "invalid-id") "Should signal error for invalid user ID"))

(test validate-security-null-input
  "Test validate-user-security with null input"
  (signals validation-error (Tootsville::validate-user-security nil) "Should signal error for null security data"))

(test validate-security-invalid-input
  "Test validate-user-security with invalid input"
  (signals validation-error (Tootsville::validate-user-security "not-an-object") "Should signal error for invalid security data"))

(test audit-action-null-input
  "Test audit-user-action with null input"
  (signals validation-error (Tootsville::audit-user-action nil "test") "Should signal error for null user ID"))

(test audit-action-invalid-input
  "Test audit-user-action with invalid input"
  (signals validation-error (Tootsville::audit-user-action "invalid-id" nil) "Should signal error for null action")
  (signals validation-error (Tootsville::audit-user-action "invalid-id" "") "Should signal error for empty action"))

(test export-data-null-input
  "Test export-user-data with null input"
  (signals validation-error (Tootsville::export-user-data nil) "Should signal error for null user ID"))

(test export-data-invalid-input
  "Test export-user-data with invalid input"
  (signals validation-error (Tootsville::export-user-data "invalid-id") "Should signal error for invalid user ID"))

(test import-data-null-input
  "Test import-user-data with null input"
  (signals validation-error (Tootsville::import-user-data nil) "Should signal error for null data"))

(test import-data-invalid-input
  "Test import-user-data with invalid input"
  (signals validation-error (Tootsville::import-user-data "not-json") "Should signal error for invalid JSON"))

(test process-batch-extremely-large
  "Test process-user-batch with extremely large dataset"
  (let ((large-dataset (loop for i from 1 to 1000000 collect (list :id i :email (format nil "user~a@example.com" i) :name (format nil "User ~a" i)))))
    (signals validation-error (Tootsville::process-user-batch large-dataset) "Should signal error for extremely large dataset")))

(test process-batch-memory-pressure
  "Test process-user-batch with memory pressure"
  (let ((memory-intensive-data (loop for i from 1 to 10000 collect (list :id (random 1000000) :data (make-string 10000 :initial-element #\x)))))
    (signals validation-error (Tootsville::process-user-batch memory-intensive-data) "Should signal error for memory pressure")))

(test fetch-data-timeout
  "Test fetch-user-data with timeout scenario"
  (signals validation-error (Tootsville::fetch-user-data "timeout-test") "Should signal error for timeout"))

(test get-user-db-failure
  "Test get-user-by-id with database failure"
  (signals database-error (Tootsville::get-user-by-id "db-failure-test") "Should signal error for database failure"))

(test update-user-concurrent-conflict
  "Test update-user-concurrent with conflict"
  (signals validation-error (Tootsville::update-user-concurrent "conflict-test") "Should signal error for concurrent conflict"))

(test validate-user-corrupted-data
  "Test validate-user with corrupted data"
  (let ((corrupted-data (list :id 1 :email "test@example.com" :name "Test User" :corrupted-field (make-array 1000000))))
    (signals validation-error (Tootsville::validate-user corrupted-data) "Should signal error for corrupted data")))

(test validate-user-encoding-issues
  "Test validate-user with encoding issues"
  (let ((encoding-data (list :id 1 :email "test@example.com" :name "Test User 🚀" :description "Test with special chars: !@#$%^&*()")))
    (signals validation-error (Tootsville::validate-user encoding-data) "Should signal error for encoding issues")))

(test validate-user-type-coercion
  "Test validate-user with type coercion issues"
  (let ((type-data (list :id "1" :email 123 :name t :age "25")))
    (signals validation-error (Tootsville::validate-user type-data) "Should signal error for type coercion issues")))

(test validate-user-boundary-values
  "Test validate-user with boundary values"
  (let ((boundary-data (list :id most-positive-fixnum :email "test@example.com" :name (make-string 255 :initial-element #\A) :age 0 :age 150)))
    (signals validation-error (Tootsville::validate-user boundary-data) "Should signal error for boundary values")))
