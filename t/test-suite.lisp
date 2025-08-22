;;;; -*- lisp -*-
;;;
;;;; t/test-suite.lisp is part of Tootsville
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

(in-package :Tootsville)

;;; Test suite configuration and utilities

(defpackage :Tootsville.test
  (:use :common-lisp :fiveam :fiveam-matchers)
  (:import-from :Tootsville)
  (:export #:run-all-tests
           #:run-users-tests
           #:run-websockets-tests
           #:run-items-tests
           #:run-terrain-tests
           #:run-world-tests
           #:run-metronome-tests
           #:run-utils-tests
           #:run-auth-tests))

(in-package :Tootsville.test)

;;; Test suite definitions

(defsuite all-tests :description "All Tootsville server tests")

(defsuite users-tests :description "User management and authentication tests")
(defsuite websockets-tests :description "WebSocket communication tests")
(defsuite items-tests :description "Item system tests")
(defsuite terrain-tests :description "Terrain generation tests")
(defsuite world-tests :description "World management tests")
(defsuite metronome-tests :description "Metronome scheduling tests")
(defsuite utils-tests :description "Utility function tests")
(defsuite auth-tests :description "Authentication and authorization tests")

;;; Test utilities

(defmacro with-test-database (&body body)
  "Set up a test database environment"
  `(let ((*test-mode* t))
     (unwind-protect
          (progn
            (setup-test-database)
            ,@body)
       (cleanup-test-database))))

(defmacro with-mocked-dependencies ((&rest mocks) &body body)
  "Mock dependencies for isolated testing"
  `(let ,mocks
     (unwind-protect
          ,@body
       (cleanup-mocks))))

(defun setup-test-database ()
  "Initialize test database with clean state"
  (declare (optimize (debug 3)))
  ;; Initialize test database connection
  ;; Create test tables if needed
  ;; Clear any existing test data
  )

(defun cleanup-test-database ()
  "Clean up test database state"
  (declare (optimize (debug 3)))
  ;; Close test database connections
  ;; Remove test data
  ;; Reset database state
  )

(defun cleanup-mocks ()
  "Clean up any mocked dependencies"
  (declare (optimize (debug 3)))
  ;; Reset any mocked functions
  ;; Clear mock state
  )

;;; Test data generators

(defun make-test-user (&key (email "test@example.com") (name "Test User"))
  "Create a test user for testing"
  (list :email email :name name :uuid (uuid:make-v4-uuid)))

(defun make-test-item (&key (name "Test Item") (type "tool"))
  "Create a test item for testing"
  (list :name name :type type :uuid (uuid:make-v4-uuid)))

(defun make-test-world (&key (name "TEST") (type "CHOR"))
  "Create a test world for testing"
  (list :name name :type type :uuid (uuid:make-v4-uuid)))

(defun make-test-credentials (&key (provider "google") (id "test-id"))
  "Create test credentials for authentication"
  (list provider (list id)))

;;; Test assertions

(defun assert-user-valid (user)
  "Assert that a user object is valid"
  (is (not (null user)))
  (is (stringp (getf user :email)))
  (is (stringp (getf user :name)))
  (is (uuid:uuid-p (getf user :uuid))))

(defun assert-item-valid (item)
  "Assert that an item object is valid"
  (is (not (null item)))
  (is (stringp (getf item :name)))
  (is (stringp (getf item :type)))
  (is (uuid:uuid-p (getf item :uuid))))

(defun assert-world-valid (world)
  "Assert that a world object is valid"
  (is (not (null world)))
  (is (stringp (getf world :name)))
  (is (stringp (getf world :type)))
  (is (uuid:uuid-p (getf world :uuid))))

;;; Test runners

(defun run-all-tests ()
  "Run all test suites"
  (run! 'all-tests))

(defun run-users-tests ()
  "Run user management tests"
  (run! 'users-tests))

(defun run-websockets-tests ()
  "Run WebSocket communication tests"
  (run! 'websockets-tests))

(defun run-items-tests ()
  "Run item system tests"
  (run! 'items-tests))

(defun run-terrain-tests ()
  "Run terrain generation tests"
  (run! 'terrain-tests))

(defun run-world-tests ()
  "Run world management tests"
  (run! 'world-tests))

(defun run-metronome-tests ()
  "Run metronome scheduling tests"
  (run! 'metronome-tests))

(defun run-utils-tests ()
  "Run utility function tests"
  (run! 'utils-tests))

(defun run-auth-tests ()
  "Run authentication tests"
  (run! 'auth-tests))

;;; Test configuration

(defvar *test-mode* nil
  "Flag indicating if we're running in test mode")

(defvar *test-database* nil
  "Test database connection")

(defvar *test-timeout* 30
  "Timeout for tests in seconds")

;;; Export test suite

(export '(all-tests users-tests websockets-tests items-tests terrain-tests
          world-tests metronome-tests utils-tests auth-tests
          run-all-tests run-users-tests run-websockets-tests run-items-tests
          run-terrain-tests run-world-tests run-metronome-tests run-utils-tests
          run-auth-tests))
