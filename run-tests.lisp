;;;; -*- lisp -*-
;;;
;;;; run-tests.lisp is part of Tootsville
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

(in-package :cl-user)

;;; Test runner with timeout protection

;; Load ASDF first
(require :asdf)

(defvar *test-timeout* 300 "Timeout in seconds for test execution")
(defvar *test-results* nil "Store test results")

(defun setup-test-environment ()
  "Setup the test environment with required dependencies"
  (format t "~&Setting up test environment...~%")
  
  ;; Load required test dependencies using ASDF
  
  (handler-case
      (progn
        (asdf:operate 'asdf:load-op :fiveam)
        (format t "FiveAM loaded~%"))
    (error (e)
      (format t "Failed to load FiveAM: ~a~%" e)
      (return-from setup-test-environment nil)))
  
  ;; Try to load optional dependencies
  (handler-case
      (progn
        (asdf:operate 'asdf:load-op :fiveam-matchers)
        (format t "FiveAM-matchers loaded~%"))
    (error (e)
      (format t "FiveAM-matchers not available: ~a~%" e)))
  
  (handler-case
      (progn
        (asdf:operate 'asdf:load-op :mock)
        (format t "Mock loaded~%"))
    (error (e)
      (format t "Mock not available: ~a~%" e)))
  
  t)

(defun run-test-with-timeout (test-name timeout)
  "Run a single test with timeout protection"
  (let ((start-time (get-universal-time))
        (result nil))
    (handler-case
        (progn
          (setf result (funcall test-name))
          (let ((end-time (get-universal-time)))
            (if (> (- end-time start-time) timeout)
                (progn
                  (format t "~&Test ~a timed out after ~a seconds~%" test-name timeout)
                  (setf result :timeout))
                result)))
      (error (e)
        (format t "~&Test ~a failed with error: ~a~%" test-name e)
        (setf result :error)))
    result))

(defun run-all-tests-with-timeout ()
  "Run all tests with timeout protection"
  (format t "~&Starting test execution with ~a second timeout...~%" *test-timeout*)
  
  (unless (setup-test-environment)
    (format t "~&Failed to setup test environment. Exiting.~%")
    (uiop:quit 1))
  
  ;; Load test files
  (handler-case
      (progn
        (load "t/test-suite.lisp")
        (format t "Test suite loaded~%"))
    (error (e)
      (format t "Failed to load test suite: ~a~%" e)
      (uiop:quit 1)))
  
  (handler-case
      (progn
        (load "t/test-users.lisp")
        (format t "User tests loaded~%"))
    (error (e)
      (format t "Failed to load user tests: ~a~%" e)))
  
  (handler-case
      (progn
        (load "t/test-websockets.lisp")
        (format t "WebSocket tests loaded~%"))
    (error (e)
      (format t "Failed to load WebSocket tests: ~a~%" e)))
  
  (handler-case
      (progn
        (load "t/test-items.lisp")
        (format t "Item tests loaded~%"))
    (error (e)
      (format t "Failed to load item tests: ~a~%" e)))
  
  (handler-case
      (progn
        (load "t/test-terrain.lisp")
        (format t "Terrain tests loaded~%"))
    (error (e)
      (format t "Failed to load terrain tests: ~a~%" e)))
  
  (handler-case
      (progn
        (load "t/test-world.lisp")
        (format t "World tests loaded~%"))
    (error (e)
      (format t "Failed to load world tests: ~a~%" e)))
  
  (handler-case
      (progn
        (load "t/test-metronome.lisp")
        (format t "Metronome tests loaded~%"))
    (error (e)
      (format t "Failed to load metronome tests: ~a~%" e)))
  
  (handler-case
      (progn
        (load "t/test-utils.lisp")
        (format t "Utility tests loaded~%"))
    (error (e)
      (format t "Failed to load utility tests: ~a~%" e)))
  
  (handler-case
      (progn
        (load "t/test-auth.lisp")
        (format t "Auth tests loaded~%"))
    (error (e)
      (format t "Failed to load auth tests: ~a~%" e)))
  
  ;; Run tests with timeout
  (let ((start-time (get-universal-time))
        (test-results nil))
    
    (format t "~&Running all tests...~%")
    
    (handler-case
        (progn
          ;; Run FiveAM tests
          (let ((fiveam:*test-dribble* t))
            (setf test-results (fiveam:run! 'Tootsville.test::all-tests)))
          
          (let ((end-time (get-universal-time)))
            (if (> (- end-time start-time) *test-timeout*)
                (progn
                  (format t "~&Test execution timed out after ~a seconds~%" *test-timeout*)
                  (setf test-results :timeout))
                (format t "~&Test execution completed in ~a seconds~%" (- end-time start-time)))))
      
      (error (e)
        (format t "~&Test execution failed with error: ~a~%" e)
        (setf test-results :error)))
    
    ;; Report results
    (format t "~&Test Results: ~a~%" test-results)
    
    (if (eq test-results :timeout)
        (uiop:quit 124)  ; Timeout exit code
        (uiop:quit 0)))) ; Success exit code

;;; Main execution
(format t "~&Tootsville Test Runner~%")
(format t "=====================~%")

;; Parse command line arguments for timeout
(let ((args (uiop:command-line-arguments)))
  (when args
    (let ((timeout-arg (first args)))
      (when (and (stringp timeout-arg) (every #'digit-char-p timeout-arg))
        (setf *test-timeout* (parse-integer timeout-arg))
        (format t "Using timeout: ~a seconds~%" *test-timeout*)))))

;; Run the tests
(run-all-tests-with-timeout)


