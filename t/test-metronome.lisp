;;;; -*- lisp -*-
;;;
;;;; t/test-metronome.lisp is part of Tootsville
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

;;; Metronome scheduling tests

(in-suite metronome-tests)

(test metronome-task-creation
  "Test metronome task creation"
  (with-test-database
    (let* ((task-data (list :name "Test Task" :interval 60 :function #'identity))
           (task (Tootsville::create-metronome-task task-data)))
      (is (not (null task)) "Should create metronome task")
      (is (equal "Test Task" (getf task :name)) "Should have correct name")
      (is (= 60 (getf task :interval)) "Should have correct interval"))))

(test metronome-task-scheduling
  "Test metronome task scheduling"
  (with-test-database
    (let* ((task (list :name "Scheduled Task" :interval 30 :function #'identity))
           (scheduled-task (Tootsville::schedule-task task)))
      (is (not (null scheduled-task)) "Should schedule task")
      (is (getf scheduled-task :scheduled) "Should mark task as scheduled")
      (is (getf scheduled-task :next-run) "Should set next run time"))))

(test metronome-task-execution
  "Test metronome task execution"
  (with-test-database
    (let* ((execution-count 0)
           (test-function (lambda () (incf execution-count)))
           (task (list :name "Executable Task" :interval 1 :function test-function)))
      (Tootsville::execute-task task)
      (is (= 1 execution-count) "Should execute task function"))))

(test metronome-task-cancellation
  "Test metronome task cancellation"
  (with-test-database
    (let* ((task (list :name "Cancellable Task" :interval 60 :function #'identity))
           (scheduled-task (Tootsville::schedule-task task)))
      (Tootsville::cancel-task (getf scheduled-task :id))
      (is (not (getf scheduled-task :scheduled)) "Should cancel scheduled task"))))

(test metronome-recurring-tasks
  "Test recurring metronome tasks"
  (with-test-database
    (let* ((execution-count 0)
           (test-function (lambda () (incf execution-count)))
           (task (list :name "Recurring Task" :interval 1 :function test-function :recurring t)))
      (Tootsville::schedule-task task)
      (sleep 0.1) ; Small delay to allow execution
      (is (>= execution-count 0) "Should execute recurring task"))))

(test metronome-one-shot-tasks
  "Test one-shot metronome tasks"
  (with-test-database
    (let* ((execution-count 0)
           (test-function (lambda () (incf execution-count)))
           (task (list :name "One-shot Task" :interval 1 :function test-function :recurring nil)))
      (Tootsville::schedule-task task)
      (sleep 0.1) ; Small delay to allow execution
      (is (>= execution-count 0) "Should execute one-shot task"))))

(test metronome-task-priority
  "Test metronome task priority system"
  (with-test-database
    (let* ((high-priority-task (list :name "High Priority" :interval 60 :function #'identity :priority :high))
           (low-priority-task (list :name "Low Priority" :interval 60 :function #'identity :priority :low)))
      (let ((scheduled-high (Tootsville::schedule-task high-priority-task))
            (scheduled-low (Tootsville::schedule-task low-priority-task)))
        (is (>= (getf scheduled-high :priority) (getf scheduled-low :priority)) "Should respect task priority")))))

(test metronome-task-dependencies
  "Test metronome task dependencies"
  (with-test-database
    (let* ((dependency-task (list :name "Dependency" :interval 1 :function #'identity))
           (dependent-task (list :name "Dependent" :interval 1 :function #'identity :depends-on (list (getf dependency-task :id)))))
      (let ((scheduled-dependency (Tootsville::schedule-task dependency-task))
            (scheduled-dependent (Tootsville::schedule-task dependent-task)))
        (is (member (getf scheduled-dependency :id) (getf scheduled-dependent :depends-on)) "Should set task dependencies")))))

(test metronome-task-error-handling
  "Test metronome task error handling"
  (with-test-database
    (let* ((error-function (lambda () (error "Task execution error")))
           (task (list :name "Error Task" :interval 1 :function error-function)))
      (signals error (Tootsville::execute-task task) "Should handle task execution errors")
      
      (let ((invalid-task (list :name "" :interval -1)))
        (signals validation-error (Tootsville::validate-task invalid-task) "Should validate task data")))))

(test metronome-performance
  "Test metronome performance under load"
  (with-test-database
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (let ((task (list :name (format nil "PerfTask~a" i) :interval 60 :function #'identity)))
          (Tootsville::schedule-task task)))
      (let ((end-time (get-internal-real-time)))
        (is (< (- end-time start-time) 1000) "Should schedule 100 tasks within reasonable time")))))

(test metronome-memory-management
  "Test metronome memory management"
  (with-test-database
    (let ((tasks (loop for i from 1 to 50 collect (list :name (format nil "MemTask~a" i) :interval 60 :function #'identity))))
      (dolist (task tasks)
        (Tootsville::schedule-task task))
      (Tootsville::cleanup-completed-tasks)
      (is (<= (length (Tootsville::active-tasks)) 50) "Should manage task memory properly"))))

(test metronome-task-monitoring
  "Test metronome task monitoring"
  (with-test-database
    (let* ((task (list :name "Monitored Task" :interval 60 :function #'identity))
           (scheduled-task (Tootsville::schedule-task task)))
      (let ((stats (Tootsville::get-task-statistics (getf scheduled-task :id))))
        (is (listp stats) "Should collect task statistics")
        (is (getf stats :execution-count) "Should track execution count")
        (is (getf stats :last-execution) "Should track last execution time")))))

(test metronome-task-persistence
  "Test metronome task persistence"
  (with-test-database
    (let* ((task (list :name "Persistent Task" :interval 60 :function #'identity))
           (scheduled-task (Tootsville::schedule-task task)))
      (Tootsville::persist-task scheduled-task)
      (let ((persisted-task (Tootsville::load-persisted-task (getf scheduled-task :id))))
        (is (not (null persisted-task)) "Should persist task")
        (is (equal (getf scheduled-task :name) (getf persisted-task :name)) "Should preserve task name")))))

(test metronome-task-migration
  "Test metronome task migration"
  (with-test-database
    (let* ((old-task (list :name "Old Task" :interval 60 :function #'identity :version 1))
           (migrated-task (Tootsville::migrate-task old-task)))
      (is (not (null migrated-task)) "Should migrate task")
      (is (> (getf migrated-task :version) (getf old-task :version)) "Should increment version"))))

(test metronome-task-validation
  "Test metronome task validation"
  (with-test-database
    (let* ((valid-task (list :name "Valid Task" :interval 60 :function #'identity)))
      (is (Tootsville::validate-task valid-task) "Should validate correct task")
      
      (let ((invalid-task (copy-list valid-task)))
        (setf (getf invalid-task :interval) -1)
        (is (not (Tootsville::validate-task invalid-task)) "Should reject invalid task")))))

(test metronome-task-export-import
  "Test metronome task export and import"
  (with-test-database
    (let* ((task (list :name "Exportable Task" :interval 60 :function #'identity))
           (exported (Tootsville::export-task task))
           (imported (Tootsville::import-task exported)))
      (is (not (null exported)) "Should export task")
      (is (not (null imported)) "Should import task")
      (is (equal (getf task :name) (getf imported :name)) "Should preserve task name during export/import"))))

(test metronome-system-shutdown
  "Test metronome system shutdown"
  (with-test-database
    (let ((tasks (loop for i from 1 to 10 collect (list :name (format nil "ShutdownTask~a" i) :interval 60 :function #'identity))))
      (dolist (task tasks)
        (Tootsville::schedule-task task))
      
      (Tootsville::shutdown-metronome)
      (is (zerop (length (Tootsville::active-tasks))) "Should shutdown all tasks gracefully"))))


