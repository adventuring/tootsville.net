;;;; -*- lisp -*-
;;;
;;;; src/metronome.lisp is part of Tootsville
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



(defvar *metronome* nil)
(defvar *metronome-tasks* nil)

(defstruct metronome-task
  frequency
  one-shot-time
  name
  function
  thread)

(defmethod print-object ((task metronome-task) s)
  (format s "#<Metronome-Task \"~a\" ~a ~a>"
          (metronome-task-name task)
          (if (metronome-task-one-shot-time task)
              (if (metronome-task-frequency task)
                  (format nil "every ~a until ~a"
                          (human-duration (metronome-task-frequency task))
                          (human-future-time (metronome-task-one-shot-time task)))
                  (format nil "once at ~a"
                          (human-future-time (metronome-task-one-shot-time task))))
              (if (metronome-task-frequency task)
                  (format nil "every ~a"
                          (human-duration (metronome-task-frequency task)))
                  "never"))
          (if (metronome-task-thread task) "running" "waiting")))

(defvar *metronome-next-tick* (get-universal-time))

(defvar *metronome-task-lock* nil)

(defun metronome-idle-tasks ()
  "Returns only those Metronome tasks without a live thread.

Also reaps (by joining) finished threads.

See `RUN-METRONOME-TASKS'"
  (loop for task in *metronome-tasks*
     if (when-let (th (metronome-task-thread task))
          (if (thread-alive-p th)
              t
              (progn (ignore-errors (join-thread th))
                     nil)))
     do (v:warn :metronome "Still running: ~a" (metronome-task-thread task))
     else collect task))

(defun run-metronome-tasks ()
  "Runs tasks scheduled for the game's metronome.

Typically these  tasks are scheduled in  one of three ways.  They may be
scheduled to occur at a given frequency in seconds, at a single time, or
at a give frequency up until a certain time.

Tasks are usually created by `DO-METRONOME', which in turn uses `METRONOME-REGISTER' to safely enqueue the tasks with locking.

The metronome runs  at approximately 1 second resolution,  but steps its
time forward at precisely 1 second  intervals, so no task will be missed
due to system scheduler tie-ups.

Tasks are not allowed to ``stack up;'' if a task has not finished by the
time  its  next  execution  window   comes  around,  it  will  miss  its
opportunity and have to wait for the next window."
  (loop for now from *metronome-next-tick* 
     below (get-universal-time)
     do
       (dolist (task (metronome-idle-tasks))
         (when (and (metronome-task-frequency task)
                    (zerop (mod now (metronome-task-frequency task))))
           (setf (metronome-task-thread task)
                 (make-thread (metronome-task-function task)
                              :name (format nil "Metronome: ~a"
                                            (metronome-task-name task)))))
         (when (and (metronome-task-one-shot-time task)
                    (< (metronome-task-one-shot-time task) now))
           (make-thread (metronome-task-function task)
                        :name (format nil "Metronome: ~a"
                                      (metronome-task-name task)))
           (metronome-remove task)))
     do (setf *metronome-next-tick* now)))

(defun metronome-remove (task)
  "Safely remove TASK from the metronome's schedule.

See  `RUN-METRONOME-TASKS'  for  a  discussion  of  the  metronome;  see
`DO-METRONOME' and `METRONOME-REGISTER' to schedule a task."
  (with-lock-held (*metronome-task-lock*)
    (removef *metronome-tasks* task))
  t)

(defun metronome-register (task)
  "Safely register TASK with the metronome.

Most  users  will  prefer  `DO-METRONOME' for  that  purpose.  See  also
`RUN-METRONOME-TASKS'   for  a   discussion   of   the  metronome,   and
`METRONOME-REMOVE' for the complementary function."
  (with-lock-held (*metronome-task-lock*)
    (push task *metronome-tasks*))
  t)

(defmacro do-metronome ((&key frequency one-shot-time name)
                        &body body)
  `(metronome-register (make-metronome-task
                        :frequency ,frequency
                        :one-shot-time ,one-shot-time
                        :name ,name
                        :function (lambda () ,@body))))

(defvar *the-metronome-thread* nil)

(defun start-metronome-thread% ()
  "See `START-GAME-METRONOME'"
  (make-thread (lambda ()
                 (loop while *metronome-run*
                    do (run-metronome-tasks)
                    do (sleep 1)))
               :name "Metronome main thread"))

(defvar *metronome-run* t)

(defun start-game-metronome ()
  (setf *metronome-next-tick* (get-universal-time)
        *metronome-task-lock* (make-lock "Metronome task lock")
        *metronome-run* t)
  (unless *the-metronome-thread*
    (setf *the-metronome-thread*
          (start-metronome-thread%)))
  (v:info :metronome "Game metronome started")
  (register-metronome-tasks))

(defun stop-game-metronome ()
  (dolist (task *metronome-tasks*)
    (v:warn :metronome "Canceling task ~a" task)
    (metronome-remove task))
  (v:warn :metronome "Stopping metronome thread")
  (setf *metronome-run* nil)
  (join-thread *the-metronome-thread*))

(defun register-metronome-tasks ()
  (do-metronome (:frequency 120
                            :name "Websocket AYT facility")
    (ayt-idle-users))
  (do-metronome (:frequency 300
                            :name "Toot Quiesce facility")
    (quiesce-connected-Toots))
  (do-metronome (:frequency (* 4 60 60)
                            :name "Reap uninteresting child requests")
    (reap-uninteresting-child-requests)))
