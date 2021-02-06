;;;; -*- lisp -*-
;;;
;;;; src/metronome.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018-2021  The
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

(defclass metronome-task ()
  ((frequency :initarg :frequency :initform (* 24 60 60) :accessor metronome-task-frequency)
   (one-shot-time :initarg :one-shot-time :initform nil :accessor metronome-task-one-shot-time)
   (name :initarg :name :initform "Unnamed Metronome Task" :accessor metronome-task-name)
   (function :initarg :function :initform nil :accessor metronome-task-function)
   (thread :initarg :thread :initform nil :accessor metronome-task-thread)))

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

(defvar *metronome-next-tick* (get-universal-time)
  "The time at which the Metronome should next ``tick''.")

(defvar *metronome-task-lock* nil
  "A lock used to protect inter-thread access to the Metronome tasks.")

(defun metronome-idle-tasks ()
  "Returns only those Metronome tasks without a live thread.

Also reaps (by joining) finished threads.

See `RUN-METRONOME-TASKS'"
  (loop for task in *metronome-tasks*
        unless (when-let (th (metronome-task-thread task))
                 (if (thread-alive-p th)
                     (v:warn :metronome "Still running: ~a" (metronome-task-thread task))
                     (ignore-errors (join-thread th))))
          collect task))

(defun run-metronome-tasks ()
  "Runs tasks scheduled for the game's metronome.

Typically these  tasks are scheduled in  one of three ways.  They may be
scheduled to occur at a given frequency in seconds, at a single time, or
at a give frequency up until a certain time.

Tasks are usually created by `DO-METRONOME', which in turn uses
`METRONOME-REGISTER' to safely enqueue the tasks with locking.

The metronome runs  at approximately 1 second resolution,  but steps its
time forward at precisely 1 second  intervals, so no task will be missed
due to system scheduler tie-ups.

Tasks are not allowed to ``stack up;'' if a task has not finished by the
time  its  next  execution  window   comes  around,  it  will  miss  its
opportunity and have to wait for the next window."
  (let ((lost-time (- (get-universal-time) *metronome-next-tick*)))
    (when (> lost-time 90)
      (v:warn :metronome "Skipping Metronome ahead: ~:d seconds have passed"
              lost-time)
      (setf *metronome-next-tick* (- (get-universal-time) 30))))
  (loop for now from *metronome-next-tick* 
          below (get-universal-time)
        do
           (dolist (task (metronome-idle-tasks))
             (when (and (metronome-task-frequency task)
                        (zerop (mod now (metronome-task-frequency task))))
               (run-async (metronome-task-function task)
                          :name (format nil "Metronome: ~a" (metronome-task-name task)))
               #+ (or)
               (setf (metronome-task-thread task)
                     (make-thread (metronome-task-function task)
                                  :name (format nil "Metronome: ~a"
                                                (metronome-task-name task)))))
             (when (and (metronome-task-one-shot-time task)
                        (<= (metronome-task-one-shot-time task) now))
               (run-async (metronome-task-function task)
                          :name (format nil "Metronome: ~a" (metronome-task-name task)))
               #+ (or)
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
  "Perform BODY as a metronome facility named NAME, at FREQUENCY or once at ONE-SHOT-TIME.

FREQUENCY is given in seconds, or ONE-SHOT-TIME is given in Universal
time. When both are given, the facility will execute at the rate of
FREQUENCY until a final execution at ONE-SHOT-TIME.
"
  `(metronome-register (make-instance 'metronome-task
                                      :frequency ,frequency
                                      :one-shot-time ,one-shot-time
                                      :name ,name
                                      :function (lambda () ,@body))))

(defvar *the-metronome-thread* nil
  "The thread from which the metronome's coördination efforts are conducted.")

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
  "Stop the metronome facility by canceling all tasks and stopping the metronome thread."
  (dolist (task *metronome-tasks*)
    (v:warn :metronome "Canceling task ~a" task)
    (metronome-remove task))
  (v:warn :metronome "Stopping metronome thread")
  (setf *metronome-run* nil)
  (join-thread *the-metronome-thread*))

(defun register-metronome-tasks ()
  "Register certain metronome tasks for miscellaneous services.

This is a list of specific facilities that are started up during the
system boot process.

@table @code

@item Websocket AYT facility
This facility, `AYT-IDLE-USERS' runs every 120 seconds to detect and
disconnect users who are no longer actually connected. (Note that AYT
is netspeak for ``are you there?'')

@item Toot Quiesce facility
This facility runs every 600 seconds to asks Toots to quiesce themselves
to the database. See `QUIESCE-CONNECTED-TOOTS'.

@item Reap uninteresting child requests
See `REAP-UNINTERESTING-CHILD-REQUESTS'. Every 4 hours clears out some
uninteresting records from the ``child\_requests'' database table.

@end table
"
  (do-metronome (:frequency 120
                            :name "Websocket AYT facility")
    (ayt-idle-users))
  (do-metronome (:frequency 600
                            :name "Toot Quiesce facility")
    (quiesce-connected-Toots))
  (do-metronome (:frequency (* 4 60 60)
                            :name "Reap uninteresting child requests")
    (reap-uninteresting-child-requests)))

(defmacro do-after ((time) &body body)
  "Perform BODY after TIME seconds have elapsed.

Uses a one-shot-timer metronome facility."
  `(do-metronome (:one-shot-time (+ (get-universal-time) ,time))
     ,@body))
