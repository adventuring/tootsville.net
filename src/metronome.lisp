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
  function)

(defvar *metronome-next-tick* (get-universal-time))

(defvar *metronome-task-lock* nil)

(defun run-metronome-tasks ()
  (loop for now from *metronome-next-tick* 
     below (get-universal-time)
     do
       (dolist (task *metronome-tasks*)
         (when (and (metronome-task-frequency task)
                    (zerop (mod now (metronome-task-frequency task))))
           (make-thread (metronome-task-function task)
                        :name (format nil "Metronome: ~a"
                                      (metronome-task-name task))))
         (when (and (metronome-task-one-shot-time task)
                    (< (metronome-task-one-shot-time task) now))
           (make-thread (metronome-task-function task)
                        :name (format nil "Metronome: ~a"
                                      (metronome-task-name task)))
           (metronome-remove task)))))

(defun metronome-remove (task)
  (with-lock-held (*metronome-task-lock*)
    (removef task *metronome-tasks*))
  t)

(defun metronome-register (task)
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

(defun start-metronome-thread ()
  (make-thread (lambda ()
                 (loop
                    (run-metronome-tasks)
                    (sleep 1)))
               :name "Metronome main thread"))

(defun start-game-metronome ()
  (setf *metronome-next-tick* (get-universal-time)
        *metronome-task-lock* (make-lock "Metronome task lock"))
  (start-metronome-thread)
  (register-metronome-tasks))

(defun register-metronome-tasks ()
  (do-metronome (:frequency 90 :name "Websocket AYT facility")
    (ayt-idle-users))
  (do-metronome (:frequency 300 :name "Toot Quiesce facility")
    (quiesce-connected-Toots)))
