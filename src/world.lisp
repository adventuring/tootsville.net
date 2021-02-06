;;;; -*- lisp -*-
;;;
;;;; src/world.lisp is part of Tootsville
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

(defun sky-contents (x y z &optional (now (get-universal-time)))
  (let ((day (nth-value 9 (Choerogryllum:decode*-universal-time now))))
    (list :|sun| (list :|azimuth| (sinus (- now (* 21 18 60 60)) (* 360 18 60 60))
                       :|elevation| (sinus now (* 18 60 60)))
          :|moon| (list :|azimuth| (sinus now +moon-year+)
                        :|elevation| (sinus now +moon-day+)
                        :|fullness| (sinus day 30)
                        :|phase| (if (plusp (- 30/2 day))
                                     "waxing" "waning"))
          :|other-moon| (let* ((now (+ now (* 11 18 60 60)))
                               (day (nth-value 9 (Choerogryllum:decode*-universal-time now))))
                          (list :|azimuth| (sinus now +other-moon-year+)
                                :|elevation| (sinus now +other-moon-day+)
                                :|fullness| (sinus day 71)
                                :|phase| (if (plusp (- 71/2 day))
                                             "waxing" "waning")))
          :|pink-moon| (let* ((now (+ now (* 18 60 60)))
                              (day (nth-value 9 (Choerogryllum:decode*-universal-time now))))
                         (list :|azimuth| (sinus now +pink-moon-year+)
                               :|elevation| (sinus now +pink-moon-day+)
                               :|fullness| (sinus day 53)
                               :|phase| (if (plusp (- 53/2 day))
                                            "waxing" "waning")))
          :|clouds| (clouds x y z)
          :|precipitation| (multiple-value-list (precipitation x y z)))))

(defun world-mistp (latitude longitude altitude world)
  (when (eql world :orbit)
    (return-from world-mistp t))
  ;; XXX for now …
  (unless (eql world :chor)
    (return-from world-mistp t))

  (when (= latitude longitude altitude 0)
    (return-from world-mistp nil))

  (ignore-not-found
    (find-record 'terrain-height
                 :world world :latitude latitude :longitude longitude)))

(defun describe-world (latitude longitude altitude world)
  "Describe the world at LATITUDE, LONGITUDE, ALTITUDE in WORLD.

Returns a PList with :TERRAIN and :FURNITURE.

:TERRAIN  contains a  201×201  grid of  1m corners  of  a 200×200  meter
space (that is, the entire  space at LATITUDE, LONGITUDE at ALTITUDE=0).
If ALTITUDE is not zero, the :TERRAIN is omitted.

:FURNITURE contains a list of item descriptions as per `ITEMS-AT', which
are as per `ITEM-INFO'."
  (when (world-mistp latitude longitude altitude world)
    (if (zerop altitude)
        (spawn-terrain world latitude longitude)
        (error 'unimplemented)))
  (list
   :terrain (make-array '(200 200) :element-type 'fixnum :initial-element 0)
   :furniture (items-at latitude longitude altitude world)))

(defun item-in-inventory-p (item)
  "Is ITEM in a character's inventory?"
  (and nil
       (ignore-not-found (find-record 'inventory-item
                                      :item (item-uuid item)))))

(defun items-at (latitude longitude altitude world)
  "All items in the space at LATITUDE, LONGITUDE, and ALTITUDE in WORLD.

Returns all items in that volume which are not in a character's inventory."
  (remove-if #'item-in-inventory-p
             (find-records 'item
                           :latitude latitude
                           :longitude longitude
                           :altitude altitude
                           :world world)))



(defun square (x) (* x x))

(defun distance (x₁ y₁ z₁ x₂ y₂ z₂)
  (sqrt (+ (square (- x₂ x₁)) (square (- y₁ y₂)) (square (- z₁ z₂)))))




(defclass wtl-course ()
  ((speed :initarg :speed :accessor wtl-course-speed)
   (start-time :initarg :start-time :accessor wtl-course-start-time)
   (end-time :initarg :end-time :accessor wtl-course-end-time)
   (start-point :initarg :start-point :accessor wtl-course-start-point)
   (end-point :initarg :end-point :accessor wtl-course-end-point)
   (latitude :initarg :latitude :accessor latitude)
   (longitude :initarg :longitude :accessor longitude)
   (altitude :initarg :altitude :accessor altitude)
   (world :initarg :world :accessor world)
   (facing :initarg :facing :accessor facing)))

(defmethod current-position ((course wtl-course))
  (let ((now (get-Unix-time)))
    (if (> now (or (wtl-course-end-time course) 0))
        (wtl-course-end-point course)
        (destructuring-bind (x₁ y₁ z₁) (wtl-course-start-point course)
          (destructuring-bind (x₂ y₂ z₂) (wtl-course-end-point course)
            (let* ((δ-x (- x₂ x₁))
                   (δ-y (- y₂ y₁))
                   (δ-z (- z₂ z₁))
                   (δ-τ (- (wtl-course-end-time course)
                           (wtl-course-start-time course)))
                   (τ (- now (wtl-course-start-time course)))
                   (fraction (/ τ δ-τ))
                   (x (+ x₁ (* δ-x fraction)))
                   (y (+ y₁ (* δ-y fraction)))
                   (z (+ z₁ (* δ-z fraction))))
              (list x y z)))))))

(defmethod current-position ((Toot Toot))
  (if-let (stream (user-stream Toot))
    (current-position (wtl-course stream))
    (current-position (find-robot Toot))))

(defgeneric parse-wtl-course (course)
  (:documentation "Parse COURSE into a WTL-COURSE object"))

(defmethod parse-wtl-course ((course wtl-course))
  course)

(defmethod parse-wtl-course ((null null))
  nil)

(defmethod parse-wtl-course ((course string))
  (parse-wtl-course (jonathan.decode:parse course)))

(defmethod parse-wtl-course ((course cons))
  (destructuring-bind (&key |course| |facing|) course
    (destructuring-bind (&key |startTime| |endTime| |speed| |startPoint| |endPoint|) |course|
      (destructuring-bind (&key |x| |y| |z| &allow-other-keys) |startPoint|
        (let ((x₁ |x|) (y₁ |y|) (z₁ |z|))
          (destructuring-bind (&key |x| |y| |z| &allow-other-keys) |endPoint|
            (let ((x₂ |x|) (y₂ |y|) (z₂ |z|))
              (make-wtl-course :speed |speed|
                               :start-time |startTime|
                               :end-time (if |endTime| |endTime|
                                             (+ |startTime| (/ (distance x₁ y₁ z₁ x₂ y₂ z₂) (or |speed| 0.1))))
                               :start-point (list x₁ y₁ z₁)
                               :end-point (list x₂ y₂ z₂)
                               :speed (or |speed| 0.1)
                               :facing (interpret-facing |facing|)))))))))
