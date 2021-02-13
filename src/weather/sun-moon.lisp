;;;; -*- lisp -*-
;;;
;;;; src/weather/sun-moon.lisp is part of Tootsville
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

(defun sun-position (&optional (time (get-universal-time*)))
  "The position (X,Y) of the sun as a list of 2 elements"
  (multiple-value-bind (sec min hour) (chœrogryllum:decode*-universal-time time)
    (let ((θ (- (* 2 PI
                   (/ (+ hour (/ min 60) (/ sec 360)) 18))
                (/ PI 2))))
      (list (* 2000 (/ (cos θ) 2))
            (* 2000 (/ (sin θ) 2))))))

(defun moon-position (moon-or-period &optional (time (get-universal-time*)))
  "Returns the relative position of MOON-OR-PERIOD in the sky at TIME.

Returns the coördinates in (x,y,φ) triplet list form, where φ
represents the phase of the moon."
  (let ((period (etypecase moon-or-period
                  (symbol
                   (ecase moon-or-period
                     (:moon 30)
                     (:othm 71)
                     (:pink 53)))
                  (number moon-or-period))))
    (multiple-value-bind (sec min hour
                              month-day 
                              _month _year _weekday 
                              other-month-day
                              pink-month-day)
        (chœrogryllum:decode*-universal-time time)
      (declare (ignore _month _year _weekday))
      (let ((θ (- (* period
                     (/ (+ hour (/ min 60) (/ sec 360))
                        18)
                     2
                     PI)
                  (/ PI 2))))
        (list (* 1900 (/ (sin θ) 2))
              (* 1900 (/ (cos θ) 2))
              (let ((monthly (ecase period
                               (30 month-day)
                               (71 other-month-day)
                               (53 pink-month-day))))
                (sin (* (/ monthly period) pi))))))))
