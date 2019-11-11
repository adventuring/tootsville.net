;;;; -*- lisp -*-
;;;
;;;; src/world.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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


(defconstant +moon-year+ 3600000)
(defconstant +other-moon-year+ 583243)
(defconstant +pink-moon-year+ 452398723)

(defconstant +moon-day+ (* 18 60 59))
(defconstant +other-moon-day+ (* 2 18 60 58))
(defconstant +pink-moon-day+ (* 8 18 60 56))

(defun sinus (x range)
  (sin (/ (mod x range) (/ range pi))))

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
  (ignore-not-found
    (find-record 'terrain-height
                 :world world :latitude latitude :longitude longitude)))

(defun describe-world (latitude longitude altitude world)
  (check-type world world-moniker)
  (if (world-mistp latitude longitude altitude world)
      (generate-terrain latitude longitude world)
      (progn
        (spawn-terrain :tootanga latitude longitude altitude)
        ))
  )
