;;;; -*- lisp -*-
;;;
;;;; src/characters/robot.lisp is part of Tootsville
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



(defvar *robots* (make-hash-table :test 'equalp))

(defclass robot ()
  ((Toot :accessor Toot :initform nil :initarg :Toot :type 'Toot)
   (context :accessor context :initform nil)
   (location :accessor Toot-position :initform (list :CHOR 0 0 0))))

(defun robotp (user)
  (typep user 'robot))

(defmethod initialize-robot (robot Toot-name)
  (warn "No initializer for robot ~a" Toot-name))

(defmethod initialize-instance :after (robot &key Toot &allow-other-keys)
  (check-type Toot Toot)
  (initialize-robot robot (make-keyword (string-upcase (Toot-name Toot))))
  (setf (gethash (Toot-name Toot) *robots*) robot))

(defun nearp (robot place)
  ;; TODO
  t)

(defun robot-broadcast (message near)
  (dolist (robot (hash-table-values *robots*))
    (when (nearp robot near)
      (robot-unicast message robot))))

(defmethod robot-unicast (message robot)) ; default no-op

