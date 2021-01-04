;;;; -*- lisp -*-
;;;
;;;; src/characters/robot.lisp is part of Tootsville
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



(defvar *robots* (make-hash-table :test 'equalp))

(defclass robot ()
  ((Toot :accessor Toot :initarg :Toot :type 'Toot)
   (context :accessor context :initform nil)
   (location :accessor Toot-position :initform (list :CHOR 0 0 0))))

(defmethod print-object ((robot robot) s)
  (format s "#<Robot for Toot ~a; context ~s>"
          (Toot-name (Toot robot))
          (context robot)))

(defun robotp (user)
  (typep user 'robot))

(defmethod initialize-robot ((robot robot) (Toot-name symbol))
  (error "No initializer for robot ~:(~a~) (~s)" Toot-name robot))

(defmethod initialize-instance :after ((robot robot) &key Toot &allow-other-keys)
  (check-type Toot Toot)
  (initialize-robot robot (make-keyword (string-upcase (Toot-name Toot))))
  (setf (gethash (Toot-name Toot) *robots*) robot))

(defmethod nearp ((robot robot) place)
  ;; TODO
  t)

(defun robot-broadcast (message near &key except)
  (v:info :robots "Robots hear message: ~s" message)
  (dolist (robot (hash-table-values *robots*))
    (when (and (nearp robot near) (not (equalp except robot)))
      (robot-unicast message robot))))

(defmethod user-stream ((robot robot))
  nil)

