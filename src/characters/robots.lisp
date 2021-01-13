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



(defvar *robots* (make-hash-table :test 'equalp)
  "All robots currently active in the game world from this node.")

(defun restore-robot-wtl (robot)
  "Restore the walk-the-line positioning data for ROBOT

Pulls quiesced data, where available, or creates a new one with
`RANDOM-START-WTL-FOR-TOOT' if no quiescent data is available."
  (let* ((quiesced (ignore-errors (find-record 'Toot-quiesced :Toot (Toot-uuid (Toot robot)))))
         (wtl (jonathan.decode:parse (or (and quiesced (Toot-quiesced-wtl quiesced))
                                         (random-start-wtl-for-Toot)))))
    (setf (robot-course robot) (parse-wtl-for-robot wtl))))

(defclass robot ()
  ((Toot :accessor Toot :initarg :Toot :type 'Toot)
   (heard :initform (make-hash-table :test 'equalp) :accessor robot-has-heard)
   (mode :accessor robot-mode :initform (make-hash-table :test 'equalp))
   (walk-the-line :accessor robot-course))
  (:documentation "An in-game robot character"))

(defun robotp (user)
  "Is USER a robot?

USER may be a robot or a Toot that is controlled by a robot."
  (or (typep user 'robot)
      (and (typep user 'Toot)
           (gethash (Toot-name user) *robots*))))

(defmethod initialize-instance :after ((robot robot) &key Toot &allow-other-keys)
  (check-type Toot Toot)
  (setf (gethash (Toot-name Toot) *robots*) robot
        (robot-course robot) (restore-robot-wtl robot))
  (v:info :robots "Added a robot: ~:(~a~)" (Toot-name Toot)))

(defstruct game-point
  latitude
  longitude
  altitude
  world
  x
  y
  z)

(defmethod world ((cons cons))
  (first cons))

(defmethod latitude ((cons cons))
  (second cons))

(defmethod longitude ((cons cons))
  (third cons))

(defmethod altitude ((cons cons))
  (fourth cons))

(defmethod latitude ((game-point game-point))
  (game-point-latitude game-point))

(defmethod longitude ((game-point game-point))
  (game-point-longitude game-point))

(defmethod altitude ((game-point game-point))
  (game-point-altitude game-point))

(defmethod world ((game-point game-point))
  (game-point-world game-point))

(defmethod latitude ((robot robot))
  (wtl-course-latitude (robot-course robot)))

(defmethod longitude ((robot robot))
  (wtl-course-longitude (robot-course robot)))

(defmethod altitude ((robot robot))
  (wtl-course-altitude (robot-course robot)))

(defmethod world ((robot robot))
  (wtl-course-world (robot-course robot)))

(defmethod latitude (thing)
  (wtl-course-latitude (wtl-course thing)))

(defmethod longitude (thing)
  (wtl-course-longitude (wtl-course thing)))

(defmethod altitude (thing)
  (wtl-course-altitude (wtl-course thing)))

(defmethod world (thing)
  (wtl-course-world (wtl-course thing)))

(defmethod Toot-robot ((Toot Toot))
  (gethash (Toot-name Toot) *robots*))

(defun Toot-quiesced-data (Toot)
  (find-record 'Toot-quiesced :Toot (Toot-UUID Toot)))

(defmethod wtl-course ((Toot Toot))
  (if (robotp Toot)
      (robot-course (Toot-robot Toot))
      (parse-wtl-for-robot (jonathan.decode:parse
                            (Toot-quiesced-wtl
                             (Toot-quiesced-data Toot))))))

(defmethod nearp ((robot robot) place)
  "Is ROBOT near PLACE?" ; FIXME, do better
  (or (null place)
      (and (= (latitude place) (latitude robot))
           (= (longitude place) (longitude robot))
           (= (altitude place) (altitude robot))
           (eql (world place) (world robot)))))

(defun robot-broadcast (message near &key except)
  "Broadcast MESSAGE to all robots near NEAR, except robot EXCEPT."
  (v:info :robots "Robots hear message: ~s" message)
  (dolist (robot (hash-table-values *robots*))
    (when (and (nearp robot near) (not (equalp except robot)))
      (robot-unicast message robot))))

(defmethod user-stream ((robot robot))
  "Robots do not have a user stream."
  nil)

(defstruct wtl-course
  speed
  start-time
  end-time
  start-point
  end-point
  latitude
  longitude
  altitude
  world)

(defun parse-wtl-for-robot (wtl)
  "Parse the WTL JSON into a WTL-Course structure "
  (destructuring-bind (&key |facing| |course|) wtl
    (declare (ignore |facing|))
    (destructuring-bind (&key |speed| |startTime| |endTime|
                           |startPoint| |endPoint|
                           |latitude| |longitude| |altitude|
                           |world|)
        |course|
      (make-wtl-course :speed |speed|
                       :start-time |startTime|
                       :end-time |endTime|
                       :start-point (destructuring-bind (&key |x| |y| |z|) |startPoint|
                                      (list |x| |y| |z|))
                       :end-point (destructuring-bind (&key |x| |y| |z|) |endPoint|
                                    (list |x| |y| |z|))
                       :latitude (or |latitude| 0)
                       :longitude (or |longitude| 0)
                       :altitude (or |altitude| 0)
                       :world (or |world| :chor)))))

(defmethod Toot-position ((robot robot))
  (let ((course (robot-course robot)))
    (with-slots (world latitude longitude altitude) course
      (list world latitude longitude altitude))))

(defmethod robot-say (robot format &rest format-args)
  "Robot ROBOT says the string formatted from FORMAT and FORMAT-ARGS."
  (let ((string (apply #'format nil format format-args)))
    (do-metronome (:one-shot-time (+ (get-universal-time) 1 (/ (length string) 32)))
      (broadcast (list :|from| "pub"
                       :|id| (Toot-UUID (Toot robot))
                       :|u| (Toot-name (Toot robot))
                       :|t| string
                       :|x| nil)
                 :near (Toot-position robot)
                 :except robot))))

(defun robot-course-wtl (robot)
  "Get the course of ROBOT in Walk-The-Line JSON form."
  (let ((wtl (robot-course robot)))
    (with-slots (speed start-time end-time
                 start-point end-point
                 latitude longitude altitude world)
        wtl
        (destructuring-bind (x₁ y₁ z₁) start-point
          (destructuring-bind (x₂ y₂ z₂) end-point
            (list :|facing| 0 ; FIXME
                  :|course| (list :|speed| speed
                                  :|startTime| start-time
                                  :|endTime| end-time
                                  :|startPoint| (list :|x| x₁ :|y| y₁ :|z| z₁)
                                  :|endPoint| (list :|x| x₂ :|y| y₂ :|z| z₂)                 
                                  :|latitude| latitude
                                  :|longitude| longitude
                                  :|altitude| altitude
                                  :|world| world)))))))

(defmethod robot-go-to (robot x y z)
  (destructuring-bind (start-x start-y start-z) (Toot-position robot)
    (let* ((facing 0) ; FIXME
           (distance (distance start-x start-y start-z x y z))
           (start-time (timestamp-to-unix (now)))
           (end-time (+ start-time (* 1000 (/ distance 10)))))
      (broadcast (list :|from| "wtl"
                       :|status| t
                       :|course| (list :|speed| 0.1
                                       :|startTime| start-time
                                       :|startPoint| (list :|x| start-x
                                                           :|y| start-y
                                                           :|z| start-z)
                                       :|endTime| end-time
                                       :|endPoint| (list :|x| x
                                                         :|y| y
                                                         :|z| z))
                       :|facing| facing
                       :|u| (Toot-UUID (Toot robot))
                       :|n| (Toot-name (Toot robot))))
      (setf (robot-course robot) (make-wtl-course
                                  :start-time start-time
                                  :end-time end-time
                                  :start-point (list start-x start-y start-z)
                                  :end-point (list x y z)
                                  :latitude (wtl-course-latitude (robot-course robot))
                                  :longitude (wtl-course-longitude (robot-course robot))
                                  :altitude (wtl-course-altitude (robot-course robot))
                                  :world (wtl-course-world (robot-course robot))
                                  :speed 0.1)))))

(defmacro robot-set-mode (mode)
  `(setf (gethash speaker (robot-mode robot)) ,(make-keyword (string mode))))

(defmacro robot-match ((&rest strings) &body body)
  `(when (or ,@(loop for string in strings
                     collecting `(cl-ppcre:scan ,(if (constantp string)
                                                     (string-downcase string)
                                                     string)
                                                mention)))
     (v:info :robots "Matched ~s to “~a”" ',strings mention)
     ,@body
     (return-from robot-heard t)))

(defun listener-name (listener)
  (let ((robot-name (intern (concatenate 'string "ROBOT-" (string-upcase listener)))))
    (if (find-class robot-name nil)
        robot-name
        (intern (concatenate 'string (string-upcase listener) "-PERSONALITY")))))

(defmacro define-reply ((listener mode) &body body)
  (let ((listener-name (listener-name listener)))
    `(defmethod robot-heard ((robot ,listener-name)
                             speaker (mode ,(cond
                                              ((null mode) 'null)
                                              ((eql mode t) 't)
                                              (t `(eql ,(make-keyword (string mode))))))
                             heard)
       (let ((mention (string-downcase (lastcar heard))))
         ,@body
         (call-next-method)))))
