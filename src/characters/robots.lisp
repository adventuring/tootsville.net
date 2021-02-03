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

(defgeneric find-robot (identifier)
  (:documentation "Find a robot based on IDENTIFIER.

IDENTIFIER may be a name string or Toot object."))

(defmethod find-robot ((name string))
  (gethash name *robots*))

(defmethod find-robot ((Toot Toot))
  (gethash (Toot-name Toot) *robots*))

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
   (heard :initform (make-hash-table :test 'equalp :synchronized t) :accessor robot-has-heard)
   (mode :accessor robot-mode :initform (make-hash-table :test 'equalp))
   (walk-the-line :accessor robot-course))
  (:documentation "An in-game robot character"))

(defun robotp (user)
  "Is USER a robot?

USER may be a robot or a Toot that is controlled by a robot."
  (or (typep user 'robot)
      (and (typep user 'Toot)
           (find-robot user))))

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

(defgeneric world (thing)
  (:documentation "The keyword name of the world on which THING is."))

(defgeneric latitude (thing)
  (:documentation "The latitude of THING"))

(defgeneric longitude (thing)
  (:documentation "The longitude of THING"))

(defgeneric altitude (thing)
  (:documentation "The altitude of THING"))

(defmethod world ((cons cons))
  (first cons))

(defmethod latitude ((cons cons))
  (second cons))

(defmethod longitude ((cons cons))
  (third cons))

(defmethod altitude ((cons cons))
  (fourth cons))

(defmethod world ((null null)) 
  :chor)
(defmethod latitude ((null null))
  0)
(defmethod longitude ((null null))
  0)
(defmethod altitude ((null null))
  0)

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

(defun Toot-quiesced-data (Toot)
  (find-record 'Toot-quiesced :Toot (Toot-UUID Toot)))

(defgeneric wtl-course (thing)
  (:documentation "The course of THING's current movement in WTL form.

See `INFINITY-WTL' for a discussion of this format."))

(defmethod wtl-course ((Toot Toot))
  "Get the walk-the-line course of Toot"
  (if (robotp Toot)
      (robot-course (find-robot Toot))
      (parse-wtl-for-robot (jonathan.decode:parse
                            (Toot-quiesced-wtl
                             (Toot-quiesced-data Toot))))))

(defgeneric nearp (thing place)
  (:documentation "Is THING near to PLACE?

``Near,''  in this  case, means  ``close  enough to  observe actions  at
PLACE.''  Network events  are not  propagated to  observers who  are not
NEARP to the event being observed."))

(defmethod nearp ((robot robot) place)
 (nearp (robot-position robot) place))

(defmethod nearp (place (robot robot))
 (nearp place (robot-position robot)))

(defun robot-broadcast (message near &key except)
  "Broadcast MESSAGE to all robots near NEAR, except robot EXCEPT."
  (v:info :robots "Robots hear message: ~s" message)
  (dolist (robot (hash-table-values *robots*))
    (when (and (nearp robot near) (not (equalp except robot)))
      (robot-unicast message robot))))

(defmethod user-stream ((robot robot))
  "Robots do not have a user stream."
  nil)

(defmethod current-position ((robot robot))
  (current-position (wtl-course robot)))

(defun robot-position (robot)
  (list (world robot) (latitude robot) (longitude robot) (altitude robot)))

(defun parse-wtl-for-robot (wtl)
  "Parse the WTL JSON into a WTL-Course structure 

XXX this is basically a weak duplicate of `PARSE-WTL-COURSE' "
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
                       :start-point (destructuring-bind (&key |x| |y| |z| &allow-other-keys) |startPoint|
                                      (list |x| |y| |z|))
                       :end-point (destructuring-bind (&key |x| |y| |z| &allow-other-keys) |endPoint|
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
                 latitude longitude altitude world facing)
        wtl
        (destructuring-bind (x₁ y₁ z₁) start-point
          (destructuring-bind (x₂ y₂ z₂) end-point
            (list :|facing| facing
                  :|course| (list :|speed| speed
                                  :|startTime| start-time
                                  :|endTime| end-time
                                  :|startPoint| (list :|x| x₁ :|y| y₁ :|z| z₁)
                                  :|endPoint| (list :|x| x₂ :|y| y₂ :|z| z₂)                 
                                  :|latitude| latitude
                                  :|longitude| longitude
                                  :|altitude| altitude
                                  :|world| world)))))))

(defun relative-facing (x₁ z₁ x₂ z₂)
  "Compute the direction to face if traveling from X1,Z1 to X2,Z2

Returns the angle in radians"
  (if (and (= x₁ x₂) (= z₁ z₂))
      nil
      (atan (/ (- z₂ z₁) (- x₂ x₁)))))

(defmethod robot-go-to (robot x y z &optional (speed 0.1))
  "Plot a course for ROBOT to walk to X,Y,Z in their current sector."
  (let* ((course (robot-course robot))
         (start-point (current-position course))
         (x₁ (first start-point))
         (y₁ (second start-point))
         (z₁ (third start-point))
         (facing (or (relative-facing x₁ z₁ x z)
                     (wtl-course-facing course)))
         (distance (distance x₁ y₁ z₁ x y z))
         (start-time (timestamp-to-unix (now)))
         (end-time (+ start-time (* 1000 (/ distance (/ 1 speed))))))
    (broadcast (list :|from| "wtl"
                     :|status| t
                     :|course| (list :|speed| speed
                                     :|startTime| start-time
                                     :|startPoint| (list :|x| x₁
                                                         :|y| y₁
                                                         :|z| z₁)
                                     :|endTime| end-time
                                     :|endPoint| (list :|x| x
                                                       :|y| y
                                                       :|z| z))
                     :|facing| facing
                     :|u| (Toot-UUID (Toot robot))
                     :|n| (Toot-name (Toot robot)))
               :except robot)
    (setf (robot-course robot) (make-wtl-course
                                :start-time start-time
                                :end-time end-time
                                :start-point (list x₁ y₁ z₁)
                                :end-point (list x y z)
                                :latitude (wtl-course-latitude (robot-course robot))
                                :longitude (wtl-course-longitude (robot-course robot))
                                :altitude (wtl-course-altitude (robot-course robot))
                                :world (wtl-course-world (robot-course robot))
                                :speed 0.1))))

(defmacro robot-set-mode (mode)
  `(setf (gethash speaker (robot-mode robot)) ,(make-keyword (string mode))))

(defmacro robot-match ((&rest strings) &body body)
  `(when (or ,@(loop for string in strings
                     collecting `(cl-ppcre:scan ,(if (constantp string)
                                                     (string-downcase string)
                                                     string)
                                                mention)))
     ,@body
     (return-from robot-heard t)))

(defun listener-name (listener)
  (let ((robot-name (intern (concatenate 'string "ROBOT-" (string-upcase listener)))))
    (if (find-class robot-name nil)
        robot-name
        (intern (concatenate 'string (string-upcase listener) "-PERSONALITY")))))

(defmacro define-reply ((listener mode) &body body)
  (let ((listener-name (listener-name listener))
        (docstring (if (stringp (first body)) (first body) nil)))
    `(defmethod robot-heard ((robot ,listener-name)
                             speaker (mode ,(cond
                                              ((null mode) 'null)
                                              ((eql mode t) 't)
                                              (t `(eql ,(make-keyword (string mode))))))
                             heard)
       ,docstring
       (let ((mention (string-downcase (lastcar heard))))
         ,@body
         (call-next-method)))))
