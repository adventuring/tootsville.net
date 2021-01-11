;;;; -*- lisp -*-
;;;
;;;; src/characters/robot-toot.lisp is part of Tootsville
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



(defmethod robot-handle (robot from status message))

(defmethod robot-heard ((robot robot) speaker mode heard)
  (v:warn :Robot "Unhandled speech or mode, ~:(~a~) did not hear “~a” in mode ~s"
          (class-name (class-of robot)) (lastcar heard) mode))

(defmacro define-reply ((listener mode) &body body)
  (let ((listener-name (let ((robot-name (intern (concatenate 'string "ROBOT-" (string-upcase listener)))))
                         (if (find-class robot-name nil)
                             robot-name
                             (intern (concatenate 'string (string-upcase listener) "-PERSONALITY"))))))
    `(defmethod robot-heard ((robot ,listener-name)
                             speaker (mode ,(cond
                                              ((null mode) 'null)
                                              ((eql mode t) 't)
                                              (t `(eql ,(make-keyword (string mode))))))
                             heard)
       (let ((mention (string-downcase (lastcar heard))))
         ,@body
         (call-next-method)))))

(defmethod robot-listen (robot listener-name speaker text extra-class)
  (when (eql (Toot robot) speaker)
    (return-from robot-listen nil))
  (let* ((heard (robot-has-heard robot))
         (heard-from-speaker (if heard
                                 (gethash speaker heard)))
         (mode (robot-mode robot))
         (mode-for-speaker (if mode
                               (gethash speaker mode nil)
                               nil)))
    (appendf heard-from-speaker (cons text nil))
    (when (> (length heard-from-speaker) 10)
      (setf heard-from-speaker (subseq heard-from-speaker 1)))
    (setf (gethash speaker heard) heard-from-speaker)
    (v:info :Robot "~:(~a~) ~:(~a~): “~a”" robot (Toot-name speaker) text)
    (robot-heard robot speaker mode-for-speaker heard-from-speaker)))

(defmethod robot-unicast (message (robot robot))
  (destructuring-bind (&key |from| |status| &allow-other-keys) message
    (v:info :Robots "~a Handle message from ~a" robot |from|)
    (robot-handle robot (make-keyword |from|) |status| message)))

(defmethod robot-handle (robot (from (eql :|logOK|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|avatars|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|bots|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|passport|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|startEvent|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|scoreUpdate|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|endEvent|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|gameAction|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|beam|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|earning|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|getAwardRankings|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|getApple|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|login|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|parentApproval|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|getStoreItems|)) (status (eql t)) message))

(defmethod robot-handle (robot (from (eql :|pub|)) status message)
  (destructuring-bind (&key |t| |u| |x| |id| &allow-other-keys) message
    (let ((speaker (or (find-record 'Toot :uuid |id|) (find-record 'Toot :name |u|))))
      (robot-listen robot (make-keyword (string-upcase (Toot-name (Toot robot))))
                        speaker |t| |x|))))

(defmethod robot-handle (robot (from (eql :|purchase|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|inventory|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|ping|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|getColorPalettes|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|wardrobe|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|initUserRoom|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|getAvailableHouses|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|getMailInBox|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|getMailMessage|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|sendMailMessage|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|postman|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|getUserLists|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|buddyList|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|buddyRequest|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|outOfBand|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|admin|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|serverTime|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|badgeUpdate|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|forceMove|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|reportBug|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|tootList|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|playWith|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|newScript|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|joinOK|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|wtl|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|bye|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|goToWeb|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|c|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|ayt|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|rv|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|prompt|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|quiesce|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|kick|)) (status (eql t)) message))
(defmethod robot-handle (robot (from (eql :|burgeon|)) (status (eql t)) message))

(defmethod robot-say (robot format &rest format-args)
  (let ((string (apply #'format nil format format-args)))
    (do-metronome (:one-shot-time (+ (get-universal-time) 1 (/ (length string) 32)))
      (broadcast (list :|from| "pub"
                       :|id| (Toot-UUID (Toot robot))
                       :|u| (Toot-name (Toot robot))
                       :|t| string
                       :|x| nil)
                 :near (Toot-position robot)
                 :except robot))))


(defmethod robot-go-to (robot x y z)
  (destructuring-bind (world start-x start-y start-z) (Toot-position robot)
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
           (do-metronome (:one-shot-time end-time)
        (setf (Toot-position robot) (list world x y z))))))

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

