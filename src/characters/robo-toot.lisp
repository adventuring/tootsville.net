;;;; -*- lisp -*-
;;;
;;;; src/characters/robo-toot.lisp is part of Tootsville
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



(defclass robo-Toot (robot) ())

(defclass robo-Toot-context ()
  ((heard :initform (make-hash-table :test 'equalp) :accessor robo-Toot-has-heard)
   (mode :accessor robo-Toot-mode :initform (make-hash-table :test 'equalp))))

(defmethod robo-Toot-handle (robo-Toot from status message))

(defmethod robo-Toot-heard ((robo-Toot robot) listener-name speaker mode heard)
  (v:warn :Robot "Unhandled speech or mode, ~:(~a~) did not hear “~a” in mode ~s"
        listener-name (lastcar heard) mode))

(defmethod robo-Toot-listen (robo-Toot listener-name speaker text extra-class)
  (when (string-equal listener-name (Toot-name speaker))
    (return-from robo-Toot-listen nil))
  (let* ((robo-Toot-context (context robo-Toot))
         (heard (robo-Toot-has-heard robo-Toot-context))
         (heard-from-speaker (if heard
                                 (gethash speaker heard)))
         (mode (robo-Toot-mode robo-Toot-context))
         (mode-for-speaker (if mode
                               (gethash speaker mode nil)
                               nil)))
    (appendf heard-from-speaker (cons text nil))
    (when (> (length heard-from-speaker) 10)
      (setf heard-from-speaker (subseq heard-from-speaker 1)))
    (setf (gethash speaker heard) heard-from-speaker)
    (v:info :Robot "[~:(~a~)] ~:(~a~): “~a”" listener-name (Toot-name speaker) text)
    (robo-Toot-heard robo-Toot listener-name speaker mode-for-speaker heard-from-speaker)))

(defmethod robot-unicast (message (robo-Toot robot))
  (destructuring-bind (&key |from| |status| &allow-other-keys) message
    (v:info :Robots "[~a] Handle message from ~a" (Toot-name (Toot robo-Toot)) |from|)
    (robo-Toot-handle robo-Toot (make-keyword |from|) |status| message)))

(defmethod robo-Toot-handle (robo-Toot (from (eql :|logOK|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|avatars|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|bots|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|passport|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|startEvent|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|scoreUpdate|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|endEvent|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|gameAction|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|beam|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|earning|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|getAwardRankings|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|getApple|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|login|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|parentApproval|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|getStoreItems|)) status message))

(defmethod robo-Toot-handle (robo-Toot (from (eql :|pub|)) status message)
  (destructuring-bind (&key |t| |u| |x| |id| &allow-other-keys) message
    (let ((speaker (or (find-record 'Toot :uuid |id|) (find-record 'Toot :name |u|))))
      (robo-Toot-listen robo-Toot (make-keyword (string-upcase (Toot-name (Toot robo-Toot))))
                        speaker |t| |x|))))

(defmethod robo-Toot-handle (robo-Toot (from (eql :|purchase|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|inventory|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|ping|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|getColorPalettes|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|wardrobe|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|initUserRoom|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|getAvailableHouses|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|getMailInBox|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|getMailMessage|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|sendMailMessage|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|postman|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|getUserLists|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|buddyList|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|buddyRequest|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|outOfBand|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|admin|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|serverTime|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|badgeUpdate|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|forceMove|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|reportBug|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|tootList|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|playWith|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|newScript|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|joinOK|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|wtl|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|bye|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|goToWeb|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|c|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|ayt|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|rv|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|prompt|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|quiesce|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|kick|)) status message))
(defmethod robo-Toot-handle (robo-Toot (from (eql :|burgeon|)) status message))

(defmethod initialize-robo-Toot (robot)
  (let* ((quiesced (ignore-errors (find-record 'Toot-quiesced :Toot (Toot-uuid (Toot robot)))))
         (wtl (jonathan.decode:parse (or (and quiesced (Toot-quiesced-wtl quiesced))
                                         (random-start-wtl-for-Toot)))))
    (setf (context robot) (make-instance 'robo-Toot-context)
          (Toot-position robot) (destructuring-bind (&key |course| |facing|) wtl
                                  (declare (ignore |facing|))
                                  (let ((start-point (getf |course| :|startPoint|)))
                                    (destructuring-bind (&key |x| |y| |z|) start-point
                                        (list :chor |x| |y| |z|))))))
  t)

(defmethod robo-Toot-say (robot format &rest format-args)
  (broadcast (list :|from| "pub"
                   :|id| (Toot-UUID (Toot robot))
                   :|u| (Toot-name (Toot robot))
                   :|t| (apply #'format nil format format-args)
                   :|x| nil)
             :near (Toot-position robot)
             :except robot))

(defmethod robo-Toot-go-to (robot x y z)
  (destructuring-bind (world start-x start-y start-z) (Toot-position robot)
    (let ((facing 0)) ; FIXME
      (broadcast (list :|from| "wtl"
                       :|status| t
                       :|course| (list :|speed| 0.1
                                       :|startTime| (timestamp-to-unix (now))
                                       :|startPoint| (list :|x| start-x
                                                           :|y| start-y
                                                           :|z| start-z)
                                       :|endPoint| (list :|x| x
                                                         :|y| y
                                                         :|z| z))
                       :|facing| facing
                       :|u| (Toot-UUID (Toot robot))
                       :|n| (Toot-name (Toot robot))))
      ;; FIXME: Adjust over time
      (setf (Toot-position robot) (list world x y z)))))

(defmacro robo-Toot-heard* ((listener mode) &body body)
  `(defmethod robo-Toot-heard ((robot robot) (listener-name (eql ,(make-keyword (string listener))))
                               speaker (mode ,(cond
                                                ((null mode) 'null)
                                                ((eql mode t) 't)
                                                (t `(eql ,(make-keyword (string mode))))))
                               heard)
     ,@body
     (call-next-method)))

(defmacro robo-set-mode (mode)
  `(setf (gethash speaker (robo-Toot-mode (context robot))) ,(make-keyword (string mode))))

(defmacro robo-match ((&rest strings) &body body)
  `(let ((mention (string-downcase (lastcar heard))))
     (when (or ,@(loop for string in strings
                       collecting `(cl-ppcre:scan ,(string-downcase string) mention)))
       (v:info :robots "Matched ~s to “~a”" ',strings mention)
       ,@body
       (return-from robo-toot-heard t))))

