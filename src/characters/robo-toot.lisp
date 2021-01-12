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



(defmethod robot-handle (robot from status message)
  (v:warn :robot "Unhandled message from ~a (~:[false~;true~]), robots don't know about that message source"
          from status))

(defmethod robot-heard ((robot robot) speaker mode heard)
  (v:warn :Robot "Unhandled speech or mode, ~s did not hear “~a” in mode ~s"
          robot (lastcar heard) mode))

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
  "Send MESSAGE to ROBOT individually."
  (destructuring-bind (&key |from| |status| &allow-other-keys) message
    (v:info :Robots "~a Handle message from ~a (~:[false~;true~])" robot |from| |status|)
    (robot-handle robot (make-keyword |from|) |status| message)))

(defmethod robot-unicast (message (Toot Toot))
  "Send MESSAGE to the robot piloting TOOT"
  (robot-unicast message (or (gethash (Toot-name Toot) *robots*)
                             (error "Toot is not a robot: ~s" Toot))))

(defmethod robot-handle (robot (from (eql :|logOK|)) (status (eql t)) message)
  "Handle message logOK for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|avatars|)) (status (eql t)) message)
  "Handle message avatars for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|bots|)) (status (eql t)) message)
  "Handle message bots for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|passport|)) (status (eql t)) message)
  "Handle message passport for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|startEvent|)) (status (eql t)) message)
  "Handle message startEvent for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|scoreUpdate|)) (status (eql t)) message)
  "Handle message scoreUpdate for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|endEvent|)) (status (eql t)) message)
  "Handle message endEvent for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|gameAction|)) (status (eql t)) message)
  "Handle message gameAction for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|beam|)) (status (eql t)) message)
  "Handle message beam for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|earning|)) (status (eql t)) message)
  "Handle message earning for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|getAwardRankings|)) (status (eql t)) message)
  "Handle message getAwardRankings for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|getApple|)) (status (eql t)) message)
  "Handle message getApple for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|login|)) (status (eql t)) message)
  "Handle message login for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|parentApproval|)) (status (eql t)) message)
  "Handle message parentApproval for robots — no operation.")

(defmethod robot-handle (robot (from (eql :|getStoreItems|)) (status (eql t)) message)
  "Handle message getStoreItems for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|pub|)) status message)
  "Handle message pub (public speech) for robots"
  (destructuring-bind (&key |t| |u| |x| |id| &allow-other-keys) message
    (let ((speaker (or (find-record 'Toot :uuid |id|) (find-record 'Toot :name |u|))))
      (robot-listen robot (make-keyword (string-upcase (Toot-name (Toot robot))))
                    speaker |t| |x|))))

(defmethod robot-handle (robot (from (eql :|purchase|)) (status (eql t)) message)
  "Handle message purchase for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|inventory|)) (status (eql t)) message)
  "Handle message inventory for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|ping|)) (status (eql t)) message)
  "Handle message ping for robots — no op.")

(defmethod robot-handle (robot (from (eql :|getColorPalettes|)) (status (eql t)) message)
  "Handle message getColorPalettes for robots — no op.")

(defmethod robot-handle (robot (from (eql :|wardrobe|)) (status (eql t)) message)
  "Handle message wardrobe for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|initUserRoom|)) (status (eql t)) message)
  "Handle message initUserRoom for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|getAvailableHouses|)) (status (eql t)) message)
  "Handle message getAvailableHouses for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|getMailInBox|)) (status (eql t)) message)
  "Handle message getMailInBox for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|getMailMessage|)) (status (eql t)) message)
  "Handle message getMailMessage for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|sendMailMessage|)) (status (eql t)) message)
  "Handle message sendMailMessage for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|postman|)) (status (eql t)) message)
  "Handle message postman for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|getUserLists|)) (status (eql t)) message)
  "Handle message getUserLists for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|buddyList|)) (status (eql t)) message)
  "Handle message buddyList for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|buddyRequest|)) (status (eql t)) message)
  "Handle message buddyRequest for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|outOfBand|)) (status (eql t)) message)
  "Handle message outOfBand for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|admin|)) (status (eql t)) message)
  "Handle message admin for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|serverTime|)) (status (eql t)) message)
  "Handle message serverTime for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|badgeUpdate|)) (status (eql t)) message)
  "Handle message badgeUpdate for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|forceMove|)) (status (eql t)) message)
  "Handle message forceMove for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|reportBug|)) (status (eql t)) message)
  "Handle message reportBug for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|tootList|)) (status (eql t)) message)
  "Handle message tootList for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|playWith|)) (status (eql t)) message)
  "Handle message playWith for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|newScript|)) (status (eql t)) message)
  "Handle message newScript for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|joinOK|)) (status (eql t)) message)
  "Handle message joinOK for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|wtl|)) (status (eql t)) message)
  "Handle message wtl for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|bye|)) (status (eql t)) message)
  "Handle message bye for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|goToWeb|)) (status (eql t)) message)
  "Handle message goToWeb for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|c|)) (status (eql t)) message)
  "Handle message c for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|ayt|)) (status (eql t)) message)
  "Handle message ayt for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|rv|)) (status (eql t)) message)
  "Handle message rv for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|prompt|)) (status (eql t)) message)
  "Handle message prompt for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|quiesce|)) (status (eql t)) message)
  "Handle message quiesce for robots; with status=true, this is a no-op.")

(defmethod robot-handle (robot (from (eql :|quiesce|)) (status null) message)
  "Handle message quiesce for robots.

With status=false, this is a demand to quiesce oneself."
  (let ((wtl (robot-course-wtl robot)))
    (infinity-quiesce (list :|latitude| (getf wtl :|latitude|)
                            :|longitude| (getf wtl :|longitude|)
                            :|altitude| (getf wtl :|altitude|)
                            :|world| (getf wtl :|world|)
                            :|wtl| wtl
                            :|d3| (make-hash-table)
                            :|emotion| "" ; TODO
                            )
                      (Toot robot) nil)))

(defmethod robot-handle (robot (from (eql :|kick|)) (status (eql t)) message)
  "Handle message kick for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

(defmethod robot-handle (robot (from (eql :|burgeon|)) (status (eql t)) message)
  "Handle message burgeon for robots"
  (v:warn :robot "UNIMPLEMENTED: message ~a ignored" message))

