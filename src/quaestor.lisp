;;;; -*- lisp -*-
;;;
;;;; src/quaestor.lisp is part of Tootsville
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



(defun quaestor-start-event/fountain% (item Toot)
  (fountain-reject-as-already-done (item-uuid item))
  (quaestor-start-general item :fountain Toot))

(defun quaestor-start-event/item% (item Toot)
  "Start an event whose source is ITEM, for TOOT.

See `INFINITY-START-EVENT' for  an overview. This function  starts a new
quaestor event  whose source is ITEM,  which might be a  fountain, among
other things."
  (quaestor-start-event/item-template% (item-template item) item Toot))

(defun quaestor-start-event/minigame% (item Toot)
  ;; TODO check for existing unfinished play of that minigame
  (start-minigame-event item Toot))

(defun start-minigame-event (item Toot)
  (let ((event (make-record 'quaestor-event
                            :uuid (uuid:make-v4-uuid)
                            :source (item-uuid item)
                            :started-by (Toot-uuid Toot)
                            :completedp nil
                            :ended-at nil
                            :peanuts 0
                            :fairy-dust 0
                            :item nil
                            :score 0
                            :medal nil)))
    (destructuring-bind (filename function score-scalar)
        (split-sequence #\# (item-attributes item))
      (declare (ignore score-scalar))
      (list 202 (list :|from| "startEvent"
                      :|handler| "minigame"
                      :|status| t
                      :|eventID| (quaestor-event-uuid event)
                      :|filename| filename
                      :|function| function
                      :|asVersion| "html5")))))

(defun quaestor-start-event/vitem% (item Toot)
  (start-vitem-gifting-event item Toot))

(defun start-vitem-gifting-event (item Toot)
 (let ((event (make-record 'quaestor-event
                            :uuid (uuid:make-v4-uuid)
                            :source (item-uuid item)
                            :started-by (Toot-uuid Toot)
                            :completedp nil
                            :ended-at nil
                            :peanuts 0
                            :fairy-dust 0
                            :item nil
                            :score 0
                            :medal nil))
       (vitem (or (when-let (id (parse-integer (item-attributes item)))
                    (find-record 'item-template :id id))
                  (find-reference item :template))))
    
      (list 202 (list :|from| "startEvent"
                      :|handler| "vitem"
                      :|status| t
                      :|eventID| (quaestor-event-uuid event)
                      :|vitem| (item-template-info vitem)))))

(defun quaestor-start-event/shop% (item Toot)
  (start-purchase-event item Toot))

(defun start-purchase-event (store-item Toot)
  "Start an event for TOOT to purchase STORE-ITEM"
  (let ((event (make-record 'quaestor-event
                            :uuid (uuid:make-v4-uuid)
                            :source (store-item-uuid store-item)
                            :started-by (Toot-uuid Toot)
                            :started-at (now)
                            :completedp nil
                            :ended-at nil
                            :peanuts 0
                            :fairy-dust 0
                            :item nil
                            :score 0
                            :medal nil)))
    (list 202 (list :|from| "startEvent"
                    :|handler| "shop"
                    :|item| (item-info store-item)
                    :|eventID| (quaestor-event-UUID event)))))

(defun Toot-can-afford-p (Toot store-item)
  "Whether TOOT can afford STORE-ITEM"
  (<= (store-item-price store-item)
      (Toot-peanuts Toot)))

(defun quaestor-reject-cannot-afford% (start-end event-id store-item)
  (list 412 (list :|from| start-end
                  :|status| :false
                  :|eventID| event-id
                  :|err| "cost"
                  :|error| (format nil "You cannot afford ~:d 🥜."
                                   (store-item-price store-item)))))

(defun reject-event-not-found% ()
  "Send a rejection for an event not found."
  (list 404 (list :|from| "startEvent"
                  :|status| :false
                  :|err| "eventType.notFound"
                  :|error| "No such event can be started")))

(defun quaestor-start-event (item &optional (Toot *Toot*))
  "TOOT wants to start an event triggered by SOURCE

See `INFINITY-START-EVENT' for details of the procedure."
  (let ((item (ensure-item item)))
    (case (item-effect item)
      (:fountain (quaestor-start-event/fountain% item Toot))
      (otherwise (error 'unimplemented)))))

(defun quaestor-complete-event (event score &optional medal)
  "Complete EVENT with SCORE and MEDAL earned.

See `INFINITY-END-EVENT' for details of the procedure.

If  EVENT  is a  purchase,  then  purchase  the associated  store  item;
otherwise, perform  whatever specific event side-effects  are related to
the item template."
  (case (quaestor-event-kind event)
    (:fountain (quaestor-complete-event/fountain% event))
    (:minigame (quaestor-complete-event/minigame% event score medal))
    (:vitem (quaestor-complete-event/vitem% event))
    (:shop (quaestor-complete-event/shop% event))
    ((:nil nil) (list 204 nil))
    (otherwise (error 'unimplemented))))

(defun quaestor-cancel-event (event)
  "Cancel EVENT.

See `INFINITY-END-EVENT' for details of the procedure."
  (v:info :end-event "Cancel event ~s" event)
  (setf (quaestor-event-completedp event) nil
        (quaestor-event-ended-at event) (now)
        (quaestor-event-peanuts event) 0
        (quaestor-event-fairy-dust event) 0
        (quaestor-event-item event) nil
        (quaestor-event-score event) 0
        (quaestor-event-medal event) nil)
  (save-record event)
  (list 200 (list :|from| "endEvent"
                  :|status| t
                  :|ended| (quaestor-event-uuid event)
                  :|canceled| t)))



(defgeneric quaestor-start-event/item-template% (template-id item Toot)
  (:documentation "TOOT starts an event with ITEM of type TEMPLATE-ID."))

(defgeneric quaestor-complete-event/item-template% (template-id event score medal)
  (:documentation "End EVENT, with related item of type TEMPLATE-ID, with SCORE and MEDAL."))

(defmethod quaestor-start-event/item-template% ((template-id t) item Toot)
  "The default for starting an event with a moniker that is not recognized.
 
Reports an error to the client."
  (list 404 (list :|from| "startEvent"
                  :|status| :false
                  :|err| "eventType.notFound"
                  :|error| "No such event can be started")))

(defmacro without-medal ((medal) &body body)
  "Assert that MEDAL is null and run BODY, or return a medal.notFound error."
  `(if (null ,medal)
       (progn ,@body)
       (list 404 (list :|from| "endEvent"
                       :|status| :false
                       :|err| "medal.notFound"
                       :|error| "No such medal can be earned for this event"))))

(defmacro with-score-in-range ((score min &optional max) &body body)
  "Assert that SCORE is in range of MIN (to MAX, if any) and run BODY, or return a score.range error."
  `(if ,(if max
            `(< ,min ,score ,max)
            `(< ,min ,score))
       ,@body
       (list 404 (list :|from| "endEvent"
                       :|status| :false
                       :|err| "score.range"
                       :|error| "The score reported is out of range"))))

(defmethod quaestor-complete-event/item-template% ((template-id t) event score medal)
  "Complete an event: default behavior. Terminate EVENT ignoring SCORE. MEDAL should be null.

This  is  the  fallback  behavior  if no  more  specific  handler  (e.g.
a  purchase  of a  store  item  or  something  specialized on  the  item
TEMPLATE-ID  like a  magic  fountain), which  basically  ends the  event
without any peanuts, fairy dust, or items being awarded (nor charged).
"
  (without-medal (medal)
    (setf (quaestor-event-ended-at event) (now)
          (quaestor-event-completedp event) t
          (quaestor-event-peanuts event) 0
          (quaestor-event-fairy-dust event) 0
          (quaestor-event-item event) nil
          (quaestor-event-score event) score)
    (save-record event)
    (list 200 (list :|from| "endEvent"
                    :|status| t
                    :|ended| (quaestor-event-uuid event)
                    :|peanuts| 0
                    :|fairyDust| 0
                    :|totalPeanuts| (Toot-peanuts (quaestor-event-started-by event))
                    :|totalFairyDust| (Toot-fairy-dust (quaestor-event-started-by event))))))



(defun Toot-peanuts (Toot)
  "Compute the total balance of peanuts that TOOT has earned over the course of the game."
  (or (cadar (db-select-all 
              :friendly 
              (format nil "select sum(peanuts) from quaestor_events
where started_by='~a' and completedp = 'Y'"
                      (uuid-to-base64 (etypecase Toot
                                        (Toot (Toot-uuid Toot))
                                        (uuid:uuid Toot))))))
      0))

(defun Toot-fairy-dust (Toot)
  "Compute the total balance of fairy dust that TOOT has earned over the course of the game."
  (or (cadar (db-select-all 
              :friendly 
              (format nil "select sum(fairy_dust) from quaestor_events 
where started_by='~a' and completedp = 'Y'"
                      (uuid-to-base64 (etypecase Toot
                                        (Toot (Toot-uuid Toot))
                                        (uuid:uuid Toot))))))
      0))



(defun quaestor-start-general (item kind Toot)
  "Start a general event sourced on ITEM for TOOT."
  (let ((event (make-record 'quaestor-event
                            :uuid (uuid:make-v4-uuid)
                            :source (item-uuid item)
                            :started-by (Toot-uuid Toot)
                            :started-at (now)
                            :completedp nil
                            :ended-at nil
                            :peanuts 0
                            :fairy-dust 0
                            :item nil
                            :kind kind
                            :score 0
                            :medal nil)))
    (list 202 (list :|from| "startEvent"
                    :|status| t
                    :|eventID| (quaestor-event-uuid event)
                    :|handler| kind))))

(defun fountain-duplicate-p (event-source)
  "Returns generalized true if EVENT-SOURCE has happened already on the same Tootsville day as today."
  (let ((prior (find-records-by-sql 
                'quaestor-event
                (format nil "SELECT * FROM quaestor_events
WHERE source='~a' AND ended_at > CURRENT_TIMESTAMP - INTERVAL 18 HOUR
ORDER BY ended_at DESC LIMIT 1"
                        (uuid-to-base64 event-source)))))
    (when prior
      (multiple-value-bind (sec_ min_ hour_ last-day last-month)
          (choerogryllum:decode*-universal-time
           (timestamp-to-universal (quaestor-event-ended-at (first prior))))
        (declare (ignore sec_ min_ hour_))
        (multiple-value-bind (sec_ min_ hour_ day month)
            (choerogryllum:decode*-universal-time (get-universal-time*))
          (declare (ignore sec_ min_ hour_))
          (and (= day last-day) (= month last-month)))))))

(defun fountain-reject-as-already-done (moniker)
  "Send a rejection to an attempt to end a fountain event identified by MONIKER.

Tells the player to make a wish again tomorrow."
  (list 412 (list :|from| "endEvent"
                  :|status| :false
                  :|err| "event.alreadyDone"
                  :|alreadyDone| t
                  :|moniker| moniker
                  :|error| "Make a wish again tomorrow")))

(defun compute-fountain-peanuts ()
  (multiple-value-bind (sec_ min_ hr_ day_ month_ year_ day-of-week)
      (Choerogryllum:decode*-universal-time (get-universal-time*))
    (declare (ignore sec_ min_ hr_ day_ month_ year_))
    (+ 25 (mod (* 10 day-of-week) 75))))

(defun compute-fountain-random-fairy-dust ()
  "How much fairy dust is obtained from the fountain?

Usually nothing, with a 1% change of being a random amount up to 10."
  (if (zerop (random 100))
      (random 10)
      0))



(defun quaestor-complete-event/vitem% (event)
  (let* ((item (find-record 'item :uuid (quaestor-event-source event)))
         (vitem-id (or (parse-integer (item-attributes item))
                       (item-template item)))
         (Toot (find-record 'Toot :uuid (quaestor-event-started-by event)))
         (granted (grant-item vitem-id Toot)))
    (list 200 (list :|from| "endEvent"
                    :|status| t
                    :|ended| (quaestor-event-uuid event)
                    :|peanuts| 0
                    :|fairyDust| 0
                    :|totalPeanuts| (Toot-peanuts Toot)
                    :|totalFairyDust| (Toot-fairy-dust Toot)
                    :|item| (item-info granted)))))

(defun quaestor-complete-event/shop% (event)
  (error 'unimplemented))

(defun quaestor-complete-event/minigame% (event score medal)
  (error 'unimplemented))

(defun quaestor-complete-event/fountain% (event)
  "End the Fountain event."
  (if (fountain-duplicate-p (quaestor-event-source event))
      (progn
        (quaestor-cancel-event event)
        (fountain-reject-as-already-done (quaestor-event-uuid event)))
      (let ((peanuts (compute-fountain-peanuts))
            (fairy-dust (compute-fountain-random-fairy-dust)))
        (setf (quaestor-event-ended-at event) (now)
              (quaestor-event-completedp event) t
              (quaestor-event-peanuts event) peanuts
              (quaestor-event-fairy-dust event) fairy-dust
              (quaestor-event-score event) 1)
        (save-record event)
        (list 200 (list :|from| "endEvent"
                        :|status| t
                        :|ended| (quaestor-event-uuid event)
                        :|peanuts| peanuts
                        :|fairydust| fairy-dust
                        :|totalPeanuts| (Toot-peanuts (quaestor-event-started-by event))
                        :|totalFairyDust| (Toot-fairy-dust (quaestor-event-started-by event)))))))



(defun quaestor-new-Toot (Toot)
  "Give the new TOOT their starting peanuts."
  (make-record 'quaestor-event
               :uuid (uuid:make-v4-uuid)
               :kind "new-game"
               :source (Toot-UUID Toot)
               :started-by (Toot-uuid Toot)
               :started-at (now)
               :completedp t
               :ended-at (now)
               :peanuts 100
               :fairy-dust 0
               :score 0
               :item nil
               :medal nil))
