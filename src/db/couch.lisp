;;;; -*- lisp -*-
;;;
;;;; src/db/couch.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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

(declaim (optimize (speed 3)))

(defun connect-mixer ()
  (setf clouchdb:*couchdb*
        (let ((conf (db-config :mixer)))
          (v:info :mixer "Connecting to Mixer at ~a" (extract conf :host))
          (with-timeout (2)
            (clouchdb:set-connection :host (extract conf :host)
                                     :port (or (extract conf :port)
                                               "5984")
                                     :user (extract conf :player :name)
                                     :password (extract conf :player :password)
                                     :protocol "http"
                                     :name "tootsville/5"
                                     :document-fetch-fn #'identity
                                     :document-update-fn #'identity))))
  (if-let (motd (ignore-errors (clouchdb:get-document "tootsville%2f5/motd")))
    (v:info :mixer "MotD from Mixer: ~a"
            (cdr (assoc :|motd| motd)))
    (v:info :mixer "Connected to Mixer; no MotD"))
  clouchdb::*couchdb*)


(defstruct gossip-initiation
  uuid
  answer)

(defmethod database-for ((type (eql 'gossip-initiation)))
  (list :couch :gossip-exchange))

(defmethod db-table-for ((type (eql 'gossip-initiation)))
  "gossip-exchange")

(defmethod id-column-for ((type (eql 'gossip-initiation)))
  :|uuid|)

(defmethod %to-json ((object gossip-initiation))
  (%to-json (to-plist object)))

(defvar *couch-rev-cache* (make-hash-table :test 'eq))

(defmethod to-plist ((object gossip-initiation))
  (list :|uuid| (gossip-initiation-uuid object)
        :|answer| (gossip-initiation-answer object)))

(defmethod to-json-alist ((object gossip-initiation))
  (list (list :|uuid| (format nil "~a" (gossip-initiation-uuid object)))
        (list :|answer| (gossip-initiation-answer object))))

(defmethod destroy-record ((init gossip-initiation))
  (clouchdb:delete-document (gossip-initiation-uri init))
  (remhash init *couch-rev-cache*))

(defun put-couchdb-document-with-revision (init)
  (setf (gethash init *couch-rev-cache*)
        (assoc-value (clouchdb:put-document
                      (cons (cons :|_rev|
                                  (gethash init *couch-rev-cache*))
                            (to-json-alist init))
                      :id (gossip-initiation-uri init))
                     :|rev|))
  init)

(defun put-couchdb-new-document (init)
  (setf (gethash init *couch-rev-cache*)
        (assoc-value (clouchdb:put-document (to-json-alist init)
                                            :id (gossip-initiation-uri init))
                     :|rev|))
  init)

(defmethod save-record ((init gossip-initiation))
  (cond
    ((null (gossip-initiation-uuid init))
     (setf (gossip-initiation-uuid init) (uuid:make-v4-uuid))
     (put-couchdb-new-document init))
    ((gethash init *couch-rev-cache*)
     (put-couchdb-document-with-revision init))
    (t (put-couchdb-new-document init)))
  (to-json init))

(defmethod load-record ((class (eql 'gossip-initiation)) alist)
  (let ((init (make-gossip-initiation :uuid (assoc-value alist :|uuid|)
                                      :answer (assoc-value alist :|answer|))))
    (setf (gethash init *couch-rev-cache*) (assoc-value alist :|_rev|))))

(defmethod make-record ((class (eql 'gossip-initiation)) &rest plist)
  (let ((init (apply #'make-gossip-initiation plist)))
    (save-record init)
    init))

(defmethod invalidate-cache ((init gossip-initiation))
  (declare (ignore init))
  ;; no op
  )

(defmethod find-record ((class (eql 'gossip-initiation))
                        &key uuid)
  (if uuid
      (let* ((doc (clouchdb:get-document (gossip-initiation-uri uuid)
                                         :if-missing nil))
             (uuid$ (assoc-value doc :|uuid|))
             (answer$ (assoc-value doc :|answer|)))
        (when (and uuid$
                   (stringp uuid$)
                   (= 36 (length uuid$))
                   answer$
                   (stringp answer$))
          (make-gossip-initiation
           :uuid (uuid:make-uuid-from-string uuid$)
           :answer (jonathan.decode:parse answer$))))
      (error "Must provide UUID")))

(defmethod find-records ((class (eql 'gossip-initiation))
                         &key offeror
                              (answeror nil answerorp)
                              (answer nil answerp))
  (cond
    ((< 1 (+ (if offeror 1 0)
             (if answerorp 1 0)
             (if answerp 1 0)))
     (error "Can't search that way: ~
supply exactly one of OFFEROR, ANSWEROR, ANSWER"))
    (offeror
     (mapcar (the function (curry #'load-record 'gossip-initiation))
             (clouchdb:invoke-view
              "offeror" "offeror"
              :key (uuid-to-uri (person-uuid offeror)))))
    ((and answerp
          (null answer))
     (mapcar (the function (curry #'load-record 'gossip-initiation))
             (clouchdb:invoke-view
              "pending" "pending")))
    ((and answerorp
          (null answeror))
     (mapcar (the function (curry #'load-record 'gossip-initiation))
             (clouchdb:invoke-view
              "unanswered" "unanswered")))
    (t (clouchdb:all-docs-by-seq))))


(defun couch-get-document (uri)
  (handler-case
      (clouchdb:get-document uri)
    (clouchdb:document-missing (c)
      (declare (ignore c))
      nil)))


(defun find-active-Toot-for-user (&optional (user *user*))
  (when user
    (when-let (uuid (couch-get-document
                     (concatenate 'string "/player/link/toot/"
                                  (uuid:format-as-urn nil (person-uuid user)))))
      (find-record 'toot :uuid uuid))))

(defun link-active-Toot-to-user (Toot &optional (user *user*))
  (declare (ignore Toot user))
  (error 'unimplemented))
