;;;; -*- lisp -*-
;;;
;;;; src/errors.lisp is part of Tootsville
;;;
;;;; Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2024 The
;;;; Corporation for Inter-World Tourism and Adventuring (CIWTA.org); © 2024-2025
;;;; Interworldly Adventuring, LLC
;;;
;;;; This program is Free Software: you can redistribute it and/or
;;;; modify it under the terms of the GNU Affero General Public License
;;;; as published by the Free Software Foundation; either version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;; Affero General Public License for more details.
;;;
;;;; You should have received a copy of the GNU Affero General Public
;;;; License along with this program. If not, see
;;;; <https://www.gnu.org/licenses/>.

(in-package :Tootsville)

;;; Error classes for Tootsville server

(define-condition tootsville-error (error)
  ((message :initarg :message :reader error-message :initform "Unknown Tootsville error")
   (code :initarg :code :reader error-code :initform :unknown)
   (timestamp :initarg :timestamp :reader error-timestamp :initform (get-universal-time))
   (context :initarg :context :reader error-context :initform nil))
  (:documentation "Base error class for all Tootsville errors")
  (:report (lambda (condition stream)
             (format stream "Tootsville Error [~a]: ~a~@[ (Context: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-context condition)))))

(define-condition authentication-error (tootsville-error)
  ((user-id :initarg :user-id :reader error-user-id :initform nil)
   (provider :initarg :provider :reader error-provider :initform nil)
   (attempts :initarg :attempts :reader error-attempts :initform 0))
  (:documentation "Error raised during authentication failures")
  (:report (lambda (condition stream)
             (format stream "Authentication Error [~a]: ~a~@[ (User: ~a, Provider: ~a, Attempts: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-user-id condition)
                     (error-provider condition)
                     (error-attempts condition)))))

(define-condition websocket-error (tootsville-error)
  ((client-id :initarg :client-id :reader error-client-id :initform nil)
   (connection-id :initarg :connection-id :reader error-connection-id :initform nil)
   (message-type :initarg :message-type :reader error-message-type :initform nil))
  (:documentation "Error raised during WebSocket operations")
  (:report (lambda (condition stream)
             (format stream "WebSocket Error [~a]: ~a~@[ (Client: ~a, Connection: ~a, Type: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-client-id condition)
                     (error-connection-id condition)
                     (error-message-type condition)))))

(define-condition database-error (tootsville-error)
  ((table :initarg :table :reader error-table :initform nil)
   (operation :initarg :operation :reader error-operation :initform nil)
   (query :initarg :query :reader error-query :initform nil))
  (:documentation "Error raised during database operations")
  (:report (lambda (condition stream)
             (format stream "Database Error [~a]: ~a~@[ (Table: ~a, Operation: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-table condition)
                     (error-operation condition)))))

(define-condition user-error (tootsville-error)
  ((user-id :initarg :user-id :reader error-user-id :initform nil)
   (action :initarg :action :reader error-action :initform nil)
   (permissions :initarg :permissions :reader error-permissions :initform nil))
  (:documentation "Error raised during user operations")
  (:report (lambda (condition stream)
             (format stream "User Error [~a]: ~a~@[ (User: ~a, Action: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-user-id condition)
                     (error-action condition)))))

(define-condition item-error (tootsville-error)
  ((item-id :initarg :item-id :reader error-item-id :initform nil)
   (item-type :initarg :item-type :reader error-item-type :initform nil)
   (operation :initarg :operation :reader error-operation :initform nil))
  (:documentation "Error raised during item operations")
  (:report (lambda (condition stream)
             (format stream "Item Error [~a]: ~a~@[ (Item: ~a, Type: ~a, Operation: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-item-id condition)
                     (error-item-type condition)
                     (error-operation condition)))))

(define-condition world-error (tootsville-error)
  ((world-id :initarg :world-id :reader error-world-id :initform nil)
   (world-type :initarg :world-type :reader error-world-type :initform nil)
   (operation :initarg :operation :reader error-operation :initform nil))
  (:documentation "Error raised during world operations")
  (:report (lambda (condition stream)
             (format stream "World Error [~a]: ~a~@[ (World: ~a, Type: ~a, Operation: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-world-id condition)
                     (error-world-type condition)
                     (error-operation condition)))))

(define-condition terrain-error (tootsville-error)
  ((terrain-type :initarg :terrain-type :reader error-terrain-type :initform nil)
   (coordinates :initarg :coordinates :reader error-coordinates :initform nil)
   (operation :initarg :operation :reader error-operation :initform nil))
  (:documentation "Error raised during terrain operations")
  (:report (lambda (condition stream)
             (format stream "Terrain Error [~a]: ~a~@[ (Type: ~a, Operation: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-terrain-type condition)
                     (error-operation condition)))))

(define-condition metronome-error (tootsville-error)
  ((task-id :initarg :task-id :reader error-task-id :initform nil)
   (schedule :initarg :schedule :reader error-schedule :initform nil)
   (operation :initarg :operation :reader error-operation :initform nil))
  (:documentation "Error raised during metronome operations")
  (:report (lambda (condition stream)
             (format stream "Metronome Error [~a]: ~a~@[ (Task: ~a, Operation: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-task-id condition)
                     (error-operation condition)))))

(define-condition validation-error (tootsville-error)
  ((field :initarg :field :reader error-field :initform nil)
   (value :initarg :value :reader error-value :initform nil)
   (constraint :initarg :constraint :reader error-constraint :initform nil))
  (:documentation "Error raised during data validation")
  (:report (lambda (condition stream)
             (format stream "Validation Error [~a]: ~a~@[ (Field: ~a, Value: ~a, Constraint: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-field condition)
                     (error-value condition)
                     (error-constraint condition)))))

(define-condition rate-limit-error (tootsville-error)
  ((client-id :initarg :client-id :reader error-client-id :initform nil)
   (limit :initarg :limit :reader error-limit :initform nil)
   (window :initarg :window :reader error-window :initform nil))
  (:documentation "Error raised when rate limits are exceeded")
  (:report (lambda (condition stream)
             (format stream "Rate Limit Error [~a]: ~a~@[ (Client: ~a, Limit: ~a/~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-client-id condition)
                     (error-limit condition)
                     (error-window condition)))))

(define-condition security-error (tootsville-error)
  ((threat-type :initarg :threat-type :reader error-threat-type :initform nil)
   (source :initarg :source :reader error-source :initform nil)
   (severity :initarg :severity :reader error-severity :initform :medium))
  (:documentation "Error raised for security violations")
  (:report (lambda (condition stream)
             (format stream "Security Error [~a]: ~a~@[ (Threat: ~a, Source: ~a, Severity: ~a)~]"
                     (error-code condition)
                     (error-message condition)
                     (error-threat-type condition)
                     (error-source condition)
                     (error-severity condition)))))

;;; Error utility functions

(defun signal-authentication-error (message &key user-id provider attempts code context)
  "Signal an authentication error with appropriate data"
  (error 'authentication-error
         :message message
         :code (or code :authentication-failed)
         :user-id user-id
         :provider provider
         :attempts attempts
         :context context))

(defun signal-websocket-error (message &key client-id connection-id message-type code context)
  "Signal a WebSocket error with appropriate data"
  (error 'websocket-error
         :message message
         :code (or code :websocket-error)
         :client-id client-id
         :connection-id connection-id
         :message-type message-type
         :context context))

(defun signal-database-error (message &key table operation query code context)
  "Signal a database error with appropriate data"
  (error 'database-error
         :message message
         :code (or code :database-error)
         :table table
         :operation operation
         :query query
         :context context))

(defun signal-user-error (message &key user-id action permissions code context)
  "Signal a user error with appropriate data"
  (error 'user-error
         :message message
         :code (or code :user-error)
         :user-id user-id
         :action action
         :permissions permissions
         :context context))

(defun signal-item-error (message &key item-id item-type operation code context)
  "Signal an item error with appropriate data"
  (error 'item-error
         :message message
         :code (or code :item-error)
         :item-id item-id
         :item-type item-type
         :operation operation
         :context context))

(defun signal-world-error (message &key world-id world-type operation code context)
  "Signal a world error with appropriate data"
  (error 'world-error
         :message message
         :code (or code :world-error)
         :world-id world-id
         :world-type world-type
         :operation operation
         :context context))

(defun signal-terrain-error (message &key terrain-type coordinates operation code context)
  "Signal a terrain error with appropriate data"
  (error 'terrain-error
         :message message
         :code (or code :terrain-error)
         :terrain-type terrain-type
         :coordinates coordinates
         :operation operation
         :context context))

(defun signal-metronome-error (message &key task-id schedule operation code context)
  "Signal a metronome error with appropriate data"
  (error 'metronome-error
         :message message
         :code (or code :metronome-error)
         :task-id task-id
         :schedule schedule
         :operation operation
         :context context))

(defun signal-validation-error (message &key field value constraint code context)
  "Signal a validation error with appropriate data"
  (error 'validation-error
         :message message
         :code (or code :validation-failed)
         :field field
         :value value
         :constraint constraint
         :context context))

(defun signal-rate-limit-error (message &key client-id limit window code context)
  "Signal a rate limit error with appropriate data"
  (error 'rate-limit-error
         :message message
         :code (or code :rate-limit-exceeded)
         :client-id client-id
         :limit limit
         :window window
         :context context))

(defun signal-security-error (message &key threat-type source severity code context)
  "Signal a security error with appropriate data"
  (error 'security-error
         :message message
         :code (or code :security-violation)
         :threat-type threat-type
         :source source
         :severity severity
         :context context))

;;; Error handling utilities

(defmacro with-error-handling ((&key (error-type 'tootsville-error) (fallback nil)) &body body)
  "Handle errors with appropriate logging and fallback"
  `(handler-case
       (progn ,@body)
     (,error-type (condition)
       (v:error :error-handling "~a" condition)
       ,(if fallback fallback '(values nil condition)))))

(defun log-error (error-condition &optional additional-context)
  "Log an error with additional context"
  (v:error :error-logging "~a~@[ (Additional context: ~a)~]" error-condition additional-context))

(defun error-to-json (error-condition)
  "Convert an error to JSON format for API responses"
  (list :error (list :type (type-of error-condition)
                     :message (error-message error-condition)
                     :code (error-code error-condition)
                     :timestamp (error-timestamp error-condition)
                     :context (error-context error-condition))))

;;; Export error classes and functions

(export '(tootsville-error authentication-error websocket-error database-error
          user-error item-error world-error terrain-error metronome-error
          validation-error rate-limit-error security-error
          signal-authentication-error signal-websocket-error signal-database-error
          signal-user-error signal-item-error signal-world-error signal-terrain-error
          signal-metronome-error signal-validation-error signal-rate-limit-error
          signal-security-error with-error-handling log-error error-to-json))
