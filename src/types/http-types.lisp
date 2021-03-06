;;;; -*- lisp -*-
;;;
;;;; src/types/http-types.lisp is part of Tootsville
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

;;; HTTP Request Methods (aka verbs)

(deftype http-request-method ()
  "All HTTP request methods (aka verbs) defined in an IETF RFC."
  '(member :get :head :post :put :delete :trace :options :connect :patch))

(deftype http-safe-request-method ()
  "HTTP request methods that make no changes, so can be replayed ad infinitum."
  '(member :get :head :options :trace))

(deftype http-idempotent-request-method ()
  "HTTP request methods which, if replayed, do no harm, but may yield an
harmless  error   message  on  the  second   and  subsequent  attempts."
  '(member :get :head :options :trace :put :delete))

;;; Conditions  that are  returned to  be  handled by  the client;  i.e.
;;; these are conditions that translate directly into HTTP errors.

(deftype http-response-success-status-number ()
  '(member 100 101
    200 201 202 203 204 205 206 207
    300 301 302 303 304 305 307))

(deftype http-response-failure-status-number ()
  '(member
    400 401 402 403 404 405 406 407 408 409
    410 411 412 413 414 415 416 417 422 424 428 429 431
    500 501 502 503 504 505 511))

(deftype http-response-status-number ()
  '(or http-response-success-status-number
    http-response-failure-status-number))

(define-condition http-client-error (error)
  ((http-status-code :type http-response-failure-status-number
                     :initarg :http-status-code
                     :initarg :status-code
                     :initarg :status
                     :reader http-status-code))
  (:default-initargs :message "Error")
  (:documentation "An error that can be returned to an HTTP client. 

Note that we use these error codes  internally, as well, so they are not
necessarily always propagated over HTTP --- but they could be."))

(defmethod print-object ((condition HTTP-client-error) stream)
  (format stream "HTTP error to report to client (code ~a)"
          (if (slot-boundp condition 'HTTP-status-code)
              (format nil "~a: ~a" (HTTP-status-code condition)
                      (gethash (HTTP-status-code condition) *HTTP-status-message*))
              "unbound")))

(defmethod http-status-code ((error error))
  500)



(defun pretty-print-html-error (condition)
  "Produces an HTML page explaining CONDITION.

TODO: Use templates, filter backtrace like Rollbar, do better."
  (format nil "<!DOCTYPE html><html><head>
<meta charset=\"utf-8\">
<title> Error ~D — Tootsville</title>
<link rel=\"stylesheet\"
 href=\"https://www.tootsville.org/error/simple-error.2021.css\">
</head>
<body>
<h2> Error ~:*~D </h2>
<h1> ~A </h1>
<ul>
<li>
 <a href=\"http://wiki.tootsville.org/wiki/Error_~0@*~D\">More info…</a>~*
</li>
<li>
 <a href=\"http://~a/\">~:*~a</a>
</li>
</ul>
~:[<pre>~A</pre>~;Our operations team can find out more in the server logs.~]
~@[<ul class=\"backtrace\">
~{<li> <dl> ~{ <dt> ~a </dt> ~^ <dd> ~a </dd> ~} </dl> </li> ~}
</dl>~]
</body>
</html>"
          (if (slot-boundp condition 'http-status-code)
              (http-status-code condition)
              500)
          (if hunchentoot:*show-lisp-errors-p*
              condition
              (gethash (http-status-code condition) *http-status-message*))
          (cluster-name)
          hunchentoot:*show-lisp-backtraces-p*
          (ignore-errors (coerce (rollbar::find-appropriate-backtrace) 'list))
          (if hunchentoot:*show-lisp-backtraces-p*
              (mapcar
               (lambda (restart)
                 ;; TODO report-function for restarts?
                 (list restart (princ-to-string restart)))
               (compute-restarts condition))
              nil)))



(define-condition not-your-Toot-error (http-client-error)
  ((http-status-code :initform 404)
   (name :initarg name :accessor which-Toot-is-not-yours))
  (:report (lambda (c s)
             (format s "You do not have a Toot named “~a.”"
                     (which-Toot-is-not-yours c))))
  (:documentation "An error thrown when a player tries to alter another player's Toot"))

(define-condition unidentified-player-error (http-client-error)
  ((http-status-code :initform 403))
  (:report "Unidentified player")
  (:documentation "An error thrown when the player can't be identified.

They may have sent no credentials, or bad credentials."))

(define-condition unimplemented (http-client-error)
  ((http-status-code :initform 501)
   (feature :initarg :feature :accessor unimplemented-feature
            :initform "The feature you tried to access"))
  (:report (lambda (c s)
             (format s "~a has not been implemented." (unimplemented-feature c))))
  (:documentation "Signals that a feature has not been implemented yet."))

(define-condition bad-request (http-client-error)
  ((http-status-code :initform 400)
   (thing :initarg :thing :initarg :object :initarg :item :initarg :the
          :initform "A value provided by the client"
          :accessor bad-request-thing))
  (:report (lambda (c s)
             (format s "The value of ~a submitted was incorrect type."
                     (bad-request-thing c))))
  (:documentation "A value submitted was the incorrect type, or out of range."))

(define-condition unprocessable (bad-request)
  ((http-status-code :initform 422))
  (:report (lambda (c s)
             (format s "The value of ~a submitted could not be processed."
                     (bad-request-thing c))))
  (:documentation "A value submitted could not be processed."))

(define-condition not-found (http-client-error)
  ((http-status-code :initform 404)
   (thing :initarg :thing :initarg :object :initarg :item :initarg :the
          :initform "The requested object"
          :accessor not-found-thing))
  (:report (lambda (c s)
             (format s "~a was not found." (not-found-thing c))))
  (:documentation "Some object could not be found based on the identification provided."))

(define-condition gone (not-found)
  ((http-status-code :initform 402))
  (:report (lambda (c s)
             (format s "~a is gone." (not-found-thing c))))
  (:documentation "A resource is no longer available.

In particular, this is returned for functions which were discontinued in
Romance 2 but existed in earlier versions of the protocol."))
