;;;; -*- lisp -*-
;;;
;;;; src/types/date+time.lisp is part of Tootsville
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

(defun legal-age (date-of-birth &optional (reference-date (local-time:now)))
  "The age of  a person born on DATE-OF-BIRTH, as  of REFERENCE-DATE (or
right  now);  this uses  the  legal  definition  that the  person's  age
increments at  the midnight of their  date of birth each  year, with the
date 29 February treated as 1 March on non-leap-years.

The time  zone used for  this computation  is the not  defined, however,
yielding  rather  irregular  behaviour   depending  on  time  zones  and
the like.

TODO: Determine  in what  time zone  we should compute this  for legal
reasons, eg, COPPA."
  (check-type date-of-birth timestamp)
  (check-type reference-date timestamp)
  (unless (timestamp< date-of-birth reference-date)
    (return-from legal-age 0))
  (multiple-value-bind (msec sec min hour day month year)
      (decode-timestamp reference-date)
    (declare (ignore msec sec min hour))
    (multiple-value-bind (msec sec min hour
                               day-of-birth month-of-birth year-of-birth)
        (decode-timestamp date-of-birth)
      (declare (ignore msec sec min hour))
      (let ((had-birthday-p (or (< month-of-birth month)
                                (and (= month-of-birth month)
                                     (<= day-of-birth day)))))
        (+ (- year
              year-of-birth
              1)
           (if had-birthday-p 1 0))))))



(defun yesterday ()
  "Get a timestamp for yesterday."
  (timestamp- (now) 24 :hour))

(defun 2-days-ago ()
  "Get a timestamp for the second day in the past (the day before yesterday)."
  (timestamp- (now) 48 :hour))

(defun 3-days-ago ()
  "Get a timestamp for the third day in the past (the day before the day before yesterday)."
  (timestamp- (now) 72 :hour))

(defun header-time (&optional (time (now)))
  "Get TIME in RFC-1123 format, as needed for HTTP headers.

TIME defaults to the present (@code{(NOW)})."
  (format-timestring nil time :format +rfc-1123-format+))

(defun get-Unix-time (&optional (universal-time (get-universal-time)))
  (+ universal-time +Unix-zero-in-universal-time+))

(defconstant +Unix-zero-in-universal-time+ 2208988800
  "The Unix zero timestamp occurs at Universal Time 2,208,988,800seconds.")
