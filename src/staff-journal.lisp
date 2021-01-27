;;;; -*- lisp -*-
;;;
;;;; src/staff-journal.lisp is part of Tootsville
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




(defun read-staff-journal (&key (start-date (yesterday)) (end-date (now)))
  "Read staff journal entries between (inclusive) START-DATE and END-DATE; default, yesterday and today."
  (error 'unimplemented))

(defun write-staff-journal-entry (entry who)
  "Write ENTRY to the staff journal, timestamped now. Reference WHO in the entry.

WHO may be:

@itemize
@item
A Toot, Toot name, or Toot UUID
@item
A player email address, or player UUID
@item
a list of Toots or players
@item
NIL
@end itemize

Journal entries are associated with the person (people) owning the relevant Toot(s)
"
  (error 'unimplemented))

(defun read-related-journal (who)
  "Read staff journal entries related to WHO.

See `WRITE-STAFF-JOURNAL-ENTRY' for the format of WHO."
  (error 'unimplemented))


