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




(defun read-staff-journal (&key (start-date (yesterday)) (end-date (now))
                             (last 0))
  "Read staff journal entry between (inclusive) START-DATE and END-DATE.

The defaults are yesterday and today.

Or, read the single item LAST from the end; when LAST = 0, the very
latest entry; when LAST < 0, then the ``nth'' entry from the
end. Thus, -1 is the next-to-last entry, -2 is the third from the
end."

  "SELECT * FROM staff_journal_entries 
WHERE written_at >= ? AND written_at <= ?
ORDER BY written_at DSC
OFFSET ?
LIMIT 1" start-date end-date (- last)
  (error 'unimplemented))

(defun write-staff-journal-entry (entry who)
  "Write ENTRY to the staff journal, timestamped now. Reference WHO in the entry.

WHO may be anything accepted by `ENSURE-LIST-OF-PEOPLE'.
 
Journal entries are associated with the person (people) owning the
relevant Toot(s)
"
  (let ((entry (make-record 'staff-journal-entry
                            :uuid (uuid:make-v4-uuid)
                            :written-by (person-uuid (Toot-player *Toot*))
                            :written-at (now)
                            :entry entry))
        (related (ensure-list-of-people who)))
    (dolist (rel related)
      (when rel
        (make-record 'staff-journal-reference
                     :entry (staff-journal-entry-uuid entry)
                     :person rel)))
    entry))

(defun read-related-journal (who &key (last 0))
  "Read staff journal entries related to WHO.

Or, read the single item LAST from the end; when LAST = 0, the very
latest entry; when LAST < 0, then the ``nth'' entry from the
end. Thus, -1 is the next-to-last entry, -2 is the third from the
end.

WHO may be anything accepted by `ENSURE-LIST-OF-PEOPLE'."
  (error 'unimplemented))

(defun ensure-list-of-people (identifier)
  "Map IDENTIFIER to a list of humans.

IDENTIFIER may be:

@itemize
@item
A person
@item
A Toot (whose owner is returned)
@item
A person's eMail address
@item
A Toot name
@item
A person or Toot's UUID, in UUID or string-UUID form
@item 
A list of any of the above
@item
A string list of the above, joined by #\, or  #\;
@item
NIL (returns nil)
@end itemize
"
  (if (listp identifier)
      (remove-if #'null (flatten (mapcar #'ensure-list-of-people identifier)))
      (cons
       (cond
         ((null identifier)
          nil)
         ((typep identifier 'person)
          identifier)
         ((typep identifier 'Toot)
          (Toot-player identifier))
         ((find #\, identifier)
          (ensure-list-of-people (split-sequence #\, identifier)))
         ((find #\; identifier)
          (ensure-list-of-people (split-sequence #\; identifier)))
         ((find #\@ identifier)
          (ignore-not-found
            (contact-owner
             (find-record 'contact
                          :url (concatenate 'string "mailto:" identifier)))))
         ((potential-Toot-name-p identifier)
          (Toot-player (find-record 'Toot :name identifier)))
         ((uuid-string-p identifier)
          (ensure-list-of-people (uuid:make-uuid-from-string identifier)))
         ((stringp identifier)
          (error 'not-found))
         ((typep identifier 'uuid:uuid)
          (or (ignore-not-found (Toot-player (find-record 'Toot :uuid identifier)))
              (ignore-not-found (find-record 'person :uuid identifier))
              (error 'not-found))))
       nil)))


