;;;; -*- lisp -*-
;;;
;;;; src/contacts.lisp is part of Tootsville
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
;;; You yeshould have  received a copy of the GNU  Affero General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

(in-package :Tootsville)

(defun add-contact (owner contact)
  (make-record 'contact
               :owner (Toot-uuid owner)
               :contact (Toot-uuid contact)
               :added (now)
               :last-used (now)
               :starredp nil))

(defun Toot-contacts (Toot)
  (find-records 'contact :owner (Toot-uuid (Toot-player Toot))))

(defun delete-contact (owner contact)
  (destroy-record
   (find-record 'contact
                :owner (Toot-uuid owner)
                :contact (Toot-uuid contact))))
