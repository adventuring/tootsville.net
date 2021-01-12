;;;; -*- lisp -*-
;;;
;;;; src/characters/characters.lisp is part of Tootsville
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



(defvar *npc-list* (make-hash-table))

(defmacro define-personality (name &optional superclass)
  (let ((personality-name (intern (concatenate 'string (string-upcase name) "-PERSONALITY"))))
    `(defclass ,personality-name (,(if superclass
                                       (intern (concatenate 'string (string-upcase superclass) "-PERSONALITY"))
                                       'robot))
       ())))

(defmacro define-character (name &optional personality)
  (let ((full-name (intern (concatenate 'string "ROBOT-" (string-upcase name))))
        (personality-name (when personality
                            (intern (concatenate 'string (string-upcase personality) "-PERSONALITY")))))
    `(progn
       (setf (gethash ',name *npc-list*) ',name)
       (defclass ,full-name (,(or personality-name 'robot)) ()))))

(defun init-characters ()
  "Initialize non-player characters in the game world."
  (dohash ((name kind) *npc-list*)
    (let ((full-name (intern (concatenate 'string "ROBOT-" (string-upcase name)))))
      (unless (gethash (string name) *robots*)
        (make-instance full-name
                       :Toot (find-record 'Toot :name (string-capitalize name))))))
  (hash-table-keys *npc-list*))
