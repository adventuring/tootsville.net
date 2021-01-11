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
  (setf (gethash name *npc-list*) name)
  (let ((full-name (intern (concatenate 'string "ROBOT-" (string-upcase name))))
        (personality-name (when personality
                            (intern (concatenate 'string (string-upcase personality) "-PERSONALITY")))))
    `(progn
       (defclass ,full-name (,(or personality-name 'robot)) ())
       (defmethod initialize-instance :after ((robot ,full-name) &key &allow-other-keys)
         (let* ((quiesced (ignore-errors (find-record 'Toot-quiesced :Toot (Toot-uuid (Toot robot)))))
                (wtl (jonathan.decode:parse (or (and quiesced (Toot-quiesced-wtl quiesced))
                                                (random-start-wtl-for-Toot)))))
           (setf (Toot-position robot) (destructuring-bind (&key |course| |facing|) wtl
                                         (declare (ignore |facing|))
                                         (let ((start-point (getf |course| :|startPoint|)))
                                           (destructuring-bind (&key |x| |y| |z|) start-point
                                             (list :chor |x| |y| |z|))))))))))

(defun init-characters ()
  "Initialize non-player characters in the game world."

  (dohash ((name kind) *npc-list*)
    (let ((full-name (intern (concatenate 'string "ROBOT-" (string-upcase name)))))
      (unless (gethash (string name) *robots*)
        (make-instance full-name
                       :Toot (find-record 'Toot :name (string-capitalize name)))))))
