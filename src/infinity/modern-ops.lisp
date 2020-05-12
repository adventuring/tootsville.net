;; -*- lisp -*-

;;; src/infinity/modern-ops.lisp is part of Tootsville
;;;
;;; Copyright ©  2008-2017, Bruce-Robert  Pocock; Copyright  © 2009,2010
;;; Res  Interactive LLC;  Copyright  © 2018-2020,  the Corporation  for
;;; Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;; This program is Free Software: you can redistribute it and/or modify
;;; it  under the  terms of  the GNU  Affero General  Public License  as
;;; published by the  Free Software Foundation; either version  3 of the
;;; License, or (at your option) any later version.
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

(define-operator-command quick-reload (words u r)
  "Quicklisp reload of the Tootsville package from disk.

Reloads   the   ASDF   file   with   `ASDF:LOAD-ASD'   and   then   does
a `QL:QUICKLOAD'."
  (reload-production)
  (format nil "Now running Tootsville.net server version ~a"
          (asdf:component-version (asdf:find-system :Tootsville))))

(define-operator-command git-pull (words u r)
  "Does a @code{git pull} in the server directory.

See also  `TOOTSVILLE-USER::QUICK-RELOAD' to actually load  any new code
that's downloaded."
  (run-program (format nil "cd ~a; git pull"
                       (asdf:system-source-directory :Tootsville))
               :output :string
               :error :output))

(define-operator-command push-script (words u r)
  "Instruct clients to load a new  script file.
 
Pushes a script filename to clients."
  (broadcast (list :|from| "newScript"
                   :|status| t
                   :|script| (first words))))

(define-operator-command ws-stats (words u r)
  "Returns some statistics about WebSockets connections.

See `WS-STATS'."
  (format nil "This server is ~a.~2%~a"
          (machine-instance) (ws-stats)))

(define-operator-command ws-bandwidth-by-source (words u r)
  "Returns some statistics about WebSockets bandwidth by source.

See `WS-BANDWIDTH-BY-SOURCE'."
  (format nil "This server is ~a.~2%~a"
          (machine-instance) (ws-bandwidth-by-source)))

(define-operator-command infinity-stats (words u r)
  "Returns some statistics about Infinity-mode requests.

See `INFINITY-STATS'."
  (format nil "This server is ~a.~2%~a"
          (machine-instance) (infinity-stats)))

(define-operator-command doodle (words u r)
  "Change the colors of a Toot.

Usage: #doodle WHO (base|pad|pattern) NEW-COLOR

The Toot's  color will immediately be  changed and be advertised  to any
interested listener.

NEW-COLOR can be in any format understood by `PARSE-COLOR24'."
  (if (= 3 (length words))
      (let ((who (ignore-not-found (find-record 'Toot :name (first words)))))
        (if who
            (let ((color (ignore-errors (parse-color24 (third words)))))
              (if color
                  (progn (string-case (second words)
                           ("base"
                            (setf (Toot-base-color who) color))
                           ("pad"
                            (setf (Toot-pad-color who) color))
                           ("pattern"
                            (setf (Toot-pattern-color who) color))
                           (otherwise
                            "Color place name must be base, pad, or pattern"))
                         (save-record who)
                         (broadcast (from-avatars (list :|doodled| (Toot-name who)))))
                  (format nil "Can't understand color ~a" (fourth words))))
            (format nil "No Toot named ~a" (first words))))
      (format nil "Usage: #doodle WHO (base|pad|pattern) NEW-COLOR ~s" words)))

(define-operator-command doodle-pattern (words u r)
  "Change the pattern of a Toot.

Usage: #doodle-pattern WHO NEW-PATTERN

As a special  case, \"Polka Dots\" should be passed  as POLKA-DOTS (with
an hyphen),  as well  as any  other pattern names  with spaces  (such as
\"Maple Leaf\"). "
  (if (= 2 (length words))
      (let ((who (ignore-not-found (find-record 'Toot :name (first words)))))
        (if who
            (if (typep (second words) 'Toot-pattern-name)
                (if-let (pattern (find-record 'pattern :name
                                              (substitute #\Space #\- (second words))))
                  (progn (setf (Toot-pattern who) (pattern-id pattern))
                         (save-record who)
                         (broadcast (from-avatars (list :|doodled| (Toot-name who))))))
                (format nil "Can't understand color ~a" (third words)))
            (format nil "No Toot named ~a" (first words))))
      "Usage: #doodle-pattern WHO PATTERN-NAME"))

(define-operator-command server-list (words u r)
  "Enumerate the servers active in this cluster."
  (format nil "~{~a~^, ~}" (server-list)))

(define-operator-command at (words u r)
  "Issue an operator command on a particular server instance.

Usage: #at SERVER #OTHER-COMMAND OTHER-PARAMS

For a list of servers, try #server-list

To issue a command on every server, send #at #each #OTHER-COMMAND"
  (if (string-equal (machine-instance) (first words))
      (parse-operator-command (format nil "~{~a~^ ~}" (rest words)))
      (if (string-equal "#each" (first words))
          (parse-operator-command (format nil "#at ~a ~{~a~^ ~}" server (rest words)))
          (format nil "Can't execute ~s at ~a" (rest words) (first words)))))

(define-operator-command gc (words u r)
  "Perform immediate garbage collection.

Usage:

#gc

#gc #full

"
  (if (and (<= 1 (length words))
           (string-equal "#full" (first words)))
      (sb-ext:gc :full t)
      (sb-ext:gc))
  (Tootsville-user::mem))

(define-operator-command doc (words u r)
  "Obtain documentation string in raw form about a symbol.

Syntax for use:

#doc [PACKAGE] SYMBOL [TYPE]


TYPE can  be VARIABLE,  FUNCTION, STRUCTURE,  TYPE, SETF,  or T.  If not
supplied, defaults to FUNCTION.

PACKAGE is optional and defaults to TOOTSVILLE-USER.

Examples of use:

#doc cdr

#doc doc function

#doc Tootsville ws-stats

#doc Tootsville ws-bandwidth-by-source function

This is based upon `DOCUMENTATION', qv."
  (format nil
          "~{~a~^ ~}: ~a" words
          (let ((doc-types '(variable function structure type setf t)))
            (case (length words)
              (1 (documentation (find-symbol (string-upcase (first words)) 
                                             :Tootsville-user)
                                'function))
              (2 (if (member (second words) doc-types :test 'string-equal)
                     (documentation (find-symbol (string-upcase (first words)) :Tootsville-user) 
                                    (find (second words) doc-types :test 'string-equal))
                     (documentation (find-symbol (second words)
                                                 (find-package (string-upcase (first words))))
                                    'function)))
              (3 (documentation (find-symbol (second words)
                                             (find-package (string-upcase (first words))))
                                (find (third words) doc-types :test 'string-equal)))))))


