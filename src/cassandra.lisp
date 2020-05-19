;;;; -*- lisp -*-
;;;
;;;; src/cassandra.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018-2020  The
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



(defvar *cassandra-blacklist* (make-hash-table :test 'eql :synchronized t)
  "The blacklist for text filtering.

This list is applied whenever children or sensitive players are
around.

The keys are the string versions of the regexes; the values are the
compiled scanners.")

(defvar *cassandra-redlist* (make-hash-table :test 'eql :synchronized t)
  "The redlist for text filtering.

This list is applied in all areas except \"adults only\" zones.

The keys are the string versions of the regexes; the values are the
compiled scanners.")

(defun cassandra-add-to-blacklist (regex)
  "Add REGEX to the blacklist"
  (setf (gethash regex *cassandra-blacklist*)
	(create-scanner regex
			:case-insensitive-mode t
			:multi-line-mode t)))

(defun cassandra-add-to-redlist (regex)
  "Add REGEX to the redlist"
  (setf (gethash regex *cassandra-redlist*)
	(create-scanner regex
			:case-insensitive-mode t
			:multi-line-mode t)))

(defun cassandra-remove-from-blacklist (regex)
  "Remove REGEX from the blacklist."
  (remhash regex *cassandra-blacklist*))

(defun cassandra-remove-from-redlist (regex)
  "Remove REGEX from the redlist."
  (remhash regex *cassandra-redlist*))

(defun cassandra-filter (text &optional children-present-p)
  "Filter TEXT for obscenities on the redlist; and, if CHILDREN-PRESENT-P, the blacklist too.

Returns a generalized true value if TEXT should be allowed."
  (when children-present-p
    (dolist (expr (hash-table-values *cassandra-blacklist*))
      (when (all-matches expr text)
	(return-from cassandra-filter nil))))
  (dolist (expr (hash-table-values *cassandra-redlist*))
    (when (all-matches expr text)
      (return-from cassandra-filter nil)))
  text)

(defun cassandra-obnoxious-filter (text vol)
  "Filter TEXT for obnoxious content. Starting volume is VOL.

Returns multiple values: the altered versions of TEXT and VOL.

If TEXT is in @samp{ALL CAPS LOCK COMPLETELY}, it will be downcased,
but VOL will be increased one level (if possible). If TEXT contains
certain common repeated or mistyped punctuation, they will be
converted.

"
  (when (string= text (string-upcase text))		 
    (setf text (string-downcase text)
          vol (string-case vol
                ("shout" "shout")
                ("talk" "shout")
                ("whisper" "talk"))))
  (setf text (regex-replace-pairs '(( "!!+" . "!" )
                                    ( "\\?\\?+" . "?" )
                                    ( ",,+" . "," )
                                    ( "\\bi\\b" . "I" )
                                    ( "\\b\"" . "“" )
                                    ( "\"\\b" . "”" )
                                    ( "([a-z])'([a-z])" . "\\1’\\2")
                                    ( "\\b'" . "‘" )
                                    ( "'\\b" . "’" )
                                    ( "\\bet\\b" . "&" )
                                    ( "\\b\\(c\\)\\b" . "©" )
                                    ( "\\b\\(tm\\)\\b" . "™" )
                                    ( "\\b\\(r\\)\\b" . "®" )
                                    ( "--" . "–" )
                                    ( "---+" . "—" )
                                    ( "\\*\\*+" . "*" )
                                    ( "\\.\\.\\." . "…" )
                                    ( "\\.\\." . "." )
                                    ( "…\\.+" . "…" ))
                                  text))
  (values text vol))

(assert (equal "why am I shouting?"
               (cassandra-obnoxious-filter "WHY AM I SHOUTING?" "talk")))
(assert (equal "One exclamation mark suffices!"
               (cassandra-obnoxious-filter "One exclamation mark suffices!!!!" "talk")))
(assert (equal "Why can’t people …"
               (cassandra-obnoxious-filter "Why can't people ..." "talk")))
