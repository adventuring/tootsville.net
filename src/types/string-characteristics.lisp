;;;; -*- lisp -*-
;;;
;;;; src/types/string-characteristics.lisp is part of Tootsville
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

(defun string-length-2-p (s)
  "Is S a string of length 2?"
  (check-type s string)
  (= 2 (length s)))

(defun string-all-alpha-chars-p (s)
  "Is S a string of only alphabetical characters?"
  (check-type s string)
  (every #'alpha-char-p s))

(deftype two-letter-string ()
  "A string of two letters (alphabetical characters)"
  '(and string
    (satisfies string-length-2-p)
    (satisfies string-all-alpha-chars-p)))



(defun two-chars-in-a-row-p (string char-bag)
  "Do any two characters in CHAR-BAG occur together in STRING?"
  (check-type string string)
  (check-type char-bag sequence)
  (loop for i from 1 below (length string)
        when (and (find (char string i) char-bag)
                  (find (char string (1- i)) char-bag))
          do (return-from two-chars-in-a-row-p i))
  nil)

(defun three-chars-in-a-row-p (string &optional char-bag)
  "Do any three characters in CHAR-BAG occur together in STRING?

If CHAR-BAG is NIL, then any  character that occurs three times matching
itself returns true."
  (check-type string string)
  (check-type char-bag (or null sequence))
  (unless (<= 3 (length string))
    (return-from three-chars-in-a-row-p nil))
  (if char-bag
      (progn
        (assert (every #'characterp char-bag))
        (loop for i from 2 below (length string)
              when (and (find (char string i) char-bag)
                        (find (char string (1- i)) char-bag)
                        (find (char string (- i 2)) char-bag))
                do (return-from three-chars-in-a-row-p i)))
      (loop for i from 2 below (length string)
            when (char= (char string i)
                        (char string (- i 1))
                        (char string (- i 2)))
              do (return-from three-chars-in-a-row-p i)))
  nil)



(defun limit-string-length (string length)
  "Returns up to LENGTH characters from STRING.

If   STRING   is   less   than  LENGTH   characters   in   length,   the
entire (original) string is returned."
  (if (<= (length string) length)
      string
      (subseq string 0 length)))

(defun first-paragraph (string)
  "Returns the first paragraph of STRING.

\(Up to the first blank line)"
  (unless (find #\Newline string)
    (return-from first-paragraph string))
  (loop for line in (split-sequence #\Newline string)
        collecting line into lines
        when (emptyp line)
          do (return-from first-paragraph (join #\Newline lines))))

(defun uuid-string-p (string)
  "Does STRING look like a UUID?

Checks for 36 characters with #\- in the correct positions and hex
characters elsewhere.

@subsection Example

@verbatim
6D559B46-D021-4814-A7F7-D8D67AD64800
@end verbatim
"
  (and (= 36 (length string))
       (char= #\- (char string 8))
       (char= #\- (char string 13))
       (char= #\- (char string 18))
       (char= #\- (char string 23))
       (let ((hex (string-upcase (remove #\- string))))
         (and (= 32 (length hex))
              (every (lambda (char)
                       (or (char<= #\0 char #\9)
                           (char<= #\A char #\F)))
                     hex)))))
