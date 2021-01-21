;;;; -*- lisp -*-
;;;
;;;; src/types/color+pattern.lisp is part of Tootsville
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


(define-constant +Toot-base-color-names+
    '(Cyan Indigo Orange Pink Red Turquoise Violet White Yellow)
  :test #'equalp
  :documentation "Named colors allowed as Toot base colors")

(define-memo-function Toot-base-color-name-p (string-designator)
  "Is STRING-DESIGNATOR a color that can be used for a Toot's base color.

See also `+TOOT-BASE-COLOR-NAMES' and `TOOT-BASE-COLOR-NAME'"
  (check-type string-designator string-designator)
  (member string-designator +Toot-base-color-names+
          :test #'string-equal))

(deftype Toot-base-color-name ()
  "A string designator which describes a valid color for a Toot's base color.

See `+TOOT-BASE-COLOR-NAMES' for the list."
  '(and string-designator
    (satisfies Toot-base-color-name-p)))

(define-constant +Toot-pad-color-names+
    '(Cyan Indigo Pink Red "Spring Green" Violet White Yellow)
  :test #'equalp
  :documentation "Named colors allowed as Toot pad colors")

(define-memo-function Toot-pad-color-name-p (string-designator)
  "Is STRING-DESIGNATOR a color that can be used for foot pads and trunk tips"
  (check-type string-designator string-designator)
  (member string-designator +Toot-pad-color-names+
          :test #'string-equal))

(deftype Toot-pad-color-name ()
  "A color name that can be used for Toot foot pads and nose tip.

Formerly known as the ``extra color'' of the avatar."
  '(and string-designator
    (satisfies Toot-pad-color-name-p)))

(define-constant +Toot-pattern-color-names+
    '(Black Cyan Indigo Orange Pink Rainbow Turquoise Violet White Yellow)
  :test #'equalp
  :documentation "Named colors allowed as Toot pattern colors")

(define-memo-function Toot-pattern-color-name-p (string-designator)
  "Is STRING-DESIGNATOR the name of a color that can be used for a pattern?"
  (check-type string-designator string-designator)
  (member string-designator
          +Toot-pattern-color-names+
          :test #'string-equal))

(deftype Toot-pattern-color-name ()
  "The name of a color that can be used for a pattern"
  '(and string-designator
    (satisfies Toot-pattern-color-name-p)))



(define-constant +Toot-basic-pattern-names+
    '(Flowers Horseshoes Lightning Patches "Polka Dots"
      Notes Sparkles Spots Stars Swirls)
  :test #'equalp
  :documentation "Basic patterns available to any Toot")

(define-constant +Toot-extended-pattern-names+
    '(Circuitboard Peace)
  :test #'equalp
  :documentation "Extended patterns that require special effort to obtain")

(define-memo-function Toot-pattern-name-p (string-designator)
  "Is STRING-DESIGNATOR the name of a Toot pattern?"
  (check-type string-designator string-designator)
  (member string-designator (concatenate 'list
                                         +Toot-basic-pattern-names+
                                         +Toot-extended-pattern-names+)
          :test #'string-equal))

(deftype Toot-pattern-name ()
  "The name of a Toot pattern"
  '(and string-designator
    (satisfies Toot-pattern-name-p)))

(define-memo-function allowed-base-colors-under-pattern (pattern-color)
  (remove-if (curry #'equalp pattern-color) +Toot-base-color-names+))

(define-memo-function  allowed-pattern-colors-on-base (base-color)
  (remove-if (curry #'equalp base-color) +Toot-pattern-color-names+))



(defun check-pattern-on-base-color (pattern-color base-color
                                    &key Toot-name pad-color pattern address)
  "Ensure that the PATTERN-COLOR is allowed on the BASE-COLOR.

TOOT-NAME,  PAD-COLOR,   PATTERN,  and  ADDRESS  are   used  to  provide
additional error details.

Provides restarts CHANGE-PATTERN-COLOR  and CHANGE-BASE-COLOR to correct
any deficiencies."
  (tagbody do-over
     (restart-case
         (progn
           (check-type base-color Toot-base-color-name "The name of a Toot base color")
           (check-type pattern-color Toot-pattern-color-name "The name of a Toot pattern color")
           (when (equal pattern-color base-color)
             (error "A Toot may not have the same base and pattern color; currently, both are ~:(~a~)~
~@[, with a pattern of ~:(~a~)~]~
~@[, and pad color of ~:(~a~)~]~
~@[ for the Toot named ~a~]~
~@[ (player with e-mail ~a)~]."
                    base-color pattern pad-color Toot-name address))
           (list pattern-color base-color))
       (change-pattern-color (color)
         :report (lambda (s)
                   (format s "Change the pattern color to one of: ~:(~{~a~^, ~}~)"
                           (allowed-pattern-colors-on-base base-color)))
         (setf pattern-color color)
         (check-type pattern-color Toot-pattern-color-name)
         (go do-over))
       (change-base-color (color)
         :report (lambda (s)
                   (format s "Change the base color to one of: ~:(~{~a~^, ~}~)"
                           (allowed-base-colors-under-pattern pattern-color)))
         :interactive-function (lambda (s)
                                 (let ((bases (allowed-base-colors-under-pattern pattern-color)))
                                   (format s "~&Choose a new base color by name or number:")
                                   (loop for color in bases
                                      for i from 1
                                      do (format s "~% [ ~d ] ~:(~a~)" i color))
                                   (format s "~% Base Color 🢩 ")
                                   (finish-output s)
                                   (let ((in (read s)))
                                     (typecase in
                                       (number
                                        (assert (<= 1 in (length bases)) (in)
                                                "The number ~d is not ~
the index from 1 to ~d of a new base color in the list where 1=~{~a~^, ~}"
                                                in (length bases) bases)
                                        (elt bases in))
                                       (t (princ-to-string in))))))
         (setf base-color color)
         (check-type base-color Toot-pattern-color-name)
         (go do-over)))))



(defstruct color24 red green blue)

(defun color24= (a b &rest more)
  "Comparator of two color24s"
  (if more
      (and (color24= a b)
           (apply #'color24= a more))
      (and (= (color24-red a)
              (color24-red b))
           (= (color24-green a)
              (color24-green b))
           (= (color24-blue a)
              (color24-blue b)))))

(defun color24/= (a b)
  "Comparator of two color24s"
  (not (color24= a b)))

(defun color24-rgb (r g b)
  "Construct a Color24 from a R G B triplet"
  (make-color24 :red r :green g :blue b))

(defun color24-hsv (color)
  "Extract the Hue, Saturation, Value of a Color24 as a list of reals 0...1"
  (declare (optimize (speed 1) (safety 2)))
  (let* ((red (the (real 0 1)
                   (/ (the (unsigned-byte 8) (color24-red color))
                      255.0d0)))
         (green (the (real 0 1)
                     (/ (the (unsigned-byte 8) (color24-green color))
                        255.0d0)))
         (blue (the (real 0 1)
                    (/ (the (unsigned-byte 8) (color24-blue color))
                       255.0d0)))
         (c-max (the (real 0 1) (max red green blue)))
         (c-min (the (real 0 1) (min red green blue)))
         (delta (the (real 0 1) (- (the (real 0 1) c-max)
                                   (the (real 0 1) c-min)))))
    (if (< 0 delta)
        (list
         ;; hue
         (mod (* (/ (* 60.0d0
                       (cond
                         ((= c-max red) (mod (/ (- green blue) delta) 6))
                         ((= c-max green) (+ (/ (- blue red) delta) 2))
                         ((= c-max blue) (+ (/ (- red green) delta) 4))
                         (t (error "unreachable"))))
                    360.0d0)
                 2 pi)
              (* 2 pi))
         ;; saturation
         (if (< 0 c-max)
             (/ delta c-max)
             0)
         ;; value
         c-max)
        ;; else
        (list 0 0 c-max))))

(defun color24-hue (color)
  "The HSV Hue channel of COLOR.

See `COLOR24-HSV'."
  (first (color24-hsv color)))

(defun color24-saturation (color)
  "The HSV Saturation channel of COLOR.

See `COLOR24-HSV'."
  (second (color24-hsv color)))

(defun color24-value (color)
  "The HSV Value channel of COLOR.

See `COLOR24-HSV'."
  (third (color24-hsv color)))

(defun integer-to-color24 (number)
  "Return a color represented by the 24-bit integer NUMBER.

The upper 8  bits are the red  channel; the next 8 bits,  green; and the
lowest 8 bits, the blue channel."
  (make-color24 :red (ldb (byte 8 16) number)
                :green (ldb (byte 8 8) number)
                :blue (ldb (byte 8 0) number)))

(defun color24-to-integer (color)
  "Return a 24-bit integer representing COLOR.

The upper 8  bits are the red  channel; the next 8 bits,  green; and the
lowest 8 bits, the blue channel."
  (+ (ash (color24-red color) 16)
     (ash (color24-green color) 8)
     (color24-blue color)))

(define-constant +color24-names+
    '(("silver" . #xdddddd)
      ("charcoal" . #x333333)
      ("white" . #xffffff)
      ("black" . #x000000)
      ("deep-purple" . #xb117ff)
      ("yellow" . #xff216f)
      ("pink" . #xe73e97)
      ("turquoise" . #x00a290)
      ("periwinkle" . #x96b4de)
      ("violet" . #x9669ad)
      ("gold" . #xf7d023)
      ("burgundy" . #x9c0059)
      ("green" . #x7ac142)
      ("blue" . #x0082c8)
      ("lavender" . #xba9dca)
      ("tan" . #xffd2a0)
      ("red" . #xe51b24)
      ("spring-green" . #xc4d82d)
      ("indigo" . #x0028ff)
      ("orange" . #xff7b26)
      ("cyan" . #xccffff)
      ("rainbow" . #x000001))
  :test 'equalp)

(define-constant +color24-values+
    (mapcar (lambda (pair)
              (cons (cdr pair) (car pair)))
            +color24-names+)
  :test 'equalp)

(defun parse-color24 (color)
  "Parse COLOR as a name for a color, or a hex 24-bit color value.

It can also be an HTML-style or CSS-style value.


"
  (integer-to-color24
   (etypecase color
     (string (cond ((string-equal "rgb(" color :end2 4)
                    (register-groups-bind (r g b)
                        ("rgb\\((\\d+),\\s*(\\d+),\\s*(\\d+)\\)" color)
                      (logior (ash (parse-integer r) 16)
                              (ash (parse-integer g) 8)
                              (parse-integer b))))
                   ((char= #\# (char color 0))
                    (ecase (length color)
                      (4 (let ((cc (concatenate 'string
                                                (string (char color 1))
                                                (string (char color 1))
                                                (string (char color 2))
                                                (string (char color 2))
                                                (string (char color 3))
                                                (string (char color 3)))))
                           (parse-number cc :radix 16)))
                      (7 (parse-number (subseq color 1) :radix 16))))
                   (t (if-let (as (assoc (substitute #\- #\Space color)
                                         +color24-names+ :test #'string-equal))
                        (cdr as)
                        (parse-number color :radix 16)))))
     (number color)))))

(defun color24-name (color)
  "Given COLOR, return the name or hex string for it.

If  COLOR is  a named  color  in `+COLOR24-VALUES+',  returns its  name.
Otherwise, returns the 6-digit hex value, as per HTML or CSS."
  (let ((n (color24-to-integer color)))
    (if-let (as (assoc n +color24-values+))
      (cdr as)
      (format nil "~6,'0x" n))))

(defmethod print-object ((object color24) stream)
  "Print the COLOR24 in a readable format to STREAM.

The output  format is  as a  call to `PARSE-COLOR24'  and can  be `READ'
back in."
  (princ "(Parse-Color24 \"" stream)
  (princ (color24-name object) stream)
  (princ "\")" stream)
  nil)



(define-constant +initial-t-shirt-colors+
    '(cyan indigo pink red "Spring Green" violet white yellow)
  :test #'equalp)

(defun rgb-bytes->rgb (bytes)
  "Convert BYTES into a list of red, green, and blue values.

BYTES is an  RGB triplet of 3 8-bit bytes,  like `COLOR24-TO-INTEGER' or
`INTEGER-TO-COLOR24'  representation; i.e.  an integer  24 bits  long of
which the upper 8  bits are the red channel, next 8  bits are green, and
lower 8 bits are the blue channel."
  (list (ldb (byte 8 0) bytes)
        (ldb (byte 8 8) bytes)
        (ldb (byte 8 16) bytes)))
