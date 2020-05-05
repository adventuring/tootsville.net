(in-package :Tootsville)

;;; new-commands-20.lisp is part of Tootsville
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


;; This file contains commands which were added in 2.0

(definfinity enumerate-wear-slots (nil u recipient/s)
  "Enumerates all possible wear slots for any avatar.

@subsection 200 OK

Returns     an     object     with     @code{status:     true,     from:
\"enumerateWearSlots\"}, and a key @code{slots}  under which is an array
of information about each wear  slot, in the format of `WEAR-SLOT-INFO',
q.v."
  (list 200
        (list :|status| t
              :|from| "enumerateWearSlots"
              :|slots| (mapcar #'wear-slot-info (find-records 'wear-slot)))))

(definfinity wardrobe (nil u recipient/s)
  "Describe what your Toot is wearing.

Note  that   several  other  commands  will   actually  return  wardrobe
information packets.

@subsection 200 OK

The returned packet, aside from  the expected @code{ status: true, from:
\"wardrobe\"},  contains a  key @code{wardrobe}  which in  turn contains
a key @code{avatar} which itself contains the JSON data in the fromat of
`TOOT-INFO', q.v.

@subsection Changes from 1.2 to 2.0

The actual `INFINITY-WARDROBE' function is new, but the returned packets
@code{from:  \"wardrobe\"} were  already being  used by  other commands,
including `INFINITY-DON' and `INFINITY-DOFF' and `INFINITY-DOFFF'.
"
  (list 200
        (list :|status| t
              :|from| "wardrobe"
              :|wardrobe| (list :|avatar| (Toot-info Toot)))))

(defun sky-room-var ()
  (list :|sun|
        (let ((xy (sun-position)))
          (list :|x| (first xy)
                :|y| (second xy)))
        :|moon|
        (let ((xyφ (moon-position :moon)))
          (list :|x| (first xyφ)
                :|y| (second xyφ)
                :|φ| (third xyφ)))
        :|othM|
        (let ((xyφ (moon-position :othm)))
          (list :|x| (first xyφ)
                :|y| (second xyφ)
                :|φ| (third xyφ)))
        :|pink|
        (let ((xyφ (moon-position :pink)))
          (list :|x| (first xyφ)
                :|y| (second xyφ)
                :|φ| (third xyφ)))))

(defun place-room-var (place)
  (format nil "~a:~{~d,~d,~d~^~~~}"
          (symbol-munger:lisp->camel-case (place-kind place))
          (place-coords place)))

(defun place-kind (place) :grass) ; TODO
(defun place-coords (place) nil); TODO

(defun places-at-position (world lat long alt)
  nil)

(definfinity get-room-vars (nil u recipient/s)
  "Returns room variables

@subsection Room Environment

These room  variables define the  general environment.

@table @code
@item s

The Sky. Consists of the background (sky) texture file as a URL, or, the
position of a sky object such as the sun, a moon, or a cloud.

@item f

The Floor; no longer used in 5.0

@item w

The Weather, or overlay artwork. Used to indicate precipitation.

@end table

@subsection
Room Objects

@itemize
@item item

Placed items: key: “item” + Unique-ID = value: item-description \"~\"
x-position \"~\" y-position \"~\" facing \"~\" z-position

@item item2

Placed items, new form: JSON object

@verbatim
{ position: {  x: y: z: },
 facing: radians,
 baseColor: color,
 altColor: color,
 energy: number,
 scale:  { x: y: z: },
 world:  { world: lat: long: alt: },
 template:
 { id:
 name:
 description:
 trade: [  Y N X  ],
 avatar:
 energyKind:
 energyMax:
 onZero:
 wearSlot:
 weight: } }

@item furn

User-positioned items: key: “furn”

@item text

Text items: key: \"text\" + unique-ID = value

@end itemize

@subsection Places

Places are regions of the room  defined by polygonal outlines. These are
held in Room Variables with names of the form \"zone\" plus an arbitrary
identifier.  The  contents  of  the room  variable  are  a  @emph{key}
followed by \":\" and a series of coördinates.

Each coördinate pair is given as x,y,z in decimal, literally, like:
\"100,0,200\". They are separated with \"~\". To stop one polygon and start
on another, give \"~~\" with no coördinates between.

The key of a Place specifies its purpose. The keys understood by the
server include:

@table @code

@item grass

@item tallGrass

@item water

@item unwalkable

@item doormat

@item parking

@item driveway

@item stairs

@end table

"
  (let* ((world (Toot-world *Toot*))
         (pos (Toot-position *Toot*))
         (i 0) (j 0))
    (list 200
          (list :|from| "rv"
                :|var|
                (concatenate
                 'list
                 (:|s| (sky-room-var)
                   :|rad| t)
                 (mapcan (lambda (item)
                           (list (make-keyword
                                  (format nil "item2~~~36r" (incf i)))
                                 (item-info item)))
                         (remove-if
                          (lambda (item)
                            (ignore-not-found
                              (find-record 'inventory-item
                                           :item (item-uuid item))))
                          (find-records 'item
                                        :world world
                                        :latitude (elt pos 0)
                                        :longitude (elt pos 1)
                                        :altitude (elt pos 2))))
                 (mapcan (lambda (place)
                           (list (make-keyword
                                  (format nil "zone~~~36r"
                                          (place-room-var place)))))
                         (apply #'places-at-position world pos)))))))
