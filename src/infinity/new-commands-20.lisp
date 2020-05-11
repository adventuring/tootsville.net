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

(in-package :Tootsville)

;; This file contains  commands which were added in 1.2/2.0  --- some of
;; these  were previously  proprietary  extensions by  Tootsville /  Res
;; Interactive, LLC.

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
              :|wardrobe| (list :|avatar| (Toot-info *Toot*)))))

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

(defun local-room-vars ()
  (let* ((world (Toot-world *Toot*))
         (pos (Toot-position *Toot*))
         (i 0))
    ;; TODO  ---  create  an  hash table  rather  than  interning
    ;; a bunch of garbage as keywords
    (list :|from| "rv"
          :|status| t
          :|rad| t
          :|var|
          (concatenate
           'list
           (list :|s| (sky-room-var))
           (mapcan (lambda (item)
                     (list (make-keyword
                            (format nil "itm2~~~36r" (incf i)))
                           (item-info item)))
                   (find-records 'item
                                 :world world
                                 :latitude (elt pos 0)
                                 :longitude (elt pos 1)
                                 :altitude (elt pos 2)))
           (mapcan (lambda (place)
                     (list (make-keyword
                            (format nil "zone~~~36r"
                                    (place-room-var place)))))
                   (apply #'places-at-position world pos))))))

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

@subsection Room Objects

@table @code
@item item

Placed items: key: “item” + Unique-ID = value: item-description \"~\"
x-position \"~\" y-position \"~\" facing \"~\" z-position

@item itm2

Placed items, new form: JSON object

@verbatim
{ uuid:
 position: {  x: y: z: },
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
@end verbatim

@item furn

User-positioned items: key: “furn”

@item text

Text items: key: \"text\" + unique-ID = value

@end table

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
  (list 200 (local-room-vars)))

(definfinity wtl ((course facing) u r)
  "Walk the Line

Users send  a ``wtl''  packet when  they're moving  in a  straight line;
while  other  (arc)  shapes   were  considered,  they're  not  currently
supported. Each ``wtl'' packet has a  start and end point, a start time,
and a  speed; this  course is  enough information  for other  clients to
determine where along the line (linear interpolation) the walker is now.

Usage: 
 
@verbatim
{  c:  wtl,  d:  {  course: {  startPosition:  {  x:  y:  z:  },
endPosition: { x: y: z: }, speed: }, facing: }}
@end verbatim

In return, all observers receive these ``wtl'' packets back

Return: 

@verbatim
{ from: \"wtl\", status: true, course: {}, facing:, u: UUID, n: NAME }
@end verbatim

"
  (broadcast (list :|from| "wtl"
                   :|status| t
                   :|course| course
                   :|facing| facing
                   :|u| (Toot-uuid *Toot*)
                   :|n| (Toot-name *Toot*))
             :except *user*))

(definfinity wtl-4 ((u course facing) u r)
  "Walk the Line indirect refresher from observer"
  (broadcast (list :|from| "wtl"
                   :|status| t
                   :|course| course
                   :|facing| facing
                   :|u| (Toot-uuid (find-record 'Toot :name u))
                   :|n| u)
             :except *user*))

(defun toot-list-message ()
  "Send a player (user) their list of Toots.

Used primarily  in the  login process.  Might also  be used  for gifting
inventory back-and-forth later."
  (if-let (player-Toots (player-Toots))
    (list 200
          (list :|status| t
                :|from| "tootList"
                :|toots| (mapcar #'Toot-info
                                 (sort player-Toots
                                       #'timestamp>
                                       :key (lambda (Toot)
                                              (or (Toot-last-active Toot)
                                                  (universal-to-timestamp 0)))))))
    (list 404
          (list :|status| :false
                :|from| "tootList"))))

(definfinity toot-list (nil u recipient/s)
  "Enumerates all Toots owned by the user.

@subsection 200 OK

Returns  an object  with  @code{status: true,  from: \"tootList\"},  and
a key @code{toots} under  which is the list of Toots  owned by the user.
Each Toot object is as per `TOOT-INFO', q.v."
  (toot-list-message))

(defun plist-with-index (list)
  (loop for i from 0
     for el in list
     appending (list i el)))

(defun play-with-Toot (Toot)
  "Set up the *USER* to play with Toot object TOOT.

Performs announcement of the player to the world and other bookkeeping."
  (setf (player-toot *user*) Toot
        (Toot-last-active Toot) (get-universal-time))
  (save-record Toot)
  (when *client*
    (setf (Toot *client*) Toot))
  (unicast
   (list :|status| t
         :|from| "playWith"
         :|playWith| (Toot-name Toot)
         :|uuid| (Toot-UUID Toot)
         :|player| (list :|uuid| (person-uuid *user*)
                         :|name| (person-display-name *user*)
                         :|email| (person-first-email *user*))))
  (broadcast (Toot-join-message Toot) :except *user*)
  (broadcast (list :|status| t
                   :|from| "avatars"
                   :|inRoom| (Toot-world Toot)
                   :|avatars| (list :|joined| (Toot-info Toot))))
  (unicast (local-room-vars))
  (list 200 (from-avatars (plist-with-index (connected-toots)))))

(definfinity play-with ((character) u r)
  "Choose a Toot as your active CHARACTER in the game. "
  (if-let (Toot (find-record 'Toot :name character))
    (if (uuid:uuid= (toot-player toot) (person-uuid *user*))
        (play-with-Toot Toot)
        (list 403
              (v:warn :toot-security "Attempt by ~a to access ~a" *user* Toot)
              (list :|status| :false
                    :|from| "playWith"
                    :|error| "Not your Toot")))
    (list 404
          (v:warn :toot-security "Attempt by ~a to access non-existent ~a" *user* character)
          (list :|status| :false
                :|from| "playWith"
                :|error| "No such Toot"))))
