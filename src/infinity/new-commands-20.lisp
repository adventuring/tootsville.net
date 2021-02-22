;;; new-commands-20.lisp is part of Tootsville
;;;
;;; Copyright ©  2008-2017, Bruce-Robert  Pocock; Copyright  © 2009,2010
;;; Res  Interactive LLC;  Copyright  © 2018-2021,  the Corporation  for
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

@subsection Usage

This command takes no arguments.  It returns the wear-slots associated
with the caller's avatar.

See `WEAR-SLOT-INFO' for the format of the reply data.

@subsection 200 OK

Returns an object with @code{status: true, from:
\"enumerateWearSlots\"}, and a key @code{slots} under which is an
array of information about each wear slot, in the format of
`WEAR-SLOT-INFO', q.v.

@verbatim
{ from: \"enumerateWearSlots\",
  status: true,
  slots: [ WEAR-SLOT-INFO, WEAR-SLOT-INFO, ... ] }
@end verbatim
"
  (list 200
        (list :|status| t
              :|from| "enumerateWearSlots"
              :|slots| (mapcar #'wear-slot-info (find-records 'wear-slot)))))

(definfinity wardrobe (nil u recipient/s)
  "Describe what your Toot is wearing.

Note  that   several  other  commands  will   actually  return  wardrobe
information packets.

@subsection Usage

This command requires no parameters.

@subsection 200 OK

The returned packet, aside from  the expected @code{ status: true, from:
\"wardrobe\"},  contains a  key @code{wardrobe}  which in  turn contains
a key @code{avatar} which itself contains the JSON data in the fromat of
`TOOT-INFO', q.v.

@verbatim
{ from: \"wardrobe\",
  status: true,
  wardrobe: { avatar: { AVATAR INFO } } }
@end verbatim

@subsection Changes from 1.2 to 2.0

The actual `INFINITY-WARDROBE' function is new, but the returned packets
@code{from:  \"wardrobe\"} were  already being  used by  other commands,
including `INFINITY-DON' and `INFINITY-DOFF' and `INFINITY-DOFFF'.
"
  (list 200
        (list :|status| t
              :|from| "wardrobe"
              :|wardrobe| (list :|avatar| (Toot-info *Toot*)))))

(defun sky-room-var (world)
  "Returns the current state of the skies over WORLD.

This data is in the form of a Plist suitable for JSON-ification. It's
expected to be used by `LOCAL-ROOM-VARS' particularly, q.v.

When WORLD is CHOR (Chœrogryllum), the sky will contain a sun, and
three moons. For each body, the X and Y positions will be returned; in
addition, for each moon, the phase (φ) of the moon will be returned.

@subsection Example structure

@verbatim
{ sun: { x: 120, y: 120 },
  moon: { x: 120, y: 120, φ: 1 },
  othM: { x: 120, y: 120, φ: 1 },
  pink: { x: 120, y: 120, φ: 1 } }
@end verbatim

"
  (assert (string-equal world :chor))
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

(defun places-at-position (world lat long alt)
  "Returns all Places at WORLD at LAT-itude, LONG-itude, ALT-itude.

See `INFINITY-GET-ROOM-VARS' for a discussion of the Place system."
  (remove-if #'null
             (find-records 'place 
                           :world world
                           :latitude lat
                           :longitude long
                           :altitude alt)))

(defun local-room-vars (&optional (observer *client*))
  "Gets ``room variables'' local to *CLIENT*.

See `INFINITY-GET-ROOM-VARS' for a discussion.

@subsection Example
@verbatim
{ from: \"rv\",
  status: true,
  lat: LAT, long: LONG, alt: ALT, world: WORLD,
  var: { [ key: value ] ... } }
@end verbatim"
  (let* ((world (world observer))
         (latitude (latitude observer))
         (longitude (longitude observer))
         (altitude (altitude observer))
         (vars (make-hash-table :test 'equal)))
    (setf (gethash "s" vars) (sky-room-var world))
    (when (and (= 0 altitude) (eql :chor world))
      (terrain world latitude longitude))
    (do-records (item item :world (princ-to-string world) ; string needed for do-records
                           :latitude latitude
                           :longitude longitude
                           :altitude altitude)
      (setf (gethash (format nil "itm2~~~a" (item-uuid item))
                     vars) 
            (item-info item)))
    (when-let (locale-music (ignore-not-found (find-record 'locale-music
                                                           :latitude latitude
                                                           :longitude longitude
                                                           :altitude altitude
                                                           :world world)))
      (when-let (music (ignore-not-found (find-reference music :music)))
        (setf (gethash "m" vars) (list :|title| (music-title music)
                                       :|artist| (music-artist music)
                                       :|file| (music-file music)
                                       :|link| (music-link music)
                                       :|license| (music-license music)))))
    (dolist (place (places-at-position world (latitude observer)
                                       (longitude observer) (altitude observer)))
      (setf (gethash (format nil "zone~~~a" (place-uuid place))
                     vars)
            (place-string place)))
    (list :|from| "rv"
          :|status| t
          :|lat| latitude 
          :|long| longitude
          :|alt| altitude
          :|world| world
          :|var| vars)))

(definfinity get-room-vars (nil u recipient/s)
  "Returns ``room variables.''

@subsection Usage

This command requires no parameters.

@cindex Room Variables

@subsection Historical Usage (Romance I)

In Romance  I, the  server had  a library  of free-form  key-value pairs
which were used to control each ``room,'' or screen, of the game.

These   variables,  which   were  usually   edited  using   the  special
``Zookeeper''  client by  Eric Feilding,  eventually metamorphosed  into
a library of very specific ``room variables'' as described herein.

We no longer support arbitrary key-value  pairs; at this point, all room
variables are  specifically enumerated  in the  following documentation;
however, future releases  could expand this list,  so conforming clients
are required to accept and ignore unrecognized variables silently.

@subsection Room Environment

These room  variables define the  general environment.

@table @code
@item s

The Sky. Consists of the background (sky) texture file as a URL, or, the
position of a sky object such as the sun, a moon, or a cloud. 

@item f

The Floor; no longer used in 2.0. (This was the actual SWF file that had
the room background in it, in Romance I.)

@item m

Music.  A JSON  object describing  the  background music  for the  area.
Attributes are  @code{title}, @code{artist}, @code{link},  @code{file} —
@code{file} is  the URL fragment  base name;  there are three  files for
each song, in  the @code{mp3}, @code{ogg}, and  @code{webm} formats, all
found in @code{https://jumbo.tootsville.org/Assets/Music/5/}.

@item w

The Weather, or overlay artwork. Used to indicate precipitation.

@end table

@subsection Sky Variables

WRITEME

See `SKY-ROOM-VAR'

@subsection Weather

WRITEME

@subsection Room Objects

@table @code
@item item

A placed item can be represented by an encoded string form (``item''),
or a JSON structure (``itm2'').

The older style uses a key beginning with @code{item} and an unique
identifier string, followed by a @code{~} delimited list of:
description, X position, Y position, facing, and (optional) Z
position.

If the Z position is omitted, then the value given for Y position
should be used for Z instead. (The Y axis used to run across the
floor.)

The facing value can be given in radians, or as a special moniker from
the set: @code{N NE E SE S SW W NW}. See `INTERPRET-FACING'.

@example
itemfoo123: \"flowerPot~100~931~N\"
itembar456: \"flowerPot~100~0~1.23412952423~931\"
@end example


@item itm2

Placed items, new form: JSON object

@verbatim
{ uuid:
 position: {  x: y: z: },
 facing: FACING,
 baseColor: COLOR,
 altColor: COLOR,
 energy: NUMBER,
 scale:  { x: y: z: },
 world:  { world: lat: long: alt: },
 template:
 { id:
   name:
   description:
   trade: [  \"Y\", \"N\", or \"X\"  ],
   avatar:
   energyKind:
   energyMax:
   onZero:
   wearSlot:
   weight: } }
@end verbatim

@item furn

User-positioned items: key: “furn” --- no longer used.

@item text

Text items: key: \"text\" + unique-ID = value

Text to be displayed atop another item. The value might be x~z~string
or itm2-id~attachment~string. In the latter form, the text is attached
to the model of the ``itm2'' given at the attachment point.

The attachment point is expected to be of the form
@code{tex:TEXTURE-NAME}, i.e. a literal prefix @code{tex:} followed by
the name of the surface texture onto which the text should be drawn.

@end table

@subsubsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

The facing directions can now be cardinal directions, or radians.

User-placed ``furniture'' is no longer distinguished from other items
in the world.

@code{text} items can now be associated with items, rather than having
fixed positions of their own.

@subsubsection Changes from 1.1 to 1.2
@cindex Changes from 1.1 to 1.2

The @code{itm2} format was added.

@subsection Places

@cindex Places, within the game

Places  are regions  of the  game space  defined by  polygonal outlines.
These are held in Room Variables with names of the form \"zone\" plus an
arbitrary identifier. The contents of the room variable are a @emph{key}
followed by \":\" and a series of coördinates.

Each coördinate pair/triplet is given as x,y,z in decimal, literally,
like: \"100,0,200\". When only two coördinates are supplied, they
represent x and z. They are separated with \"~\". To stop one polygon
and start on another, give \"~~\" with no coördinates between.

The key of a Place specifies its purpose. The keys understood by the
server include:

@table @code

@item grass

This  is  the  default Place  kind;  any  area  of  ground that  is  not
explicitly part of some other kind of Place is grass.

@item tallGrass

@item water

@item unwalkable

This demarcates an invisible obstacle --- a collision-only object ---
which prevents avatars from entering that space.

@item doormat

@item parking

@item driveway

@item stairs

@item sidewalk

@item cobbles

@item slide

@item firepole

@item game

This space is part of an in-world game; e.g. a soccer field.

@item ice

@item sand

@item snow

@item cheese

The stuff the moons are made of. (Fight me.)

@item pit

A bottomless pit

@end table

@subsubsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

Places (referred to, confusingly, as zones) existed in Romance 1.0,
but they came in two forms. Some zones were ``burned in'' to the Flash
``floor'' files as invisible polygon layers with a specific naming
convention. Others were promulgated by room variables.

The variety of places has been substantially increased.

The default was for the ``floor'' to be @i{unwalkable}, with walkable
spaces marked out by zones. The reverse is now true, however, items
are now physical boundaries that block player movement.

@subsubsection Changes from 1.0 to 1.1
@cindex Changes from 1.0 to 1.1

Prior to 1.1, all floor zones were embedded permanently in the Flash
``floor'' files.

@subsection More good stuff

WRITEME --- there is more to explain about room variables.

@subsection See Also

See  `TOOTSVILLE-USER::PLACE' for  an  explanation  of creating  certain
places in the game and how they work.

"
  (list 200 (local-room-vars)))

(definfinity wtl ((course facing) u r)
  "Walk the Line

@cindex Walking the Line

Users send  a ``wtl''  packet when  they're moving  in a  straight line;
while  other  (arc)  shapes   were  considered,  they're  not  currently
supported. Each ``wtl'' packet has a  start and end point, a start time,
and a  speed; this  course is  enough information  for other  clients to
determine where along the line (linear interpolation) the walker is now.

@subsection Usage
 
@verbatim
{ course: 
  { startPosition: {  x:  y:  z:  },
    startTime: UNIX-TIME,
    endPosition: { x: y: z: }, 
    speed: SPEED }, 
  facing: RADIANS }
@end verbatim

Facing  can be  given as  `INTERPRET-FACING', and  will be  converted to
decimal radians.

In return, all observers receive these ``wtl'' packets back ... WRITEME



@subsection Reply

@verbatim
{ from: \"wtl\", status: true, course: {}, facing:, u: UUID, n: NAME }
@end verbatim

@subsection Future Directions

There is limited, partial, and broken support for the new @code{d3}
system which will eventually supersede @code{wtl} in Romance 2, but it
is not useful today. Use @code{wtl} in new code.

@subsection See Also

See `INFINITY-SET-USER-VAR' for discussion of an alternative way to
submit a @code{wtl} packet or the legacy @code{d} form (see below).

@subsection Changes from 1.1

In Romance 1.0 and 1.1, the usual way to walk was using the @code{d}
user variable, which was a string encoding very similar to the intent
of @code{wtl}. The @code{d} string was developed by Robert Dawson and
Bruce-Robert Pocock to cover up network latency by providing a
concrete (linear interpolated) position for each character at all
times, no matter how laggy players' network connections were. 

In the Persephone client software, these were sometimes referred to as
a @code{datList}.

@code{d} strings consisted of a @code{~} delimited list of either
x1~z1~x2~z2~facing~startTime or x1~y1~x2~y2~facing~startTime~z1~z2.
Note how the two-coördinate form uses x,z with y pinned at zero.

As with @code{wtl},  @code{facing} can be supplied in  either radians or
as   a  value   from  the   list  @code{N   NE  E   SE  S   SW  W   NW}.
See `INTERPRET-FACING'.
"
  (when *client*
    (unless (getf course :|world|)
      (setf (getf course :|world|) (make-keyword (string-upcase (world (Toot-position *client*))))
            (getf course :|latitude|) (latitude (Toot-position *client*))
            (getf course :|longitude|) (longitude (Toot-position *client*))
            (getf course :|altitude|) (altitude (Toot-position *client*))))
    (setf (wtl-course *client*) (parse-wtl-course (list :|course| course :|facing| facing))))
  (broadcast (list :|from| "wtl"
                   :|status| t
                   :|course| course
                   :|facing| facing
                   :|u| (Toot-uuid *Toot*)
                   :|n| (Toot-name *Toot*))
             :except *client*
             :near *client*))

(definfinity wtl-4 ((u course facing) u r)
  "Walk the Line indirect refresher from observer

@subsection Usage

@verbatim
{ u: \"TOOT-NAME\",
  course: { COURSE },
  facing: RADIANS }
@end verbatim

Facing can be provided as per `INTERPRET-FACING'.

WRITEME"
  (broadcast (list :|from| "wtl"
                   :|status| t
                   :|course| course
                   :|facing| facing
                   :|u| (Toot-uuid (find-record 'Toot :name u))
                   :|n| u)
             :except *client*))

(defvar +facing-angles+
  (make-hash-table :test 'equalp)
  "The eight cardinal directions, mapped to angles in radians.

See `INTERPRET-FACING'.

Would be a constant, except for issues with making hash-table
constants.")

(setf (gethash "N" +facing-angles+) 0
      (gethash "NE" +facing-angles+) (* pi 1/4)
      (gethash "E" +facing-angles+) (* pi 1/2)
      (gethash "SE" +facing-angles+) (* pi 3/4)
      (gethash "S" +facing-angles+) pi
      (gethash "SW" +facing-angles+) (* pi 5/4)
      (gethash "W" +facing-angles+) (* pi 3/2)
      (gethash "NW" +facing-angles+) (* pi 7/4))

(defun interpret-facing (facing)
  "Given a FACING string, return an angle in radians.

This supports a  string that is a floating-point number  of radians that
can be parsed by `PARSE-NUMBER' or  one of the cardinal eight directions
as a string: @code{N NE E SE S SW W NW}.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

Facing directions used to be @i{only} the cardinal directions; now, an
arbitrary rotation in radians is possible.

TODO: Throw a 400-type exception when junk is passed in."
  (* 1.0d0 (if facing
               (if (numberp facing)
                   facing
                   (if-let (angle (gethash facing +facing-angles+))
                     angle
                     (parse-number facing)))
               0.0d0)))

(definfinity shoot ((i course facing) u r)
  "Fire a shot from a projectile device.

UNIMPLEMENTED

The projectile device ITEM must be capable of firing a projectile;
this includes having sufficient energy (ammunition) to do so.

Projectiles are currently UNIMPLEMENTED.

@subsection Usage

@verbatim
{ c: \"shoot\",
  d: { i: ITEM,
       course: COURSE,
       facing: FACING } }
@end verbatim

Facing is interpreted by `INTERPRET-FACING'.

@subsection Example

WRITEME

@subsection See also

See also the `INFINITY-SET-USER-VAR' command for an alternative way to
promulgate shots."
  (error 'unimplemented))

(defun Toot-list-message ()
  "Send a player (user) their list of Toots.

Used primarily  in the  login process.  Might also  be used  for gifting
inventory back-and-forth later.

@subsection Format

@verbatim
{ from: \"tootList\",
  status: true,
  toots: [ TOOT-INFO, ... ] }
@end verbatim

The value of @code{toots} is an array (list) of `TOOT-INFO' ordered by
the time that the Toot was last active in the game, most recent to
least recent. Clients are encouraged to display the list of Toots in
this order.

If the player has no Toots yet, returns a 404 with @code{status:
false}.
"
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

(definfinity Toot-list (nil u recipient/s)
  "Enumerates all Toots owned by the user.

@subsection Usage

This command requires no parameters.

@subsection 200 OK

Returns  an object  with  @code{status: true,  from: \"tootList\"},  and
a key @code{toots} under  which is the list of Toots  owned by the user.
Each Toot object is as per `TOOT-INFO', q.v.

@verbatim
{ from: \"tootList\",
  status: true,
  toots: [ { INFO }, ... ] }
@end verbatim

See `TOOT-LIST-MESSAGE'.
"
  (toot-list-message))

(defun plist-with-index (list)
  "Zip LIST with sequential numbers from 0, creating a plist whose keys are sequential integers."
  (loop for i from 0
     for el in list
     appending (list i el)))

(defun random-start-wtl-for-Toot ()
  "Designate a starting position in Toot Square for a Toot.

Returns a WTL-type structure in a JSON string, with @code{course} and
@code{facing} values.

Starting positions are randomly dispersed around the Toot Square
fountain, which intentionally is the center of the coördinate system
of the world.
"
  (let* ((θ (random (* 2 pi)))
         (θ₂ (random (* 2 pi)))
         (ρ (+ 35 (random 100)))
         (x (- (* ρ (cos θ)) 50))
         (z (- (abs (* ρ (sin θ))))))
    (to-json (list :|course| (list :|endPoint| (list :|x| x :|y| 0 :|z| z)
                                   :|startPoint| (list :|x| x :|y| 0 :|z| z)
                                   :|speed| 0
                                   :|startTime| (* 1000 (- (get-universal-time*)
                                                           +unix-zero-in-universal-time+)))
                   :|facing| θ₂))))

(defun make-new-Toot-state (Toot)
  "Set up the state for TOOT, who has never logged in before.

WRITEME

@itemize
@item
Sends a private admin message welcoming the player to Toot Square
@item
Create a New Toot Quaestor event
@item
Creates a Toot-Quiesced record for them
@end itemize"
  (when (ignore-not-found (find-record 'Toot-quiesced :Toot (Toot-UUID Toot)))
    (return-from make-new-Toot-state))
  (private-admin-message
   (format nil "Welcome to Tootsville, ~:(~a~)!" (Toot-name Toot))
   "Welcome  to Tootsville!  This is  Toot Square,  the center of town.")
  (quaestor-new-Toot Toot)
  (ensure-record 'Toot-quiesced
                 :Toot (Toot-uuid Toot)
                 :world "CHOR"
                 :latitude -60
                 :longitude 20
                 :altitude 0
                 :wtl (random-start-wtl-for-Toot)
                 :d3 nil
                 :emotion nil
                 :attribs nil))

(defun burgeon-quiesced-state (Toot)
  "Restore quiescent state for TOOT as they return to the game."
  (let ((state (or (ignore-not-found
                     (find-record 'Toot-quiesced :Toot (Toot-uuid Toot)))
                   (make-new-Toot-state Toot))))
    (let ((wtl (and (Toot-quiesced-wtl state) (jonathan.decode:parse (Toot-quiesced-wtl state)))))
      (unicast (list :|status| t
                     :|from| "burgeon"
                     :|world| (Toot-quiesced-world state)
                     :|latitude| (Toot-quiesced-latitude state)
                     :|longitude| (Toot-quiesced-longitude state)
                     :|altitude| (Toot-quiesced-altitude state)
                     :|wtl| wtl
                     :|d3| (when-let (d3 (Toot-quiesced-d3 state))
                             (jonathan.decode:parse d3))
                     :|emotion| (Toot-quiesced-emotion state)
                     :|peanuts| (Toot-peanuts Toot)
                     :|fairy-dust| (Toot-fairy-dust Toot)
                     :|attribs| (Toot-quiesced-attribs state)))
      (when (and *Toot* (Toot= Toot *Toot*))
        (setf (Toot-position *client*) (list (Toot-quiesced-world state)
                                             (Toot-quiesced-latitude state)
                                             (Toot-quiesced-longitude state)
                                             (Toot-quiesced-altitude state))
              (wtl-course *client*) (parse-wtl-course wtl))))))

(defun update-Toot-last-active (Toot)
  "Set the `TOOT-LAST-ACTIVE' time for TOOT to the present time."
  (setf (Toot-last-active Toot) (now))
  (save-record Toot))

(defun play-with-Toot (Toot)
  "Set up the *USER* to play with Toot object TOOT.

Performs announcement of the player to the world and other bookkeeping.

See `INFINITY-PLAY-WITH'.

The client will receive a minor broadcast storm of information about
their Toot and the game world. This will, at a minimum, include a
success message from @code{playWith}, their own avatar information,
nearby players' avatar information, and `LOCAL-ROOM-VARS' for their
immediate vicinity.
"
  (let ((*Toot* Toot))
    (setf (Toot *client*) Toot)
    (update-Toot-last-active Toot)
    (unicast
     (list :|status| t
           :|from| "playWith"
           :|playWith| (Toot-name Toot)
           :|uuid| (Toot-UUID Toot)
           :|player| (when *user*
                       (list :|uuid| (person-uuid *user*)
                             :|name| (person-display-name *user*)
                             :|email| (person-first-email *user*)))))
    (burgeon-quiesced-state Toot)
    (broadcast (Toot-join-message Toot) :near *client* :except *client*)
    (broadcast (list :|status| t
                     :|from| "avatars"
                     :|inRoom| "@Tootsville"
                     :|avatars| (list :|joined| (Toot-info Toot)))
               :near *client*)
    (unicast (local-room-vars))
    (list 200 (from-avatars (plist-with-index 
                             (remove-if-not (lambda (Toot)
                                              (nearp Toot *Client*))
                                            (connected-toots)))))))

(definfinity play-with ((character) u r)
  "Choose a Toot as your active CHARACTER in the game.

CHARACTER must be the name of a Toot character owned by *USER*.

@subsection Usage

@verbatim
{ c: \"playWith\", d: { character: \"a-Toot-name\" } }
@end verbatim

@subsection Status 200 OK

@verbatim
{ from: \"playWith\",
  status: true }
@end verbatim

This calls `PLAY-WITH-TOOT' upon success, q.v.

@subsection Status 403 Not Your Toot

*USER* must  be the owner  of the Toot named  CHARACTER, or you  will be
 denied permission.

@verbatim
{ from: \"playWith\", status: false, error: \"Not your Toot\" }
@end verbatim

@subsection Status 404 No Such Toot

The Toot named CHARACTER must exist.

@verbatim
{ from: \"playWith\", status: false, error: \"No such Toot\" }
@end verbatim

"
  (if-let (Toot (find-record 'Toot :name character))
    (if (uuid:uuid= (Toot-player Toot) (person-uuid *user*))
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

(definfinity quiesce ((wtl d3 emotion world latitude longitude altitude)
                      Toot r)
  "Quiesce Toot values to database for logout, or periodically as a backup.

@cindex Quiescing and Burgeoning 

@subsection Usage

@verbatim
{ wtl: { course: { ... }, facing: RADIANS },
  d3: { ... },
  emotion: \"EXPRESSION\",
  world: \"WORLD\",
  latitude: LAT,
  longitude: LONG,
  altitude: ALT }
@end verbatim

Facing may be provided as per `INTERPRET-FACING'.

A quiescent copy of the character information will be saved in a
central database. Should the player lose connection and not
successfully reconnect, eg. should the player quit by closing their
browser altogether, or lose Internet connectivity, &c., the last
quiesced form of their character will be restored when they reconnect.

Note that, as with the rest of the system, we are currently using
@code{wtl} but are building up the infrastructure for @code{d3}
walking in future. When both a @code{wtl} and a @code{d3} value exist,
the @code{d3} value supersedes the @code{wtl}. @code{d} walking values
must be encoded in @code{wtl} form, but @code{d} walking values are
not expected in Tootsville V.

@subsection Status 200 OK

Upon success, the client whose status was saved is notified by a
message of the form:

@verbatim
{ from: \"quiesce\",
  status: true }
@end verbatim

@subsection Asynchronous periodic demands

From time to time, clients may be asked to update their quiescent
state. When a client receives a message of the form:

@verbatim
{ from: \"quiesce\",
  status: false }
@end verbatim

… they are expected to submit a quiesce message to the central
servers.
"
  
  (find-record 'Toot :UUID (Toot-UUID Toot)) ; signals an error if not found
  
  (if-let (old (ignore-not-found (find-record 'Toot-quiesced :Toot (Toot-UUID Toot))))
    (progn (setf (Toot-quiesced-Toot old) (Toot-UUID Toot)
                 (Toot-quiesced-world old) (or world :chor)
                 (Toot-quiesced-latitude old) (or latitude 0)
                 (Toot-quiesced-longitude old) (or longitude 0)
                 (Toot-quiesced-altitude old) (or altitude 0)
                 (Toot-quiesced-wtl old) (jonathan.encode:to-json wtl)
                 (Toot-quiesced-d3 old) (jonathan.encode:to-json d3)
                 (Toot-quiesced-peer-address old) (peer-address Toot)
                 (Toot-quiesced-emotion old) (or emotion "")
                 (Toot-quiesced-observed old) (now))
           (save-record old))
    (handler-case
        (make-record 'Toot-quiesced
                     :Toot (Toot-UUID Toot)
                     :world (or world :chor)
                     :latitude (or latitude 0)
                     :longitude (or longitude 0)
                     :altitude (or altitude 0)
                     :wtl (jonathan.encode:to-json wtl)
                     :d3 (jonathan.encode:to-json d3)
                     :peer-address (peer-address Toot)                                 
                     :emotion emotion
                     :observed (now))
      (dbi.error:dbi-database-error (e)
        ;; MariaDB error code for duplicate row in DB
        ;; XXX not sure why this is necessary, race condition maybe?
        (when (= 1452 (dbi.error:database-error-code e))
          (return))
        (error e))))
  (list :|from| "quiesce"
        :|status| t))

(definfinity consider-child-approval ((uuid) u r)
  "Consider whether to approve a child's request with ID UUID.

@subsection Usage

The client sends  this packet when the player, a  parent or guardian who
has a  child Toot account on  their user profile, wishes  to be prompted
again  as  to whether  to  approve  or deny  a  child  request with  the
given UUID.

@verbatim
{ uuid: \"UUID-OF-REQUEST\" }
@end verbatim

The  client  will  receive  a  @code{prompt}  message  to  that  effect.
See `INFINITY-PROMPT-REPLY' for a discussion of the @code{prompt} packet
and its replies.

There  is  no  direct  reply  to  this  packet,  only  the  asynchronous
prompt (or admin error message, see below).

@subsection Error conditions

In the  event of  an error,  the player  will receive  an administrative
message  explaining the  problem.  No machine-readable  error packet  is
returned to the client.

Possible error conditions include:

@itemize

@item
The UUID represents a child request from another user's Toot

@item
The UUID  does not represent any  current child request; we  assume that
this means  the relevant request once  existed, but was culled  after it
aged out  --- but  we have  no way  of knowing  for sure,  since expired
requests are hard deleted.

@end itemize

"
  (if-let (request (find-record 'child-request :uuid uuid))
    (if (uuid:uuid= (Toot-player (find-reference request :Toot))
                    (person-uuid *user*))
        (send-parent-child-login-request request)
        (private-admin-message "Unable to approve request"
                               "You are not the owner of that Toot."))
    (private-admin-message "Unable to approve request"
                           "That request is no longer available."))
  nil)

(definfinity read-map (nil u r)
  "Get the positions of badges and named locations on the map.

@subsection Usage

This command requires no parameters.

@subsection Status 200 OK

This returns  two lists,  a list of  named places, and  a list  of badge
places. These make up the map of Tootanga.

@verbatim
{ from: \"readMap\",
  status: true,
  spots: [ TootSquare: [ 0, 0, 0, \"CHOR\", \"Toot Square\" ], ... ],
  badges: [ ... ] }
@end verbatim

The  lists @code{spots}  and  @code{badges}  each consist  of  a set  of
monikers or labels as keys.

The  values of  each key  on @code{spots}  are the  latitude, longitude,
altitude, world, and UI label for that spot.

The values of each key on  @code{badges} are only a latitude, longitude,
altitude, and world. The moniker itself represents the badge.

An SVG graphic for each badge should be found on Jumbo in the form
@code{https://jumbo.tootsville.org/Assets/Badges/5/BADGE-NAME.svg}.

@subsection Overview of Spots and Badges

A Spot,  or a Named  Place, associates a moniker  with a text  label and
a position on  the map. These can be used  by certain operator commands,
or by  users, and basically represent  what used to be  rooms in earlier
versions of  Tootsville. We are  trying to enforce  the use of  the name
``spot'' to  mean a  named place  to limit  the relative  confusion with
Places found within the game world, such as cobblestone paths.

A Badge represents a graphic that appears  on the Map App on the Tootnix
Mobile phone. These  are usually magnets indicating a  special event, or
a visit by a special character,  at that location. Little Reminders mice
will be used to draw the players'  attention to these badges in order to
garner attention to special events.

"
  (error 'unimplemented))

(definfinity user-agent ((agent version navigator for-romance) u r)
  "The client can voluntarily report its version information.

The server could potentially offer a different protocol or other
affordances for known bugs or limitations in the client.

In practice, it's currently logged and forgotten."
  (v:info :user-agent "Client ~s reports user agent ~a/~a, ~
navigator ~a, for Romance version ~f"
          *client* agent version navigator for-romance)
  (list :|from| "userAgent"
        :|status| t))
