(in-package :Tootsville) ; -*- lisp -*-

;;; legacy-commands.lisp is part of Tootsville
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


;;;; Legacy ∞ mode options
;;;
;;; These options were in existence for Romance 1.1 and/or 1.2. Some are
;;; deprecated;   others   are  now   useless   and   report  as   much.
;;; Removed options will  return an HTTP 410 ``Gone''  message over REST
;;; or the analogous reply over a stream connection.
;;;
;;; TODO a manual section explaining the differences under DEFINFINITY
;;;
;;; r ∈ ( :Tootanga :Moon :Other-moon  :Pink-moon :Space ) and right now
;;; pinned at :Tootanga
;;;
;;; u is an @emph{authenticated} user object
;;;
;;; d is the d: element of the command already converted to plist form.

(definfinity add-furniture ((&rest d) user recipient/s)
  "Alias for `INFINITY-SET-FURNITURE', q.v."
  (apply #'infinity-set-furniture (list d user recipient/s)))

(definfinity add-to-list (nil user recipient/s)
  "add a user to a buddy list or ignore list (removed in 1.2)

…using   the   traditional   (online-only,   no   notification   engine)
mechanism    (using    out    of     band    methods).    Compare    vs.
requestBuddy `INFINITY-REQUEST-BUDDY'

@subsection 410 Gone

This was a legacy feature removed in Romance 1.2.

@subsection Changes from 1.1 to 1.2

This function was replaced  with `INFINITY-REQUEST-BUDDY' — requestBuddy
— q.v."
  (error 'legacy-gone))

(definfinity click ((on x y z with) user recipient/s)
  "Used by the client  to report a mouse click or  finger tap.

If the user  clicks on a placed-item, this method  should be called with
the following syntax:

@verbatim
{\"c\":\"click\", \"d\":{ \"on\": itemID, \"x\": x, \"y\": y, \"z\": z, \"with\": mods } }
@end verbatim

Note that the (x,y,z) values passed  are relative to the origin point of
the item;  thus, if an  item is placed  at (200,200,200) and  is clicked
at (210,210,210), the coördinates reported should be (10,10,10).

@subsection Modifiers characters

The mods string  can contain any of the following  symbols in any order,
representing modifier keys that were held  down when the user clicked on
the item:

@table @samp
@item \^

Caret  represents  the @key{Control}  or  @key{Ctrl}  key on  Linux®  or
Windows systems, or the @key{Command} key on macOS™.

@item S

Ess represents the @key{Shift} key on any platform.

@item C

Ci represents  the @key{Caps Lock}  state being enabled. May  be ignored
or omitted.

@item N

En represents  the @key{Num  Lock} state being  enabled. May  be ignored
or omitted.

@item M

Em  represents  the @key{Meta}  key  on  Linux,  @key{Alt} on  Linux  or
Windows, or @key{Option} on macOS.

@item L

Ell represents the @key{Scroll Lock} state being enabled. May be ignored
or omitted.

@item A

Ay  represents the  @key{Alt-Gr}  key on  any  platform (if  supported).
May be ignored or omitted.

@item *

Asterisk represents  the @key{Super} key on  Linux or @key{Windows-Logo}
key on Windows.

@item 1, 2, 3
Numbers represent mouse buttons: 1 for left, 2 for middle, 3 for right.

@item +, -
Plus represents rolling a scroll wheel down; Minus to scroll up

@item <, >
Less-than represents rolling a scroll knob left; greater-than, right.

@end table

@subsection Flash details

In the Flash MouseEvent object, you can create the \"mods\" with the following:

@verbatim
var mods:String = \"\";
if (ev.altKey) mods += \"M\";
if (ev.commandKey || ev.ctrlKey) mods += \"^\";
if (ev.shiftKey) mods += \"S\";
if (ev.type == ev.CLICK) mods += \"1\";
if (ev.type == ev.MIDDLE_CLICK) mods += \"2\";
if (ev.type == ev.RIGHT_CLICK) mods += \"3\";
if (ev.type == ev.MOUSE_WHEEL)
{ if (ev.delta < 0) mods += \"-\";
 if (ev.delta > 0) mods += \"+\"; }
if (Keyboard.numLock) mods += \"N\";
if (Keyboard.capsLock) mods += \"C\";
@end verbatim

@subsection Changes from 1.2

@itemize

@item

The @samp{z} coördinate is no longer optional.

@item

A form of the @samp{click} command which omitted the @samp{on} parameter.

@item

The itemID is  an UUID Base64 string,  not a moniker string  — but these
were always meant to be opaque identifiers.

@end itemize

@subsection 202 Accepted

The  click event  has been  noted. Any  outcomes of  that event  will be
broadcast over other channels.

@subsection 204 No Content

The click event is being ignored; ITEM-ID was not an interesting item to
the server.
"
  (error 'unimplemented))
(definfinity create-user-house ((lot house index) user recipient/s)
  "Either claim the user's house and lot, or add a room to their house.

Data describing the user's lot.

When the player has  found an empty lot and wishes to  claim it as their
own, they choose a base house item and send

@verb{| { lot: lot-ID, house: house-ID },  |}

When the player has a house and wishes to add a room, they send

 @verb{| { index: roomIndex, connectTo: roomIndex, connectAt: pointMoniker } |}

@subsection 201 Created

A house or room was created as demanded

@subsection 409 Conflict

A house already exists  on that lot, or, a room  is already connected at
the  given connection  point. The  request cannot  be completed  because
something  already  exists  where  the new  construction  was  meant  to
be placed.

@subsection 404 Not Found

The house ID or room connection point given was not found.

@subsection Changes from 1.2 to 2.0

In 1.2 adding a room required only an index.
"
  (let ((lot (find-record 'lot :id lot)))
    (unless lot
      (error 'not-found :the :lot))
    (ecase (lot-ownership lot)
      (:public (error 'infinity-error :http-status 400 :memo :lot-is-public-property))
                                        ; TODO permission error, 403?
      (:common  (error 'infinity-error :http-status 400 :memo :lot-is-common-property))
                                        ;  TODO permission error, 403?
      (:private
       (cond
         ((null (lot-owner-Toot lot))
          ;; claim a lot and build a basic house
          (unless (or (null index) (and (numberp index) (zerop index)))
            (infinity-error 400 :empty-lot-use-house-not-index))
          (setf (lot-owner-Toot lot) (Toot-uuid *Toot*))
          ;; TODO check house is an house
          ;; TODO place house on lot
          )
         ((equal (lot-owner-Toot lot) (Toot-uuid *Toot*))
          (unless (null house)
            (infinity-error 400 :did-you-want-house-or-room))
          (when (or (null index)
                    (not (numberp index))
                    (zerop index)
                    (minusp index)
                    (> index 9))
            (infinity-error 400 :index-not-valid))
          ;; TODO create room and affix to house
          ))))))

(definfinity dofff ((&rest d) user recipient/s)
  "Doff all clothing items.

Takes no parameters.

This does not  un-equip an item held in the  @code{TRUNK}. This does not
remove or  alter a Toot's pattern.  For non-Toot avatars, this  does not
un-equip an item held in the @code{HAND}, @code{LHAND}, or @code{RHAND}.

Sends two  responses: a success  reply from @code{doff}, then  total avatar
info from @code{wardrobe}. See `INFINITY-WARDROBE'.

@subsection 200 OK

All clothing items have been removed."
  ;; TODO dofff
  )

(definfinity don ((slot color) user recipient/s)
  "Don (or equip) an item

JSON  object has  the  item UUID  number to  be  worn (clothes,  pivitz,
trunk).

See `WEAR-SLOT-INFO' for  descriptions of how wear  slots are identified
and described.  Note that  the appropriate wear  slot can  be determined
from the  item's template; see  `ITEM-TEMPLATE-INFO'. For a list  of all
wear slots, see `INFINITY-ENUMERATE-WEAR-SLOTS' (new in 2.0).

Response with total avatar info from @code{wardrobe}. See `INFINITY-WARDROBE'.

Parameters: jso  - @verb{| {  slot: item-uuid }  |}

@subsection 200 OK

The item has been donned or equipped.

@subsection 400 Bad Request

The removed @code{color} attribute was submitted.

@subsection 404 Not Found

The item UUID specified (by ``@code{slot}'') was not recognized.

@subsection 403 Forbidden

The item UUID specified was not owned by the player requesting to don it.

@subsection 409 Conflict

The  item  requested   cannot  be  equipped  by   the  player's  avatar.
For  example, a  Toot  character  cannot equip  an  item which  requires
a @code{HAND}  slot, since  Toots have  no fingers.  Items which  do not
occupy a wear slot also cannot be equipped, e.g. a tree.

@subsection Changes from 1.2 to 2.0

Colors of  items can no  longer be changed  when donning them.  This was
meant  for pattern  changing  in  1.2, which  must  now be  accomplished
in-game via Doodle. The @code{color} parameter must be null.

Patterns are no longer clothing items.

Equipment  held  in the  @code{TRUNK}  is  now explicitly  supported  as
a distinct  wear slot with specific  meaning (ie, the user  can activate
that item).
"
  (unless (null color)
    (error 'infinity-error :http-status 400 :memo :cannot-select-color))
  ;; TODO don
  )

(definfinity echo ((&rest d) user recipient/s)
  "Echoes back the supplied JSON (or ActionScript) object to the client.

This method exists solely for testing purposes.

Sends response containing:

@verbatim
⇒ { c: \"echo\", d: { foo: 42 } }

{ from: \"echo\", status: true, \"You said\": { foo: 42 } }
@end verbatim

Note that the field name is literally ``@samp{You said:}.''

Parameters:
@table @code
@item jso
Any JSON object, the contents of which will be returned to the caller.
@item u
The user calling (to whom the response is sent)
@end table

@subsection 200 OK

The response is echoed back to the user.

@subsection Limitations

The echo packet must be less  than 1,024 Unicode characters in length or
it will be  truncated to 1,024 characters. No warning  will be issued to
the user in the case of truncation."
  (list :|from| "echo" :|status| t :|You said| (limit-string-length d 1024)))

(definfinity end-event ((moniker event-id score status) user recipient/s)
  "End an event started by `INFINITY-START-EVENT'

This method terminates an event (probably  a fountain, in 2.0) which was
initiated by startEvent.

For fountains, the score is ignored,  and a random number from 1..100 is
used  as  the effective  score.  Since  fountains  (should) have  a  1:1
points:peanuts ratio, this will earn  the player 1..100 peanuts randomly
per fountain visit. Fountains do not respond with a @code{highScores} array.

Input parameters are:
@table @code
@item moniker
the event's moniker;
@item eventID
the event ID to be ended;
@item score
 the earned score, in points (not peanuts);
@item status
one of ``@code{cxl}'' to cancel an event
 (in which case, @code{score} should be 0),
 or ``@code{cmp}'' to complete an event
(@code{score} may be zero or more).
@end table


@subsection 200 OK

Response:  JSON sent  to user:

@verbatim
{ ended:  event ID,
 peanuts: number  of  peanuts earned,
 highScores: array of  scores, indexed by position on the
 high score  list (1..24), each  of which  contains: { points:  number of
 points  scored by  the  high-scoring  user; userName:  the  name of  the
 user },
 totalPeanuts: user's new total peanut balance }
@end verbatim

Additionally, if this user earned a  high score on this event, s/he will
get the attribute in the top  level of the response as \"gotHighScore\":
with the value being the position  number which was earned. For example,
earning no  high score omits the  \"gotHighScore\" attribute altogether;
earning  the third  highest score  will return  instead @code{\"gotHighScore\":
3}.

@subsection Changes from 1.0 to 1.1

Added the @code{status} parameter. In 1.1, this was optional and defaulted to
 @code{cmp}.

@subsection Changes from 1.1 to 1.2

Made the @code{status} parameter mandatory.

@subsection Changes from 1.2 to 2.0

In 1.0 - 1.2, the primary use of this was for out-of-world Flash minigames,
with the fountains as a secondary usage. In 2.0, this is used for fountains as
well as treasure chests (magic boxes, &c).

The Flash minigames would report a score, and a server-side table would scale
that score to an appropriate number of peanuts earned.

Now, the @code{score} and @code{highScores} functionality is ignored and the
caller should listen for a subsequent gift of peanuts, fairy dust, or an item. "
  (error 'unimplemented))

(definfinity finger ((&rest Toots-with-keys) user recipient/s)
  "Get public info for a list of Toots.

Reply format:

@verbatim
{ from: avatars, status: true, avatars: { 0: { TOOT-INFO … }, … }
@end verbatim

User public information is in the format of `TOOT-INFO', which should be
a supserset  of what @code{AbstructUser.getPublicInfo()} used  to return
in 1.2.

jso - JSON object, with (ignored) keys  tied to values which must be the
names of Toots."
  (list 200
        (from-Avatars Toots-with-keys)))

(defun from-avatars (Toots-with-keys)
  (let ((hash (make-hash-table :test 'equal)))
    (list :|avatars| 
          (loop for (key Toot) on Toots-with-keys by #'cddr
             do (setf (gethash (princ-to-string key) hash)
                      (Toot-info (ensure-Toot Toot)))
             return hash)
          :|from| "avatars"
          :|inRoom| "@Tootsville"
          :|status| t)))

(definfinity game-action ((&rest more-params &key action &allow-other-keys) user recipient/s)
  "Send an in-world game's action.

These are actions that affect in-world minigames.

@subsection Overview of In-World Minigames

In-world minigames generally don't use much of a special interface,
but sometimes require some kind of additional overlay. The game
actions are usually signaled by in-game items.

In-world minigames include soccer, volleyball, croquet, bowling, card
table games, tag, and more. Each of these games may have a score and
possibly some enforceable rules, although we often leave enforcement
of the rules to the players (so that they can choose which rule set
they like).

These game actions are identified by function names beginning with
``GAME-ACTION-.'' Some of them include:

@itemize

@item

`GAME-ACTION-START-SPORTS-BALL-GAME'

@item

`GAME-ACION-JOIN-CARD-GAME'

@item

`GAME-ACTION-TAG-YOU-RE-IT'

@end itemize

@subsection General Structure

A @code{gameAction} packet has a @code{d} datum with a key
@code{action}, which is used to further dispatch the game action to
its appropriate handler. The @code{action} value is the smallCamelCase
version of the ``GAME-ACTION-function-name'' that will actually handle
it.

Refer to the individual game action functions for further details.

@subsection Status 400 Error

If the @code{action} is not supplied, or if no such action is known to
the server, then an error 400 is returned, with a JSON error packet of
the usual form:

@verbatim
{ from: \"gameAction\",
  status: false,
  error: \"error message text\" }
@end verbatim
"
  (if-let (fn (find-symbol (concatenate 'string "GAME-ACTION-"
                                        (symbol-munger:camel-case->lisp-name action))
                           (find-package :Tootsville)))
    (apply fn more-params)
    (list 400 (list :|from| "gameAction"
                    :|status| :false
                    :|error| (format nil "No such gameAction: ~a" action)))))

(definfinity get-avatars ((&rest _+user-names) user recipient/s)
  "Get avatar data for a list of (other) users. cv. `INFINITY-FINGER'

Parameters:

jso - JSON object, with (ignored) keys  tied to values which must be the
names of users. e.g. @{ 0: \"someUser\", 1: \"otherUser\" @}

u - The calling user. The calling user's avatar data will not be returned."
  (error 'unimplemented))

(definfinity get-color-palettes (nil user recipient/s)
  "getColorPalettes

@subsection 410 Gone

Removed.. This  routine appeared to be  unused by anyone in  Romance 1.1
and was removed in 1.2.

returns palettes  in \"extraColors\",  \"baseColors\", \"patternColors\"
in the JSON result object (from: \"getColorPalettes\")

@subsection Changes from 1.1 to 1.2

Not used in Tootsville any more.  The analogous palettes in Li'l Vampies
and Empires  of the Air are  being replaced with algorithmic  checks, so
this routine was removed in Romance 1.2.0.

@subsection Revival?

This might be revived in 2.0 for the UI to present lists of named colors
during character creation, rather than  using hard-coded lists that have
to be separately maintained in the client and server both."
  (error 'legacy-gone))

(definfinity get-inventory ((&rest d) user recipient/s)
  "get all inventory for an user (themself) — both active and inactive

Returns a set of items as
@verbatim
inv: { 0: { id: 123, isActive: boolean }, ... }
@end verbatim

furniture with  placement data  will also  have x,  y, and  facing vars.
Other attributes  are \"from\":\"inventory\",  \"type\": matching the  type of
the question

d — empty

u — The user whose inventory to be searched
"
  )
(definfinity get-Inventory-By-Type ((type) user recipient/s)
  "Get a subset of items from your own inventory

The @code{type} can be one of two options.

For legacy compatibility, the following list of @code{type} codes can be
supplied. These may  be more convenient for the  front-end. Legacy users
of  code sequences  beginning with  @code{#} or  @code{$} are  no longer
supported, however.

@table @code

@item clothes

All items which can be worn in any slot other than @code{TRUNK}, @code{HAND},
 @code{LHAND} or @code{RHAND}, or @code{PIVITZ}

@item pivitz

Only Pivitz items

@item patterns

Ignored for backward-compatibility.

@item furniture

Any item which cannot be equipped in any way

@item structure

Ignored for backward-compatibility

@item music

Ignored for backward-compatibility

@item tootsBook

Ignored for backward-compatibility

@item stationery

Ignored in 2.0 but may be revived in 2.1

@item accessories

All items which can be equipped in @code{TRUNK} slot, or @code{HAND},
 @code{LHAND} or @code{RHAND} (for non-Toot characters).

@end table

In  addition,   @code{type}  can  be   a  string  containing   the  word
@code{point} followed by a space and the moniker of an avatar attachment
point,  in  which  case  all  items   which  can  be  equipped  to  that
point (regardless  of valence)  are returned;  or, the  word @code{slot}
followed by a space and the ID number of a specific wear-slot.

FIXME — the following documentation has lies in it.

Parameters:

jso    -       type:    TYPE-STRING     —
JSON object has the type of item from the strings in the config file.

OR, you can specify  an item type by passing # plus its  ID, or a string
of them; e.g. for items of type 1, pass \"#1,\" for items of types 2 or 3,
pass \"#2:3\"

OR, you  can specify  a list  of item  type strings  using '$'  plus the
string identifiers divided by ':', e.g. \"$Pants:Shirts\"

Returns a  set of items  as
@verbatim
inv:  { 0: {  id: 123, isActive:  boolean },
 ... }
@end verbatim
— furniture with placement data  will also have x, y, and facing
vars. Other attributes are \"from\":\"inventory\", \"type\": matching the type
of the question

You can also supply withActive: false to screen out active items.
Return data

@verbatim
 { from: inventory, for: USER-LOGIN, type: TYPE-STRING,
 inv: {
 #: { ITEM-INFO },
 #: { ITEM-INFO } …
 }
 }
@end verbatim

TODO see Java getInventoryByType(JSONObject, AbstractUser, AbstractUser,
Room)  for   discussion  of   TYPE-STRING;  or  @{   type:  TYPE-STRING,
withActive:  BOOLEAN @}  to  mask  out active  items;  optional @{  who:
LOGIN-NAME @} to look at someone else's inventory

u - The user whose inventory to be searched, who is the caller of this routine

"
  (string-case type
    ("clothes")
    ;; All items which can be worn in any slot other than @code{TRUNK}, @code{HAND},
    ;; @code{LHAND} or @code{RHAND}, or @code{PIVITZ}
    ("pivitz")
    ("furniture")
    ("stationery")
    ("accessories")
    ;; All items which can be equipped in @code{TRUNK} slot, or @code{HAND},
    ;; @code{LHAND} or @code{RHAND} (for non-Toot characters).
    ))

(definfinity get-online-users ((in-room) user recipient/s)
  "Get a list of users in a Zone, or in a Room.

This is an administrative function, only available to staff members.

Parameters:

jso  -  The JSON  data  provided  by the  caller.  If  this contains  an
attribute of \"inRoom\"  with a room moniker, we'll only  return the users
in that room. Otherwise, all users in the Zone will be returned.

u - The caller's ID. Must have staff privileges.

room - The room from which the caller is making the extension call: ignored.
Throws:

org.json.JSONException - if the JSON data can't be processed, in or out.

PrivilegeRequiredException    -    if     the    user    doesn't    have
STAFF_LEVEL_STAFF_MEMBER

NotFoundException - if the room requested doesn't exist

"
  (if (builder-Toot-p)
      (list 200 (list :|from| "get-online-users"
                      :|status| t
                      :|inRoom| "@Tootsville"
                      :|toots| (connected-toots)))
      (list 403 (list :|from| "get-online-users"
                      :|status| :false
                      :|error| "That is a Builder Toot command"))))

(definfinity get-Room-List (nil user recipient/s)
  "Get a list of all ``well known'' Rooms currently active/visible.

``Rooms'' no longer exist. The ``rooms'' are now known as ``planes.''

The following planes exist in Tootsville:

@table @code

@item CHOR

Tootanga (Choerogyllum)

@item ORBIT

Space (Orbit)

@item MOON

The Moon

@item OTHM

The Other Moon

@item PINK

The Pink Moon

@end table
"
  #("CHOR" "ORBIT" "MOON" "OTHM" "PINK"))

(definfinity get-server-time (nil user recipient/s)
  "Send the server time to the client requesting it

For synchronization purposes.

Sends a JSON object with a property, @code{serverTime}, with the current
time in milliseconds (give or take transit time). This is the Unix time,
not the Universal time, and in milliseconds, not seconds."
  (list 200
        (list :|from| "getServerTime"
              :|status| t
              :|serverTime| (* (- (get-universal-time)
                                  +unix-zero-in-universal-time+)
                               1000))))

(definfinity get-session-apple ((&rest d) user recipient/s)
  "Initialise a session key for stream or batch mode operations (Unused now)

@subsection 410 Gone

This function is no longer needed.

@subsection New in 1.1

This feature was added in Romance 1.1 and removed in 2.0"
  (error 'legacy-gone))

(definfinity get-store-item-info ((&rest jso) user recipient/s)
  "Get information about items in a store which can be purchased.

Input:  jso -  JavaScript array-style  object  where the  key names  are
insignificant, but the values are store item ID's

The returned packet is @code{from: \"getStoreItemInfo\"} and contains an
object @code{items}  with a matching set  of keys, but whose  values are
objects in the form of `STORE-ITEM-INFO', qv.

@subsection Changes from 1.2 to 2.0

Additional information is returned in `STORE-ITEM-INFO' objects.

@subsection 200 OK

Returns the details about store items queried-for by the user.

@subsection 404 Not Found

If any item ID cannot be found, the entire query fails with a 404."
  (let ((hash (make-hash-table :test 'equal)))
    (setf (gethash "from" hash) "getStoreItemInfo"
          (gethash "status" hash) t)
    (loop for (key id) on jso by #'cddr
       do (setf (gethash key hash) (store-info
                                    (find-record 'store-item
                                                 :id (parse-integer id)))))
    (list 200 hash)))

(defun Toot-buddy-list (&optional (Toot *Toot*))
  (concatenate 
   'list
   (mapcar
    (lambda (a-Toot)
      (list :|id| (Toot-uuid a-Toot)
            :|n| (Toot-name a-Toot)
            :|starredP| t))
    (remove-if (lambda (a-Toot)
                 (equal (Toot-name a-Toot)
                        (Toot-name Toot)))
               (sort (player-Toots)
                     #'timestamp>
                     :key (lambda (a-Toot)
                            (or (Toot-last-active a-Toot)
                                (universal-to-timestamp 0))))))
   (mapcar 
    (lambda (contact)
      (list :|id| (contact-uuid contact)
            :|n| (Toot-name (find-reference contact :contact))
            :|starredP| (contact-starredp contact)
            :|added| (contact-added contact)
            :|lastUsed| (contact-last-used contact)))
    (sort
     (find-records 'contact :owner (Toot-UUID Toot))
     #'< :key #'contact-last-used))))

(definfinity get-user-lists (nil user recipient/s)
  "Get the user's buddy list and ignore list.

@verbatim
{ buddyList: { … } , ignoreList: { … } }
@end verbatim

jso - no parameters needed

u - The user whose buddy and ignore lists will be fetched

@subsection Changes from 1.2

Buddies on the buddy list can be starred, with attribute @code{starred: true}.

"
  (list 200 (list :|from| "getUserLists"
                  :|status| t
                  :|buddyList| (Toot-buddy-list)
                  :|ignoreList| #())))

(definfinity get-Wallet ((&rest d) user recipient/s)
  "Get the contents of the player's wallet (peanuts and fairy dust)

Returns information in the form `WALLET-INFO', qv.

@subsection Changes from 1.1 to 1.2

Currencies were made explicit, allowing currencies other than peanuts to
be potentially supported in future.

@subsection Changes from 1.2 to 2.0

Fairy Dust was added after 1.2.

@subsection 200 OK

Returns the wallet info."
  (list 200 (wallet-info *Toot*)))

(definfinity get-Zone-List (nil user recipient/s)
  "Get a list of all Zones currently active/visible.

This returns \"Universe\" as the only Zone.

@subsection Changes from 1.2 to 2.0

Zones no longer exist."
  (list 200 (list 0 "Universe")))

(definfinity give ((slot to) user recipient/s)
  "Give an item to another user.

XXX: notify the recipient using notifications (currently using a Message
Box popup message)

jso - @verb{| { slot: ITEM-UUID, to: TOOT-NAME } |}

u - giver

If the item  is currently equipped or being worn,  it will be unequipped
as it is being given away.

@subsection 412 Precondition Failed

An item cannot be given if you do not possess it to begin with.

@subsection 404 Not Found

The item and the recipient must each exist.

@subsection 403 Forbidden

Certain items  cannot be  traded. This  includes gifting,  dropping, &c.
See `ITEM-TEMPLATE-CAN-TRADE-P' for a discussion.

@subsection Changes from 1.2 to 2.0

Players used  to be unable  to gift items  to non-VIT members;  with the
abolition  of   VIT  status,   everyone  is   very  important   and  can
receive items.

"
  (gift-item (find-record 'item :uuid slot)
             *Toot*
             (find-record 'Toot :name to)))

(definfinity go ((do x y z facing) user recipient/s)
  "go to a place and/or perform a gesture

@verbatim

{ do: VERB (required)
 x: DEST, y: DEST, z: DEST (each optional, but if one is given, all 3 must be)
facing: FACING (optional)
}
@end verbatim

u - the user doing something

@subsection Changes from 1.2 to 2.0

@code{z} can no longer be omitted if @code{x} or @code{y} are specified.
"
  (error 'unimplemented))
(definfinity init-user-room ((room autoJoin) user recipient/s)
  "Create a user's private room (in their house).



Creates room  named user/user's  name/room — room  is the  room index
number given in the JSON data as  ``room,'' it will always be zero right
now  as  all users  have  single-room  houses.  This will  populate  all
furniture-type items for that room onto a set of room variables owned by
the user. The  user calling this method  must be the owner  of the room.
If the  user has not visited  his/her house before, this  will return an
asynchronous \"make a  new house\" notification to do  the \"first run\"
screen,  by sending  a  message of  type

@verbatim
{ \"from\":  \"initUserRoom\",
\"status\": false, \"err\": \"showFirstRun\" }.
@end verbatim

Success:   responds    with   true,   and   \"moniker\":    the   room's
moniker (user/WHOEVER/123)

If unneccessary, returns an error of \"exists\" meaning that the room is
already existing

@verbatim
jso - { room: (room-number), autoJoin: (boolean) }
@end verbatim

u - The user whose house-room needs to be initialized

@subsection 410 Gone

Removed in 2.0.

User rooms are no longer needed nor supported."
  (error 'unimplemented))

(definfinity join ((room from) user recipient/s)
  "Join a room.

On success, sends back the set  of room join events;
 but on failure, replies with  @{ from: roomJoin, status: false, err:
 ...@}

 NOTE the inconsistency: the command is  join, but the reply comes from
 roomJoin

zone.notFound
 The user is not in a Zone

room.noMoniker
 No room moniker was given to be joined

room.notFound
 The room moniker does not refer to an actual room in this Zone

room.full
 The room is too full (too many users)

 Parameters:
 jso -  @{ room: MONIKER @} or @{ room: MONIKER, from: MONIKER @}

u - the user joining the room

@subsection 410 Gone

Removed in 2.0."
  (error 'unimplemented))

(definfinity logout ((&rest d) user recipient/s)
  "Log out of this game session

@subsection Changes from 1.2 to 2.0

There was a bug in the Persephone client that caused it to explode if we
logged it out before it received  & processed the logout message. So, we
waited for the expected lag time to expire and then throw 2 full seconds
of wasted wait time after it, which had ought to be enough time. This is
no longer supported."
  (error 'unimplemented))

(definfinity mail-customer-service ((&rest d) user recipient/s)
  "  send an eMail to customer service (feedback)

 Parameters:
 jso - @{ subject: STRING, body: STRING @}
 u - the user sending the feedback

 "
  (error 'unimplemented))

(definfinity peek-at-inventory ((who type) user recipient/s)
  "Handle looking at other user's inventories

Parameters: jso - @{\"who\": the login name of the user of whom to get
the inventory @}; optional \"type\": to filter by type.  (see
`INFINITY-GET-INVENTORY-BY-TYPE' for details)

u  - The  user requesting  the inventory

room  - The  room in
 which the  request occurs

Throws: org.json.JSONException -  Thrown if
 the data  cannot be interpreted  from the  JSON objects passed  in, or
 conversely,  if  we   can't  encode  a  response  into   a  JSON  form

NotFoundException - Could not find a user with that name"
  (error 'unimplemented))

(definfinity ping ((ping-started) user recipient/s)
  "Send a ping to the server to get back a pong.

This also updates the user's last-active timestamp.

@subsection 200 OK

The response packet contains literally

@table @code
@item from
@code{\"ping\"}
@item status
@code{true}
@item ping
@code{\"pong\"}
@item pingStarted
see below
@item serverTime
The server's time as a Unix-epoch timestamp in milliseconds.
@end table

If  the  user sends  a  @code{pingStarted}  value,  it is  replied  back
unchanged; otherwise, @code{pingStarted} is replied with the server-time
as well.
"
  (let ((java-now (* (- (get-universal-time)
                        +Unix-zero-in-universal-time+)
                     1000)))
    (list 200 (list :|from| "ping"
                    :|ping| "pong"
                    :|status| t
                    :|pingStarted| (or ping-started java-now)
                    :|serverTime| java-now))))

(definfinity prompt-reply ((id reply) user recipient/s)
  "Accept a reply to a server-initiated prompt

@subsection Overviow of Prompts


 Server initiates prompt with:

@verbatim

 { \"from\" : \"prompt\",
 \"id\" : $ID,
 \"label\" : $LABEL,
 \"label_en_US\" : $LABEL,
 \"title\" : $TITLE,
 [ \"attachUser\" : $AVATAR_LABEL || \"attachItem\" : $ITEM_ID ] ,
 \"msg\" : $TEXT,
 \"replies\" :
 {  $TOKEN :
 { \"label\" : $BUTTON_LABEL,
 \"label_en_US\" : $BUTTON_LABEL,
 \"type\" : $BUTTON_TYPE },
 [ … ]
 }
 }

@end verbatim

 Where:

 $ID = arbitrary string with no \0 representing this question uniquely.
 This is not an user-visible string.

 $LABEL  =  concatenated to  the  window  title,  but  can be  used  to
 special-case / theme dialogs in future for certain purposes

 $TITLE = dialog title

 Only one of  either ``attachUser'' or ``attachItem''  will be included.
 $AVATAR_LABEL is the full avatar label  of the user/avatar to which the
 prompt  should  be attached  —  including  ``$''  and instance  ID,  if
 necessary —  where $ITEM_ID is the  room variable item ID  for a placed
 item in the room.

 $TEXT = message text, may have  \n, will often need word-wrapping, and
 ideally might make use of scroll bars

 The \"replies\"  assoc-array is of  arbitrary length,  where the
 key to each item is a $TOKEN,  again an arbitrary string without \0 to
 represent this response uniquely. This is not an user-visible string.

 $BUTTON_LABEL = the text to display. In future, the client may want to
 special-case specific text  to use icons or something:  e.g. \"OK\" will
 always be sent as precisely \"OK\" in English locale.

 $BUTTON_TYPE  = the  type of  the  button for  theming purposes  only.
 This is from the enumerated set [ \"aff\" | \"neg\" | \"neu\" ];

 aff = affirmative button, e.g. green button

 neg = negative button, e.g. red button

 neu = neutral button, e.g. purple button

 To  simplify future  i18n/l10n efforts,  the $LABEL  and $BUTTON_LABEL
 will always be sent twice. The user's current language version will be
 in  the \"label\"  properties. The  versions of  those strings  in the
 \"en_US\"  locale will  always be  in the  \"label_en_US\" properties.
 For purposes of theming and such, the label_en_US properties should be
 considered; the  \"label\" properties, however, should  always be used
 in presentation to the end-user.

 Example:
@verbatim

 { \"from\": \"prompt\", \"status\": \"true\",
 \"id\": \"fountain/tootSquare/Ã¾=?/x'deadbeef'\",
 \"label\": \"Fountain\", \"label_en_US\": \"Fountain\",
 \"title\": \"Make a Wish?\", \"msg\": \"Do you want to make a wish on the Toot Square fountain?\",
 \"replies\":
 { \"yes\": { \"label\": \"Make a Wish!\", \"label_en_US\": \"Make a Wish!\", \"type\": \"aff\" },
 \"no\": { \"label\": \"Not now\", \"label_en_US\": \"Not now\", \"type\": \"neg\" }
 }
 }

@end verbatim

 The client's response is a bit simpler:
@verbatim
 { \"c\": \"promptReply\", \"d\": { \"id\": $ID, \"reply\": $TOKEN } }
@end verbatim

 e.g.

@verbatim
 { \"c\":\"promptReply\", \"d\": { \"id\":  \"fountain/tootSquare/Ã¾=?/x'deadbeef'\", \"reply\": \"yes\" } }
@end verbatim


 As a special-case, for the reply only, the special $TOKEN of \"close\"
 should  be  sent  if  the  user dismissed  the  dialog  box  with  the
 close button.

 I'd  suggest that  the GUI  attach anonymous  functions with  the reply
 packets  already constructed  to  the various  dialog  box controls  at
 creation   time,  rather   than  trying   to  manage   some  queue   of
 pending prompts.

 To handle user expectations, it would  be best to display the button in
 a \"down\"  state until receiving  the server's acknowledgement  of the
 \"promptReply\" and disallow multiple-clicking in the window.

 The server will respond with

@verbatim
 { \"from\": \"promptReply\", \"status\": \"true\", \"id\": $ID }
@end verbatim


 For debugging purposes, the server may reply with

@verbatim
 { \"from\": \"promptReply\", \"status\": \"false\", \"err\": $ERR }
@end verbatim


 Where $ERR  will be  a brief  description of the  problem.
 
@table @code

@item reply.notFound

 a reply  button that was not a valid $TOKEN from the \"prompt\" command nor the special case @code{close}. 

@item id.notFound

a reply to a prompt that was not (recently) asked.

@end table

 A prompt  ID is not valid  across sessions; pending prompts  should be
 auto-closed   on  logout.   Prompts   can,   however,  remain   active
 indefinitely, even across room joins.

 Optional implementation:  the server may cancel  an outstanding prompt
 request by sending a packet with the following properties:

@verbatim
 from: prompt
 status: true
 cancel: $ID
@end verbatim

 Client applications may choose to dismiss the prompt automatically upon
 receiving such  a packet. Failure  to do so  is not an  error, however,
 later  attempting to  reply to  a canceled  prompt will  return status:
 @code{false,  err:  id.notFound}.  Clients must  accept  a  cancelation
 packet silently if they do not process it.

@subsection Usage

 Parameters:
 jso - in the form @{ id: $ID, reply: $TOKEN @}, as detailed above

 " 
  (v:info :prompt "Prompt reply: ~a → ~a" id reply)
  (if-let (request (and (zerop (search "child-request-" id))
                        (find-record 'child-request
                                     :uuid (subseq id 14))))
    (progn
      (or
       (string-case reply
         ("affirm" (parent-grant-permission request))
         ("deny" (parent-deny-permission request))
         ("close" #| no op |#)
         ("1hour" (parent-grant-permission request :hours 1))
         ("2hours" (parent-grant-permission request :hours 2))
         ("3hours" (parent-grant-permission request :hours 3))
         ("24hours" (parent-grant-permission request :hours 24))
         (otherwise
          (list 404 (list :|from| "promptReply"
                          :|status| :false
                          :|err| "reply.notFound"))))
       (list 200 (list :|from| "promptReply"
                       :|status| t
                       :|id| id))))
    (list 404 (list :|from| "promptReply"
                    :|status| :false
                    :|err| "id.notFound"))))

(definfinity remove-from-list ((buddy ignore) user recipient/s)
  "Remove someone from a buddy list or ignore list.

 jso - To remove a buddy: @{ buddy: (name) @};

or to attend to someone who had previously been ignored: @{ ignore: (name) @}

 u - The user whose buddy list or ignore list will be updated
 "
  (error 'unimplemented))

(definfinity report-bug ((info) user recipient/s)
  "This method allows the client to ``phone home'' to report a bug.

The bug  report itself is  just a giant  string embedded in  the ``bug''
element,  but  a ``cause''  element  will  be  treated as  the  subject.
Note that  the bug report  — like all  JSON input —  will be cut  off at
a certain limit (typically 4KiB), so  it's most helpful to keep it short
&  sweet:  Typically, this  should  be  something  like a  single  stack
backtrace (with as much detail as  possible), rather than a complete log
trace or something.

 The suggested usage  is to include the exception  itself as ``cause,''
 the backtrace up to a maximum of  1KiB, a log backtrace up to its last
 1KiB as ``bug,''  and as much machine-formatted  system information as
 possible in the ``info'' object. Fields of ``info''

 As many fields as possible, limit the contents to a reasonable length though…

 Note that the keys listed are strings, so e.g.:

@verbatim
 info [\"navigator.language\"] = navigator.language;
 info [\"navigator.product\"] = navigator.product;
@end verbatim

 ActionScript example:

@verbatim
 var info:Object = {
 \"flash.sys.ime\": flash.system.System.ime,
 \"flash.sys.totalMemory\": flash.system.System.totalMemory,
 \"flash.sys.useCodePage\": flash.system.System.useCodePage
 };
 // imperfect but close
 for ( var key in flash.system.Capabilities ) {
 info[\"flash.sysCap.\" + key] = flash.system.Capabilities[key];
 }
@end verbatim

@table @samp

@item navigator.language
 JavaScript: navigator.language
 @item navigator.product
 JavaScript: navigator.product
 @item navigator.appVersion
 JavaScript: navigator.appVersion
 @item navigator.platform
 JavaScript: navigator.platform
 @item navigator.vendor
 JavaScript: navigator.vendor
 @item navigator.appCodeName
 JavaScript: navigator.appCodeName
 @item navigator.cookieEnabled
 JavaScript: navigator.cookieEnabled
 @item navigator.appName
 JavaScript: navigator.appName
 @item navigator.productSub
 JavaScript: navigator.productSub
 @item navigator.userAgent
 JavaScript: navigator.userAgent
 @item navigator.vendorSub
 JavaScript: navigator.vendorSub
 @item screen.height
 JavaScript: screen.height; ActionScript: flash.system.Capabilities.screenResolutionX
 @item screen.width
 JavaScript: screen.width; ActionScript: flash.system.Capabilities.screenResolutionY
 @item screen.availHeight
 JavaScript: screen.availHeight; ActionScript: flash.display.Stage.fullScreenHeight
 @item screen.availWidth
 JavaScript: screen.availWidth; ActionScript: flash.display.Stage.fullScreenWidth
 @item window.outerHeight
 JavaScript: window.outerheight note case
 @item window.outerWidth
 JavaScript: window.outerwidth note case
 @item window.innerHeight
 JavaScript: window.innerheight note case
 @item window.innerWidth
 JavaScript: window.innerwidth note case
 @item window.windowName
 JavaScript: the window.name property of the highest parent of this window (frame); e.g.
@verbatim
 var topWindow = window.parent;
 for (; topWindow.parent != topWindow; topWindow = topWindow.parent)
 ;
 info [\"window.windowName\"] = topWindow.name;
@end verbatim

 @item flash.sys.totalMemory
 ActionScript: flash.system.System.totalMemory
 @item flash.sys.ime
 ActionScript: flash.system.System.ime
 @item flash.sys.useCodePage
 ActionScript: flash.system.System.useCodePage
 @item flash.sysCap.avHardwareDisable
 ActionScript: flash.system.Capabilities.avHardwareDisable
 @item flash.sysCap.hasAccessibility
 ActionScript: flash.system.Capabilities.hasAccessibility
 @item flash.sysCap.hasAudio
 ActionScript: flash.system.Capabilities.hasAudio
 @item flash.sysCap.hasAudioEncoder
 ActionScript: flash.system.Capabilities.hasAudioEncoder
 @item flash.sysCap.hasEmbeddedVideo
 ActionScript: flash.system.Capabilities.hasEmbeddedVideo
 @item flash.sysCap.hasIME
 ActionScript: flash.system.Capabilities.hasIME
 @item flash.sysCap.hasMP3
 ActionScript: flash.system.Capabilities.hasMP3
 @item flash.sysCap.hasPrinting
 ActionScript: flash.system.Capabilities.hasPrinting
 @item flash.sysCap.hasScreenBroadcast
 ActionScript: flash.system.Capabilities.hasScreenBroadcast
 @item flash.sysCap.hasScreenPlayback
 ActionScript: flash.system.Capabilities.hasScreenPlayback
 @item flash.sysCap.hasStreamingAudio
 ActionScript: flash.system.Capabilities.hasStreamingAudio
 @item flash.sysCap.hasStreamingVideo
 ActionScript: flash.system.Capabilities.hasStreamingVideo
 @item flash.sysCap.hasTLS
 ActionScript: flash.system.Capabilities.hasTLS
 @item flash.sysCap.hasVideoEncoder
 ActionScript: flash.system.Capabilities.hasVideoEncoder
 @item flash.sysCap.isDebugger
 ActionScript: flash.system.Capabilities.isDebugger
 @item flash.sysCap.isEmbeddedInAcrobat
 ActionScript: flash.system.Capabilities.isEmbeddedInAcrobat
 @item flash.sysCap.language
 ActionScript: flash.system.Capabilities.language
 @item flash.sysCap.localFileReadDisable
 ActionScript: flash.system.Capabilities.localFileReadDisable
 @item flash.sysCap.manufacturer
 ActionScript: flash.system.Capabilities.manufacturer
 @item flash.sysCap.os
 ActionScript: flash.system.Capabilities.os
 @item flash.sysCap.pixelAspectRatio
 ActionScript: flash.system.Capabilities.pixelAspectRatio
 @item flash.sysCap.playerType
 ActionScript: flash.system.Capabilities.playerType
 @item flash.sysCap.screenColor
 ActionScript: flash.system.Capabilities.screenColor
 @item flash.sysCap.screenDPI
 ActionScript: flash.system.Capabilities.screenDPI
 @item flash.sysCap.version
 ActionScript: flash.system.Capabilities.version
 @item flash.displayState
 ActionScript: if flash.display.Stage.displayState == FULL_SCREEN_INTERACTIVE, then \"fullScreen\"; for NORMAL, return \"window\".
 @item flash.frameRate
 ActionScript: flash.display.Stage.frameRate
 @item flash.quality
 ActionScript: flash.display.Stage.quality
 @item flash.scaleMode
 ActionScript: flash.display.Stage.scaleMode
@end table

@verbatim

 // ActionScript example
 function systemReport:Object () {
 return {
 \"screen\": {
 \"height\": flash.system.Capabilities.screenResolutionX,
 \"width\": flash.system.Capabilities.screenResolutionY,
 \"availHeight\": flash.display.Stage.fullScreenHeight,
 \"availWidth\": flash.display.Stage.fullScreenWidth,
 },
 \"flash\": {
 \"sys\": {
 \"totalMemory\": flash.system.System.totalMemory,
 \"ime\": flash.system.System.ime,
 \"useCodePage\": flash.system.System.useCodePage,
 },
 \"sysCap\": {
 \"avHardwareDisable\": flash.system.Capabilities.avHardwareDisable,
 \"hasAccessibility\": flash.system.Capabilities.hasAccessibility,
 \"hasAudio\": flash.system.Capabilities.hasAudio,
 \"hasAudioEncoder\": flash.system.Capabilities.hasAudioEncoder,
 \"hasEmbeddedVideo\": flash.system.Capabilities.hasEmbeddedVideo,
 \"hasIME\": flash.system.Capabilities.hasIME,
 \"hasMP3\": flash.system.Capabilities.hasMP3,
 \"hasPrinting\": flash.system.Capabilities.hasPrinting,
 \"hasScreenBroadcast\": flash.system.Capabilities.hasScreenBroadcast,
 \"hasScreenPlayback\": flash.system.Capabilities.hasScreenPlayback,
 \"hasStreamingAudio\": flash.system.Capabilities.hasStreamingAudio,
 \"hasStreamingVideo\": flash.system.Capabilities.hasStreamingVideo,
 \"hasTLS\": flash.system.Capabilities.hasTLS,
 \"hasVideoEncoder\": flash.system.Capabilities.hasVideoEncoder,
 \"isDebugger\": flash.system.Capabilities.isDebugger,
 \"isEmbeddedInAcrobat\": flash.system.Capabilities.isEmbeddedInAcrobat,
 \"language\": flash.system.Capabilities.language,
 \"localFileReadDisable\": flash.system.Capabilities.localFileReadDisable,
 \"manufacturer\": flash.system.Capabilities.manufacturer,
 \"os\": flash.system.Capabilities.os,
 \"pixelAspectRatio\": flash.system.Capabilities.pixelAspectRatio,
 \"playerType\": flash.system.Capabilities.playerType,
 \"screenColor\": flash.system.Capabilities.screenColor,
 \"screenDPI\": flash.system.Capabilities.screenDPI,
 \"version\": flash.system.Capabilities.version
 },
 \"displayState\": ( flash.display.Stage.displayState == FULL_SCREEN_INTERACTIVE ? \"fullScreen\" : \"window\" ),
 \"frameRate\": flash.display.Stage.frameRate,
 \"quality\": flash.display.Stage.quality,
 \"scaleMode\": flash.display.Stage.scaleMode
 }
 };
 }
@end verbatim

jso - Must contain a  single string attribute named ``bug.'' Should
contain  an  attribute  named   ``info''  with  system  information
key-value pairs (see  above). May also have a  subject of ``cause''
as a string.

 u - The user reporting the bug.

 "
  (error 'unimplemented))

(definfinity report-user ((user-Name) user recipient/s)
  "Report an user to the moderator(s) on duty for breaking a rule

 @{ userName = user to be reported @}

 "
  (error 'unimplemented))

(defun generate-buddy-list-signature (requestor requestee)
  (error 'unimplemented))

(definfinity request-buddy ((buddy) user recipient/s)
  "Request adding a user to your buddy list (mutual-add) using the notification-based system

\(Added in 1.1)

 jso - @{ buddy: LOGIN @}

u - user who is requesting the addition

@subsection Changes from 1.0 to 1.1

The old system  allowed users to simply add anyone  to their buddy list;
cv.   `INFINITY-ADD-TO-LIST'.   The   new   system   requires   mutually
confirmed adding. AKA the Twitter vs. Facebook mechanisms.

@subsection New in 1.1

This is new in Romance 1.1
"
  (unicast (list :|from| "buddyRequest"
                 :|status| t
                 :|sender| (Toot-name *Toot*)
                 :|signature| (generate-buddy-list-signature *Toot* buddy))))

(definfinity send-out-of-band-message ((sender from status body send-Room-List) user recipient/s)
  "Send an arbitrary JSON packet to another user, or all of the users

Out of the band of communications.

This is neither a public nor a private message in the chat context: just
some additional data that is being provided.


@{ sender: sender, from: outOfBand, status: true, body: @{JSON@} @}

Adds \"roomTitle\"  to body if  body contains \"room\"  and title
can be determined

Add  @samp{\"sendRoomList\":  \"true\"}  to give  the  user  an
updated  room list  as well.  (Necessary for  invitations to  new
       rooms.) Inviting to houses …

@verbatim
initUserRoom { room: 0, autoJoin: false }
{ from: initUserRoom, status: true, moniker: ROOM-MONIKER } ** OK

=>  { from:  initUserRoom, status:  false, err:  exists, moniker:
ROOM-MONIKER } ** OK

=> {  from: initUserRoom, status:  false, err: showFirstRun  } **
ERR (player does not have that room)

sendOutOfBandMessage   {  to:   USER-LOGIN,   body:  {   locType:
\"house\", type: \"invite\", room: MONIKER } }

{  from:  outOfBand,  sender:  YOUR-LOGIN,  status:  true,  body:
{ locType: \"house\", type: \"invite\", room: MONIKER, roomTitle:
USER-VISIBLE-NAME } }
@end verbatim
for user houses, roomTitle will be like \"BlackDaddyNerd's House\"

Parameters:

jso - To send to one user:  @{ to: userName, body: @{JSON@} @}, or to
broadcast to the entire room: @{ toRoom: true, body: @{JSON@} @}

u - The sender of the out-of-band-message

room -  The room in which  the sender is standing.  Necessary for
the toRoom version of this method.

Throws:

org.json.JSONException - Thrown if the data cannot be interpreted
from  the JSON  objects passed  in,  or conversely,  if we  can't
encode a response into a JSON form

"
  (error 'unimplemented))

(definfinity server-time ((server-time) u r )
  "Accept  the client's  notification of  a server-time  adjustment.

 This is used to compute the client's round-trip lag time.

 jso - @{ serverTime: LONG milliseconds since Unix epoch @}"
  (list 200 (list :|from| "serverTime"
                  :|status| t
                  :|serverTime| (* 1000
                                   (- (get-universal-time)
                                      +Unix-time-in-Universal+)))))

(definfinity set-avatar-color ((base extra) user recipient/s)
  "Set the avatar base and extra (pad) colours for the given user.

 Colour numbers  are given in  X'RRGGBB' form  as an integer  — to
 compute one from byte (0..255) RGB values, do ( red << 16 & green
 << 8 & blue )

 Parameters:
 jso - @{ \"base\": (colour number), \"extra\": (colour number) @}
 u - The user whose avatar colours are being set
 room - The room in which the user is standing
 Throws:

 org.json.JSONException - Thrown if the data cannot be interpreted
 from  the JSON  objects passed  in,  or conversely,  if we  can't
 encode a response into a JSON form

SQLException - if the palettes can't be loaded"
  (error "This requires Doodle's intervention now")
  (destructuring-bind (base-red base-green base-blue) (rgb-bytes->rgb base)
    (setf (Toot-base-color *Toot*) (color24-rgb base-red base-green base-blue)))
  (destructuring-bind (pad-red pad-green pad-blue) (rgb-bytes->rgb extra)
    (setf (Toot-pad-color *Toot*) (color24-rgb pad-red pad-green pad-blue))))

(definfinity set-furniture ((item slot x y z facing remove) user recipient/s)
  "Set or change a furniture item.

To add  a structural item  to the room,  put item: 123  without anything
else.  To place  furniture  on  the floor,  also  add  attributes x,  y,
and facing.

To  change furniture,  replace item:  with slot:  (to avoid  ambiguities
about ``which chair'')

 To remove an item from the room, send @{ slot: 123, remove: true @}

 Parameters:

jso - @{ slot:  #, x: #, y: #, facing:  $ @} or @{ item: #,  x: #, y: #,
 facing: $ @} or @{ slot: #, remove: true @}

 u - The user calling this method

room - The room in which this user is standing

Throws:

org.json.JSONException - Thrown  if the data cannot  be interpreted from
the JSON objects passed in, or conversely, if we can't encode a response
into a JSON form

NotFoundException - if the furniture doesn't exist

Changes since 2.0:

z position is required

structural items no longer exist

slot is the item's UUID

"
  (cond
    (remove
     (with-errors-as-http (400) (assert (and (null item) (null x) (null y) (null z))))
     (remove-furniture% slot))
    (slot
     (with-errors-as-http (400) (assert (and (null item) x y z facing)))
     (place-furniture% slot x y z facing))
    (item
     (with-errors-as-http (400) (assert (and (null slot) x y z facing)))
     (let ((slot (drop-item item)))
       (place-furniture% slot x y z facing)))
    (t (error 'http-client-error :status 400))))

(definfinity set-room-var ((&rest key+value-pairs) user recipient/s)
  "Set a room variable or set of room variables.

 Parameters:
 jso - key-value pair(s) for room variable(s) to be set
 u - the user requesting the change
 room - the room to which the variable(s) are associated
 Throws:
 org.json.JSONException - if the packet is malformed
 PrivilegeRequiredException - if a non-privileged user attempts to set a room variable."
  (error 'unimplemented))

(definfinity set-user-var ((&rest key+value-pairs) user recipient/s)
  "setUserVar

 public static void setUserVar(org.json.JSONObject jso,
 AbstractUser u,
 Room room)
 throws org.json.JSONException

 Set user variable(s)
 Input: @{ key : value @} (one or more)

 Parameters:
 jso - user variable(s) to set
 u - the user setting them
 room - the room in which the user is standing
 Throws:
 org.json.JSONException - if the JSO can't be decoded

 "
  (error 'unimplemented))

(definfinity spawn-zone ((&rest d) user recipient/s)
  "spawnZone

 Spawn an additional zone.

 
@subsection Implementation in 5.0

We no longer have zones, but we can have server paritings 

 "
  (error 'unimplemented))

(defun parse-operator-command (string)
  "Parse an operator command in STRING (beginning with #)"
  (assert (char= (char string 0) #\#))
  (let ((command (subseq string 1 (position #\Space string)))
        (params (when-let (space (position #\Space string))
                  (split-sequence #\Space (subseq string space)
                                  :remove-empty-subseqs t))))
    (if-let (sym (or (find-symbol (concatenate 'string "*" (string-upcase command)) 
                                  :tootsville-user)
                     (find-symbol (string-upcase command)
                                  :tootsville-user)))
      (if (and (fboundp sym)
               (= 2 (length (function-lambda-list sym)))
               (eql '&rest (first (function-lambda-list sym))))
          (apply sym params)
          (private-admin-message 
           (concatenate 'string "Can't run #" command) 
           "Not a remote operator command"))
      (private-admin-message "Can't do that"
                             "Not an operator command"))))

(definfinity speak ((key speech vol) user recipient/s)
  "speak

 Handle speech by the user.

 Speech is public to all users in a room/area

 Emotes are simply speech beginning with \"/\". A few are special-cased. WRITEME: which

 Commands are speech beginning with \"#\"

 Parameters:
 jso - @{ \"speech\": TEXT-TO-BE-SPOKEN @}

\"key\" --- WRITEME

\"vol\"  ---  Volume is  one  of  \"talk\", \"shout\",  or  \"whisper\".
The default is always \"talk\".


@verbatim

 private static String nonObnoxious (final String speech) {
 return speech.replace (\"!!\", \"!\").replace (\",,\", \",\").replace (
 \"....\", \"...\").replace (\"??\", \"?\");
 }
@end verbatim

 "
  (when (emptyp speech)
    (return))
  (case (char speech 0)
    (#\~ (v:warn :speak "Received a client command ~a" speech)) ; TODO
    (#\# (parse-operator-command speech)
         (return))
    (#\@ (v:warn :speak "@ command not handled ~a" speech)) ; TODO
    (#\/ (v:warn :speak "emote not handled ~a" speech))     ; TODO
    ((#\\ #\! #\% #\_ #\^ #\*) (v:warn :speak "command not supported ~a" speech)) ; XXX
    (otherwise
     (when (search ",dumpthreads" speech)
       (v:debug :dump-threads "Dumping threads on end user imperative ~{~%~a~}"
                (bt:all-threads)))
     (if (equal ",credits" speech)
         (dump-credits)
         (let ((vol (when (member vol '("shout" "whisper") :test 'equal)
                      vol)))
           (toot-speak speech :Toot *Toot* :vol vol))))))

(define-constant +credits+
    "Tootsville V by Bruce-Robert Pocock at the Corporation for Inter-World Tourism and Adventuring.

Special thanks to  Ali Dolan, Mariaelisa Greenwood,  Maureen Kenny, Levi
Mc Call, and Zephyr Salz.

Tootsville  IV  by  Brandon  Booker, Gene  Cronk,  Robert  Dawson,  Eric
Feiling,  Tim  Hays,  Sean  King, Mark  Mc  Corkle,  Cassandra  Nichols,
Bruce-Robert Pocock, and Ed Winkelman at Res Interactive, LLC.
"
  :test 'equal)

(defun dump-credits ()
  "Send +CREDIT+ as a private admin message. Response to the ,credits user utterance."
  (private-admin-message "Credits" +credits+))

(definfinity start-event ((moniker) user recipient/s)
  "Attempt to begin an event. Might return an error. Uses Quæstor for the heavy lifting.


TODO verify params

Note that for all fountains, use the magic moniker ``fountain''

Calls back the user with either of:

@itemize
@item

@code{alreadyDone: true; status: false; err: \"event.alreadyDone\"}

This returns for fountains that  have already given peanuts today (where
today started at midnight, database local time)

@item
@code{ eventID: (NUM),  filename: \"blah.swf\",  asVersion: @{ 2,  3, or  not @}, status: true}

For successfully registered events. Must  be completed or canceled using
`INFINITY-END-EVENT', qv

@end itemize
"
  (error 'unimplemented))

(definfinity end-event ((moniker) user recipient/s)
  "Attempt to end an event begun by `INFINITY-START-EVENT'

 Parameters:

jso - JSON payload from the caller. Data: moniker = event moniker.

u - The caller = the user performing the event

room - The  caller's room. For fountains, we'll use  this room's moniker
to figure out which fountain is which

Throws:

org.json.JSONException - if  JSON data can't be put into  a response, or
gotten out of a command.

SQLException  - probably  means that  the moniker  is bad,  but I'm  not
really doing much to validate it here

 "
  (error 'unimplemented))
(definfinity use-equipment ((|t| x y z on) user recipient/s)
  "useEquipment

 WRITEME: Document this method brpocock@@star-hope.org

 Parameters:

jso - @{ t: slot-type-char, x: target-x, y: target-y, z: target-z, [ on: target-name ] @}

u - WRITEME

r - WRITEME

Throws:

org.json.JSONException - WRITEME

t ∈ 1,2 for primary or secondary item
 "
  (error 'unimplemented))
