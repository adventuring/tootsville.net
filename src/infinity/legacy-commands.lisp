;; -*- lisp -*-

;;; legacy-commands.lisp is part of Tootsville
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



(definfinity add-furniture ((buddy ignore) user recipient/s)
  "Alias for INFINITY-SET-FURNITURE.

Alias for `INFINITY-SET-FURNITURE', q.v."
  (apply #'infinity-set-furniture (list d user recipient/s)))

(definfinity add-to-list (nil user recipient/s)
  "Add a user to a buddy list or ignore list (removed in 1.2)

…using   the   traditional   (online-only,   no   notification   engine)
mechanism    (using    out    of     band    methods).    Compare    vs.
requestBuddy `INFINITY-REQUEST-BUDDY'

@subsection Usage

This command can no longer be used to add a buddy, only to ignore someone.

@verbatim
{ ignore: USER }
@end verbatim

@subsection 200 OK

When you begin ignoring someone, you'll get back a reply as from @code{getUserLists} `INFINITY-GET-USER-LISTS', q.v.

@subsection 410 Gone

Using this to add a buddy was a legacy feature removed in Romance 1.2.

@subsection Changes from 1.1 to 1.2
@cindex Changes from 1.1 to 1.2

This function was replaced  with `INFINITY-REQUEST-BUDDY' — requestBuddy
— q.v. It's only used for @code{ignore} now."
  (when buddy (error 'legacy-gone))
  (unless ignore (error 'bad-request))
  (error 'unimplemented))

(definfinity click ((on x y z with) user recipient/s)
  "Used by the client  to report a mouse click or  finger tap.

@subsection Usage

If the user  clicks on a placed-item, this method  should be called with
the following syntax:

@verbatim
{ on: ITEM-ID, x: X, y: Y, z: Z, with: MODS }
@end verbatim

Note that the (x,y,z) values passed  are relative to the origin point of
the item;  thus, if an  item is placed  at (200,200,200) and  is clicked
at (210,210,210), the coördinates reported should be (10,10,10).

Note: We currently have nothing that cares about the (x,y,z) relative
coördinates, the item ID is the important bit. (February 2021)

If the  user clicks on the  ground, normally it will  result in walking,
but it could instead be reported as:

@verbatim
{ x: X, y: Y, z: Z, with: MODS }
@end verbatim

@subsection Modifiers characters

The modifiers  string can contain  any of  the following symbols  in any
order,  representing modifier  keys that  were held  down when  the user
clicked on the item:

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

In the  Flash MouseEvent object,  you can  create the \"mods\"  with the
following:

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

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

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

(definfinity create-user-house ((lot house index connect-to connect-at) user recipient/s)
  "Either claim the user's house and lot, or add a room to their house.

@subsection Usage

@verbatim
{ lot: LOT-ID,
  house: HOUSE-ID }

{ index: ROOM-INDEX,
  connectTo: ROOM-INDEX,
  connectAt: CONNECTION-POINT-MONIKER }

{ query: \"houses\" }
@end verbatim

Returns data describing the user's lot for the first two forms, or
data describing houses and rooms available in the third form.

@subsection Examples

When the player has found an empty lot and wishes to claim it as their
own, they choose a base house and send

@verbatim
 { lot: lot-ID, house: house-ID }
@end verbatim

When the player has a house and wishes to add a room, they send

@verbatim
 { index: roomIndex,
   connectTo: roomIndex,
   connectAt: pointMoniker } 
@end verbatim

Connection point monikers need to be obtained from the house design.

@verbatim
 { query: \"houses\" }
@end verbatim

This queries the list of houses and rooms available. UNIMPLEMENTED.
This command was added in Romance 2.0 so that the client need not be
updated with all available houses and rooms.

@subsection 200 OK --- Query Form

Returns an enumeration of houses and rooms available as follows:

@verbatim
{ houses: [ { name: \"NAME\",
              description: \"DESCRIPTION\",
              preview: \"URL\",
              moniker: \"UUID\",
              rooms: [ { id: \"UUID\",
                         connect: { \"MONIKER\": \"UUID\", ... },
                         preview: \"URL\" },
                       ... ] },
            ... ] }

Each house has a list of rooms. The first room in each house is the
default, and is the room created when the lot is first claimed.

The houses themselves have visible names, descriptions, and preview
URLs (also relative to the same base Buildings URL).  The client
should give the user the opportunity to select a house from this set
by browsing their names, descriptions, and preview graphics.

Rooms have connection points with monikers. Each room has a preview
URL which is an image file (e.g. a PNG) relative to the base URL
@url{https://jumbo.tootsville.org/Assets/Buildings/5/}. The preview
can be shown to the user when they are choosing to connect a new
room. Rooms have no established names --- we want to allow the players
full freedom to develop each room as they like.

WRITEME

@subsection 201 Created --- Add house or add room

A house or room was created as demanded.

Returns a description of the house as follows:

WRITEME

@subsection 409 Conflict

A house already exists  on that lot, or, a room  is already connected at
the  given connection  point. The  request cannot  be completed  because
something  already  exists  where  the new  construction  was  meant  to
be placed.

@subsection 404 Not Found

The house ID or room connection point given was not found.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

In 1.2 adding a room required only an index.

In 2.0 we added the @verb{|query: \"houses\"|} form; in 1.2 (and
prior) the client had a hard-coded list of available houses and rooms
for each.

@subsection Changes from 1.1 to 1.2
@cindex Changes from 1.1 to 1.2

In 1.1, houses could have only one room
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
            (error 'infinity-error :http-status 400 :memo :empty-lot-use-house-not-index))
          (setf (lot-owner-Toot lot) (Toot-uuid *Toot*))
          ;; TODO check house is an house
          ;; TODO place house on lot
          )
         ((equal (lot-owner-Toot lot) (Toot-uuid *Toot*))
          (unless (null house)
            (error 'infinity-error :http-status 400 :memo :did-you-want-house-or-room))
          (when (or (null index)
                    (not (numberp index))
                    (zerop index)
                    (minusp index)
                    (> index 9))
            (error 'infinity-error :http-status 400 :memo :index-not-valid))
          ;; TODO create room and affix to house
          ))))))

(definfinity dofff (() user recipient/s)
  "Doff all clothing items.

See also `INFINITY-DOFF' for single items.  To put on (don) an item, see
`INFINITY-DON'. Mnemonic: Like @code{doff} but more so.

@subsection Usage

This command takes no parameters.

@subsection Limitations

This does not  un-equip an item held in the  @code{TRUNK}. This does not
remove or  alter a Toot's pattern.  For non-Toot avatars, this  does not
un-equip an item held in the @code{HAND}, @code{LHAND}, or @code{RHAND}.

Sends two  responses: a success  reply from @code{dofff}, then  total avatar
info from @code{wardrobe}. See `INFINITY-WARDROBE'.

@subsection Status 200 OK

All clothing items have been removed.

@verbatim
{ from: \"dofff\",
  status: true }
@end verbatim

A separate @code{wardrobe} packet will be sent."
  (error 'unimplemented))

(definfinity don ((slot color) user recipient/s)
  "Don (or equip) an item

@subsection Usage

@verbatim
{ slot: \"item-UUID\",
  [ color: \"color ID\" ] }
@end verbatim

JSON  object has  the  item UUID  number to  be  worn (clothes,  pivitz,
trunk).

See `WEAR-SLOT-INFO' for  descriptions of how wear  slots are identified
and described.  Note that  the appropriate wear  slot can  be determined
from the  item's template; see  `ITEM-TEMPLATE-INFO'. For a list  of all
wear slots, see `INFINITY-ENUMERATE-WEAR-SLOTS' (new in 2.0).

Response with total avatar info from @code{wardrobe}. See `INFINITY-WARDROBE'.

Color ID is no longer allowed; it will be rejected.

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
@cindex Changes from 1.2 to 2.0

Colors of  items can no  longer be changed  when donning them.  This was
meant  for pattern  changing  in  1.2, which  must  now be  accomplished
in-game via Doodle. The @code{color} parameter must be null or absent.

Patterns are no longer clothing items.

@subsection Changes from 1.0 to 1.1
@cindex Changes from 1.0 to 1.1

Equipment  held  in the  @code{TRUNK}  is  now explicitly  supported  as
a distinct  wear slot with specific  meaning (ie, the user  can activate
that item).
"
  (unless (null color)
    (return-from infinity-don
      (list 400 (list :|from| "don"
                      :|error| "Color selection is not allowed any more"
                      :|err| "cannotSelectColor"
                      :|status| :false))))
  (let ((item (find-record 'inventory-item :uuid (uuid:make-uuid-from-string slot))))
    (if (item-owned-by-p item *Toot*)
        (don-item item (item-template-wear-slot (item-template (inventory-item-item item))))
        (list 403 (list :|from| "don"
                        :|error| "That is not your item"
                        :|err| "notYourItem"
                        :|status| :false)))))

(definfinity echo ((&rest d) user recipient/s)
  "Echoes back the supplied JSON (or ActionScript) object to the client.

@subsection Usage

The datum (@code{d}) is returned identically, in a return element
named literally @code{You said}.

This method exists solely for testing purposes.

@verbatim
{ c: \"echo\"
  d: DATA-TO-ECHO }
@end verbatim

@subsubsection Parameters

@table @code
@item jso
Any JSON object, the contents of which will be returned to the caller.
@item u
The user calling (to whom the response is sent)
@end table

@subsection Example

@verbatim
⇒ { c: \"echo\", d: { foo: 42 } }

{ from: \"echo\", status: true, \"You said\": { foo: 42 } }
@end verbatim

Note that the field name is literally @samp{You said} with a space.

@subsection 200 OK

The response is echoed back to the user.

@subsection Limitations

The echo packet must be less  than 1,024 Unicode characters in length or
it will be  truncated to 1,024 characters. No warning  will be issued to
the user in the case of truncation.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

The 1kc limit was introduced in 2.0.

@subsection Known bugs
@cindex Known bugs

This feature is not working correctly as of version 0.6.
"
  (list 200 (list :|from| "echo"
                  :|status| t
                  :|You said| (limit-string-length d 1024))))

(definfinity finger ((&rest Toots-with-keys) user recipient/s)
  "Get public info for a list of Toots.

For details, see the synonym `INFINITY-GET-AVATAR-INFO'.

@subsection Usage

@verbatim
{ c: \"finger\", d: { key: \"toot-name\", ... } }
@end verbatim

@subsection Reply format

@verbatim
{ from: avatars, status: true, avatars: { 0: { TOOT-INFO … }, … }
@end verbatim

User public information is in the format of `TOOT-INFO', which should be
a supserset  of what @code{AbstructUser.getPublicInfo()} used  to return
in 1.2.

"
  (list 200
        (from-Avatars Toots-with-keys)))

(defun from-avatars (Toots-with-keys)
  "Returns a from: \"avatars\" packet which is the result of a number of commands.

The packet format is as follows:

@verbatim
{ from: \"avatars\",
  avatars: { KEY: TOOT-INFO, [ ... ] },
  inRoom: \"@Tootsville\",
  status: true }
@end verbatim

The avatar information is in the form given by `TOOT-INFO', q.v.

The parameter TOOTS-WITH-KEYS is a property list whose keys are
arbitrary strings (or symbols, whose names will be taken) and whose
values are Toot designators suitable to be passed to `ENSURE-TOOT',
eg. Toot names or Toot objects."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (key Toot) on Toots-with-keys by #'cddr
             do (setf (gethash (princ-to-string key) hash)
                      (Toot-info (ensure-Toot Toot))))
    (list :|avatars| hash
          :|from| "avatars"
          :|inRoom| "@Tootsville"
          :|status| t)))

(definfinity game-action ((&rest more-params &key action &allow-other-keys) user recipient/s)
  "Send an in-world game's action.

@gaindex Overview of Game Actions

These are actions that affect in-world minigames.

@subsection Usage

@verbatim
{ c: \"gameAction\",
  d: { game: \"AEB967CB-5598-40D5-9B4A-894C9BC38501\",
       action: ACTION-NAME, 
       [ ... PARAMS ... ] } }
@end verbatim

@subsection Example

@verbatim
{ c: \"gameAction\",
  d: { game: \"AEB967CB-5598-40D5-9B4A-894C9BC38501\",
       action: \"tagYouReIt\", 
       tagged: \"5047F44E-8B1D-4B8A-9EC6-4E1D6E1653AD\" } }
@end verbatim

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
`GAME-ACTION-JOIN-CARD-GAME'

@item
`GAME-ACTION-TAG-YOU-RE-IT'

@end itemize

@subsection General Structure

A @code{gameAction} packet has a @code{d} datum with a key
@code{action}, which is used to further dispatch the game action to
its appropriate handler. The @code{action} value is the smallCamelCase
version of the ``GAME-ACTION-function-name'' that will actually handle
it.

The specific game which is being addressed must be identified by its
UUID. This is usually discovered by finding a game tag on an item or
place in the game world.

WRITEME: Explain how to find a game tag.

Refer to the individual game action functions for further details.

See Appendix 8 for an index of game actions.

@subsection Response format

The individual game action handlers will provide their own response
formats. In general, they will come from @code{gameAction}, with a
@code{status} of true or false; when false, they should include an
@code{error} text which may be user-visible, and may include an
@code{err} tag which is a general machine-readable code.

@subsection Status 400 Error

If the @code{action} is not supplied, or if no such action is known to
the server, then an error 400 is returned, with a JSON error packet of
the usual form:

@verbatim
{ from: \"gameAction\",
  status: false,
  error: \"error message text\",
  err: \"game-action-not-found\" }
@end verbatim
"
  (if-let (fn (find-symbol (concatenate 'string "GAME-ACTION-"
                                        (symbol-munger:camel-case->lisp-name action))
                           (find-package :Tootsville)))
    (apply fn more-params)
    (list 400 (list :|from| "gameAction"
                    :|status| :false
                    :|error| (format nil "No such gameAction: ~a" action)
                    :|err| "game-action-not-found"))))

(definfinity get-avatars ((&rest _+user-names) user recipient/s)
  "Get avatar data for a list of (other) users.

Synonym for `INFINITY-FINGER'

@subsection Usage

The @code{d} datum is a JSON object, with (ignored) keys tied to
values which must be the names of users.

@subsection Example

@verbatim
{ c: \"getAvatars\",
  d: { \"foo\": \"mouser\",
       \"bar\": \"catvlle\" } }
@end verbatim

@subsection Status 200 OK

The avatar information for each user requested will be returned in an
associative array object with the same keys as the source query. The
values of each key are the avatar data as returned by
`INFINITY-FINGER', i.e. the information returned by `TOOT-INFO'.

"
  (list 200
        (from-Avatars Toots-with-keys)))

(definfinity get-color-palettes (nil user recipient/s)
  "getColorPalettes

@subsection Usage

This command requires no parameters

@subsection Status 410 Gone

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

(definfinity get-inventory (nil user recipient/s)
  "Get all inventory for an user (themself) — both active and inactive

@subsection Usage

This command requires no parameters.

@verbatim
{ c: \"getInventory\" }
@end verbatim

@subsection Status 200 OK

Returns a set of items as
@verbatim
{ from: \"getInventory\", 
  inv: { 0: { id: 123, isActive: boolean }, ... } }
@end verbatim

WRITEME

"
  (error 'unimplemented))

(definfinity get-Inventory-By-Type ((type) user recipient/s)
  "Get a subset of items from your own inventory

@subsection Usage

@verbatim
{ c: \"getInventoryByType\",
  d: { type: TYPE,
        [withActive: BOOLEAN ],
        [who: LOGIN-NAME ] } }
@end verbatim


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

Finally, multiple codes can be enumerated by passing as string beginning
with @code{$} plus  a series of identifiers from the  above delimited by
@code{:}, e.g. @code{$clothes:pivitz}.

You can also supply @code{withActive: false} to screen out active items.

The  optional parameter  @code{who} specifies  whose inventory  to list.
If  not specified,  the inventory  of the  Toot posing  the question  is
returned.  Note that  inactive  items  of Toots  not  owned  by you  are
generally not returned.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

In Romance  1.2, the  @code{type} code  could not  be a  @code{point} or
@code{slot}, but it could be a  string beginning @code{#} with a list of
type code numbers; e.g. @code{#2:3}.

In Romance  1.2, placed furniture was  also returned; this is  no longer
the case.

@subsection Status 200 OK

Returns a  set of items  as
@verbatim
{from: \"inventory\", for: USER-LOGIN, type: TYPE-QUERY
  inv:  { 0: {  id: 123, isActive:  boolean },
 ... }
@end verbatim

See GET-INVENTORY-BY-TYPE (UNIMPLEMENTED)

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
  "Get a list of users online.

This is an administrative function, only available to staff members.

@subsection Usage

@verbatim
{ c: \"getOnlineUsers\",
  d: { [ inRoom: ROOM ] }
@end verbatim


If this contains an attribute of \"inRoom\" with a room moniker, we'll
only return the users in that room. Otherwise, all users in the Zone
will be returned.

This optional parameter should not be specified and will be ignored if
present.

@subsection Example

@verbatim
{ c: \"getOnlineUsers\" }
@end verbatim

@subsection Status 200 OK

@verbatim
{ from: \"getOnlineUsers\",
  status: true,
  inRoom: \"@Tootsville\",
  toots: [ TOOT-INFO, ... ] }
@end verbatim

The @samp{toots} array is a list of `TOOT-INFO' JSON objects
describing every user online at the moment. This can be quite large.

@subsection Status 403 Permission Denied

This is returned if the user is not a Builder Toot.

@verbatim
{ from: \"getOnlineUsers\",
  status: false,
  error: \"That is a Builder Toot command.\" }
@end verbatim


"
  (if (builder-Toot-p)
      (list 200 (list :|from| "getOnlineUsers"
                      :|status| t
                      :|inRoom| "@Tootsville"
                      :|toots| (mapcar #'Toot-info (connected-toots))))
      (list 403 (list :|from| "getOnlineUsers"
                      :|status| :false
                      :|error| "That is a Builder Toot command"))))

(definfinity get-Room-List (nil user recipient/s)
  "Get a list of all ``well known'' Rooms currently active/visible.

``Rooms'' no longer exist. The ``rooms'' are now known as ``spots.''

UNIMPLEMENTED

@subsection Usage

@verbatim
{ c: \"getRoomList\" }
@end verbatim

@subsection Status 200 OK

WRITEME

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

The system used to be broken into ``rooms,'' each about one ``screen''
size, and communications and game events were mostly restricted to the
room in which they occurred.

This is no longer the case.

However, ``named spots'' have been introduced, so this function was
repurposed to that end.

"
  (error 'unimplemented))

(definfinity get-server-time (nil user recipient/s)
  "Send the server time to the client requesting it

For synchronization purposes.

Sends a JSON object with a property, @code{serverTime}, with the current
time in milliseconds (give or take transit time). This is the Unix time,
not the Universal time, and in milliseconds, not seconds."
  (list 200
        (list :|from| "getServerTime"
              :|status| t
              :|serverTime| (get-java-time))))

(definfinity get-session-apple ((&rest d) user recipient/s)
  "Initialise a session key for stream or batch mode operations.

Note that this command is still available, but only in the pre-login
phase of communications; once signed it, it will signal an error if
called.

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
@cindex Changes from 1.2 to 2.0

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
  (mapcar 
   (lambda (contact)
     (list :|id| (contact-uuid contact)
           :|n| (Toot-name (find-reference contact :contact))
           :|starredP| (contact-starredp contact)
           :|added| (contact-added contact)
           :|lastUsed| (contact-last-used contact)))
   (sort
    (find-records 'contact :owner (Toot-UUID Toot))
    #'< :key #'contact-last-used)))

(definfinity get-user-lists (nil user recipient/s)
  "Get the user's buddy list and ignore list.

@verbatim
{ buddyList: { … } , ignoreList: { … } }
@end verbatim

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

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
@cindex Changes from 1.1 to 1.2

Currencies were made explicit, allowing currencies other than peanuts to
be potentially supported in future.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

Fairy Dust was added after 1.2.

@subsection 200 OK

Returns the wallet info."
  (list 200 (wallet-info *Toot*)))

(definfinity get-Zone-List (nil user recipient/s)
  "Get a list of all Zones currently active/visible.

This returns \"Universe\" as the only Zone.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

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
@cindex Changes from 1.2 to 2.0

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

The facing can be given as per `INTERPRET-FACING'.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

@code{z} can no longer be omitted if @code{x} or @code{y} are specified.
In 1.2, a pair with only x,y was valid.
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

(definfinity join ((room from lat long alt world) user recipient/s)
  "Join a room or place.

The ``room'' form is no longer needed. We no longer have rooms.

The ``place'' form using latitude and longitude is used instead.

@subsection Usage

@verbatim
{ c: \"join\", d: { room: NEW-ROOM, [ from: OLD-ROOM ] } }

{ c: \"join\", d: { lat: LAT, long: LONG, alt: ALT, world: WORLD }}
@end verbatim

@subsection Status 200 OK

You may get this reply for joining a place instead:

@verbatim
{ from: \"roomJoin\",
  status: true,
  lat: LAT,
  long: LONG,
  alt: ALT,
  world: WORLD }
@end verbatim

This will usually be followed by an @code{rv} packet with the local
room vars (see `LOCAL-ROOM-VARS').

@emph{NOTE the inconsistency}: the command is @code{join}, but the
reply comes from @code{roomJoin}

Joining a room used to return a packet like:

@verbatim
{ from: \"roomJoin\",
  status: true,
  room: MONIKER }
@end verbatim

You will never get this reply in Romance 2.0.

@subsection Error Return values (room join form)

@table @code
@item zone.notFound
 The user is not in a Zone
@item room.noMoniker
 No room moniker was given to be joined
@item room.notFound
 The room moniker does not refer to an actual room in this Zone
@item room.full
 The room is too full (too many users)
@end table

@subsection 410 Gone

Removed in 2.0.

Attempting to call @code{join} a room will always result in

@verbatim
{ from: \"roomJoin\",
  status: false,
  err: \"room.notFound\",
  error: \"There are no rooms in Tootsville V.\" }
@end verbatim

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

In Romance 1.2,  the room was divided into ``rooms.''  This is no longer
the case, so there is never any need to join a room.

The success and error return codes are documented here for completeness,
but only @code{room.notFound} will be returned.

The new form, taking latitude and longitude, was added in 2.0.
"
  (when room
    (return (list 410 (list :|from| "roomJoin"
                            :|status| :false
                            :|err| "room.notFound"
                            :|error| "There are no rooms in Tootsville V."))))
  (setf (Toot-position  *client*) (list world lat long alt))
  (unicast (list :|from| "roomJoin"
                 :|status| t
                 :|lat| lat :|long| long
                 :|alt| alt :|world| world))
  (unicast (list :|from| "quiesce" :|status| :false))
  (list 200 (local-room-vars)))

(definfinity logout ((&rest d) user recipient/s)
  "Log out of this game session

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

There was a bug in the Persephone client that caused it to explode if we
logged it out before it received  & processed the logout message. So, we
waited for the expected lag time to expire and then throw 2 full seconds
of wasted wait time after it, which had ought to be enough time. This is
no longer supported."
  (error 'unimplemented))

(definfinity mail-customer-service ((subject body) user recipient/s)
  "Send an eMail to customer service (feedback)

UNIMPLEMENTED

This sends an email with the given subject and body to
@code{support@@Tootsville.org}.

@subsection Usage

@verbatim
{ subject: STRING, body: STRING }
@end verbatim

 "
  (error 'unimplemented))

(definfinity peek-at-inventory ((who type) user recipient/s)
  "Look at other users' inventories

When requesting the inventory of another player, only their public
inventory will be returned.

The optional type code is as per `INFINITY-GET-INVENTORY-BY-TYPE'.

@subsection Usage
@verbatim
{ who: LOGIN-NAME, [ type: TYPE-CODE ] }
@end verbatim

@subsection Examples

@verbatim
{ who: \"user-name\" }

{ who: \"user-name\",
  type: \"type-code\" }
@end verbatim

@subsection Status 200 OK

@verbatim
{ from: \"peekAtInventory\",
  status: true,
  for: USER-NAME,
  inv: { 0: ITEM-INFO, [ ... ] } }
@end verbatim

@subsection Status 404 Not Found

 The user name given was not found.

@verbatim
{ from: \"peekAtInventory\",
  status: false,
  err: \"login.notFound\",
  error: \"There is no user named LOGIN\" }
@end verbatim

WRITEME

@subsection Status 400 Argument Error

 The type code given was not understood

@verbatim
{ from: \"peekAtInventory\",
  status: false,
  err: \"typeCode.notFound\",
  error: \"Parameter error: The type code given is not recognized.\" }
@end verbatim

"
  (error 'unimplemented))

(definfinity ping ((ping-started) user recipient/s)
  "Send a ping to the server to get back a pong.

This also updates the user's last-active timestamp.

@subsection Usage

@verbatim
{ [ pingStarted: TIMESTAMP ] } 
@end verbatim

@subsection Examples

@verbatim
{ pingStarted: 1589849202000 }

{}
@end verbatim

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
  (let ((java-now (get-java-time)))
    (when *Toot*
      (setf (Toot-last-active *Toot*) (now))
      (save-record *Toot*))
    (setf (last-active *client*) (get-Unix-time))
    (list 200 (list :|from| "ping"
                    :|ping| "pong"
                    :|status| t
                    :|pingStarted| (or ping-started java-now)
                    :|serverTime| java-now))))

(definfinity prompt-reply ((id reply) user recipient/s)
  "Accept a reply to a server-initiated prompt

@cindex Server Prompts and Replies

@subsection Usage

@verbatim
{ id: ID, reply: TOKEN }
@end verbatim

@subsection Overview of Prompts

 Server initiates prompt with:

@verbatim

 { \"from\" : \"prompt\",
   \"id\" : $ID,
   \"label\" : $LABEL,
   \"label_en_US\" : $LABEL,
   \"title\" : $TITLE,
   [ \"attachUser\" : $AVATAR_LABEL || \"attachItem\" : $ITEM_ID ] ,
   \"msg\" : $TEXT,
   \"replies\":
     { $TOKEN :
       { \"label\" : $BUTTON_LABEL,
         \"label_en_US\" : $BUTTON_LABEL,
         \"type\" : $BUTTON_TYPE },
       [ … ]
     }
 }

@end verbatim

Where:

@table @code
@item $ID
arbitrary string with no \0 (null byte) representing this
question uniquely.  This is not an user-visible string.
@item $LABEL
concatenated to  the  window  title,  but  can be  used  to
special-case / theme dialogs in future for certain purposes
@item $TITLE
dialog title
@end table

Only one of either ``attachUser'' or ``attachItem'' will be included.
$AVATAR_LABEL is the full avatar label of the user/avatar to which the
prompt should be attached — including ``$'' and instance ID, if
necessary — where $ITEM_ID is the room variable item ID for a placed
item in the room.

$TEXT = message text, may have  \n, will often need word-wrapping, and
ideally might make use of scroll bars

The \"replies\" assoc-array is of arbitrary length, where the key to
each item is a $TOKEN, again an arbitrary string without \0 to
represent this response uniquely. This is not an user-visible string.

$BUTTON_LABEL = the text to display. In future, the client may want to
special-case specific text  to use icons or something:  e.g. \"OK\" will
always be sent as precisely \"OK\" in English locale.

$BUTTON_TYPE  = the  type of  the  button for  theming purposes  only.
This is from the enumerated set [ \"aff\" | \"neg\" | \"neu\" ];

@table @code

@item aff
affirmative button, e.g. green button

@item neg
negative button, e.g. red button

@item neu
neutral button, e.g. purple button

@end table

To simplify future i18n/l10n efforts, the $LABEL and $BUTTON_LABEL
will always be sent twice. The user's current language version will be
in the \"label\" properties. The versions of those strings in the
\"en_US\" locale will always be in the \"label_en_US\" properties.
For purposes of theming and such, the label_en_US properties should be
considered; the \"label\" properties, however, should always be used
in presentation to the end-user.

 Example:
@verbatim

 { \"from\": \"prompt\",
   \"status\": \"true\",
   \"id\": \"fountain/tootSquare/Ã¾=?/x'deadbeef'\",
   \"label\": \"Fountain\", 
   \"label_en_US\": \"Fountain\",
   \"title\": \"Make a Wish?\", 
   \"msg\": \"Do you want to make a wish on the Toot Square fountain?\",
   \"replies\":
   { \"yes\": { \"label\": \"Make a Wish!\", 
                \"label_en_US\": \"Make a Wish!\", 
                \"type\": \"aff\" },
     \"no\": { \"label\": \"Not now\", 
               \"label_en_US\": \"Not now\", 
               \"type\": \"neg\" }
   }
 }

@end verbatim

 The client's response is a bit simpler:
@verbatim
 { \"c\": \"promptReply\", \"d\": { \"id\": $ID, \"reply\": $TOKEN } }
@end verbatim

 e.g.

@verbatim
 { \"c\":\"promptReply\",
   \"d\": { \"id\":  \"fountain/tootSquare/Ã¾=?/x'deadbeef'\", 
            \"reply\": \"yes\" } }
@end verbatim

As a special-case, for the reply only, the special $TOKEN of \"close\"
should be sent if the user dismissed the dialog box with the close
button.

I'd suggest that the GUI attach anonymous functions with the reply
packets already constructed to the various dialog box controls at
creation time, rather than trying to manage some queue of pending
prompts.

To handle user expectations, it would be best to display the button in
a \"down\" state until receiving the server's acknowledgement of the
\"promptReply\" and disallow multiple-clicking in the window.

The server will respond with

@verbatim
 { \"from\": \"promptReply\", \"status\": \"true\", \"id\": $ID }
@end verbatim


For debugging purposes, the server may reply with

@verbatim
 { from: \"promptReply\", \"status\": false, err: $ERR }
@end verbatim


Where $ERR  will be  a brief  description of the  problem.
 
@table @code

@item reply.notFound

a reply button that was not a valid $TOKEN from the \"prompt\" command
nor the special case @code{close}.

@item id.notFound

a reply to a prompt that was not (recently) asked.

@end table

A prompt ID is not valid across sessions; pending prompts should be
auto-closed on logout.  Prompts can, however, remain active
indefinitely, even across room joins.

@subsection Canceling a prompt

Optional implementation: the server may cancel an outstanding prompt
request by sending a packet with the following properties:

@verbatim
{ from: prompt
  status: true
  cancel: $ID }
@end verbatim

Client applications may choose to dismiss the prompt automatically
upon receiving such a packet. Failure to do so is not an error,
however, later attempting to reply to a canceled prompt will return
status: @code{false, err: id.notFound}.  Clients must accept a
cancelation packet silently if they do not process it.

" 
  (v:info :prompt "Prompt reply: ~a → ~a" id reply)
  (if-let (request (and (zerop (search "child-request-" id))
                        (find-record 'child-request
                                     :uuid (subseq id 14))))
    (progn ; XXX factor out function
      (or
       (string-case reply
         ("affirm" (parent-grant-permission request))
         ("deny" (parent-deny-permission request))
         ("close" #| no op |#)
         ("1hour" (parent-grant-permission request :hours 1))
         ("2hours" (parent-grant-permission request :hours 2))
         ("4hours" (parent-grant-permission request :hours 4))
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

@subsection Usage

To drop a buddy from the buddy list:

@verbatim
{ buddy: \"user-name\" }
@end verbatim

To attend to someone who had previously been ignored:

@verbatim
{ ignore: \"name\" }
@end verbatim

@subsection Status 200 OK

The user was removed from the buddy list or ignore list.

@verbatim
{ from: \"removeFromList\",
  status: true,
  buddy: \"buddy-name\" }
@end verbatim

@verbatim
{ from: \"removeFromList\",
  status: true,
  ignore: \"ignored-name\" }
@end verbatim

@subsection Status 404 Not Found

The user name given was not found.

@verbatim
{ from: \"removeFromList\",
  status: false,
  err: \"login.notFound\" }
@end verbatim

@subsection Status 412 Precondition Failed

An attempt was made to remove someone  from the buddy or ignore list who
was not on that list.

@verbatim
{ from: \"removeFromList\",
  status: false,
  err: \"notOnList\" }
@end verbatim
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
possible in the ``info'' object.

@subsection Usage

@verbatim
@end verbatim

WRITME

@subsection Fields of ``info''

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
 JavaScript: screen.height; 

 ActionScript: flash.system.Capabilities.screenResolutionX
@item screen.width
 JavaScript: screen.width; 

 ActionScript: flash.system.Capabilities.screenResolutionY
@item screen.availHeight
 JavaScript: screen.availHeight; 

 ActionScript: flash.display.Stage.fullScreenHeight
@item screen.availWidth
 JavaScript: screen.availWidth; 

 ActionScript: flash.display.Stage.fullScreenWidth
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
 for (; topWindow.parent != topWindow;
        topWindow = topWindow.parent)
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
 \"displayState\": ( flash.display.Stage.displayState == 
                                                 FULL_SCREEN_INTERACTIVE ?
                                             \"fullScreen\" : \"window\" ),
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


 "
  (error 'unimplemented))

(definfinity report-user ((user-Name) user recipient/s)
  "Report an user to the moderator(s) on duty for breaking a rule

@subsection Usage

@verbatim
{ c: \"reportUser\", d: { userName: LOGIN } }
@end verbatim

@subsection Example

@verbatim
{ c: \"reportUser\", d: { userName: \" } }
@end verbatim

"
  (error 'unimplemented))

(defun generate-buddy-list-signature (requestor requestee)
  "Generate a signature for a buddy-list request."
  (sha1-hex (concatenate 'string requestor "/" requestee
                         "/buddy-list-request/"
                         *stable-nonce*)))

(defun check-buddy-list-signature (requestor requestee signature)
  "Check whether a buddy-list request is valid"
  (equal signature (generate-buddy-list-signature requestor requestee)))

(definfinity request-buddy ((buddy sign) user recipient/s)
  "Request adding a user to your buddy list (mutual-add) using the notification-based system.

\(Added in 1.1)

To request a buddy, first you send a @code{requestBuddy} packet. That
user will be given an unique signature code and prompted whether they
agree to be your buddy. If they agree, they'll send a
@code{requestBuddy} packet back with your name and the signature code.

@subsection Usage

@verbatim
{ buddy: LOGIN }

{ buddy: LOGIN, sign: SIGNATURE }
@end verbatim

@subsection Example

@verbatim
{ buddy: \"catvlle\" }

{ buddy: \"catvlle\", sign: \"xyzzyfoo\" }
@end verbatim

@subsection Changes from 1.0 to 1.1
@cindex Changes from 1.0 to 1.1

The old system  allowed users to simply add anyone  to their buddy list;
cv. `INFINITY-ADD-TO-LIST'.  The new system requires  mutually confirmed
adding. AKA the Twitter vs. Facebook mechanisms.

@subsection New in 1.1

This was new in Romance 1.1
"
  (if sign
      (error 'unimplemented)
      (progn (unicast (list :|from| "buddyRequest"
                            :|status| t
                            :|sender| (Toot-name *Toot*)
                            :|sign| (generate-buddy-list-signature (Toot-name *Toot*) buddy))
                      (find-record 'Toot :name buddy))
             (list 204 nil))))

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

@subsection Usage

@verbatim
{ serverTime: LONG milliseconds since Unix epoch }
@end verbatim

@subsection Example

@verbatim
{ serverTime: 1589850683000 }
@end verbatim"
  (list 200 (list :|from| "serverTime"
                  :|status| t
                  :|serverTime| (get-java-time))))

(definfinity set-avatar-color ((base extra) user recipient/s)
  "Set the avatar base and extra (pad) colours for the given user.

This function is no longer available. Doodle must change the avatar's
color now.

@subsection Romance 1.1 instructions

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

(defun remove-furniture (slot)
  (error 'unimplemented))

(defun place-furniture (slot x y z facing)
  (error 'unimplemented))

(definfinity set-furniture ((item slot x y z facing remove) user recipient/s)
  "Set or change a ``furniture'' item.

There is no longer a distinction between ``furniture'' items and other
items in the game world --- this command's name is historical.

There are 3 distinct forms in which this command can be used.

@table @code
@item item
To add an item to the area, send a packet with the
following data:
@table @code
@item item
The item template ID number
You must have an item of this type in your inventory. or:
@item x, y, z
The position at which to place the item
@item facing
The angle (in radians) of the facing of this item. Alternatively, for 
backward compatibility with 1.2, the facing direction can be
specified as @code{N}, @code{NE}, @code{E}, @code{SE}, @code{S},
@code{SW}, @code{W}, or @code{NW} (case-insensitive). 
See `INTERPRET-FACING'.
@end table
@item slot
To position or reposition a particular item by its UUID, send a
packet like:
@table @code
@item slot
The item's UUID. This item must be in your inventory or owned by you.
@item x, y, z
The position at which to (re)position the item
@item facing
The angle (in radians) of the facing of this item. Alternatively, for 
backward compatibility with 1.2, the facing direction can be
specified as @code{N}, @code{NE}, @code{E}, @code{SE}, @code{S},
@code{SW}, @code{W}, or @code{NW} (case-insensitive).
See `INTERPRET-FACING'
@end table
@item remove
Remove a furniture item from the scene, putting it back into your
inventory. The packet keys will look like:
@table @code
@item remove
This value must be @code{true}.
@item slot
The UUID of the item to remove from the scene.
@end table
@end table

Note that you may be able to pick up an item by @code{remove} even
if it does not belong to you, if it is disowned. WRITEME

@subsection Romance 1.2 instructions

@quotation 

To add a structural item to the room, put item: 123 without anything
else.  To place furniture on the floor, also add attributes x, y, and
facing.

To change furniture, replace item: with slot: (to avoid ambiguities
about ``which chair'')

To remove an item from the room, send @{ slot: 123, remove: true @}

Parameters:

jso - @{ slot:  #, x: #, y: #, facing:  $ @} or @{ item: #,  x: #, y: #,
 facing: $ @} or @{ slot: #, remove: true @}


@end quotation

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

@itemize
@item
z position is required
@item
structural items no longer exist
@item
slot is the item's UUID
@item
facing can  be specified  as an  arbitrary angle,  rather than  just the
eight cardinal directions.
@end itemize

@subsection 200 OK

WRITEME

@subsection 400 Error in parameters

This error is thrown if the parameters are not in one of ;
the accepted formats

WRITEME
"
  (cond
    (remove
     (with-errors-as-http (400) (assert (and (null item) (null x) (null y) (null z))))
     (remove-furniture slot))
    (slot
     (with-errors-as-http (400) (assert (and (null item) x y z facing)))
     (place-furniture slot x y z facing))
    (item
     (with-errors-as-http (400) (assert (and (null slot) x y z facing)))
     (let ((slot (drop-item item)))
       (place-furniture slot x y z facing)))
    (t (error 'http-client-error :status 400))))

(definfinity set-room-var ((&rest key+value-pairs) user recipient/s)
  "Set a room variable or set of room variables.

There are no longer room variables (as such) in Romance 2.0. However,
some of them can be fake-set to actually alter some underlying facts
of the system, by a Builder Toot. ;

UNIMPLEMENTED

WRITEME

@subsection Usage

 jso - key-value pair(s) for room variable(s) to be set

WRITEME

@subsection Example

WRITEME

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0
WRITEME

"
  (error 'unimplemented))

(defun set-user-var-d (Toot value)
  "Set the ``d user variable''

See `INFINITY-SET-USER-VAR' for discussion."
  (destructuring-bind (x₁ y₁ x₂ y₂ facing start-time &optional z₁ z₂)
      (split-sequence #\~ value)
    (unless (and z₁ z₂)
      (setf z₁ y₁ z₂ y₂
            y₁ 0 y₂ 0))
    (setf facing (interpret-facing facing))
    (broadcast (list :|from| "wtl"
                     :|status| t
                     :|course| (list :|startPosition| (list :|x| x₁
                                                            :|y| y₁
                                                            :|z| z₁)
                                     :|startTime| start-time
                                     :|endPosition| (list :|x| x₂
                                                          :|y| y₂
                                                          :|z| z₂)
                                     :|speed| 0.1)
                     :|facing| (interpret-facing facing))
               :near Toot)))

(defun set-user-var-wtl (Toot value)
  "Sets the ``wtl user variable''

See `INFINITY-SET-USER-VAR' for discussion."
  (destructuring-bind (&key |course| |facing|) value
    (broadcast (list :|from| "wtl"
                     :|status| t
                     :|course| |course|
                     :|facing| (interpret-facing |facing|))
               :near Toot)))

(defun set-user-var (Toot key value)
  "Set a ``user variable''

See `INFINITY-SET-USER-VAR' for discussion."
  (cond
    ((equal "d" key)
     (set-user-var-d Toot value))
    ((equal "wtl" key)
     (set-user-var-wtl Toot value))
    ((equal "d3" key)
     (error 'unimplemented))
    ((equal "xpr" key)
     (error 'unimplemented))
    ((string-begins "s" key)
     (error 'unimplemented))
    ((string-begins "shot" key)
     (error 'unimplemented))))

(definfinity set-user-var ((&rest key+value-pairs) user recipient/s)
  "Set ``User Variables''

@subsection Usage

@verbatim
{ \"KEY\": \"VALUE\" [ ... ] }

{ d: \"D-String\" }

{ wtl: course: { COURSE }, facing: FACING }

{ d3: course: { COURSE } }

{ xpr: \"expression\" }

{ sN: \"D-String\" }

{ shotN: course: { COURSE }, facing: FACING }
@end verbatim

This is a legacy-type method, which is provided for the convenience of
client implementors.

@subsection Example

@verbatim
{ c: \"setUserVar\",
  { d: \"100~100~200~200~NE~6029604401000\",
    s0: \"100~100~300~300~NE~6029604401000\",
    xpr: \"smile\" } }
@end verbatim

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

Historically, arbitrary  attributes could be  attached to a user  in the
game,  as transient  values  that  remained as  long  as  that user  was
connected. Thus, any key:value pair could be ``advertised'' by a user by
posting them to this method.

@subsection Available Attributes (2.0)

In  Romance II,  only  the  following ``user  variable''  key names  are
actually supported:

@table @code

@item d
This is the legacy ``d'' string for purposes of positioning and
guiding a character. Since it was designed for a 2-dimensional space,
the coördinate space is treated as (x,z) rather than (x,y) if there is
no ``z'' coördinate given. Since Romance II clients are expected to
use @code{wtl} or @code{d3} packets only, this will be translated into
a @code{wtl} course and then transmitted.

See `INFINITY-WTL' for a discussion of its structure.

@item wtl
See `INFINITY-WTL' for the structure of this linear course.

@item d3
This is an experimental format not yet used in Romance 2.0. It will
support more complex path descriptions.

@item xpr
This sets the player's expression (on  their face); not yet supported in
Tootsville V.

@item s@i{N}

This is a shot position in @code{d} form, where @i{N} is an arbitrary
unique identifier chosen by the client. See `INFINITY-SHOOT' for
another way to provide this data.

@item shot@i{N}

This is a shot position in @code{wtl} form, where @i{N} is an
arbitrary unique identifier chosen by the client.

@end table

Any other KEY value will result in an error.

@subsection 200 OK

When all keys are set successfully, this will return with a packet like

@verbatim
{ from: \"setUserVar\",
  status: true,
  set: [ \"key\", ... ] }
@end verbatim

When some keys could not be set, they will be listed separately

@verbatim
{ from: \"setUserVar\",
  status: true,
  set: [ \"key\", ... ],
  unset: [ \"key\", ... ] }
@end verbatim

@subsection 400 Illegal

When no key is from the set of supported keys, an error is returned:

@verbatim
{ from: \"setUserVar\",
  status: false,
  unset: [ \"key\", ... ] }
@end verbatim
 "
  (doplist (key value key+value-pairs)
           (set-user-var *Toot* key value)))

(definfinity spawn-zone ((&rest d) user recipient/s)
  "Spawn an additional server peer pairing.

UNIMPLEMENTED

@subsection Implementation in 2.0

We no longer have zones, but we can have server paritings.

This is used to establish a new server pairing ...

WRITEME

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

WRITEME

 "
  (error 'unimplemented))

(defun parse-operator-command (string)
  "Parse and execute an operator command in STRING (beginning with #)"
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
          (progn
            (v:info :infinity "Operator command #~a ~{~a~^ ~} from ~a"
                    command params *client*)
            (handler-case
                (apply sym params)
              (error (e)
                (private-admin-message (format nil "~a error" command)
                                       (format nil "~/HTML/" e)))))
          (private-admin-message 
           (concatenate 'string "Can't run #" command) 
           "Not a remote operator command"))
      (private-admin-message (concatenate 'string "Can't run #" command)
                             "Not an operator command"))))

(defun @-message (string)
  "Interpret @ as a private message."
  (let ((recipient (find-record 'Toot :name
                                (subseq string
                                        (1+ (position #\@ string))
                                        (position #\Space string))))
        (text (subseq string (1+ (position #\Space string)))))
    (let ((speech (cassandra-obnoxious-filter text "whisper")))
      (when (and (nearp *Toot* recipient)
                 (cassandra-filter speech #|children-present-p FIXME |#))
        (Toot-private-message *Toot* recipient speech)))))

(defun player-speak (speech vol &optional (Toot *Toot*))
  (let ((vol (or (when (member vol '("shout" "whisper") :test 'equalp) vol)
                 "talk")))
    (multiple-value-bind (speech vol) (cassandra-obnoxious-filter speech vol)
      (if (cassandra-filter speech #|children-present-p FIXME |#)
          (toot-speak speech :Toot Toot :vol vol)
          (private-admin-message "Oops!"
                                 "You can't say that here")))))

(definfinity speak ((key speech vol) user recipient/s)
  "The user speaks SPEECH at volume VOL in public.

@cindex Speaking, Speech

Handle speech by the user.

Speech is public to all users in an area.

@subsection Usage

@verbatim
{ c: \"speech\",
  d: { speech: \"text to be spoken\",
       vol: ( \"shout\" | \"talk\" | \"whisper\" ) } }
@end verbatim

@code{key} --- WRITEME --- optional --- currently ignored

@code{vol}  ---   Volume  is   one  of  @code{talk},   @code{shout},  or
@code{whisper}.   The   default   is  always   @code{talk}.   @code{vol}
is optional.

@subsection Speech filtering

There are two  kinds of filtering on text: foul  language, and obnoxious
typing.
  
Foul  language filtering  occurs when  there are  children or  sensitive
players   nearby   (blacklist),   and   in   all   cases   for   certain
stopwords (redlist).

See `CASSANDRA-FILTER'.

Obnoxious typing filtering  occurs all the time, and undoes  a couple of
things that are --- well, just plain obnoxious.

@itemize

@item
SPEECH IN ALL CAPS  is converted into lower-case; if it  was meant to be
whispered, it will  instead be spoken (@code{talk}); if it  was meant to
be spoken, it will instead be shouted. Shouted text in all caps remains 
shouted (but is still in lower-case).

@item
Sentences with lots of punctuation!!  are fixed; aside from ellipses, no
repeated punctuation is preserved.

@end itemize

See `CASSANDRA-OBNOXIOUS-FILTER'.

@subsection Special character handling

The first character of the speech can turn it into a special command
of some kind.

@table @code

@item ~

Commands beginning  with @code{~} should  be handled by the  client. A
conforming  client should  never  forward any  command beginning  with
@code{~} to the server.

@item #
Server commands  begin with @code{#}  (sharp sign / octothorpe  / hash
sign).    A   server   command   is  any   unary   function   in   the
@b{Tootsville-User} package. See `PARSE-OPERATOR-COMMAND' for details.

@item @@
@code{@@}-messages are whispered directly to the named character, if
they are located somewhere in the nearby area. For example, 
@code{@@Catvlle Hello!} will whisper the phrase @code{Hello!} to only
the player @code{Catvlle}.

@item /
Emotes begin with @code{/}. Emotes set the expression of the character
to one of a predefined list of expresssions, or display an emoji speech
balloon. 

Why these specific emojis? Backward compatiblity. This list of emotes
was inherited from Tootsville IV.

The emoji items might be replaced with more detailed animations in the
future.

A few of the emotes actually have even more complex behavior, as noted 
in the index below.

@cindex Emotes

@table @code
@item smile
The expression on the character's face should change to a smile.
@item frown
The expression on the character's face should change to a frown.
@item wink
The character should make an excaggerated wink.
@item sick
The expression on the character's face should change to disgust.
@item whoa
The expression on the character's face should change to surprise.
@item cool
An emoji of a smiling face wearing sunglasses.
@item cheese
The expression on the character's face should change to a smile
with tongue stuck out.
@item angry
The expression on the character's face should change to anger.
@item silly
The expression on the character's face should change to a silly face.
@item sleep
The character's face should look as though they are sleeping. Also,
a special ``Zzz'' graphic should appear over their head.
@item meh
The expression on the character's face should change to disinterest.
@item cry
The character should begin to cry.
@item pizza
An emoji of a pizza.
@item burger
An emoji of a burger.
@item hotdog
An emoji of an hot dog.
@item fries
An emoji of a pack of French fries.
@item drink
An emoji of a glass of an unidentified beverage.
@item icecream
An emoji of ice cream.
@item cake
An emoji of a slice of cake.
@item game
WRITEME
@item dice
An emoji will be spoken showing a single 6-sided die; however, the number of pips
shown (1-6) will be random.
@item coin
An emoji will be spoken showing a coin; however, whether that coin shows as
head or tails will be random.
@item heart
A heart emoji
@item broken
A broken heart emoji
@item rps
An emoji will appear with a random selection from: rock, paper, scissors.
@item music
A musical note emoji.
@item rainbow
A rainbow emoji.
@item lol
The character's expression should change to laughter.
@item rain
A raincloud emoji.
@item huh
The character's expression should change to confusion.
@end table

@item \ % _ ^ |

These characters are reserved for future use. You cannot speak a line
beginning with them.

@item ? !

For convenience of Spanish speakers, sentences beginning with @code{?}
or @code{!} are converted into @code{¿} and @code{¡}.

UNIMPLEMENTED

@end table

@subsection Special commands

@table @code

@item ,credits
Speaking @code{,credits} will send the server's credits as an admin
message.

@item ,disconnect
Speaking  @code{,disconnect}   will  immediately  drop   the  client's
connection   without    ceremony;   it's   used   for    testing   the
auto-reconnection code.

@item ,dumpthreads
This will log  all active threads to  the server log. Note  that it is
not an  operator command in this  context, but it is  identical to the
operator command @code{#dumpthreads}.

@end table

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

WRITEME

"
  (when (emptyp speech)
    (return))
  (case (char speech 0)
    (#\~ (v:warn :speak "Received a client command ~a" speech)
     (private-admin-message "Client Command"
                            (format nil "~a should have been handled by your client software."
                                    (first (split-sequence #\Space speech))))
     (return))
    (#\# (return (parse-operator-command speech)))
    (#\! (when (find #\! (subseq speech 1))
           (setf speech (concatenate 'string "¡" (subseq speech 1)))))
    (#\? (when (find #\? (subseq speech 1))
           (setf speech (concatenate 'string "¿" (subseq speech 1)))))
    (#\@ (@-message speech))
    (#\/ (v:warn :speak "emote not handled ~a" speech)
     "Emote not handled")               ; TODO
    ((#\\ #\% #\_ #\^ #\|) 
     (v:warn :speak "command not supported ~a" speech)
     (private-admin-message "Oops"
                            (format nil "Try saying something that does not start with ~c" (char speech 0)))) ; XXX
    (otherwise
     (when (search ",dumpthreads" speech)
       (v:debug :dump-threads "Dumping threads on end user imperative ~{~%~a~}"
                (bt:all-threads))
       (setf speech "🔍"))
     (when (search ",disconnect" speech)
       (v:info :stream "Disconnecting ~a by their request" *client*)
       (force-close-hunchensocket *client*)
       (setf speech "♥ Bye! ♥"))
     (when (search ",credits" speech)
       (dump-credits)
       (setf speech "📜"))))
  (player-speak speech vol))

(define-constant +credits+
  "Tootsville V by Bruce-Robert Pocock at the Corporation for Inter-World Tourism and Adventuring.

Special thanks to Ali Dolan, Mariaelisa Greenwood, Richard Harnden,
Levi Mc Call, Gian Ratnapala, and Zephyr Salz.

In memory of the contributions of Maureen Kenny (RIP).

Tootsville IV by Brandon Booker, Gene Cronk, Robert Dawson, Eric
Feiling, Tim Hays, Sean King, Mark Mc Corkle, Cassandra Nichol,
Bruce-Robert Pocock, and Ed Winkelman at Res Interactive, LLC."
  :test 'equal
  :documentation "The Tootsville credits")

(defun dump-credits ()
  "Send +CREDITS+ as a private admin message. Response to the ,credits user utterance."
  (private-admin-message "Credits" (concatenate 'string
                                                "<p>Tootsville V version "
                                                (asdf:component-version (asdf:find-system  :Tootsville))
                                                "</p>"
                                                (docstring->html +credits+ '+credits+))))

(definfinity start-event ((moniker) user recipient/s)
  "Attempt to begin a Quaestor Event. Might return an error.

The bulk of the actual work is in `QUAESTOR-START-EVENT', q.v.

@cindex Quaestor Events

@subsection What is a ``Quaestor Event''?

Events,  in  the context  of  this  function, are  transactions  between
a  player  and  the  world.  These transactions  might  yield  items  or
currency (peanuts,  or fairy dust), so  they have to be  proxied through
the central servers, because we can't  ultimately trust the users not to
just     tap     Control+Shift+K      and     try     something     like
@code{Tootsville.Game.addPeanuts  (1000000)}.   (Note,  that   will  ---
obviously --- not work, because this function exists.)

So, there are a few basic types of events, in general:

@itemize

@item
Magic fountains

@item
Shops

@item
Secrets and treasures

@item
Minigames

@end itemize

Each of these works a little differently.

@subsection Usage

The  basic data  element is  a  @code{moniker}, which  is typically  the
representation of a  particular item in the world which  is the focus of
the event. In  the current usage (Tootsville V/Romance 2),  this will be
an UUID.

@verbatim
{ c: \"startEvent\", d: { moniker: \"moniker\" } }
@end verbatim

@subsection Responses

There are several possible responses.

@subsubsection Event already completed

@verbatim
{ from: \"startEvent\",
  status: false,
  alreadyDone: true,
  err: \"event.alreadyDone\",
  error: \"User-visible error message\"
  moniker: \"moniker\" }
@end verbatim

Some events cannot  be started more than once by  the same character, or
more than once within a certain period of time, or more than once by the
same  character within  a  certain  period of  time.  This  is a  simple
rejection; there is not inherently any  explanation to the client of the
circumstances --- in particular, the client  is not informed when (or by
whom) the event can be fired again.

@subsubsection Event started successfully

@verbatim
{ from: \"startEvent\",
  status: true,
  eventID: \"ID\" }
@end verbatim

This is the short form. It means  that the event can be started, and the
caller had better know what to do about it; typically, that will only be
to  turn  around  and  immediately call  `INFINITY-END-EVENT'  with  the
provided event ID.

@subsubsection Event requires a download to begin

@verbatim
{ from: \"startEvent\",
  status: true,
  eventID: \"ID\",
  filename: \"blah.swf\",
  asVersion: ( 2 | 3 ) }
@end verbatim

This form  is archaic and won't  be returned right now,  but is included
for  comparison  ---  and  to  make the  modern  long  form  make  sense
by comparison.

@verbatim
{ from: \"startEvent\",
  status: true,
  eventID: \"ID\",
  filename: \"blah.js\",
  function: \"foo\",
  asVersion: \"html5\" }
@end verbatim

This is the modern long form. The client is expected to:

@itemize

@item
Download @code{blah.js}

@item 
Call the function @code{Tootsville.Event[\"foo\"](eventID)} ---
that is, literally, look in the global object @code{Tootsville.Event}
for a function named @code{foo} --- with the event ID as its
parameter.

@end itemize

In other words,

@verbatim
Tootsville.Event [ datagram.function ] ( datagram.eventID );
@end verbatim

The code in @code{blah.js} is required to use the opportunistic
object-as-namespace initialization of the form:

@verbatim
if (!('Tootville' in window))
 { Tootsville = { Event: { Foo: {} } }; }

if (!('Event' in Tootsville))
 { Tootsville.Event = { Foo: {} } }; }

if (!('Foo' in Tootsville.Event))
 { Tootsville.Event.Foo = {}; }

Tootsville.Event.foo = function (eventID) { ... };

Tootsville.Event.Foo.otherMethod = function ( ... ) { ... };
@end verbatim

See the front-end documentation for more details on the coding style
used.

@subsection Error response

@verbatim
{ from: \"startEvent\",
  status: false,
  err: \"error code\",
  error: \"User-visible error message\" }
@end verbatim

The error code can be one of:

@table @code

@item eventType.notFound
The @code{moniker} passed was invalid.

@end table

@subsection Ending an event

This event is now open, and will remain open until it has been completed
or canceled using `INFINITY-END-EVENT', q.v.

@subsection Quaestor Events in Detail

@subsubsection Magic Fountains

WRITEME

@subsubsection Shops

WRITEME

@subsubsection Secrets and Treasures

WRITEME

@subsubsection Minigames

WRITEME

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

WRITEME

"
  (quaestor-start-event moniker *Toot*))

(definfinity end-event ((moniker id event-i-d status medal score) user recipient/s)
  "Attempt to end an event.
 
End an event begun by `INFINITY-START-EVENT', q.v.

@subsection Calling

@verbatim
{ c: \"endEvent\",
  d: { moniker: \"event moniker\",
       ( id | eventID ): \"event ID\",
       status: ( \"cmp\" | \"cxl\" ),
       [ medal: \"medal\", ]
       [ score: \"score\" ] } }
@end verbatim

This command terminates an event (such as a fountain, store purchase, or
minigame) which was begun with `INFINITY-START-EVENT', qv.

The parameter @code{eventID} can be  referenced as @code{id} instead ---
but  that  is  deprecated  (since Romance  1.0)  and  will  (eventually)
be dropped.

The status code is either @code{cmp}, if the event was completed in some
way  (successfully  or  otherwise),  or @code{cxl},  if  the  event  was
canceled before it reached any ind of completion.

The @code{score} and @code{medal} parameters are optional, and depend on
the type  of event.  They should  never be  submitted with  a @code{cxl}
cancel  packet, and  are  not  needed for  an  item  purchase. They  are
sometimes  to be  used with  minigames. The  @code{score} has  a special
relationship with magic fountains, described below.

@itemize

@item
In the  event of  a magic  fountain, the client  should submit  a random
number between 1 and 100 as  the @code{score}. This will be ignored, and
a number of peanuts will be awarded to the player.

@item
In  the  event of  a  purchase,  neither @code{score}  nor  @code{medal}
are required.

@item
Other kinds of  events can pass a numeric @code{score},  or a string for
@code{medal}, as appropriate to their needs.

@end itemize

@subsection Success Response to Canceled Event

The response to a canceled (@code{status: \"cxl\"}) event will be of the
form:

@verbatim
{ from: \"endEvent\",
  status: true,
  ended: \"eventID\",
  canceled: true }
@end verbatim

@subsection Success Response to Completed Event

The response  to a completed  (@code{status: \"cmp\"}) event will  be of
the form:

@verbatim
{ from: \"endEvent\",
  status: true,
  ended: \"eventID\",
  peanuts: peanuts,
  fairyDust: fairyDust,
  [ highScores: { 1: { points: points,
                       userName: \"user name\" },
                  2: ... 24: }, ]
  totalPeanuts: total,
  totalFairyDust: total,
  [ gotHighScore: index ]
@end verbatim

The @code{endEvent} packet for a completed event indicates:

@itemize

@item
The event  ID which  was ended  --- typically a  UUID. This  matches the
@code{eventID}  returned   by  @code{startEvent}  and  passed   back  to
@code{endEvent}.

@item
The  relative change  in peanuts  and  fairy dust  (positive means  more
earned; negative means a net loss), and the player's new totals of each

@item
If the event is  a minigame or other event that could  have a high score
list, up to 24 top scores are returned, each with a point score, an user
name, and (if the event is that sort) possibly a medal earned. Note that 
it is possible to get fewer than 24 scores back; conforming clients must 
accept zero to at least 24.

@item
If the  event has a high  score list, and  the player has earned  a high
score now, the index (from 1 = first place) which was achieved.

@end itemize

@subsection Error Responses

An error response is of the form:

@verbatim
{ from: \"endEvent\",
  status: false,
  eventID: \"event UUID\",
  err: \"error code\",
  error: \"User-visible error message\" }
@end verbatim

The error code can be one of:

@table @code

@item cost
The item to be purchased costs more peanuts than you have.

@item badStatus
The status passed was not one of @code{cmp} nor @code{cxl}

@item eventID.notFound
The event ID passed was not found

@item eventID.notYours
The event ID passed represents an event started by another player

@item medal.notFound
The medal passed was not valid

@item score.range
The score reported  was not valid; it  was not in the  range of possible
scores for this event.

@end table

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

WRITEME

"
  (let* ((event-id (or event-i-d id))
         (event (ignore-not-found (find-record 'quaestor-event :id event-id))))
    (when (null event)
      (return (list 404 (list :|from| "endEvent"
                              :|status| :false
                              :|err| "eventID.notFound"
                              :|error| "You tried to end an event that was never started."))))
    (unless (uuid:uuid= (Toot-uuid *Toot*) (quaestor-event-started-by event))
      (return (list 403 (list :|from| "endEvent"
                              :|status| :false
                              :|err| "eventID.notYours"
                              :|error| "You tried to end someone else's event."))))
    (string-case status
                 ("cmp" (quaestor-complete-event event score medal))
                 ("cxl" (quaestor-cancel-event event))
                 (otherwise 
                  (list 400 (list :|from| "endEvent"
                                  :|status| :false
                                  :|err| "badStatus"
                                  :|error| "Your software tried to end an event with an unrecognized status"))))))

(definfinity use-equipment ((|t| x y z on of) user recipient/s)
  "The player wishes to use a piece of equipment on a particular item or place.

@subsection Usage

@verbatim
{ t: ( 1 | 2 ),
  x: X, y: Y, z: Z }
@end verbatim

or

@verbatim
{ t: ( 1 | 2 ),
  on: \"ITEM-OR-CHARACTER-UUID\",
  [ of: ( \"item\" | \"char\" ) ] }
@end verbatim

The  @code{t} number  indicates  whether  the user's  currently-selected
primary item (i.e.  the item equipped in their trunk)  is being used, or
their  secondary item  (which is  not  supported). In  other words,  for
Romance 1.1, 1.2, or 2.0, this must always be the number 1.

In the first form, the user wants to use their equipment on an arbitrary
point in space, whose coördinates are passed in.

In  the  second  form,  the  user   wants  to  use  their  equipment  on
a particular item or character. The optional @code{of} helps narrow down
whether it should be an item or character.

WRITEME

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

WRITEME

"
  (error 'unimplemented))

