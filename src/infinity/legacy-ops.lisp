;; -*- lisp -*-

;;; src/infinity/legacy-ops.lisp is part of Tootsville
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

(defconstant +builder-toot-hard-hat-template+ 2494)

(define-operator-command $ (words user plane)
  "Execute a command script.

@subsection Usage

@verbatim
#$ SCRIPT-NAME
@end verbatim

Example:

@verbatim
#$ do-something-funny
@end verbatim

The script name must be a function previously defined by @code{#SCRIPT};
see `SCRIPT' operator command for details.

"
  (error 'unimplemented))

(define-operator-command addevent (words user plane)
  "Add a GameEvent to a Zone

@subsection Usage

@verbatim
#addevent [EVENTNAME]
@end verbatim

Examples:

@example
 #addevent LaserTagGame
 #addevent PropsWeather
 #addevent ShaddowFalls
 #addevent Tootlympics
@end example

WRITEME
"
  (error 'unimplemented))

(define-operator-command agent (words user plane)
  "

 WRITEME: Document this method brpocock@@star-hope.org

 "
  
  (error 'unimplemented))
(define-operator-command askme (words user plane)
  "

WRITEME
"
  
  (error 'unimplemented))

(define-operator-command ban (words user plane)
  "Ban a user persistently (permanently) from the game.

@subsection Usage

@verbatim
 #ban [REASONCODE] [LOGIN]
@end verbatim

 Examples
@example
 #ban obs.rude pil
@end example

See Also: `KICK'

The  same reason  codes  and  syntax are  used  for  @code{#ban} as  for
@code{#kick}, so refer to that section of the manual for reason codes.

Unlike a @code{#kick}, a @code{#ban}  remains in effect persistently ---
permanently, unless an operator reverses it."
  
  (error 'unimplemented))

(define-operator-command banhammer (words user plane)
  "Ban an IP address from connecting.

@subsection Usage

@verbatim
#banhammer #list
#banhammer #user NICKNAME
#banhammer #+ip ADDRESS
#banhammer #-ip ADDRESS
@end verbatim

Bans can be listed using @samp{#banhammer #list}

Bans  can  be lifted  using  @samp{#banhammer  #-ip IP-ADDRESS}  (or
hostname)

A  ban  can   be  placed  with  @samp{#banhammer   #+ip  IP-ADDRESS}  or
@samp{#banhammer  #+ip HOSTNAME}  or  @samp{#banhammer #user  USERNAME}.
In the  latter case, the  user's connected IP  address is used.  This is
expected to be the most common usage.

Parameters:  the  first  word  is  a  subcommand;  one  of  @samp{#+ip},
@samp{#-ip},   @samp{#user},    or   @samp{#list}.    For   @samp{#+ip},
@samp{#-ip}, or @samp{#user}, an additional parameter is needed.

WRITEME
"
  
  (error 'unimplemented))

(define-operator-command beam (words user plane)
  "Beam yourself to a different location.

@subsection Usage
 
@verbatim
#beam LATITUDE LONGITUDE [ALTITUDE]
@end verbatim

Altitude is optional.

@subsection Changes in 2.0

In Romance  1, this command took  a room moniker as  its sole parameter;
since rooms as such no longer exist, we use latitude and longitude now.

"
  (check-type words list)
  (list 200 (list :|from| "beam"
                  :|latitude| (parse-integer (first words))
                  :|longitude| (parse-integer (second words))
                  :|altitude| (if (< 2 (length words))
                                  (parse-integer (third words))
                                  0))))

(define-operator-command spawnroom (words user plane)
  " WRITEME --- Legacy --- useless in 2.0

 Create  a new  room  in  the current  zone.  Must  have staff  level
 8 (DEVELOPER) to use this command.

 Syntax for use
 #spawnroom [MONIKER] [TITLE] [SWF]
 #spawnroom [MONIKER] [TITLE]

 NOTE: Uses tootCastleJoust.swf as default. This can be set after the
 room has been created by setting the 'f' room variable.

 Examples
 #spawnroom tootCastleJoust2 Joust2 tootCastleJoust.swf
 #spawnroom tootCastleJoust2 Joust2

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
  (error 'unimplemented))

(define-operator-command census (words user plane)
  "Load a number of users.
 
Simply  reference a  range  of  users, for  testing  purposes. Takes  an
optional  low and  high point,  or  runs 0…250000.  (250,000) This  will
load their Toots, and seriously strain the caché and database subsystems.

Afterwards, due to cache flooding,  database accesses may be slower than
usual until things balance out to a more normal workload.

Since this is designed  to stress the servers, it can  be called only by
God (Pil).

@subsection Usage

@verbatim
#census
@end verbatim
"
  (if (equal (Toot-name *Toot*) "Pil")
      (let* ((low (if (<= 1 (length words))
                      (parse-integer (first words))
                      0))
             (length (if (<= 2 (length words))
                         (1+ (- (parse-integer (second words)) low))
                         250000))
             (uuids (mapcar 
                     (compose #'base64-to-uuid #'second)
                     (db-select-all :friendly
                                    (format nil "SELECT uuid FROM people LIMIT ~d OFFSET ~d"
                                            length low))))
             (users 0) (Toots 0))
        (dolist (uuid uuids)
          (let ((person (find-record 'person :uuid uuid)))
            (person-info person)
            (incf users)
            (dolist (Toot (player-Toots person))
              (Toot-info Toot)
              (incf Toots))))
        (format nil "Stressed database with ~:d user~:p (from offset ~:d) and ~:d Toot~:p"
                users low Toots))
      "Only Pil can trigger a #census"))

(define-operator-command clearbadge (words user plane)
  "Clear a badge off the map.

UNIMPLEMENTED

@subsection Usage

@verbatim 
#clearbadge [NICKNAME] [PLACENAME]
#clearbadge #me [PLACENAME]

#clearbadge #me #here
#clearbadge [NICKNAME] #here

#clearbadge #me #all
#clearbadge [NICKNAME] #all
@end verbatim

 Examples
@example
 #clearbadge snowcone tootSquare
 #clearbadge snowcone #all
 #clearbadge snowcone #here
 #clearbadge #me #all
 #clearbadge #me #here
@end example

@subsection Badges

See `SETBADGE' for a discussion of the map badges system.
"
  (error 'unimplemented))

(define-operator-command clearcache (words user plane)
  "Forcibly clear all cachés (MemCacheD)

@subsection Usage

@verbatim
#clearcache
@end verbatim
"
  (cl-memcached:mc-flush-all))

(define-operator-command clearevent (words user plane)
  "UNIMPLEMENTED

Clear a GameEvent from a Zone. 

Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #clearevent [EVENTNAME]

 Examples
 #clearevent LaserTagGame
 #clearevent PropsWeather
 #clearevent ShaddowFalls
 #clearevent Tootlympics

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 See Also:
 op_addevent(String[], AbstractUser, Room), op_getevents(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command clearvar (words user plane)
  "Clear a room variable

@subsection Usage

@verbatim 
#clearvar @@[ROOM] [VARIABLE] [VALUE] 
#clearvar [VARIABLE] [VALUE]
@end verbatim

Examples

@example 
#clearvar @@tootsSquareWest anim~ropes 2 
#clearvar anim~ropes 2
@end example

WRITEME"
  (error 'unimplemented))

(define-operator-command cloneroom (words u plane)
  "Clone a room
 
@subsection Usage

@verbatim
#cloneroom NEW-MONIKER
#cloneroom OLD-MONIKER NEW-MONIKER
@end verbatim

WRITEME
"
  (error 'unimplemented))

(define-operator-command createroom (words user plane)
  "Create a new room

WRITEME: Document this method brpocock@@star-hope.org
"
  (error 'unimplemented))

(define-operator-command dbcpinfo (words user plane)
  "Get information from the DBI (database) layer.


Earlier versions of Romance were  Java-based, using the DBCP layer, thus
the name.

@subsection Usage

@verbatim 
#dbcpinfo
@end verbatim

 Examples:

@example 
#dbcpinfo
@end example

This identifies  the name  of the  database being  used, the  DBI driver
type, and the active connection or connection pool.
"
  (with-dbi (:friendly)
    (let ((driver (dbi:connection-driver-type *dbi-connection*)))
      (format nil "Friendly database name: ~a;
DBI connection driver type: ~a (~a::~a);
connection: ~/HTML/; working: ~/HTML/"
              (dbi:connection-database-name *dbi-connection*)
              driver
              (package-name (symbol-package (class-name (dbi:find-driver driver))))
              (class-name (dbi:find-driver driver))
              (dbi.driver:connection-handle *dbi-connection*)
              (db-select-all :friendly "select true")))))

(define-operator-command dress (words user plane)
  "UNIMPLEMENTED

 Force a character to wear a specific clothing item. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #dress [LOGIN] [ITEM] [optional: COLOUR]

 Examples
 #dress flappyperry 1337

 Parameters:
 words - login name, item ID, and optional colour string (see Colour.Colour(String))
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 Throws:
 DataException - if the colour is bad
 NumberFormatException - if the colour is bad
"
  (error 'unimplemented)
  )

(define-operator-command drop (words user plane)
  "UNIMPLEMENTED

Find an item in your inventory based upon the item ID # and drop it (to the world).

Usage: #drop ITEM-TEMPLATE-ID
"
  (error 'unimplemented))

(define-operator-command dropkick (words user plane)
  "UNIMPLEMENTED

 
Silently remove the named user from the game by disconnection. Must have
staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #dropkick [LOGIN]

 Examples
 #dropkick flappyperry
"
  (error 'unimplemented))

(define-operator-command dumpthreads (words user plane)
  "Dump debugging information including all running threads to the server logs.

@subsection Usage

@verbatim 
#dumpthreads
,dumpthreads
@end verbatim

Example

@example
#dumpthreads
,dumpthreads
@end example

Note that this  can be invoked as @code{,dumpthreads}  by a non-operator
user as well."
  (v:info :dump-threads "Dumping threads on end user imperative ~{~%~a~}"
          (all-threads))
  (format nil "Dumped names of ~:d thread~:p" (length (all-threads))))

(define-operator-command enablepathfinder (words user plane)
  "UNIMPLEMENTED

 Temporary test routine for testing pathfinders on users

Syntax:

#enablepathfinder (true|false)"
  (error 'unimplemented))

(define-operator-command evacuate (words user plane)
  "UNIMPLEMENTED

 Evacuate all users from your current Zone into another Zone. Will error if the Zone specified does not exist. Must have staff level 8 (DEVELOPER) to use this command.

 Syntax for use
 #evacuate [ZONE]

 Examples
 #evacuate dottie

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
  (error 'unimplemented))

(define-operator-command filter (words user plane)
  "Test censorship rules against words or phrases

@subsection Usage

@verbatim
#filter EXPRESSION
#filter #all EXPRESSION
#filter #child EXPRESSION
@end verbatim

 
WRITEME"
  (error 'unimplemented))

(defun json-to-html (json)
  "Converts JSON to a set of key-value pairs in pretty-printed HTML form."
  (with-output-to-string (s)
    (doplist (key value json)
        (format s "~%<div><strong>~/HTML/</strong>: &nbsp; ~/HTML/</div>" key value))))

(define-operator-command finger (words user plane)
  "Finger a user account. Return interesting details in an administrative message.

@subsection Usage

@verbatim
#finger TOOT
@end verbatim

Examples:

@example
#finger mouser
#finger shade
@end example

"
  (let* ((Toot (find-record 'Toot :name (first words)))
         (player (find-reference Toot :player)))
    (format nil " ~a is a ~a with base color ~a, pad color ~a, and pattern ~a ~a. 
 This is a~:[n adult~; child~]'s account. ~@[~*(sensitive player)~] ~@[~*(patron)~]
 The user has ~:d peanut~:p, ~:d fairy dust, and was last active ~a
 (Earth time, ~a ago; ~a)
 The player owning ~a is ~a (~a). 
 Toot: ~a; player: ~a 
"
            (Toot-name Toot)
            (avatar-moniker (find-reference Toot :avatar))
            (color24-name (Toot-base-color Toot))
            (color24-name (Toot-pad-color Toot))
            (color24-name (Toot-pattern-color Toot))
            (pattern-name (find-reference Toot :pattern))
            (Toot-childp Toot)
            (person-sensitivep player)
            (person-is-patron-p player)
            (Toot-peanuts Toot) (Toot-fairy-dust Toot)
            (Toot-last-active Toot)
            (human-duration (timestamp-difference (now)
                                                  (Toot-last-active Toot)))
            (Chœrogryllum:date-string (timestamp-to-universal (Toot-last-active Toot)))
            (Toot-name Toot)
            (person-display-name player)
            (person-first-email player)
            (Toot-uuid Toot)
            (person-uuid player))))

(define-operator-command flush (words user plane)
  "UNIMPLEMENTED

Historically, this flushed the database write cache.

@subsection Usage

@verbatim
#flush
@end verbatim

"
  (error 'unimplemented))

(define-operator-command game (words user plane)
  "UNIMPLEMENTED

 Send a command into the operator command interpreter for a running game (if that game provides one)

 Usage: #game gameClass (strings...)

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing. The GameEvent must be attached thereunto.
"
  (error 'unimplemented))

(define-operator-command getconfig (words user plane)
  "Reads a configuration key. All WORDS are expected to be the keywords
on the path to the config value.

@subsection Usage

@verbatim
#getconfig PROPERTY
#getconfig PROP1 PROP2 [...]
@end verbatim

Example:
@example
#getconfig taskmaster devel
@end example

Returns the value of the selected configuration property as a string.
 "
  (format nil "<pre>~/HTML/</pre>"
          (apply #'config (mapcar (compose #'make-keyword #'string-upcase) words))))

(define-operator-command getevents (words user plane)
  "UNIMPLEMENTED

 List GameEvents in your current Zone. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #getevents

 Examples
 #getevents

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 See Also:
 op_addevent(String[], AbstractUser, Room), op_getevents(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command getmotd (words user plane)
  "Retrieve the current Message Of The Day as a server message.

@subsection Usage

@verbatim
#getmotd
@end verbatim

Example
@example
#getmotd
@end example"
  *motd*)

(define-operator-command getschedule (words user plane)
  "

 WRITEME: Document this method brpocock@@star-hope.org

 Parameters:
 words - WRITEME
 u - WRITEME
 room - WRITEME
 Throws:
 PrivilegeRequiredException - WRITEME
"
  (error 'unimplemented))

(define-operator-command getschedulefor (words user plane)
  "UNIMPLEMENTED

 Get scheduled events for a particular class (scheduled by that class)

 Parameters:
 words - Specify the class's full, canonical name
 u - the user invoking
 room - the room in which the user is standing
 Throws:
 PrivilegeRequiredException - if the user doesn't have at least moderator privilege level
 ClassNotFoundException - is the class requested can't be found (probably a typo)
"
  (error 'unimplemented))

(define-operator-command getuvar (words user plane)
  "UNIMPLEMENTED

 Get a user variable. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #getuvar [LOGIN] [VARIABLE]
 User Name

 user name of a character
 #me for the user you are logged in as

 Examples
 #getuvar mouser d
 #getuvar #me d

 See Also:
 op_setuvar(String[], AbstractUser, Room), op_getuvars(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command getuvars (words user plane)
  "UNIMPLEMENTED

 Get all user variables for a given user. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #getuvars [LOGIN].
 User Name

 user name of a character
 #me for the user you are logged in as

 Examples
 #getuvars mouser
 #getuvars #me

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 See Also:
 op_setuvar(String[], AbstractUser, Room), op_getuvar(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command getvar (words user plane)
  "UNIMPLEMENTED

 Get a room variable. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #getvar @@[ROOM] [VARIABLE]
 #getvar [VARIABLE]

 Examples
 #getvar @@tootsSquareWest anim~ropes
 #getvar anim~ropes

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 See Also:
 op_setvar(String[], AbstractUser, Room), op_clearvar(String[], AbstractUser, Room), op_getvars(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command getvars (words user plane)
  "UNIMPLEMENTED

 Get all room variables. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #getvars [ROOM]
 #getvars

 Examples
 #getvars tootsSquare
 #getvars

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 See Also:
 op_setvar(String[], AbstractUser, Room), op_clearvar(String[], AbstractUser, Room), op_getvar(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command give (words u plane)
  "Give an item as a gift to another user.

@subsection Usage

@verbatim
#give ITEM USER
@end verbatim

Example

@example
#give CDCCA838-FB7B-423A-81DA-1514817598DB flappyperry
@end example


UNIMPLEMENTED

The item to be gifted must be in your inventory. To give a new item see `GRANT'
"
  (error 'unimplemented))

(define-operator-command givehead (words user plane)
  "Grants a new inventory item to a user and equips it.

NOTE:  @code{#grant} and  @code{#givehead}  are  identical, except  that
@code{#givehead} equips  the item  and @code{#grant}  does not.  See also
`GRANT'.

@subsection Usage

@verbatim
#givehead TEMPLATE USER
@end verbatim

Example

@example
#givehead 1337 catvlle
@end example
 
This creates  a new item  from the  item template number  indicated, and
equips it on the recipient. To give  a gift from your own inventory, see
`GIVE'. To grant a new item without equipping it, see `GRANT'."
  (error 'unimplemented))

(define-operator-command goto (words user plane)
  "UNIMPLEMENTED

 throws PrivilegeRequiredException

 WRITEME: Document this method brpocock@@star-hope.org

 Parameters:
 words - WRITEME
 u - WRITEME
 room - WRITEME
 Throws:
 PrivilegeRequiredException - WRITEME
"
  (error 'unimplemented))

(define-operator-command grant (words user plane)
  "Grants a new inventory item to a user.

NOTE:  @code{#grant} and  @code{#givehead}  are  identical, except  that
@code{#givehead} equips  the item  and @code{#grant}  does not.  See also
`GIVEHEAD'.

@subsection Usage

@verbatim
#grant TEMPLATE USER
@end verbatim

Example

@example
#grant 1337 catvlle
@end example
 
This creates  a new item  from the  item template number  indicated, and
gives it to the recipient. To give  a gift from your own inventory, see
`GIVE'. To grant a new item and equipping it, see `GIVEHEAD'."
  (error 'unimplemented))

(define-operator-command headcount (words user plane)
  "Get headcount information about the running system.

@subsection Usage

@verbatim
#headcount #all
#headcount #members
#headcount #room
#headcount #highwater
@end verbatim

Examples

@example
#headcount #all
#headcount #members
#headcount #room
#headcount #highwater
@end example

@subsection Headcount All

WRITEME

@subsection Headcount Members

WRITEME

@subsection Headcount Room

WRITEME

@subsection Headcount Highwater

WRITEME
"
  (error 'unimplemented))

(define-operator-command inv (words user plane)
  "UNIMPLEMENTED

 throws PrivilegeRequiredException

 Get inventory  items for  a particular user.  By default,  this will
 bring up  only the active  items â€”  e.g. clothing being  worn, and
 so forth.

 To get all active inventory for an user: #inv LOGIN

 To get  all inventory for an  user, active or inactive  (this may be
 very long!): #inv LOGIN #all

 To  get inventory  of a  particular type,  active or  inactive: #inv
 LOGIN #type TYPE

 The    type    strings    accepted    are    those    accepted    by
 Commands.do_getInventoryByType(JSONObject,  AbstractUser,   Room)  ;
 this means that both the  $SPECIFIC-TYPE and TYPE-SET-NAME forms are
 accepted. The list  of specific types might include  e.g. $Hair, and
 a  type-set-name  might  be  something like  clothing.  The  set  of
 available type-set-names is specified in the configuration file.

 Parameters:

words - User  name, and optional tag  #all to show all  items instead of
just  active, or  optional tag  #type  and a  type string  for items  of
a     specific      type.     For      item     types,      refer     to
Commands.do_getInventoryByType(JSONObject, AbstractUser,  Room) â€” note
that this supports only a single item type (or type-set)

u - user calling this command

room - the room in which that user is standing

Throws:
 PrivilegeRequiredException - if the user lacks staff privileges to invoke this command
"
  (error 'UNIMPLEMENTED))

(define-operator-command kick (words user plane)
  "Kick a user offline for a certain reason

  
@subsection Usage

@verbatim
#kick [REASONCODE] [LOGIN]
@end verbatim

Kick LOGIN offline for REASONCODE

@verbatim
#kick #list
@end verbatim

List reason codes.

Example

@example 
#kick bully pil
#kick #list
@end example

@subsection Reason Codes

@table @code
@item BULLY
Bullies are not allowed here.
@item CHEAT
Cheaters are not allowed here.
@item DIAMOND
Watch your language around children.
@item MEAN
Don't be mean!
@item NICE
Be nice!
@item PARENT
You need your parent's permission to play in Tootsville.
@end table

@subsection Reason Codes from 1.2

These are no longer supported

@table @code
@item PER.MAIL
Don't share personal information like eMail addresses!
@item PER.NAME
 Don't share personal information like your real name!
@item PER.PASS
 Don't share personal information like passwords!
@item PER.CHAT
 Don't share personal information like chat and instant messaging \ information! @item PER.LOCA 
Don't share personal information like your location! 
@item PER.AGES 
Don't share personal information like your age!
@item PER.BDAY 
Don't share personal information like your birth date!
@item BUL.MEAN 
Don't be mean!
@item OBS.RUDE 
Don't be rude!
@item OBS.FOUL 
Don't use foul words!
@item NET.CHTR 
No cheating!
@item APP.PARN 
You need your parent's permission in order to chat in Tootsville.
@item APP.MAIL 
You need to confirm your eMail address in order to chat in Tootsville.
@item APP.AGES 
Lying about your birth date is against the law!
@end table
 "
  (if (equal "#list" (first words))
      "Reasons: BULLY CHEAT DIAMOND MEAN NICE PARENT"
      (error 'unimplemented)))

(define-operator-command king (words user plane)
  "UNIMPLEMENTED

 Apply  a  gift  membership  to   an  user.  Must  have  staff  level
 4 (DESIGNER) to use this command.

 Syntax for use
 #king [DAYS] [LOGIN]

 Examples
 #king 2 flappyperry

"
  (error 'unimplemented))

(define-operator-command liftban (words user plane)
  "Lift the ban upon a user.

@subsection Usage

@verbatim
#liftban REASON USER yes
@end verbatim

NOTE: In order to un-ban a user,  you must key in the literal word “yes”
as the third parameter, and supply the  ban reason as the first. This is
to avoid accidentally lifting a ban.

Example

@example
#liftban CHEAT silly-biscuits yes
@end example

"
  (error 'unimplemented))

(define-operator-command loadlists (words user plane)
  "Reload the censorship lists.

@subsection Usage

@verbatim
#loadlists
#loadlists #blacklist
#loadlists #redlist
@end verbatim

This reloads the blacklist or  redlist from the database, discarding any
unsaved or recent changes.
"
  (error 'unimplemented))

(define-operator-command mem (words user plane)
  "Display some memory usage and other debugging type information as an pop-up message. 

This is an abbreviated version of the output of `ROOM'
 
@subsection Usage

@verbatim
#mem
@end verbatim

Example

@example
#mem
@end example

@subsection Example report

@example
This server is Inktomi.
Dynamic space usage is:   756,315,840 bytes.
Immobile space usage is:   31,537,408 bytes (134,512 bytes overhead).
Read-only space usage is:           0 bytes.
Static space usage is:          1,344 bytes.
Control stack usage is:         9,656 bytes.
Binding stack usage is:           832 bytes.
Control and binding stack usage is for the current thread only.
Garbage collection is currently enabled.
@end example

"
  (format nil "This server is ~a. <pre>~a</pre>"
          (machine-instance)
          (first-paragraph (with-output-to-string (*standard-output*) (room)))))

(define-operator-command metronome (words user plane)
  "Display information  about or  micromanage the metronome. 

@subsection Usage

@verbatim
#metronome [OPTION]
@end verbatim

Examples
@verbatim
 #metronome #help
 #metronome #rate
 #metronome #last
 #metronome #start
 #metronome #stop
 #metronome #restart
 #metronome #tick
 #metronome #list
 #metronome #cancel <NAME>
@end verbatim

@subsubsection Options

@table @code
@item #help
list these options 
@item #rate
Displays  a message  indicating the  rate  that the  metronome ticks  in
milliseconds. Always 1000 (1s).
@item #last
Displays a  message indicating  the time in  milliseconds when  the last
metronome tick occured. Always rounded to 1s.
@item #start
Starts the metronome. 
@item #stop
Stops the metronome. 
@item #restart 
Restarts the metronome. 
@item #tick 
Forces the metronome to tick.
@item #list
List all tasks scheduled with the metronome
@item #cancel <NAME>
Cancel a specific task by name
@end table


@subsection Changes from 1.2 to 2.0

Added @code{#metronome #help}, @code{#metronome #list}, and @code{#metronome #cancel NAME} 

 "
  (string-case (or (first words) "#help")
    ("#help"
     "Usage: #metronome [OPTION] where [OPTION] is one of: #help #rate #last #start #stop #restart #tick #list, or #cancel NAME")
    ("#rate" "The metronome runs every second (1000ms)")
    ("#last"
     (let ((last (1- *metronome-next-tick*)))
       (format nil
               "The last metronome tick was at ~d — ~:d second~:p ago"
               last (- (get-universal-time) last))))
    ("#start" (format nil "Starting: ~s"
                      (start-game-metronome)))
    ("#stop" (format nil "Stopping: ~s"
                     (stop-game-metronome)))
    ("#restart" (format nil "Stopping: ~/HTML/; Starting: ~/HTML/"
                        (stop-game-metronome)
                        (start-game-metronome)))
    ("#tick" (run-metronome-tasks))
    ("#list" (format nil "Metronome tasks: ~{~/HTML/~}"
                     *metronome-tasks*))
    ("#cancel" (let ((task-name (join #\Space (rest words))))
                 (let ((potentials (loop for task in *metronome-tasks*
                                      when (search task-name (metronome-task-name task))
                                      collect task)))
                   (cond
                     ((null potentials)
                      (format nil "There are no tasks matching ~a" task-name))
                     ((= 1 (length potentials))
                      (format nil "Removing ~a from metronome: ~/HTML/"
                              (metronome-task-name (first potentials))
                              (metronome-remove (first potentials))))
                     (t 
                      (format nil "There are ~:d task~:p matching ~a"
                              (length potentials) task-name))))))))

(define-operator-command motd (words user plane)
  "Set the  message of the day.

@subsection Usage

@verbatim
#motd The new message of the day, literally.
@end verbatim

 
Example:

@example
 #motd Don't forget that Hallowe'en in Tootsville is on the 30th --- get your costumes ready!
@end example

 The message of the day is echoed  to every user as they sign in, before
 they choose a Toot. It is @emph{not} echoed to children."
  (when (not (emptyp words))
    (setf *motd* (format nil "~{~a~^ ~}" words))))

(define-operator-command mute (words user plane)
  "Mute a user or area

UNIMPLEMENTED

 See Also:
 `TOOTSVILLE-USER::STFU'
"
  (error 'unimplemented))

(define-operator-command nuke (words user plane)
  "Forcibly disconnect everyone in a room.

WRITEME
"
  (error 'unimplemented))

(define-operator-command parentapproves (words user plane)
  "Signal that a parent approves a user signing in.

@subsection Usage

@verbatim
#parentapproves TOOT
@end verbatim

Example:

@example
#parentapproves Pil
@end example

@subsection Limitations

This is only useful if TOOT is a child Toot account has begun to sign in
and requested  parent permission ---  that is,  there must be  a pending
child request from TOOT.

 "
  (error 'unimplemented))

(define-operator-command ping (words user plane)
  "Ping the  server, to force  a neutral administrative  message reply.
 
@subsection Usage

@verbatim
#ping
@end verbatim

Example:

@example
#ping
@end example"
  "Pong!")

(define-operator-command place (words user plane)
  "Put a thing or a Place into the game

 Add a  Place to  a room.  This command supports  the basic  types of
 event Places,  and adds them to  the room in the  given WHERE place.
 WHERE can be  a diamond-shaped area around the  operator issuing the
 command  (using  #here, #here-tiny,  or  #here-big),  or can  be  an
 explicitly-issued  polygon  string.  The  event region  ID  will  be
 automatically assigned.

 @subsection Usage

@verbatim
#place WHERE #item ITEM-NUMBER 
#place WHERE #room MONIKER 
#place WHERE #vitem PAID-ITEM-NUMBER 
#place WHERE #item2 ITEM-NUMBER PAID-ITEM-NUMBER 
#place WHERE #exit MONIKER 
#place WHERE #mini MINIGAME-MONIKER 
#place WHERE #walk

WHERE := #here | #here-tiny | #here-big | x,z~x,z~x,z~x,z polygon list
@end verbatim

Examples:

@example
#place #here #item 1337
#place #here #room TootSweets
#place #here #vitem 42
#place #here #item2 1337 42
#place #here-tiny #exit TootSquare
#place #here #mini Minigame.js minigame
#place #here-big #walk
@end example

WRITEME

@subsection Placing an item

WRITEME

@subsection Placing a ``room'' marker

WRITEME

@subsection Placing a shop item

WRITEME

@subsection Archaïc: @code{#item2}

This is no longer supported in Romance 2.0.

WRITEME

@subsection Placing an exit (teleporter)

WRITEME

@subsection Placing a minigame

WRITEME

@subsection Placing a walkable space

WRITEME
"
  (error 'unimplemented))

(define-operator-command purgephysics (words user plane)
  "Purge pending physics interactions. UNIMPLEMENTED"
  (error 'unimplemented))

(define-operator-command push (words user plane)
  "UNIMPLEMENTED

 WRITEME"
  (error 'unimplemented))

(define-operator-command put (words user plane)
  "

 
WRITEME: Document this method brpocock@@star-hope.org
"
  (error 'unimplemented))

(define-operator-command rc (words user plane)
  "UNIMPLEMENTED

Run   an    RC   (RunCommands)   script.   Both    the   â€œsystem   run
 commandsâ€   (â€œrunâ€)  method   and  the   â€œnew  zone   run
 commandsâ€ (â€œnewZoneâ€) method will be executed; the

@subsection Usage

@verbatim
#rc
@end verbatim

Example:

@example
#rc
@end example

WRITEME

"
  (error 'unimplemented))

(define-operator-command reboot (words user plane)
  "Restart the game server.

No, really;  this actually kills the  game server with an  error exit so
that it will (hopefully) be restarted by SystemD.

@subsection Usage

@verbatim 
#reboot
@end verbatim

Example:

@example
#reboot
@end example

@subsection Actual Effects

The server  will quit with  Unix exit status 66  in 3 seconds  after the
command is received.
"
  (private-admin-message "Bye!" "This server is rebooting in 3 seconds.")
  (sleep 3)
  (sb-ext:quit :unix-status 66))

(define-operator-command reloadconfig (words user plane)
  "Reloads configuration properties.

@subsection Usage

@verbatim
#reloadconfig
@end verbatim

Example

@example
#reloadconfig
@end example

@subsection Effect
 
Reloads                         the                        configuration
file (.config/Tootsville/Tootsville.config.lisp under the server owner's
home directory).  See `LOAD-CONFIG'. Reports  back the file  loaded, and
the author and write date of the file.
"
  (let ((info (load-config)))
    (format nil "Reloaded configuration from pathname ~a, truename ~a, on host ~a.
Read at ~a. File write date ~a, author ~a."
            (getf info :path) (getf info :truename) (getf info :host)
            (getf info :read) (getf info :file-write-date) (getf info :author))))

(define-operator-command retire (words user plane)
  "Retire a server,

 Forces  a  server to  retire.  This  will disconnect  anyone  currently
 connected via WebSockets to that  server; they should reconnect through
 the load balancer. Use @code{#evacuate}  to first move users to another
 server (see `EVACUATE').

 @subsection Usage

@verbatim
#retire SERVER
#retire
@end verbatim
 
Examples
@example
#retire game3.test.tootsville.org
#retire
@end example
"
  (error 'unimplemented))

(define-operator-command run (words u r)
  "Run an arbitrary nullary Lisp function or method

@subsection USave

@verbatim
#run FUNCTION
#run PACKAGE FUNCTION
@end verbatim

Examples:

@example
#run ws-stats
#run infinity-stats
#run sb-ext quit
@end example

@subsection Changes in 2.0

In 1.x: 
 Run an arbitrary Java routine through an uploaded Runnable or RunCommands class

In 2.x:
 Run arbitrary nullary Lisp function or method"
  (case (length words)
    (1 (if-let (symbol (find-symbol (string-upcase (first words))))
         (ignore-errors (funcall symbol))
         (format nil "No function nor method named ~a" (first words))))
    (2 (if-let (symbol (find-symbol (string-upcase (second words))
                                    (find-package (string-upcase (first words)))))
         (ignore-errors (funcall symbol))
         (format nil "No function nor method named ~a::~a" (first words) (second words))))
    (otherwise "Improper usage: #run FUNCTION or #run PACKAGE FUNCTION")))

(define-operator-command saveroomvars (words user plane)
  "UNIMPLEMENTED

WRITEME"
  (error 'unimplemented))

(define-operator-command scotty (words user plane)
  "Force a user to relocate to another location

@subsection Usage

@verbatim
#scotty TOOT NAMED-PLACE
#scotty TOOT LATITUDE LONGITUDE [ALTITUDE] [WORLD]
@end verbatim

Altitude is optional and defaults to 0.

World is optional and defaults to CHOR.

Examples

@example
#scotty mouser TootSquareWest
#scotty mouser -1 0 0 CHOR
#scotty mouser -1 0
@end example

@subsection Changes from 1.2 to 2.0

In 1.2, this moved an user into another room.

 Syntax for use:
 #scotty [LOGIN] [ROOM]

 Examples:
 #scotty mouser tootSquareWest"
  (error 'unimplemented))

(define-operator-command setavatarcolors (words user plane)
  "Sets the  base and extra colors  of a user's avatar.  

@subsection Usage

@verbatim
#setavatarcolors LOGIN BASE EXTRA
@end verbatim

Each of BASE and EXTRA can be specified in a number of formats.

@itemize

@item
CSS  Style uses  a decimal  triplet  in the  form @code{rgb(r,g,b)}  ---
identified by the literal string @code{rgb}. Each  of R, G, and B are in
the range 0 to 255.

@item
HTML Style uses  a @code{#} sign plus  either 3 or 6  hex characters, in
the form @code{#rgb} or @code{#rrggbb}. The @code{#} sign is optional.

@item
Named colors are supported as per `PARSE-COLOR24'

@end itemize

Examples:

@example
#setavatarcolors mouser #000000 #ffffff
#setavatarcolors mouser rgb(0,0,0) rgb(255,255,255)
@end example

See also `DOODLE' for a similar-but-different way to set avatar colors.
"
  (error 'unimplemented))

(define-operator-command setbadge (words user plane)
  "Set the badge on  a map area.

@subsection Usage

@verbatim
#setbadge
#setbadge BADGE MONIKER
#setbadge #me MONIKER
#setbadge BADGE #here
#setbadge #me #here
@end verbatim

NOTE:  Using #setbadge  with no  parameters will  assume default  values
which are identical to typing #setbadge #me #here

Examples:

@example
#setbadge snowcone TootSquareWest
#setbadge #me TootSquare
#setbadge snowcone #here
#setbadge #me #here
@end example

 "
  (error 'unimplemented))

(define-operator-command setconfig (words user plane)
  "Set a config property.

@subsection Usage
 
@verbatim
#setconfig PROPERTY VALUE
#setconfig PROP1 PROP2 VALUE
@end verbatim

PROPERTY is a  sequence of keywords, which must be  delimited by spaces.
Omit the leading : on the keyword names.

Examples:

@example
#setconfig rollbar access-token 1234567890
@end example

Changes  made   with  this   command  are   only  effective   until  the
configuration file is  reloaded. See `TOOTSVILLE-USER::RELOADCONFIG' and
`LOAD-CONFIG'.
"
  (error 'unimplemented))

(define-operator-command setstafflevel (words user plane)
  "UNIMPLEMENTED 

   WRITEME
"
  (error 'unimplemented))

(define-operator-command setuvar (words user plane)
  "UNIMPLEMENTED

 Set a user variable. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #setuvar [@@LOGIN] VARIABLE [=] VALUE...

 NOTE: Using #setuvar without an @@[LOGIN] parameter will apply the changes
 to the user issuing the command.

 Examples:

 #setuvar @@mouser d = 254~376~254~376~SE~1267735566759

 #setuvar d = 254~376~254~376~SE~1267735566759
"
  (error 'unimplemented))

(define-operator-command setvar (words user plane)
  "UNIMPLEMENTED

 Set a room variable. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #setvar #replace VARIABLE FIND REPLACE
 #setvar [@@ROOM] VARIABLE VALUE...
 
 WARNING: SETTING ROOM VARIABLES TO INVALID VALUES CAN CAUSE UNEXPECTED RESULTS. DOUBLE CHECK ALL VALUES BEING SET FOR CORRECTNESS.

 Use #replace to change a room variable from one value to another.

 Examples:

 #setvar @@tootsSquareWest anim~ropes 2

 #setvar anim~ropes 2
"
  (error 'unimplemented))

(define-operator-command shanghai (words user plane)
  "UNIMPLEMENTED

 Force a client into a different room and zone

 WRITEME"
  (error 'unimplemented))

(define-operator-command shout (words user plane)
  "Speak in another zone.

This  is intended  for using  operator commands  in a  remote zone,  not
normal  chat  messages.

WRITEME

@subsection Usage

#shout [ZONE] [ROOM] [COMMAND...]

 Examples:

 #shout dottie tootSquareWest #wall Hello Everyone

 #shout dottie tootSquare #retire

See modern version `TOOTSVILLE-USER::AT' also

 "
  (error 'unimplemented))

(define-operator-command spawnzone (words user plane)
  "Create a new zone.

  Syntax for use
  #spawnzone [ZONE]

  Examples
  #spawnzone Cupcake

WRITEME
"
  (error 'unimplemented))

(define-operator-command speak (words user plane)
  "UNIMPLEMENTED

  Allows a user to speak. Must have staff level 2 (MODERATOR) to use this command.

  Syntax for use
  #speak [LOGIN]

  Examples
  #speak flappyperry

  Parameters:
  words - The command parameters (whitespace-delimited list) provided after the # command name
  u - The user invoking the operator command
  room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
  "
  (error 'unimplemented))

(define-operator-command stfu (words user plane)
  "Silences a user.

@subsection Usage

@verbatim
#stfu TOOT
#stfu TOOT MINUTES
@end verbatim

Example

@example
#stfu louis
#stfu louis 30
@end example

@subsection Effects

This sets an attribute on TOOT  that prevents them from actually sending
any public  speech messages;  however, @emph{that  user will  not know}.
The  user will  see their  own  speech, but  it  will not  be echoed  to
anyone else.

In other words,  this basically sets up  a global ignore of  the user to
whom it is applied.

If no  time limit is  given, it is effective  for 24 Earth  hours (1,440
Earth minutes).
  "
  (error 'unimplemented))

(define-operator-command testcensor (words user plane)
  "UNIMPLEMENTED

  Test a message with the censor, displays the filter result.

  Syntax for use:

  #testcensor [MESSAGE]

  Examples:
  #testcensor This message will be filtered and the result will be displayed.

  "
  (error 'unimplemented))

(define-operator-command time (words user plane)
  "Displays a message  with the current server time.
  
@subsection Usage

@verbatim
#time
@end verbatim

Example

@example
#time
@end example

@subsection Example Reply

@example
Now  it is  2020-05-18T02:14:08.676610-04:00 (Universal:  3,798,771,248;
Unix: 1,589,782,448)
@end example
  "
  (format nil "Now it is ~a (Universal: ~:d; Unix: ~:d)"
          (now) (get-universal-time) (- (get-universal-time) +unix-time-in-universal+)))

(define-operator-command unbuild (words user plane)
  "UNIMPLEMENTED
 
 Destroys a room. Must have staff level 8 (DEVELOPER) to use this command.

 Syntax for use:
 #unbuild ROOM

 Examples:
 #unbuild tootUniversity

"
  (error 'unimplemented))

(define-operator-command v (words user plane)
  "Forces a user to say a message.

Mnemonic: Ventriloquism

@subsection Usage

@verbatim
#v LOGIN MESSAGE...
@end verbatim

Example:

@example
#v mayor-louis I like to cause trouble in tootsville
@end example

See `INFINITY-SPEAK'

@subsection Changes in 2.0

This no longer allows ventriloquism of operator commands &c.
"
  (Toot-speak (join #\Space (rest words)) 
              :Toot (find-record 'Toot :name (first words))))

(define-operator-command verbosebugs (words u plane)
  "UNIMPLEMENTED

 Set verbose bug backtrace reporting on or off

 Parameters:
 words - single word \"true\" or \"false\"
 u - the user affected
 r - the room in which the user is standing
"
  (error 'unimplemented))

(define-operator-command wall (words user plane)
  "Write to all players.

Sends an admin (parrot) pop-up message to everyone currently online.

@subsection Usage

@verbatim
#wall MESSAGE...
@end verbatim

Example

@example
#wall This message will go to everyone currently on-line.
@end example
"
  (broadcast
   (list :|from| "admin"
         :|status| t
         :|title| "Squawk!"
         :|message| (format nil "~{~a~^ ~}" words))))

(define-operator-command wallops (words user plane)
  "Write to all operators

Sends an pop-up message to all Builder Toots currently online

@subsection Usage

@verbatim
#wallops MESSAGE
@end verbatim

Example

@example
#wallops This message will go to all other staff members in this zone.
@end example

 "
  (error 'unimplemented))

(define-operator-command wallzones (words user plane)
  "

 Sends an  pop-up message to  all everyone  in every zone.  Must have
 staff level 8 (DEVELOPER) to use this command.

 Syntax for use
 #wallzones [MESSAGE...]

 Examples
 #wallzones This message will go to everyone in every zone.

 Parameters:

words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name

u - The user invoking the operator command

room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
  (error 'unimplemented))

(define-operator-command warn (words user plane)
  "Warn a user about breaking a rule.

 Syntax for use: #warn [REASONCODE] [LOGIN]

 Examples
 #warn obs.rude pil

 Reason Codes:

WRITEME ... these are no longer used ...  see `KICK' for the current list

 PER.MAIL = Don't share personal information like eMail addresses!
 PER.NAME = Don't share personal information like your real name!
 PER.PASS = Don't share personal information like passwords!
 PER.CHAT = Don't share personal information like chat and instant messaging \ information!
 PER.LOCA = Don't share personal information like your location!
 PER.AGES = Don't share personal information like your age!
 PER.BDAY = Don't share personal information like your birth date!
 BUL.MEAN = Don't be mean!
 OBS.RUDE = Don't be rude!
 OBS.FOUL = Don't use foul words!
 NET.CHTR = No cheating!
 APP.PARN = You need your parent's permission in order to chat in Tootsville.
 APP.MAIL = You need to confirm your eMail address in order to chat in Tootsville.
 APP.AGES = Lying about your birth date is against the law!

 Parameters:

words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name

"
  (error 'unimplemented))

(define-operator-command whatis (words user plane)
  "Displays information about an item template. 

@subsection Usage

@verbatim
#whatis ITEM-TEMPLATE
@end verbatim

Example:

@example
#whatis 1337
@end example

 "
  (unless (= 1 (length words))
    (return "Give exactly one template item ID"))
  (let ((item-id (parse-integer (first words) :junk-allowed t)))
    (unless (plusp item-id)
      (return "Usage: #whatis <TEMPLATE ID #>"))
    (let ((template (find-record 'item-template :id item-id)))
      (return (format nil "~{~a: ~a~%<BR>~}"
                      (item-template-info template))))))

(define-operator-command whereami (words user plane)
  "Return an administrative message with the  name of the server to which
 the player is currently connected.

@subsection Usage

@verbatim
#whereami
@end verbatim

Example:

@example
#whereami
@end example

The  response admin  message is  simply the  machine name  to which  you
are connected.
"
  (machine-instance))

(define-operator-command whereis (words user plane)
  "Find  out in  what what  room a  character is  standing, if  s/he is
 logged in at the moment. Must  have staff level 2 (MODERATOR) to use
 this command.

 Syntax for use
 #whereis [LOGIN]
 Login

 User Name of a specific user
 #everyone for a the location of every user in the zone.
 @@[ROOM] for the location of every user in the specified room.

 Examples
 #whereis snowcone
 #whereis #everyone
 #whereis @@tootSquare

 Parameters:

words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name

u - The user invoking the operator command

room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.

See Also:
 whereis_atRoom(AbstractUser, Room, String), whereis_everyone(AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command who (words user plane)
  "Displays a  list of everyone  currently in  a room.

 Syntax for use
 #who [ROOM]
 #who

 NOTE: Leaving off the ROOM  parameter will default to displaying for
 the room the command was initialized in.

 Examples
 #who tootSquare
 #whereis
"
  (error 'unimplemented))

(define-operator-command whoami (words user plane)
  "Cause  the character  to speak  his/her  name in  the current  room.
 Appears as dialogue in the form:  ``Hello, my name is NAME.''

@subsection Usage

@verbatim
#whoami
@end verbatim

Example:

@example
#whoami
@end example

Note that the response is public speech; everyone in the room will see it.
"
  (toot-speak (format nil "Hello, my name is ~a." (Toot-name *Toot*)))
  nil)

(define-operator-command whoareyou (words user plane)
  "Ask  the  server who  it  is.  This  command should  return  version
 information on some of the critical classes used in the game server.
 
@subsection Usage

@verbatim
#whoareyou
@end verbatim

Example:

@example
#whoareyou
@end example
 
@subsection Example Response

@example
This server is Inktomi, a X86-64  Intel(R) Core(TM) i7 CPU 860 @ 2.80GHz
running    Linux   5.6.8-300.fc32.x86_64    with   SBCL    2.0.1-1.fc32.
Quicklisp    dist   version    2020-04-27;   Ultralisp    dist   version
20200501011006; Tootsville version 0.6.4
@end example
"
  (format nil "This server is ~a, a ~a ~a running ~a ~a with ~a ~a.
Quicklisp dist version ~a; 
~@[Ultralisp dist version ~a; ~]
Tootsville version ~a"
          (machine-instance) (machine-type) (machine-version)
          (software-type) (software-version)
          (lisp-implementation-type) (lisp-implementation-version)
          (ql:dist-version "quicklisp")
          (ignore-errors (ql:dist-version "ultralisp"))
          (asdf:component-version (asdf:find-system :Tootsville))))

(define-operator-command zoom (words user plane)
  "

 WRITEME: Document this method brpocock@@star-hope.org

 
"
  (error 'unimplemented))
