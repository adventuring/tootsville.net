;; -*- lisp -*-

;;; src/infinity/legacy-ops.lisp is part of Tootsville
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

(defconstant +builder-toot-hard-hat-template+ 2494)

(define-operator-command $ (words user plane)
  "Execute a command script

    Usage:
@example
#$ SCRIPT
@end example

The script name must be a function previously defined by @samp{#SCRIPT}

"
  (error 'unimplemented))
(define-operator-command addevent (words user plane)
  "Add a GameEvent to a Zone

    Add a GameEvent to a Zone. Must have staff level 4 (DESIGNER) to use
    this command.

    Syntax for use:
@example
#addevent [EVENTNAME]
@end example

 Examples
@example
 #addevent LaserTagGame
 #addevent PropsWeather
 #addevent ShaddowFalls
 #addevent Tootlympics
@end example
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
  "

 Ban a user  permanently. Must have staff level 2  (MODERATOR) to use
 this command.

 Syntax for use
@example
 #ban [REASONCODE] [LOGIN]
@end example

 Examples
@example
 #ban obs.rude pil
@end example

See Also: `OPERATOR-KICK'"

  (error 'unimplemented))
(define-operator-command banhammer (words user plane)
  "Ban an IP address from connecting.

Bans can be listed using @samp{#banhammer #list}

Bans  can  be lifted  using  @samp{#banhammer  #-ip IP-ADDRESS}  (or
hostname)

A  ban  can   be  placed  with  @samp{#banhammer   #+ip  IP-ADDRESS}  or
@samp{#banhammer  #+ip HOSTNAME}  or  @samp{#banhammer #user  USERNAME}.
In the  latter case, the  user's connected IP  address is used.  This is
expected to be the most common usage.

Parameters:  the  first  word  is  a  subcommand;  one  of  @samp{#+ip},
@samp{#-ip},   @samp{#user},    or   @samp{#list}.    For   @samp{#+ip},
@samp{#-ip}, or @samp{#user}, an additional parameter is needed."

  (error 'unimplemented))

(define-operator-command beam (words user plane)
  "
 throws org.json.JSONException,
 PrivilegeRequiredException

 Beam yourself to a different location.

 Syntax for use:
 #beam LATITUDE LONGITUDE [ALTITUDE]

Altitude is optional.

@subsection Changes in 2.0

In Romance  1, this command took  a room moniker as  its sole parameter;
since rooms as such no longer exist, we use latitude and longitude now.

"
  (list 200 (list :|from| "beam"
                  :|latitude| (parse-integer (first words))
                  :|longitude| (parse-integer (second words))
                  :|altitude| (if (< 2 (length words))
                                  (parse-integer (third words))
                                  0))))

(define-operator-command spawnroom (words user plane)
  "

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

Since this is designed  to stress the servers, it can  be called only by
God (Pil)."
  (assert (equal (Toot-name *Toot*) "Pil"))
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
            users low Toots)))

(define-operator-command clearbadge (words user plane)
  "UNIMPLEMENTED

 Clear badges off of the map interface. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #clearbadge [LOGIN] [ROOM].
 Login

 username of a character
 #me for the character you are logged in as

 Room

 room moniker of a room
 #here for the room you are currently in
 #all for every room

 Examples
 #clearbadge snowcone tootSquare
 #clearbadge snowcone #all
 #clearbadge snowcone #here
 #clearbadge #me #all
 #clearbadge #me #here

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
  (error 'unimplemented))

(define-operator-command clearcache (words user plane)
  "Forcibly clear all cachés (MemCacheD)"
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
  "

 Clear a room variable. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #clearvar @@[ROOM] [VARIABLE] [VALUE]
 #clearvar [VARIABLE] [VALUE]

 See op_setvar(String[], AbstractUser, Room) to set a variable.

 Examples
 #clearvar @@tootsSquareWest anim~ropes 2
 #clearvar anim~ropes 2

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 See Also:
 op_setvar(String[], AbstractUser, Room), op_getvar(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command cloneroom (words u plane)
  "
 throws PrivilegeRequiredException

 clone a room

 Parameters:
 words - moniker of the new room
 u - user cloning the room
 r - room to be cloned, in which the user must currently be standing
 Throws:
 PrivilegeRequiredException - if the user doesn't have Designer level privileges, at least
"
  (error 'unimplemented))

(define-operator-command createroom (words user plane)
  "
 throws PrivilegeRequiredException,
 NotReadyException

 WRITEME: Document this metho WRITEMEd brpocock@@star-hope.org

 Parameters:
 words - WRITEME
 u - WRITEME
 room - WRITEME
 Throws:
 PrivilegeRequiredException - WRITEME
 NotReadyException - WRITEME
"
  (error 'unimplemented))

(define-operator-command dbcpinfo (words user plane)
  "Get information from the DBI (database) layer.

Earlier versions of Romance were  Java-based, using the DBCP layer, thus
the name.


 Syntax for use:
 #dbcpinfo

 Examples:
 #dbcpinfo
"
  (with-dbi (:friendly)
    (let ((driver (dbi:connection-driver-type *dbi-connection*)))
      (format nil "Friendly database name: ~a;
DBI connection driver type: ~a (~a::~a);
connection: ~s"
              (dbi:connection-database-name *dbi-connection*)
              driver
              (package-name (symbol-package (class-name (dbi:find-driver driver))))
              (class-name (dbi:find-driver driver))
              (dbi.driver:connection-handle *dbi-connection*)))))

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

 Silently remove the named user from the game by disconnection. Must have staff level 4 (DESIGNER) to use this command.

 Syntax for use
 #dropkick [LOGIN]

 Examples
 #dropkick flappyperry

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
  (error 'unimplemented))

(define-operator-command dumpthreads (words user plane)
  "Dump debugging information including all running threads to the server logs.

Syntax for use:
 #dumpthreads"
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
  "

 Parameters:
 words - see op_testcensor(String[], AbstractUser, Room)
 u - see op_testcensor(String[], AbstractUser, Room)
 room - see op_testcensor(String[], AbstractUser, Room)
 See Also:
 op_testcensor(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(defun json-to-html (json)
  (with-output-to-string (s)
    (doplist (key value json)
        (format s "~%<div><strong>~a</strong>: &nbsp; ~s</div>" key value))))

(define-operator-command finger (words user plane)
  "Finger a user account. Return interesting details in an administrative message.

 Syntax for use:
 #finger TOOT

 Examples:
 #finger mouser
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

 Syntax for use:
 #getconfig PROPERTY

 Examples:
 #getconfig :taskmaster :devel

 "
  (format nil "<pre>~s</pre>"
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
  "Retrieve the current Message Of The Day as a server message."
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
  "UNIMPLEMENTED

 throws NumberFormatException,
 org.json.JSONException,
 AlreadyExistsException

 give a gift

 usage: #give ITEM# USER
 WRITEME

 Parameters:
 words - item# to be given, and recipient
 u - the giver
 r - room in which the giver is standing
 Throws:
 NumberFormatException - if the item# given is not a number
 org.json.JSONException - WRITEME
 AlreadyExistsException - WRITEME
"
  (error 'unimplemented))

(define-operator-command givehead (words user plane)
  "UNIMPLEMENTED
 throws PrivilegeRequiredException

 Give an inventory item to a user. Must have staff level 1 (STAFF) to use this command.

 NOTE: #grant and #givehead are identical.

 Syntax for use
 #givehead [ITEM] [LOGIN]
 #grant [ITEM] [LOGIN]

 Examples
 #givehead 1337 louis
 #grant 1337 louis

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 Throws:
 PrivilegeRequiredException - requires staff level permissions
"
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
  "UNIMPLEMENTED

 throws PrivilegeRequiredException

 Grant an item to a user. See op_givehead(String[], AbstractUser, Room)

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 Throws:
 PrivilegeRequiredException - if the user doesn't have sufficient privileges
"
  (error 'unimplemented))

(define-operator-command headcount (words user plane)
  "UNIMPLEMENTED

 Get headcount information about the running system. Must have staff level 1 (STAFF) to use this command.

 Syntax for use
 #headcount #all
 #headcount #members
 #headcount #room

 Examples
 #headcount #all
 #headcount #members
 #headcount #room

 Parameters:

words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name

u - The user invoking the operator command

room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.

See Also:

 headcount_all(AbstractUser,                               Room)

 headcount_highwater(AbstractUser,                         Room)

 headcount_rooms(AbstractUser,                             Room)

 headcount_members(AbstractUser, Room)
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
  "
 throws NotFoundException

 Kick a user offline for a certain reason

 Kick a user offline for a certain reason. Must have staff level 2 (MODERATOR) to use this command.

 Syntax for use: #kick [REASONCODE] [LOGIN]

 Examples
 #kick obs.rude pil

 Reason Codes:

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
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 Throws:
 NotFoundException - if the warning reason code is not valid
 See Also:
 op_warn(String[], AbstractUser, Room), op_ban(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command king (words user plane)
  "UNIMPLEMENTED

 Apply  a  gift  membership  to   an  user.  Must  have  staff  level
 4 (DESIGNER) to use this command.

 Syntax for use
 #king [DAYS] [LOGIN]

 Examples
 #king 2 flappyperry

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 Throws:
 org.json.JSONException - if something can't be cast into a JSON packet underlying
"
  (error 'unimplemented))

(define-operator-command liftban (words user plane)
  "UNIMPLEMENTED

 throws PrivilegeRequiredException,
 NotFoundException

 Lift the ban upon a user. Must have staff level 2 (MODERATOR) to use
 this command.

 NOTE: In order  to un-ban a user,  you must key in  the literal word
 “yes” as  the third parameter,  and supply the ban  reason as
 the first. This is to avoid accidentally lifting a ban.

 Syntax for use
 #liftban [BANREASON] [USER] yes

 Examples
 #liftban app.mail flappyperry yes

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
 Throws:
 PrivilegeRequiredException - if the user lacks PrivilegeRequiredException
 NotFoundException - if the warning reason code is not valid
"
  (error 'unimplemented))

(define-operator-command loadlists (words user plane)
  "UNIMPLEMENTED

 Reload the censorship lists. Must have staff level 8 (DEVELOPER) to use this command.

 Syntax for use
 #loadlists

 Examples
 #loadlists

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
"
  (error 'unimplemented))

(define-operator-command mem (words user plane)
  "Display some memory usage and other debugging type information as an pop-up message. 

This is an abbreviated version of the output of `ROOM'
 
Syntax for use:
 #mem

 Examples:
 #mem
"
  (format nil "This server is ~a. <pre>~a</pre>"
          (machine-instance)
          (first-paragraph (with-output-to-string (*standard-output*) (room)))))

(define-operator-command metronome (words user plane)
  "Display information  about or  micromanage the metronome. 

 Syntax for use
@verbatim
 #metronome [OPTION]
@end verbatim

 Options

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

 Syntax for use:
 #motd [MESSAGE...]

 Examples:
 #motd I am setting the message of the day

 The message of the day is echoed  to every user as they sign in, before
 they choose a Toot."
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
  "
 throws PrivilegeRequiredException

 Forcibly disconnect everyone in a room.

 Parameters:
 words - the name of the room to be nuked
 u - The user (operator) executing this instruction
 room - The room to be nuked
 Throws:
 PrivilegeRequiredException - WRITEME
"
  (error 'unimplemented))

(define-operator-command parentapproves (words user plane)
  "Signal that a parent approves a user signing in.

Syntax:

#parentapproves TOOT

Example:

#parentapproves Pil

This is only useful if TOOT is a child Toot account has begun to sign in
and requested parent permission.

 "
  (error 'unimplemented))

(define-operator-command ping (words user plane)
  "Ping the  server, to force  a neutral administrative  message reply.
 
 Syntax for use
 #ping

 Examples
 #ping"
  "Pong!")

(define-operator-command place (words user plane)
  "Put a thing or a Place into the game

 Add a  Place to  a room.  This command supports  the basic  types of
 event Places,  and adds them to  the room in the  given WHERE place.
 WHERE can be  a diamond-shaped area around the  operator issuing the
 command  (using  #here, #here-tiny,  or  #here-big),  or can  be  an
 explicitly-issued  polygon  string.  The  event region  ID  will  be
 automatically assigned.

 Usage:
 #place WHERE #item ITEM-NUMBER
 #place WHERE #room MONIKER
 #place WHERE #vitem PAID-ITEM-NUMBER
 #place WHERE #item2 ITEM-NUMBER PAID-ITEM-NUMBER
 #place WHERE #exit MONIKER
 #place WHERE #mini MINIGAME-MONIKER
 #place WHERE #walk
 WHERE := #here | #here-tiny | #here-big | x,y~x,y~x,y~x,y polygon list

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

(define-operator-command rc (words user plane)
  "UNIMPLEMENTED
 
 Run   an   RC  (RunCommands)   script.   Both   the  â€œsystem   run
 commandsâ€  (â€œrunâ€)  method  and   the  â€œnew  zone  run
 commandsâ€ (â€œnewZoneâ€) method will be executed; the
"
  (error 'unimplemented))

(define-operator-command reboot (words user plane)
  "Restart the game server.

 No, really; this  actually kills the game server with  an error exit so
 that it will (hopefully) be restarted by SystemD.

 Syntax for use:
 #reboot

 Examples:
 #reboot"
  (private-admin-message "Bye!" "This server is rebooting in 3 seconds.")
  (sleep 3)
  (sb-ext:quit :unix-status 66))

(define-operator-command reloadconfig (words user plane)
  "Reloads configuration properties.

 Syntax for use:
 #reloadconfig

 Examples:
 #reloadconfig
"
  (let ((info (load-config)))
    (format nil "Reloaded configuration from pathname ~a, truename ~a, on host ~a.
Read at ~a. File write date ~a, author ~a."
            (getf info :path) (getf info :truename) (getf info :host)
            (getf info :read) (getf info :file-write-date) (getf info :author))))

(define-operator-command retire (words user plane)
  "UNIMPLEMENTED

TODO -- evacuate and then restart a server node -- UNIMPLEMENTED

 Forces a  zone to retire.  This will disconnect anyone  currently in
 the zone.  Use #evacuate to  move users  to another zone.  Must have
 staff level 8 (DEVELOPER) to use this command.

 Syntax for use
 #retire [ZONE]
 #retire

 Examples
 #retire dottie
 #retire

 Parameters:

words  - The  command parameter  s (whitespace-delimited  list) provided
after the # command name

u - The user invoking the operator command

room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
"
  (error 'unimplemented))

(define-operator-command run (words u r)
  "Run an arbitrary nullary Lisp function or method

Syntax:

#run FUNCTION

Examples:

#run ws-stats

#run infinity-stats

#run sb-ext quit

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

Usage:

#scotty TOOT LATITUDE LONGITUDE [ALTITUDE] [WORLD]

Altitude is optional and defaults to 0.

World is optional and defaults to CHOR.

@subsection Changes from 1.2 to 2.0

In 1.2, this movesd an user into another room.

 Syntax for use:
 #scotty [LOGIN] [ROOM]

 Examples:
 #scotty mouser tootSquareWest"
  (error 'unimplemented))

(define-operator-command setavatarcolors (words user plane)
  "Sets the  base and extra colors  of a user's avatar.  

 Syntax for use:
 #setavatarcolors [LOGIN] [BASE] [EXTRA]

  The  \"CSS   style\"  uses  a   decimal  triplet  in   the  form
 rgb(R,G,B) (the literal string \"rgb(\" identifies it).

 The \"HTML style\" uses  a # sign followed by either  3 or 6 hex
 characters, in the form #RGB or #RRGGBB.

Basic Toot avatar colors can be passed as named strings.



 Examples:

 #setavatarcolors mouser #000000 #ffffff

 #setavatarcolors mouser rgb(0,0,0) rgb(255,255,255)
"
  (error 'unimplemented))

(define-operator-command setbadge (words user plane)
  "UNIMPLEMENTED

Set the badge on  a map area.

 
 Syntax for use:

#setbadge

 #setbadge BADGE MONIKER

 #setbadge BADGE #here

 #setbadge #me MONIKER

 
 NOTE: Using #setbadge with no  parameters will assume default values
 which are identical to typing #setbadge #me #here

 Examples:

 #setbadge snowcone tootSquareWest

 #setbadge #me tootSquare

 #setbadge snowcone #here

 #setbadge #me #here

 "
  (error 'unimplemented))

(define-operator-command setconfig (words user plane)
  "UNIMPLEMENTED

 Set a config property.

 Syntax for use:
 #setconfing PROPERTY VALUE

PROPERTY is a  sequence of keywords, which must be  delimited by spaces.
Omit the leading : on the keyword names.

 Examples:
 
#setconfig rollbar access-token 1234567890

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
  "Speak in another zone. This is intended for using operator commands in a remote zone, not normal chat messages. Must have staff level 2 (MODERATOR) to use this command.

 Syntax for use:

 #shout [ZONE] [ROOM] [COMMAND...]

 Examples:

 #shout dottie tootSquareWest #wall Hello Everyone

 #shout dottie tootSquare #retire

See modern version `TOOTSVILLE-USER::AT' also

 "
  (error 'unimplemented))

(define-operator-command spawnzone (words user plane)
  "

 Create a new zone. Must have staff level 8 (DEVELOPER) to use this command.

 Syntax for use
 #spawnzone [ZONE]

 Examples
 #spawnzone Cupcake

 Parameters:
 words - The command parameters (whitespace-delimited list) provided after the # command name
 u - The user invoking the operator command
 room - The room in which the user is standing (as a room number). This can be -1 under certain circumstances.
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

WRITEME

 Syntax for use:

 #stfu [LOGIN]

 Examples:

 #stfu flappyperry

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
 
Syntax for use:
 #time

 Examples:
 #time

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

 Syntax for use:
 #v LOGIN MESSAGE...

 Examples:
 #v flappyperry I like to cause trouble in tootsville

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
  "Write to all players

 Sends an pop-up message to everyone currently online.

 Syntax for use:
 #wall [MESSAGE...]

 Examples:
 #wall This message will go to everyone currently on-line.

 Parameters:

words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name

"
  
  (broadcast
   (list :|from| "admin"
         :|status| t
         :|title| "Squawk!"
         :|message| (format nil "~{~a~^ ~}" words))))

(define-operator-command wallops (words user plane)
  "Write to all operators

 Sends an pop-up message to all  staff members in the zone. 

 Syntax for use:
 #wallops [MESSAGE...]

 Examples:
 #wallops This message will go to all other staff members in this zone.

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
  "
 throws NotFoundException

 Warn a user about breaking a rule. Must have staff level 2 (MODERATOR) to use this command.

 Syntax for use: #warn [REASONCODE] [LOGIN]

 Examples
 #warn obs.rude pil

 Reason Codes:

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

u - The user invoking the operator command

room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.

Throws:
 NotFoundException - if the warning reason code is not valid
 See Also:
 op_kick(String[], AbstractUser, Room), op_ban(String[], AbstractUser, Room)
"
  (error 'unimplemented))

(define-operator-command whatis (words user plane)
  "Displays information about an item template. 

 Syntax for use
 #whatis ITEM-TEMPLATE

 Examples
 #whatis 1337

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

 Syntax for use:
 #whereami

 Examples:
 #whereami
"
  (machine-instance))

(define-operator-command whereis (words user plane)
  "

 Find  out in  what what  room a  character is  standing, if  s/he is
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
  "
 throws PrivilegeRequiredException,
 NotFoundException

 Displays a  list of everyone  currently in  a room. Must  have staff
 level 2 (MODERATOR) to use this command.

 Syntax for use
 #who [ROOM]
 #who

 NOTE: Leaving off the ROOM  parameter will default to displaying for
 the room the command was initialized in.

 Examples
 #who tootSquare
 #whereis

 Parameters:

words  - The  command  parameters  (whitespace-delimited list)  provided
after the # command name

u - The user invoking the operator command

room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.

Throws:
 PrivilegeRequiredException - if the user calling isn't a staff member
 NotFoundException - if the chosen room does not exist
"
  (error 'unimplemented))

(define-operator-command whoami (words user plane)
  "Cause  the character  to speak  his/her  name in  the current  room.
 Appears as dialogue in the form:  ``Hello, my name is NAME.'' Must have
 staff level 1 (STAFF) to use this command.

 Syntax for use:
 #whoami

 Examples:
 #whoami
"
  (toot-speak (format nil "Hello, my name is ~a." (Toot-name *Toot*)))
  nil)

(define-operator-command whoareyou (words user plane)
  "Ask  the  server who  it  is.  This  command should  return  version
 information on some of the critical classes used in the game server.
 
 Syntax for use:
 #whoareyou

 Examples:
 #whoareyou

 Parameters:

words  - The  command  parameters  (whitespace-delimited list)  provided
 after the # command name

u - The user invoking the operator command

room  - The  room in  which the  user is  standing (as  a room  number).
This can be -1 under certain circumstances.
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
