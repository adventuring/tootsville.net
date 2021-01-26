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

@subsection Example

@verbatim
#$ do-something-funny
@end verbatim

The script name must be a function previously defined by @code{#SCRIPT};
see `SCRIPT' operator command for details.

"
  (error 'unimplemented))

(define-operator-command addevent (words user plane)
  "Add a GameEvent to a Zone

UNIMPLEMENTED

@subsection Usage

@verbatim
#addevent [EVENTNAME]
@end verbatim

@subsection Examples

@verbatim
 #addevent LaserTagGame
 #addevent PropsWeather
 #addevent ShaddowFalls
 #addevent Tootlympics
@end verbatim

This is not currently implemented.
"
  (error 'unimplemented))

(define-operator-command agent (words user plane)
  "Set the clothing and colors of a robot to match the invoking user.

@subsection Usage

@verbatim
 #agent robot-name
@end verbatim
"
  (error 'unimplemented))

(define-operator-command askme (words user plane)
  "Used to test the question-and-answer subsystem.

@subsection Usage

@verbatim
 #askme
@end verbatim

@subsection 200 OK

Returns a fixed JSON sequence that prompts the user to answer a
meaningless question.

@verbatim
{ title: \"Title Here\",
  label: \"example\",
  label_en_US: \"example\",
  attachUser: (the user name of the invoking user),
  id: \"example/2134§þ=?/x'<>'\\\\\\\",:/blah\",
  msg: \"Because it's really important to me that you are able to hear this question and give me an informed answer, I want to know: “Can you hear me now?”\",
  replies:
  { si: { label: \"Yes\",
          type: \"aff\",
          label_en_US: \"YES\" },
    no: { label: \"No\",
          type: \"neg\",
          label_en_US: \"NO\" },
    maybe: { label: \"Maybe. I'm not really sure. This one is mostly just in here to be a really long answer.\",
             type: \"neu\",
             label_en_US: \"MEBBE\" } } }
@end verbatim
" 
  (error 'unimplemented))

(define-operator-command ban (words user plane)
  "Ban a user persistently (permanently) from the game.

@subsection Usage

@verbatim
 #ban [REASONCODE] [LOGIN]
 #ban #list
@end verbatim

@subsection Examples
@verbatim
 #ban BULLY pil
 #ban #list
@end verbatim

@code{#ban #list} is identical to @code{#kick #list}, for convenience.

The same reason codes and syntax are used for @code{#ban} as for
@code{#kick}, so refer to `TOOTSVILLE-USER::KICK' for reason codes.

Unlike a @code{#kick}, a @code{#ban} remains in effect persistently
--- permanently, unless an operator reverses it."
  
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
  (string-case (first words)
    ("#list" (list-banhammers))
    ("#user" (banhammer-ip-address (user-ip (user-stream (find-Toot-by-name (second words))))))
    ("#+ip" (banhammer-ip-address (second words)))
    ("#-ip" (un-banhammer-ip-address (second words)))))

(defun list-banhammers ()
  (format nil "<ul>~{<li>~a</li>~}</ul>" (hash-table-keys *banhammer*)))

(defun banhammer-ip-address (address)
  (setf (gethash (concatenate 'string "inet:" address) *banhammer*) t)
  (format nil "Banned address inet:~a" address))

(defun un-banhammer-ip-address (address)
  (remhash (concatenate 'string "inet:" address) *banhammer*)
  (format nil "Address ~a can connect" address))

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
                                  0)
                  :|world| (if (< 3 (length words))
                               (string-upcase (fourth words))
                               "CHOR"))))

(define-operator-command spawnroom (words user plane)
  " WRITEME --- Legacy --- useless in 2.0

 Create a new room in the current zone.  Must have staff level
 8 (DEVELOPER) to use this command.

@subsection Usage

@verbatim
 #spawnroom [MONIKER] [TITLE] [SWF]
 #spawnroom [MONIKER] [TITLE]
@end verbatim

 NOTE: Uses tootCastleJoust.swf as default. This can be set after the
 room has been created by setting the 'f' room variable.

@subsection Examples

@verbatim
 #spawnroom tootCastleJoust2 Joust2 tootCastleJoust.swf
 #spawnroom tootCastleJoust2 Joust2
@end verbatim

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

This is not yet implemented for Tootsville V.

@subsection Usage

@verbatim 
#clearbadge [NICKNAME] [SPOT]
#clearbadge #me [SPOT]

#clearbadge #me #here
#clearbadge [NICKNAME] #here

#clearbadge #me #all
#clearbadge [NICKNAME] #all
@end verbatim

@subsection Examples

@verbatim
 #clearbadge snowcone tootSquare
 #clearbadge snowcone #all
 #clearbadge snowcone #here
 #clearbadge #me #all
 #clearbadge #me #here
@end verbatim

@subsection Badges

See `TOOTSVILLE-USER::SETBADGE' for a discussion of the map badges system.
"
  (error 'unimplemented))

(define-operator-command clearcache (words user plane)
  "Forcibly clear all cachés (MemCacheD)

Flush all contents of the MemCacheD server. This may negatively impact
the system's performance.

@subsection Usage

@verbatim
#clearcache
@end verbatim

@subsection Example

@verbatim
#clearcache
@end verbatim
"
  (cl-memcached:mc-flush-all))

(define-operator-command clearevent (words user plane)
  "Clear a GameEvent from a Zone. 

UNIMPLEMENTED

 Usage
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
  "Clear a room variable.

@subsection Usage

@verbatim 
#clearvar @[ROOM] [VARIABLE] [VALUE] 
#clearvar [VARIABLE] [VALUE]
@end verbatim

@subsection Examples

@verbatim
#clearvar @tootsSquareWest anim~ropes 2 
#clearvar anim~ropes 2
@end verbatim

WRITEME"
  (error 'unimplemented))

(define-operator-command cloneroom (words u plane)
  "Clone a room. (no longer supported)

This is no longer supported in Tootsville V.
 
@subsection Usage

@verbatim
#cloneroom NEW-MONIKER
#cloneroom OLD-MONIKER NEW-MONIKER
@end verbatim

@subsection Legacy Operator Command

This command existed in Romance 1.2, but is no longer effective.

"
  (error 'unimplemented))

(define-operator-command createroom (words user plane)
  "Create a new room. (no longer supported)

This is no longer supported in Tootsville V.
 
@subsection Usage

@verbatim
#createroom NEW-MONIKER
@end verbatim

@subsection Legacy Operator Command

This command existed in Romance 1.2, but is no longer effective.

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
  "Force a character to wear a specific clothing item.

UNIMPLEMENTED

@subsection Usage
@verbatim
 #dress [LOGIN] [ITEM]
 #dress [LOGIN] [ITEM] [COLOUR]
@end verbatim

@subsection Examples
@verbatim
 #dress flappyperry 1337
@end verbatim

"
  (error 'unimplemented)
  )

(define-operator-command drop (words user plane)
  "Drop an item

UNIMPLEMENTED

Find an item in your inventory based upon the item ID # and drop it (to the world).

Usage: #drop ITEM-TEMPLATE-ID
"
  (error 'unimplemented))

(define-operator-command dropkick (words user plane)
  "Silently disconnect a user

UNIMPLEMENTED
 
Silently remove the named user from the game by disconnection. Must have
staff level 4 (DESIGNER) to use this command.

@subsection Usage
@verbatim
 #dropkick [LOGIN]
@end verbatim

@subsection Example
@verbatim
 #dropkick flappyperry
@end verbatim
"
  (error 'unimplemented))

(define-operator-command dumpthreads (words user plane)
  "Dump debugging information including all running threads to the server logs.

@subsection Usage

@verbatim 
#dumpthreads
,dumpthreads
@end verbatim

@subsection Example

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
  "Temporary test routine for testing pathfinders on users

UNIMPLEMENTED

@subsection Usage
@verbatim
#enablepathfinder (true|false)
@end verbatim

@subsection Examples
@verbatim
#enablepathfinder true
#enablepathfinder false
@end verbatim"
  (string-case (first words)
    ("true" (error 'unimplemented))
    ("false" "Pathfinder is not enabled.")
    (t (error 'unimplemented))))

(define-operator-command evacuate (words user plane)
  "Evacuate all users from the current Zone to another Zone.

UNIMPLEMENTED

Evacuate all users from your current server into another server. Will
error if the server specified does not exist in the cluster.

@subsection Usage
@verbatim
 #evacuate [SERVER]
@end verbatim

@subsection Example
@verbatim
 #evacuate game2
@end verbatim

"
  (error 'unimplemented))

(define-operator-command filter (words user plane)
  "Test censorship rules against words or phrases

UNIMPLEMENTED

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
  "Finger a user account. 

Return interesting details in an administrative message.

@subsection Usage

@verbatim
#finger TOOT
@end verbatim

@subsection Examples

@example
#finger mouser
#finger shade
@end example

@subsection Changes since 1.2

The format of the response has changed slightly, but is similar.

@subsection Response

@quotation 

Mouser is a Toot with base color red, pad color black, and pattern
black spots. This is an adult's account. (sensitive player) (patron)
The user has 2,130 peanuts, 100 fairy dust, and was last active
@@2021-01-26T04:02:55.600079Z (Earth time; two minutes ago; Blanksday,
the eleventh of Procavia, 153) The player owning Mouser is John
Doe (jdoe@@gmail.com). Toot: 5112AE4B-0F8D-4823-AFD7-EC4119001D04,
player: AC14ABCF-518D-4DC5-B783-3A4DFE4838B2

@end quotation
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
  "Historically, this flushed the database write caché.

This command is not currently implemented. It is a no-op.

@subsection Usage

@verbatim
#flush
@end verbatim

@subsection Changes from 1.2

This command is not effective in Romance II. In Romance 1, it was used
to flush the database write caché, which at times could be several
minutes behind the database's on-disk version. This should no longer
be a concern in 2020 and beyond, but the command is retained as a
no-op. It might be re-activated in future as needed.
"
  "Thanks, but I don't have a write caché to flush.")

(define-operator-command game (words user plane)
  "Send a command to the operator command interpreter for a running game.

UNIMPLEMENTED

Send a command into the operator command interpreter for a running
game (if that game provides one)

@subsection Usage
@verbatim
#game gameClass (strings...)
@end verbatim

"
  (error 'unimplemented))

(define-operator-command getconfig (words user plane)
  "Reads a configuration key. 

All WORDS are expected to be the keywords on the path to the config
value.

@subsection Usage

@verbatim
#getconfig PROPERTY
#getconfig PROP1 PROP2 [...]
@end verbatim

@subsection Example
@verbatim
#getconfig taskmaster devel
@end verbatim

Returns the value of the selected configuration property as a
string. If the selected property is a key with multiple values (as a
property list --- plist) associated with it, returns the entire
plist (and possibly, nested plists).
 "
  (format nil "<pre>~/HTML/</pre>"
          (apply #'config (mapcar (compose #'make-keyword #'string-upcase) words))))

(define-operator-command getevents (words user plane)
  "List GameEvents in your current Zone

UNIMPLEMENTED

Must have staff level 4 (DESIGNER) to use this command.

@subsection Usage
@verbatim
 #getevents
@end verbatim

@subsection Example
@verbatim
 #getevents
@end verbatim

@subsection See Also

See also `TOOTSVILLE-USER::ADDEVENT'
"
  (error 'unimplemented))

(define-operator-command getmotd (words user plane)
  "Retrieve the current Message Of The Day as a server message.

@subsection Usage

@verbatim
#getmotd
@end verbatim

@subsection Example
@example
#getmotd
@end example"
  *motd*)

(define-operator-command getschedule (words user plane)
  "Get schedule

Gets all scheduled events in the metronome system, with their
schedules.
"
  (format nil "<section><h2>Schedule</h2>~{<p>~/HTML/</p>~}</section>"
          (metronome-idle-tasks)))

(define-operator-command getschedulefor (words user plane)
  "Get scheduled events for a particular class (scheduled by that class)

UNIMPLEMENTED 

"
  (error 'unimplemented))

(define-operator-command getuvar (words user plane)
  "Get a user variable.

UNIMPLEMENTED

  Must have staff level 4 (DESIGNER) to use this command.

@subsection Usage
@verbatim
 #getuvar [LOGIN] [VARIABLE]
@end verbatim

 user name of a character
 #me for the user you are logged in as

@subsection Examples
@verbatim
 #getuvar mouser d
 #getuvar #me d
@end verbatim

@subsection See Also

See also `TOOTSVILLE-USER::SETUVAR', `TOOTSVILLE-USER::GETUVARS'
"
  (error 'unimplemented))

(define-operator-command getuvars (words user plane)
  "Get all user variables for a given user.

UNIMPLEMENTED

@subsection Usage
@verbatim
 #getuvars [LOGIN]
 #getuvars #me
@end verbatim


@subsection Examples
@verbatim
 #getuvars mouser
 #getuvars #me
@end verbatim

@subsection See Also

See also `TOOTSVILLE-USER::SETUVAR', `TOOTSVILLE-USER::GETUVAR'
"
  (error 'unimplemented))

(define-operator-command getvar (words user plane)
  "Get a room variable.

@subsection Usage
@verbatim
 #getvar @[ROOM] [VARIABLE]
 #getvar [VARIABLE]
@end verbatim

@subsection Examples
@verbatim
 #getvar @tootsSquareWest anim~ropes
 #getvar anim~ropes
@end verbatim

@subsection See Also

See also `TOOTSVILLE-USER::SETVAR', `TOOTSVILLE-USER::CLEARVAR',
`TOOTSVILLE-USER::GETVARS'
"
  (error 'unimplemented))

(define-operator-command getvars (words user plane)
  "Get all room variables.

@subsection Usage
@verbatim
 #getvars [ROOM]
 #getvars
@end verbatim

@subsection Examples
@verbatim
 #getvars tootsSquare
 #getvars
@end verbatim

@subsection See Also

See also `TOOTSVILLE-USER::SETVAR', `TOOTSVILLE-USER::CLEARVAR',
`TOOTSVILLE-USER::GETVAR'
"
  (error 'unimplemented))

(define-operator-command give (words u plane)
  "Give an item as a gift to another user.

@subsection Usage

@verbatim
#give ITEM USER
@end verbatim

@subsection Example

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

@subsection Example

@example
#givehead 1337 catvlle
@end example
 
This creates  a new item  from the  item template number  indicated, and
equips it on the recipient. To give  a gift from your own inventory, see
`GIVE'. To grant a new item without equipping it, see `GRANT'."
  (error 'unimplemented))

(define-operator-command goto (words user plane)
  "WRITEME

UNIMPLEMENTED"
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

@subsection Example

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

@subsection Examples

@example
#headcount #all
#headcount #members
#headcount #room
#headcount #highwater
@end example

@subsection Headcount All

Gives the total number of users online now.

@subsection Headcount Members

Gives the total number of patron users or builder Toot users online now.

@subsection Headcount Room

Gives the total number of users who are within ``earshot'' of the
person invoking this command.

@subsection Headcount Highwater

Gives the high-water mark of the maximum number of simultaneous users
who have been online since the last boot.
"
  (string-case (first words)
    ("#all" (error 'unimplemented))
    ("#members" (error 'unimplemented))
    ("#room" (error 'unimplemented))
    ("#highwater" (error 'unimplemented))))

(define-operator-command inv (words user plane)
  "Get a user's inventory

Get inventory  items for  a particular user.  By default,  this will
bring up  only the active  items --- e.g. clothing being  worn, and
so forth.

@subsection Usage

To get all active inventory for an user: @code{#inv LOGIN}

To get  all inventory for an  user, active or inactive  (this may be
very long!): @code{ #inv LOGIN #all }

To get inventory of a particular type, active or inactive: @code{#inv
LOGIN #type TYPE }

The type strings accepted are those accepted by
`INFINITY-GET-INVENTORY-BY-TYPE'; this means that both the
$SPECIFIC-TYPE and TYPE-SET-NAME forms are accepted. The list of
specific types might include e.g. $Hair, and a type-set-name might be
something like clothing.  The set of available type-set-names is
specified in the configuration file.

"
  (error 'UNIMPLEMENTED))

(define-operator-command kick (words user plane)
  "Kick a user offline for a certain reason.
  
@subsection Usage

@verbatim
#kick [REASONCODE] [LOGIN]
@end verbatim

Kick LOGIN offline for REASONCODE

@verbatim
#kick #list
@end verbatim

List reason codes.

@subsection Example

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
  "Upgrade a user account

@subsection Usage
@verbatim
 #king [DAYS] [LOGIN]
@end verbatim

@subsection Example
@verbatim
 #king 2 flappyperry
@end verbatim

"
  (error 'unimplemented))

(define-operator-command liftban (words user plane)
  "Lift the ban upon a user.

@subsection Usage

@verbatim
#liftban REASON USER yes
@end verbatim

NOTE: In order to un-ban a user, you must key in the literal word
@code{yes} as the third parameter, and supply the ban reason as the
first. This is to avoid accidentally lifting a ban.

@subsection Example

@verbatim
#liftban CHEAT silly-biscuits yes
@end verbatim

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

This is an abbreviated version of the output of `ROOM' on the server.
 
@subsection Usage

@verbatim
#mem
@end verbatim

@subsection Example

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

Note that the output of `ROOM' can vary wildly depending on the
compiler used; the above is from a build of Tootsville compiled under
SBCL, which is the expected environment, but there is no guarantee
that this will not change in future.

@subsection Changes from 1.2

In Romance 1, we were running in a Java Virtual Machine (JVM), so the
output of @code{mem} was quite differently formatted.
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

@subsection Examples
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
     "Usage: #metronome [OPTION] where [OPTION] is one of: \
#help #rate #last #start #stop #restart #tick #list, or #cancel NAME")
    ("#rate" "The metronome runs every second (1000ms)")
    ("#last"
     (let ((last (1- *metronome-next-tick*)))
       (format nil
               "The last metronome tick was at ~d — ~:d second~:p ago"
               last (- (get-universal-time) last))))
    ("#start" (format nil "Starting: ~/HTML/"
                      (start-game-metronome)))
    ("#stop" (format nil "Stopping: ~/HTML/"
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
                      (format nil "Removing ~/HTML/ from metronome: ~/HTML/"
                              (metronome-task-name (first potentials))
                              (metronome-remove (first potentials))))
                     (t 
                      (format nil "There are ~:d task~:p matching ~a. \
Use #metronome #list to enumerate tasks."
                              (length potentials) task-name))))))))

(define-operator-command motd (words user plane)
  "Set the  message of the day.

@subsection Usage

@verbatim
#motd The new message of the day, literally.
@end verbatim

 
@subsection Example

@example
 #motd Don't forget that Hallowe'en in Tootsville is on the 30th --- get your costumes ready!
@end example

The message of the day is echoed  to every user as they sign in, before
they choose a Toot. It is @emph{not} echoed to children.

@subsection Changes from 1.2

In Romance II, we do not display the MotD to children, but their
parents will see it when approving their sign-on."
  (when (not (emptyp words))
    (setf *motd* (format nil "~{~a~^ ~}" words))))

(define-operator-command mute (words user plane)
  "Mute a user or area. 

This is a simpler form of `TOOTSVILLE-USER:STFU' that does not accept a duration.

@example
#mute user-name
@end example

The player muted will receive an admin message:

@example
You are no longer allowed to speak in Tootsville.
@end example

The invoking user will receive a confirmation.

@example
USER-NAME is no longer allower to speak in Tootsvillle.
@end example

If the user cannot be found,

@example
Can't find user “USER-NAME”
@end example
WRITEME

UNIMPLEMENTED

 See Also: `TOOTSVILLE-USER::STFU'
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

@subsection Example

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

@subsection Example

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

@subsection Examples

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
  "Purge pending physics interactions. 

UNIMPLEMENTED."
  (error 'unimplemented))

(define-operator-command push (words user plane)
  "WRITEME

 UNIMPLEMENTED"
  (error 'unimplemented))

(define-operator-command put (words user plane)
  "WRITEME

UNIMPLEMENTED
"
  (error 'unimplemented))

(define-operator-command rc (words user plane)
  "Run an RC (Run Commands) script.

UNIMPLEMENTED

Run an RC (RunCommands) script.  Both the “system run
commands” (“run”) method and the “new zone run commands” (“newZone”)
method will be executed; the

@subsection Usage

@verbatim
#rc
@end verbatim

@subsection Example

@verbatim
#rc
@end verbatim

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

@subsection Example

@verbatim
#reboot
@end verbatim

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

@subsection Example

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
 
@subsection Examples
@verbatim
#retire game3.test.tootsville.org
#retire
@end verbatim
"
  (error 'unimplemented))

(define-operator-command run (words u r)
  "Run an arbitrary nullary Lisp function or method

@subsection USave

@verbatim
#run FUNCTION
#run PACKAGE FUNCTION
@end verbatim

@subsection Examples

@verbatim
#run ws-stats
#run infinity-stats
#run sb-ext quit
@end verbatim

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
  "Save room variables.

No longer used.

@subsection Legacy Usage (1.2)

In Romance 1.2, this would save the effective room variables in a room
to the database permanently.  Now, all things that room variables used
to represent are already persisted to the database.
"
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

@subsection Examples

@verbatim
#scotty mouser TootSquareWest
#scotty mouser -1 0 0 CHOR
#scotty mouser -1 0
@end verbatim

@subsection Changes from 1.2 to 2.0

In 1.2, this moved an user into another room.

 Usage:
 #scotty [LOGIN] [ROOM]

 Examples:
 #scotty mouser tootSquareWest"
  (check-type words list)
  (let ((latitude (parse-integer (second words)))
        (longitude (parse-integer (third words)))
        (altitude (if (< 2 (length words))
                                  (parse-integer (third words))
                                  0))
        (world (if (< 3 (length words))
                               (string-upcase (fourth words))
                               "CHOR")))
    (unicast (list :|from| "beam"
                   :|latitude| latitude
                   :|longitude| longitude
                   :|altitude| altitude
                   :|world| world)
             (find-Toot-by-name (first words)))
    (list 200
          (format nil "Beamed ~a to (~:d, ~:d, ~:d) in ~a"
                  (first words) latitude longitude altitude world))))

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

@subsection Examples

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

@subsection Examples

@verbatim
#setbadge snowcone TootSquareWest
#setbadge #me TootSquare
#setbadge snowcone #here
#setbadge #me #here
@end verbatim

 "
  (error 'unimplemented))

(define-operator-command setconfig (words user plane)
  "Set a config property.

@subsection Usage
 
@verbatim
#setconfig PROPERTY VALUE
#setconfig PROP1 PROP2 VALUE
@end verbatim

PROPERTY is a sequence of keywords, which must be delimited by spaces.
Omit the leading : on the keyword names.

@subsection Example

@verbatim
#setconfig rollbar access-token 1234567890
@end verbatim

Changes  made   with  this   command  are   only  effective   until  the
configuration file is  reloaded. See `TOOTSVILLE-USER::RELOADCONFIG' and
`LOAD-CONFIG'.
"
  (error 'unimplemented))

(define-operator-command setstafflevel (words user plane)
  "Set the staff level for a user

UNIMPLEMENTED

   WRITEME
"
  (error 'unimplemented))

(define-operator-command setuvar (words user plane)
  "Set a user variable.

UNIMPLEMENTED

 Set a user variable. Must have staff level 4 (DESIGNER) to use this command.

@subsection Usage
@verbatim
 #setuvar [@@LOGIN] VARIABLE [=] VALUE...
 #setuvar @LOGIN VARIABLE=VALUE
 #setuvar @LOGIN VARIABLE = VALUE
 #setuvar @LOGIN VARIABLE VALUE
 #setuvar VARIABLE=VALUE
 #setuvar VARIABLE = VALUE
 #setuvar VARIABLE VALUE
@end verbatim

NOTE: Using #setuvar without an @@[LOGIN] parameter will apply the changes
to the user issuing the command.

@subsection Example

@verbatim
 #setuvar @mouser d = 254~376~254~376~SE~1267735566759
 #setuvar d 254~376~254~376~SE~1267735566759
@end verbatim

See `INFINITY-SET-USER-VAR' for a discussion of supported user
variables in Romance II.
"
  (error 'unimplemented))

(define-operator-command setvar (words user plane)
  "Set a room variable.

UNIMPLEMENTED.

This used to be used to set Room Variables, which were the main way
that the game design worked in Tootsville IV.  This was largely
automated through Eric Feiling's ``Zookeeper'' application.

In Tootsville V, however, room variables are a reflection of the
underlying database structures and are automatically generated as
needed; there is not currently a way to backwards-supply the
variables' data.

@subsection Description from Romance 1.2

Set a room variable. Must have staff level 4 (DESIGNER) to use this
command.

Usage
@verbatim
#setvar #replace [@@ROOM] VARIABLE FIND REPLACE
#setvar [@@ROOM] VARIABLE VALUE...
@end verbatim

@b{WARNING: SETTING ROOM VARIABLES TO INVALID VALUES CAN CAUSE
UNEXPECTED RESULTS. DOUBLE CHECK ALL VALUES BEING SET FOR
CORRECTNESS.}

Use @code{#replace} to change a room variable from one value to another.

@subsection Examples

@verbatim
 #setvar @@tootsSquareWest anim~ropes 2

 #setvar anim~ropes 2
@end verbatim
"
  (error 'unimplemented))

(define-operator-command shanghai (words user plane)
  "Force a client into a different room and zone

UNIMPLEMENTED

WRITEME"
  (error 'unimplemented))

(define-operator-command shout (words user plane)
  "Speak in another zone.

This is intended for using operator commands in a remote zone, not
normal chat messages.

Since there are no longer zones, this command is not currently
supported. The command name may be re-used for sending commands to a
different server in future.

@subsection Usage

@verbatim
#shout [ZONE] [ROOM] [COMMAND...]
@end verbatim

@subsection Examples

@verbatim
#shout dottie tootSquareWest #wall Hello Everyone

#shout dottie tootSquare #retire
@end verbatim

See modern version `TOOTSVILLE-USER::AT' also

 "
  (error 'unimplemented))

(define-operator-command spawnzone (words user plane)
  "Create a new zone.

@subsection  Usage
@verbatim
  #spawnzone [ZONE]
@end verbatim

@subsection  Examples
@verbatim
  #spawnzone Cupcake
@end verbatim

WRITEME
"
  (error 'unimplemented))

(define-operator-command speak (words user plane)
  "Allows a user to speak who had previously been muted.

See `TOOTSVILLE-USER::MUTE', `TOOTSVILLE-USER::STFU' for ways to mute
a character.

@subsection  Usage
@verbatim
  #speak [LOGIN]
@end verbatim

@subsection  Examples
@verbatim
  #speak flappyperry
@end verbatim
  "
  (error 'unimplemented))

(define-operator-command stfu (words user plane)
  "Silences (mutes) a user.

@subsection Usage

@verbatim
#stfu TOOT
#stfu TOOT MINUTES
@end verbatim

@subsection Example

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

If no time limit is given, it is effective for 24 Earth hours (1,440
Earth minutes).

See also: `TOOTSVILLE-USER:MUTE' for a more direct form that does not
have a fixed duration.
  "
  (error 'unimplemented))

(define-operator-command testcensor (words user plane)
  "Test a message with the censor, displays the filter result.

UNIMPLEMENTED.

@subsection  Usage

@verbatim
  #testcensor [MESSAGE]
@end verbatim

@subsection  Examples

@verbatim
  #testcensor This message will be filtered and the result will be displayed.
@end verbatim

  "
  (error 'unimplemented))

(define-operator-command time (words user plane)
  "Displays a message  with the current server time.
  
@subsection Usage

@verbatim
#time
@end verbatim

@subsection Example

@example
#time
@end example

@subsection Example Reply

@example 

Now it is 2021-01-26T00:35:11.341489Z (Universal: 3,820,610,111; Unix:
1,611,621,311). In Chœrogryllum, it is 0:35:11 on Blanksday, the
eleventh of Procavia, 153

@end example

@subsection Changes from 1.2

The output format has changed. The old version only displayed the Unix
time in seconds, without commas; the decoded date and time, Universal
time code, and Chœrogryllum date and time are new.

  "
  (let ((universal (get-universal-time)))
    (multiple-value-bind (second minute hour) (chœrogryllum:decode*-universal-time universal)
      (format nil "Now it is ~a (Universal: ~:d; Unix: ~:d). In Chœrogryllum, it is ~
~d:~2,'0d:~2,'0d on ~a"
              (now) universal (- universal +unix-time-in-universal+)
              hour minute second (chœrogryllum:date-string universal)))))

(define-operator-command unbuild (words user plane)
  "Destroy a room
 
UNIMPLEMENTED. 

 Destroys a room. Must have staff level 8 (DEVELOPER) to use this command.

@subsection Usage
@verbatim
 #unbuild ROOM
@end verbatim

@subsection Example
@verbatim
 #unbuild tootUniversity
@end verbatim

"
  (error 'unimplemented))

(define-operator-command v (words user plane)
  "Forces a user to say a message.

Mnemonic: Ventriloquism

@subsection Usage

@verbatim
#v LOGIN MESSAGE...
@end verbatim

@subsection Example

@example
#v mayor-louis I like to cause trouble in Tootsville
@end example

@subsection See also

See `INFINITY-SPEAK'

@subsection Changes in 2.0

This no longer allows ventriloquism of operator commands &c.
"
  (Toot-speak (join #\Space (rest words)) 
              :Toot (find-record 'Toot :name (first words))))

(define-operator-command verbosebugs (words u plane)
  "Set verbose bug backtrace reporting on or off.

UNIMPLEMENTED.

@subsection Usage

@verbatim
#verbosebugs true
#verbosebugs false
@end verbatim

@subsection Impact

When verbose bug reporting is enabled, the user requesting it will
receive stack backtraces from unhandled errors as admin messages.

"
  (error 'unimplemented))

(define-operator-command wall (words user plane)
  "Write to all players.

Sends an admin (parrot) pop-up message to everyone currently online.

@subsection Usage

@verbatim
#wall MESSAGE...
@end verbatim

@subsection Example

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

@subsection Example

@example
#wallops This message will go to all other staff members in this zone.
@end example

 "
  (error 'unimplemented))

(define-operator-command wallzones (words user plane)
  "Write to all zones.

This is now the same as `TOOTSVILLE-USER::WALL', qv.

@subsection Instructions from Romance 1.2

 Sends an  pop-up message to  all everyone  in every zone.  Must have
 staff level 8 (DEVELOPER) to use this command.

 Usage
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
  (tootsville-user::wall words user plane))

(define-operator-command warn (words user plane)
  "Warn a user about breaking a rule.

@subsection Usage

 Usage: #warn [REASONCODE] [LOGIN]

@subsection Example

@example
 #warn BULLY Pil
@end example

@subsection Reason Codes

See `KICK' for the current list

"
  (error 'unimplemented))

(define-operator-command whatis (words user plane)
  "Displays information about an item template. 

@subsection Usage

@verbatim
#whatis ITEM-TEMPLATE
@end verbatim

@subsection Example

@verbatim
#whatis 1337
@end verbatim

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

@subsection Example

@verbatim
#whereami
@end verbatim

The  response admin  message is  simply the  machine name  to which  you
are connected.
"
  (machine-instance))

(define-operator-command whereis (words user plane)
  "Find  out in  what what  room a  character is  standing, if  s/he is
 logged in at the moment. Must  have staff level 2 (MODERATOR) to use
 this command.

@subsection Usage
@verbatim
 #whereis [LOGIN]
 #whereis #everyone
 #whereis @[ROOM]
@end verbatim

User Name of a specific user;
#everyone for a the location of every user in the zone;
@@[ROOM] for the location of every user in the specified room.

@subsection Examples
@verbatim
 #whereis snowcone
 #whereis #everyone
 #whereis @tootSquare
@end verbatim

"
  (error 'unimplemented))

(define-operator-command who (words user plane)
  "Displays a  list of everyone  currently in  a room.

@subsection Usage
@verbatim
 #who [ROOM]
 #who
@end verbatim

NOTE: Leaving off the ROOM parameter will default to displaying for
the room the command was initialized in.

@subsection Examples
@verbatim
 #who tootSquare
 #who
@end verbatim
"
  (error 'unimplemented))

(define-operator-command whoami (words user plane)
  "Cause the character to speak his/her name in the current room.

Appears as dialogue in the form: ``Hello, my name is NAME.''

Note that the response is public speech; everyone in the room will see it.

@subsection Usage

@verbatim
#whoami
@end verbatim

Note that the response is public speech; everyone in the room will see it.

@subsection Example

@verbatim
#whoami
@end verbatim

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

@subsection Example

@verbatim
#whoareyou
@end verbatim
 
@subsection Example Response

@example
This server is Inktomi, a X86-64  Intel(R) Core(TM) i7 CPU 860 @ 2.80GHz
running    Linux   5.6.8-300.fc32.x86_64    with   SBCL    2.0.1-1.fc32.
Quicklisp    dist   version    2020-04-27;   Ultralisp    dist   version
20200501011006; Tootsville version 0.6.4
@end example

@subsection Changes from 1.2

The format of the response is different, but the purpose of the
command is the same.
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
  "Set the visual Zoom level of a room.

UNIMPLEMENTED

@subsection Changes from 1.2

In Tootsville IV, rooms could have a different ``zoom level'' setting
the relative size of the display. This is no longer supported; in
Tootsville V, the world is a continuous 3D environment.
"
  (error 'unimplemented))
