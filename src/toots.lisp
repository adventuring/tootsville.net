;;;; -*- lisp -*-
;;;
;;;; src/toots.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018-2020  The
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

(defvar *Toot* nil
  "The Toot that the active user, is currectly using.")

;;; Toot character data.

(defun find-Toot-by-name (Toot-name)
  (check-type Toot-name Toot-name)
  (find-record 'Toot :name Toot-name))

(defun Toot-childp (Toot)
  (and (not (emptyp (Toot-child-code Toot))) t))

(defun inventory-item-equipped-p (item)
  "Is the inventory item equipped at all?"
  (ecase (inventory-item-equipped item)
    ((:Y :A) t)
    ((:N :nil) nil)))

(defun Toot-peanuts (Toot)
  "How many magic peanuts does TOOT have?"
  ;; TODO Toot-peanuts
  (if (person-is-patron-p (find-reference Toot :player))
      1000
      100))

(defun Toot-fairy-dust (Toot)
  "How much fairy dust does TOOT have?"
  ;; TODO Toot-fairy-dust
  (if (person-is-patron-p (find-reference Toot :player))
      1000
      0))

(defun Toot-presentation-name (Toot)
  "The form of the TOOT's name for display in the UI as an avatar label.

This  is  usually  the  same  as `TOOT-NAME',  except  for  children  or
sensitive players, in which case it will have a black diamond ◆ prefixed
to it."
  (if (or (Toot-childp Toot)
          (person-sensitivep
           (find-reference Toot :player)))
      (format nil "◆~a" (Toot-name Toot))
      (Toot-name Toot)))

(defun Toot-clothes+pattern (Toot)
  "The clothes (including Pivitz) and pattern that TOOT is wearing."
  ;; TODO Toot-clothes+pattern
  (list 0 (Toot-pattern Toot)
        ))

(defun Toot-equipped-item (Toot)
  ;; TODO Toot-equipped-item
  nil)

(defun Toot-chat-foreground-color (Toot)
  "The foreground (text) color of a Toot's speech balloon in normal speech.

Shouting and whispering should alter this color appropriately.

Obtained via `TOOT-INFO'.

Always black at present (2.0).  This  should  not  be trusted  to  be
a constant; it should be updated in a later release."
  (declare (ignore Toot))
  (color24-rgb 0 0 0))

(defun ensure-Toot (Toot)
  (etypecase Toot
    (Toot Toot)
    (string (find-record 'Toot :name Toot))
    (uuid:uuid (find-record 'Toot :uuid Toot))))

(defun Toot-chat-background-color (Toot)
  "The background color of a Toot's speech balloon in normal speech.

Shouting and whispering should alter this color appropriately.

Obtained via `TOOT-INFO'.

Always  white  at present  (2.0).  This  should  not  be trusted  to  be
a constant; it should be updated in a later release."
  (declare (ignore Toot))
  (color24-rgb #xff #xff #xff))

(defun Toot-info (Toot &optional (privatep
                                  (and *user*
                                       (uuid:uuid= (person-uuid *user*)
                                                   (Toot-player Toot)))))
  "Returns a JSON-compatible structure which describes TOOT.

If PRIVATEP,  then private  information (normally  only visible  to that
Toot's user) is returned; otherwise,  private information is dummied out
or absent.

This data is returned by various functions, including `INFINITY-FINGER' or
`INFINITY-WARDROBE'.

@subsection Data Structure

@table @code
@item name
The name of the Toot character. See `TOOT-NAME'
@item userName
Also the name of the Toot character, in the form in which it should appear on
an avatar label. Notably, this means that child or sensitive users will have a
black diamond prefixed to the name.
See `TOOT-PRESENTATION-NAME'
@item avatar
The base filename of the avatar. The actual URL for the avatar model will
always be
@code{https://jumbo.tootsville.org/Assets/Avatars/5/}@i{avatar}@code{.babylon}.
See `TOOT-AVATAR' to obtain the avatar's ID, and `AVATAR-MONIKER' to obtain
the name from that ID.
@item chatFG
The foreground (text) color of this character's chat messages.
See `TOOT-CHAT-FOREGROUND-COLOR'.
@item chatBG
The background color of the speech balloons behind the character's chat
messages.
See `TOOT-CHAT-BACKGROUND-COLOR'.
@item avatarClass
This is a legacy object which describes the avatar in play, in theory.
It has the following attributes: @code{id}, the unique ID for the avatar;
@code{title}, always the same as @code{avatar} URL base name;
 @code{filename}, also the same;
@code{forFree}, always @code{true}; @code{forPaid}, always @code{false}.
@item avatarClass_B, baseColor
The base color for the avatar's skin. Sent as two identical values.
See `TOOT-BASE-COLOR'
@item avatarClass_P, patternColor
The color for the avatars's pattern, if any. Sent as two identical values.
See `TOOT-PATTERN-COLOR'
@item avatarClass_E, padColor
The color for the avatar's pad or ``extra'' color.
Sent as two identical values.
See `TOOT-PAD-COLOR'
@item format
Always the same as @code{avatar} now.
@item colors
The list of base, pattern, and extra color, a third time, as an array-like
object; keys are @code{0} for base color, @code{1} for pad color,
and @code{2} for pattern color.
@item inRoom
No longer returned; always reads exactly ``@code{@@Tootsville}''
@item vars
No longer returned; always nil.
@item clothes
The clothing currently being worn by the character.
For legacy reasons, the character's pattern is repeated here.
Pivitz are considered clothes.
See `TOOT-CLOTHES+PATTERN'
@item pattern
The name of the pattern of the avatar, if any.
See `TOOT-PATTERN'
@item gameItem
The item currently held in the character's @code{TRUNK} or @code{HAND} slot,
as appropriate to the avatar model, if any.
See `TOOT-EQUIPPED-ITEM'
@item uuid, id
The Toot character's UUID.
See `TOOT-UUID'
@item equip
If this is the requestor's Toot, a set of all inventory (equipment) as per
 `TOOT-ITEM-INFO'
@item childP
True if the Toot represents a child player.
See `TOOT-CHILDP'
@item childCode
Only available to the user owning the Toot, this is the code to log in as the
child Toot.
See `TOOT-CHILD-CODE'
@item sensitiveP
True if the Toot represents a sensitive user or a child.
See `PERSON-SENSITIVEP'
@item scaling
Scaling of the avatar in each of x, y, and z dimensions. See
 `TOOT-AVATAR-SCALE-X', `TOOT-AVATAR-SCALE-Y',
 and  `TOOT-AVATAR-SCALE-Z'
@end table

@subsection Changes from 1.0 to 1.1

The @code{avatarClass} object used to have fields @code{s}, which is the
same   as   @code{title};   @code{forVIT},   which  is   the   same   as
@code{forPaid};  and   @code{avatarClassID},  which   is  the   same  as
@code{id}. The renamed fields were supported  under both names in 1.1 or
1.2  based  on   the  setting  of  the   global  configuration  variable
@code{org.starhope.appius.events.format1.0}.

@subsection Changes from 1.1 to 1.2

Added @code{scaling} for ``Magic Toots.''

@subsection Changes from 1.2 to 2.0

@itemize
@item
Added @code{name}
@item
Dropped backwards compatibility with the @code{avatarClass} object from 1.0
@item
@code{id} now returns a UUID, not a fixnum integer.
@item
Avatars are now Babylon 3D models, not Flash objects, and are retrieved from a
different URL pattern.
@item
Prepend black diamonds to @code{userName} for children or sensitive users.
@item
Always returns white and black for @code{chatFG} and @code{chatBG}, as
``Magic Toot'' colors are not currently supported in 2.0; they may return in
2.1 or later.
@item
Added @code{baseColor}, @code{patternColor}, and @code{padColor} names in
parallel to existing, now deprecated, @code{colors} values.
@item
The  @code{avatarClass_B,P,E} values, which used to reflect default colors for
an avatar model, are now just the Toot's current colors.
@item
Added @code{uuid}, @code{childP},  @code{sensitiveP},
and @code{lastSeen}
@item
When the requestor owns this Toot, added @code{note},
@code{childCode}, @code{peanuts}, @code{fairyDust},
@item
@code{inRoom} always returns ``@@Tootsville''.
@item
@code{vars} always returns nil.
@end itemize

See also Deprecation section below.

@subsection Deprecation

The following  elements are deprecated and  will be removed in  a future
revision:

@table @code
@item id
use @code{uuid} in future.
@item avatarClass_B,_P,_E and colors
Deprecated in favor of @code{baseColor}, @code{patternColor}, @code{padColor}.
@item avatarClass
 This is deprecated and will be removed in future. Its purpose is
better served by other fields already in the structure.
@item format
This is deprecated in favor of @code{avatar}
@end table

@subsection Obtaining Toot Information

Avatar information is available through several channels.

@table @code
@item /toots/@i{Toot-Name}
Fetch only the avatar information for a single Toot from this endpoint
@item `INFINITY-FINGER'
Fetch avatar information for a list of Toots.
@end table

"
  (let* ((Toot (ensure-Toot Toot))
         (avatar-moniker (avatar-moniker (find-reference Toot :avatar))))
    (list :|name| (Toot-name Toot)
          :|userName| (Toot-presentation-name Toot)
          :|chatFG| (color24-name (Toot-chat-foreground-color Toot))
          :|chatBG| (color24-name (Toot-chat-background-color Toot))
          :|uuid| (Toot-UUID Toot)
          :|note| (and privatep (or (Toot-note Toot) ""))
          :|avatar| avatar-moniker
          :|baseColor| (color24-name (Toot-base-color Toot))
          :|pattern| (string-downcase (pattern-name
                                       (find-reference Toot :pattern)))
          :|patternColor| (color24-name (Toot-pattern-color Toot))
          :|padColor| (color24-name (Toot-pad-color Toot))
          :|childP| (or (Toot-childp Toot) :false)
          :|childCode| (or (and privatep (Toot-child-code Toot))
                           "*secret")
          :|peanuts| (or (and privatep (Toot-peanuts Toot))
                         -1)
          :|fairyDust| (or (and privatep (Toot-fairy-dust Toot))
                           -1)
          :|sensitiveP| (or (Toot-childp Toot)
                            (person-sensitivep
                             (find-reference Toot :player))
                            :false)
          :|lastSeen| (Toot-last-active Toot)
          :|clothes| (Toot-clothes+pattern Toot)
          :|gameItem| (Toot-equipped-item Toot)
          :|equip| (apply #'vector
                          (mapcar #'item-info
                                  (Toot-inventory Toot :privatep privatep)))
          :|scaling| (list :|x| (Toot-avatar-scale-x Toot)
                           :|y| (Toot-avatar-scale-y Toot)
                           :|z| (Toot-avatar-scale-z Toot))
          ;; DEPRECATED fields, can be removed in 2.1 or later
          :|id| (Toot-uuid Toot)
          :|avatarClass| (list :|id| (Toot-avatar Toot)
                               :|title| avatar-moniker
                               :|filename| avatar-moniker
                               :|forFree| t
                               :|forPaid| :false)

          :|format| avatar-moniker

          :|avatarClass_B| (color24-name (Toot-base-color Toot))

          :|avatarClass_P| (color24-name (Toot-pattern-color Toot))
          :|avatarClass_E| (color24-name (Toot-pad-color Toot))
          :|inRoom| (Toot-world Toot)
          :|colors| (list 0 (color24-name (Toot-base-color Toot))
                          1 (color24-name (Toot-pad-color Toot))
                          2 (color24-name (Toot-pattern-color Toot))))))


(defun every-Toot-name ()
  "Enumerates the names of every Toot known to the system."
  (sort (mapcar #'Toot-name (find-records 'Toot)) #'string-lessp))

(defun wallet-info (Toot)
  "Returns JSON-type data about TOOT's wallet.

This object contains

@table @code
@item walletOwner
The Toot name whose wallet is being described
@item currency
An object containing an enumeration of currencies. Each key is a currency's
ISO symbol; each value is the amount of that currency which TOOT currently
possesses.
@end table

@subsection Changes from 1.2 to 2.0

In 1.2, the only currency reported was X-TVPN, Tootsville Magic Peanuts.
Now, we also report (at least) X-FADU, fairy dust.
"
  (list :|walletOwner| (Toot-name Toot)
        :|currency| (list :x-tvpn (Toot-peanuts Toot)
                          :x-fadu (Toot-fairy-dust Toot))))

(defun rename-Toot (Toot new-name)
  (assert (potential-Toot-name-p new-name) (new-name)
          "~a is not in the format of a Toot name" new-name)
  (let ((old-name (Toot-name Toot)))
    (handler-case
        (progn 
          (setf (Toot-name Toot) new-name)
          (save-record Toot))
      (dbi.error:dbi-database-error (c)
        (let ((c$ (format nil "~a" c)))
          (if (and (search "Duplicate entry" c$)
                   (search "unique_name" c$))
              (progn
                (setf (Toot-name Toot) old-name)
                (error "Name already in use: can't rename ~a as ~a" old-name new-name))
              (signal c)))))))

(defun destroy-Toot (Toot)
  (let ((Toot (ensure-Toot Toot)))
    (cerror "Continue" "About to destroy the Toot ~a for all time." Toot)
    (destroy-record Toot)))



(defun demand-quiesce-Toot (Toot)
  (unicast (list :|from| "quiesce"
                 :|status| :false
                 :|request| t)
           Toot))

(defun quiesce-connected-Toots ()
  (dolist (Toot (connected-Toots))
    (demand-quiesce-Toot Toot)))
