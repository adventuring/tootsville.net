;;;; -*- lisp -*-
;;;
;;;; src/toots.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  ©   2018,2019  The
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

(defun Toot-item-info (inv)
  (let* ((item (find-reference inv :item))
         (template (find-reference item :template)))
    (list :|equipped| (inventory-item-equipped inv)
          :|uuid| (item-uuid item)
          :|baseColor| (item-base-color item)
          :|energy| (item-energy item)
          :|template| (item-template-id template)
          :|name| (item-template-name template)
          :|defaultBaseColor| (item-template-default-base-color template)
          :|avatar| (item-template-avatar template)
          :|energyKind| (item-template-energy-kind template)
          :|onZero| (item-template-on-zero template)
          :|wearSlot| (item-template-wear-slot template)
          :|weight| (item-template-weight template))))

(defun inventory-item-equipped-p (item)
  "Is the inventory item equipped at all?"
  (ecase (inventory-item-equipped item)
    ((:Y :A) t)
    ((:N :nil) nil)))

(defun Toot-inventory (Toot &optional (privatep
                                       (and *user*
                                            (uuid:uuid= (person-uuid *user*)
                                                        (Toot-player Toot)))))
  (remove-if (lambda (item)
               (and privatep
                    (not (inventory-item-equipped-p item))))
             (find-records 'inventory-item :Toot (Toot-uuid Toot))))

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

Always black at present (2.0)."
  (declare (ignore Toot))
  (color24-rgb 0 0 0))

(defun Toot-chat-background-color (Toot)
  "The background color of a Toot's speech balloon in normal speech.

Shouting and whispering should alter this color appropriately.

Obtained via `TOOT-INFO'.

Always white at present (2.0)."
  (declare (ignore Toot))
  (color24-rgb #xff #xff #xff))

(defun Toot-info (Toot &optional (privatep
                                  (and *user*
                                       (uuid:uuid= (person-uuid *user*)
                                                   (Toot-player Toot)))))
  "Returns a JSON-compatible structure which describes a Toot or other character.

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
@end table

@subsection Changes from 1.0 to 1.1

The @code{avatarClass} object used to have fields @code{s}, which is the
same   as   @code{title};   @code{forVIT},   which  is   the   same   as
@code{forPaid};  and   @code{avatarClassID},  which   is  the   same  as
@code{id}. The renamed fields were supported  under both names in 1.1 or
1.2  based  on   the  setting  of  the   global  configuration  variable
@code{org.starhope.appius.events.format1.0}.

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

The following elements are deprecated and will be removed in a future revision:

@table @code
@item id
use @code{uuid} in future.
@item avatarClass_B,_P,_E and colors
Deprecated in favor of @code{baseColor}, @code{patternColor}, @code{padColor}.
@item avatarClass
 This is deprecated and will be removed in future. Its purpose is
better served by other fields already in the structure.
@table format
This is deprecated in favor of @code{avatar}
@end itemize
"
  (let ((avatar-moniker (avatar-moniker (find-reference Toot :avatar))))
    (list :|name| (Toot-name Toot)
          :|userName| (Toot-presentation-name Toot)
          :|chatFG| (color24-name (Toot-chat-foreground Toot))
          :|chatBG| (color24-name (Toot-chat-background Toot))
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
                          (mapcar #'Toot-item-info
                                  (Toot-inventory Toot privatep)))
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
          :|inRoom| "@Tootsville"
          :|colors| (list 0 (color24-name (Toot-base-color Toot))
                          1 (color24-name (Toot-pad-color Toot))
                          2 (color24-name (Toot-pattern-color Toot))))))

(defun find-active-Toot-for-user (&optional (user *user*))
  (when user
    (when-let (record (ignore-not-found (find-record 'player-Toot :player (person-uuid user))))
      (values (ignore-not-found (find-record 'Toot :uuid (player-Toot-Toot record)))
              record))))

(defun link-active-Toot-to-user (Toot &optional (user *user*))
  (multiple-value-bind (old-Toot player-Toot) (find-active-Toot-for-user user)
    (when old-Toot
      (unless (Toot= Toot old-Toot)
        (destroy-record player-Toot))))
  (make-record 'player-Toot :player (person-uuid user) :Toot (Toot-uuid Toot)))

(defun every-Toot-name ()
  (sort (mapcar #'Toot-name (find-records 'Toot)) #'string-lessp)) 
