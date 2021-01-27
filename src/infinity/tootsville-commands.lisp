(in-package :Tootsville)

;;; tootsville-commands.lisp is part of Tootsville
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


(definfinity add-journal-entry ((entry) u r)
  "Add a staff journal entry.

The staff journal entries are recorded to the database
for later review.

UNIMPLEMENTED in 2.0.

@subsection Usage

@verbatim
{ entry: \"journal text\" }
@end verbatim

@subsection Example

@verbatim
{ entry: \"nothing to report\" }
@end verbatim


@subsection Romance 1.2 documentation

Staff members can create a journal entry which is stored for
review in a customer service application such as Joshua. Creating
a ModeratorJournal object will parse for certain values such as
[@@username].

@subsection Formerly Proprietary Extension

This command was formerly a proprietary extension for Tootsville.com and
has now been re-created for the AGPL version of Romance.
"
  (error 'unimplemented))

(definfinity delete-mail-message ((id) u r)
  "Delete a message from the user's (SMS) mailbox

@subsection Usage

WRITEME

@subsection Example

WRITEME

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

@code{id} was previously an integer, but is now an UUID string.

@subsection Formerly Proprietary Extension

This command was formerly a proprietary extension for Tootsville.com and
has now been re-created for the AGPL version of Romance.

"
  (error 'unimplemented))

(definfinity doff ((slot type) user recipient/s)
  "Remove clothes or Pivitz.

@subsection Usage

@verbatim
{ slot: \"SLOT\" }

{ type: \"TYPE\" }
@end verbatim

@subsection Example

@verbatim
{ type: \"pivitz\" }

{ slot: \"1D94E6C7-8643-48AE-81A4-8B0C3EB36A7A\" }
@end verbatim

When  @code{type}  is  present,  it must  be  either  @code{clothes}  or
@code{pivitz}.

When @code{slot} is present, indicates a specific item UUID to remove.

@subsection 400 Bad Request

Exactly one of @code{type} or @code{slot} must be present.

If present, @code{type} must be @code{clothes} or @code{pivitz}.

@subsection 200 OK

Responds with total wardrobe as per `INFINITY-WARDROBE'

@subsection Formerly Proprietary Extension

This command was formerly a proprietary extension for Tootsville.com and
has now been re-created for the AGPL version of Romance.
")

(definfinity get-mail-in-box ((from limit) u r)
  "Get a listing of messages in an SMS mailbox.

@subsection Usage

@verbatim
{ [ from: INDEX ],
  [ limit: COUNT ] }
@end verbatim

@subsection Examples

@verbatim
{}

{ from: 10, limit: 10 }

{ from: 0, limit: 100 }

{ limit 100 }

{ from: 10 }
@end verbatim

When specified, @code{from} is the index of the first message to return,
and @code{limit} is the number of messages to return.

@code{limit} defaults to 100 messages if not supplied.

@subsection 200 OK

Returns an object named @code{mail}. Keys under @code{mail} are indices.
Each message consists of

@table @code
@item id
An UUID for the message
@item from
The sender's name
@item to
The recipient's name
@item subject
No longer used; always \"\"
@item sentTime
The date and time sent
@item readTime
The date and time first (previously) retrieved by the client. Messages will
be marked as read ``now'' when retrieved, but only after they are retrieved
for the first time.
@item body
The contents of the message.
@end table

@subsection 416 Request Range Not Satisfiable

The @code{from} value exceeded the maximum message in the Toot's inbox.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

Message ID's are now UUID's. Messages no longer have subjects.

@subsection Formerly Proprietary Extension

This command was formerly a proprietary extension for Tootsville.com and
has now been re-created for the AGPL version of Romance.
"
  (error 'unimplemented))

(definfinity get-passport (nil u r)
  "Get the list of places that the user has gotten a passport stamp at.

Passport stamps are not currently implemented but will be returning.

See `TOOT-PASSPORT-STAMPS'.

@subsection Usage

This command requires no parameters.

@subsection 200 OK

@verbatim
WRITEME
@end verbatim

The reply format is a WRITEME but should be unchanged from 1.2.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

Passports stamps are temporarily unavailable.

@subsection Formerly Proprietary Extension

This command was formerly a proprietary extension for Tootsville.com and
has now been re-created for the AGPL version of Romance.
"
  (error 'unimplemented))

(definfinity send-mail-message ((to to-list subject body) u r)
  "Send an in-game SMS message.

@subsection Usage

@verbatim
{ ( to: \"RECIPIENT\" | toList: [ \"RECIPIENT\", ... ] ),
  [ subject: \"\" ],
  body: \"BODY\",
  [ uuid: UUID-STRING ] )
@end verbatim

@code{subject} is optional, and should be omitted in 2.0. Non-empty
subjects will return an error.

@code{uuid} is optional but recommended. It allows the client to track
when a message has been sent.

See `SEND-SMS-MESSAGE' for the underlying implementation.

@subsection Examples

@verbatim
{ to: \"shader\", subject: \"\", body: \"Hello there!\" }

{ toList: [ \"catvlle\", \"pil\" ], body: \"Howdy\" }
@end verbatim

Input: @code{subject} (must be blank); @code{to}, the Toot name to
whom to send the text; and @code{body} of the message.

Rather that @code{to}, the user can send @code{toList} with an object,
the keys of which are ignored, the values of which are Toot names.

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

Subjects are no longer supported. @code{subject} must be absent, null,
or \"\".

@code{uuid} is a new option.

Message length is now measured in Unicode characters, not bytes.

@subsection Formerly Proprietary Extension

This command was formerly a proprietary extension for Tootsville.com and
has now been re-created for the AGPL version of Romance.

@subsection 200 OK

The “SMS” message was sent.

@verbatim
{ from: \"sendMailMessage\", status: true }
{ from: \"sendMailMessage\", status: true,
  uuid: \"5047F44E-8B1D-4B8A-9EC6-4E1D6E1653AD\" }
@end verbatim

If the client supplied an UUID, it will be returned, allowing the
client to identify which of potentially many SMS messages was sent.

Sending does not imply that the message was received or read by the
destination user.

@subsection 400 Bad Request

If an UUID was supplied with the request, the response will echo it.

@code{subject} must be absent, null, or \"\"

@verbatim
{ from: \"sendMailMessage\", status: false,
  error: \"Subject is not allowed. Please leave subject blank.\" }
@end verbatim

Exactly one of @code{to} or @code{toList} must be specified

@verbatim
{ from: \"sendMailMessage\", status: false,
  error: \"Message has no destination.\",
  uuid: \"E6726651-703D-41FC-8484-E59EADEE7EA0\" }
@end verbatim

@code{body} may not be empty

@subsection 413 Payload Too Large

@code{body} can be at most 1,024 (Unicode) characters (not bytes).

@verbatim
{ from: \"sendMailMessage\", status: false,
  error: \"Message too long. Try a message with less than 1,000 characters.\" }
@end verbatim

@subsection Formerly Proprietary Extension

This command was formerly a proprietary extension for Tootsville.com and
has now been re-created for the AGPL version of Romance.


"
  (error 'unimplemented))

(definfinity stamp-passport ((room) u r)
  "Stamp the Toot's passport

Passports are not currently implemented in Tootsville V, but will be
returning.

See `STAMP-TOOT-PASSPORT'.

@subsection Usage

@verbatim
{ room: \"SPOT-MONIKER\" }
@end verbatim

@subsection Example

@verbatim
{ room: \"tootSquare\" }
@end verbatim

@subsection Changes from 1.2 to 2.0
@cindex Changes from 1.2 to 2.0

Passports stamps are temporarily unavailable.

@code{room} was previously a room moniker, but will now be a ``spot''
moniker of a Spot in the game world. Despite the change, the key name
remains @code{room}.

@subsection Formerly Proprietary Extension

This command was formerly a proprietary extension for Tootsville.com and
has now been re-created for the AGPL version of Romance.
"
  (error 'unimplemented))
