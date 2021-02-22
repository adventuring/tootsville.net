;; -*- lisp -*-

;;; src/infinity/modern-ops.lisp is part of Tootsville
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

(define-operator-command quick-reload (words u r)
  "Quicklisp reload of the Tootsville package from disk.

Reloads   the   ASDF   file   with   `ASDF:LOAD-ASD'   and   then   does
a `QL:QUICKLOAD'. See `RELOAD-PRODUCTION'.

@subsection Usage

@verbatim
#quick-reload
@end verbatim

Example:

@example
#quick-reload
@end example

@subsection Effects

Sends    an   admin    message   with    ``Stand   by,''    then   calls
`RELOAD-PRODUCTION'  (qv). When  completed, reports  the version  of the
ASDF   component   now   loaded    (i.e.   the   version   number   from
@code{tootsville.asd}).
"
  (private-admin-message "#quick-reload" "Started quick reload. Stand by.")
  (reload-production)
  (format nil "Now running Tootsville.net server version ~a"
          (asdf:component-version (asdf:find-system :Tootsville))))

(define-operator-command git-pull (words u r)
  "Does a @code{git pull} in the server directory.

See also  `TOOTSVILLE-USER::QUICK-RELOAD' to actually load  any new code
that's downloaded.

@subsection Usage

@verbatim
#git-pull
@end verbatim

Example

@example
#git-pull
@end example

@subsection Effects

Sends an admin message with ``stand by,'' then does a @code{git pull} in
the  source directory  and returns  the  results (e.g.  ``Already up  to
date.'' or otherwise)."
  (private-admin-message "#git-pull" "(stand by…)")
  (format nil "<pre>~a</pre>"
          (run-program (format nil "cd ~a; git pull"
                               (asdf:system-source-directory :Tootsville))
                       :output :string
                       :error :output)))

(define-operator-command push-script (words u r)
  "Instruct clients to load a new  script file.
 
Pushes a  script filename to clients.  The pathname must be  relative to
the  @code{play.tootsville.org}  server  (or  its  equivalent  in  other
clusters). Used to  push an emergency software  update without requiring
players to reload.

@subsection Usage

@verbatim
#push-script PATHNAME
@end verbatim

@subsection Example

@example
#push-script /play/UI/UI.js
@end example
"
  (broadcast (list :|from| "newScript"
                   :|status| t
                   :|script| (first words))))

(define-operator-command ws-stats (words u r)
  "Returns some statistics about WebSockets connections.

See `WS-STATS'.

@subsection Usage

@verbatim
#ws-stats
@end verbatim

@subsection Example

@example
#ws-stats
@end example"
  (format nil "This server is ~a.~2%~a"
          (machine-instance) (ws-stats)))

(define-operator-command ws-bandwidth-by-source (words u r)
  "Returns some statistics about WebSockets bandwidth by source.

See `WS-BANDWIDTH-BY-SOURCE'.

@subsection Usage

@verbatim
#ws-bandwidth-by-source
@end verbatim

@subsection Example

@example
#ws-bandwidth-by-source
@end example"
  (format nil "This server is ~a.~2%<pre>~a</pre>"
          (machine-instance) (ws-bandwidth-by-source)))

(define-operator-command infinity-stats (words u r)
  "Returns some statistics about Infinity-mode requests.

See `INFINITY-STATS'.

@subsection Usage

@verbatim
#infinity-stats
@end verbatim

@subsection Example

@example
#infinity-stats
@end example
"
  (format nil "This server is ~a.~2%~a"
          (machine-instance) (infinity-stats)))

(define-operator-command journal (words u r)
  "Add a staff journal entry or review last entries.

See also `INFINITY-JOURNAL' for an Infinity Mode command for the same
purpose.

@subsection Usage

To read the last journal entry, use @code{#journal #last}. To read the
one before that, use @code{#journal #last -1}, and for farther back,
use lesser (more negative) values of REFERENCE.

To read a journal entry relative to a certain user, use @code{#ref}.

To post a new journal entry, simply enter it after
@code{#journal}. You can associate it with one or more users with
@code{#for}.

@verbatim
#journal #last [REFERENCE]

#journal #ref USER [REFERENCE]

#journal [#for USER[,...]] ENTRY...
@end verbatim

REFERENCE will always be zero or negative.

@subsection Examples

@verbatim
#journal #last
#journal #last -1
#journal #last -2

#journal #ref Pil
#journal #ref Pil -1

#journal #for pil,zap Added a new game with Pil and Zap
#journal #for mayor-louis Had to kick off mayor-louis for sedition

#journal Server game2 shut down for maintenance
@end verbatim
"
  (unless (<= 1 (length words))
    (private-admin-message "#journal"
                           "Usage: #journal [#last | #ref | #for] …")
    (return-from tootsville-user::journal))
  (cond
    ((string-equal "#last" (first words))
     (let ((reference (if (<= 2 (length words))
                          (parse-integer (second words))
                          0)))
       (assert (or (zerop reference) (minusp reference)))
       (read-staff-journal :last reference)))
    ((string-equal "#ref" (first words))
     (let ((users (split-sequence #\, (second words)))
           (reference (if (<= 3 (length words))
                          (parse-integer (third words))
                          0)))
       (assert (plusp (length users)))
       (assert (or (zerop reference) (minusp reference)))
       (error 'unimplemented)))
    ((string-equal "#for" (first words))
     (let ((users (split-sequence #\, (second words))))
       (assert (plusp (length users)))
       (write-staff-journal-entry (join #\Space (subseq words 3))
                                  users)))
    (t
     (write-staff-journal-entry (join #\Space words) nil))))

(define-operator-command doodle (words u r)
  "Change the colors of a Toot.

@subsection Usage

@verbatim
#doodle WHO ( #base | #pad | #pattern ) NEW-COLOR
@end verbatim

@subsection Examples

@example
#doodle catvlle base pink
#doodle catvlle pattern black
@end example

The Toot's  color will immediately be  changed and be advertised  to any
interested listener.

NEW-COLOR can be in any format understood by `PARSE-COLOR24', qv."
  (if (= 3 (length words))
      (let ((who (ignore-not-found (find-record 'Toot :name (first words)))))
        (if who
            (let ((color (ignore-errors (parse-color24 (third words)))))
              (if color
                  (progn (string-case (second words)
                           ("#base"
                            (setf (Toot-base-color who) color))
                           ("#pad"
                            (setf (Toot-pad-color who) color))
                           ("#pattern"
                            (setf (Toot-pattern-color who) color))
                           (otherwise
                            (private-admin-message "#doodle"
                                                   "Color place name must be #base, #pad, or #pattern")
                            (return-from tootsville-user::doodle)))
                         (save-record who)
                         (broadcast (from-avatars (list :|doodled| (Toot-name who)))))
                  (format nil "Can't understand color ~a" (fourth words))))
            (format nil "No Toot named ~a" (first words))))
      (format nil "Usage: #doodle WHO (base|pad|pattern) NEW-COLOR ~s" words)))

(define-operator-command doodle-pattern (words u r)
  "Change the pattern of a Toot.

@subsection Usage

@verbatim
#doodle-pattern WHO NEW-PATTERN
@end verbatim

@subsection Example

@example
#doodle-pattern catvlle hearts
@end example

As a special  case, \"Polka Dots\" should be passed  as POLKA-DOTS (with
an hyphen),  as well  as any  other pattern names  with spaces  (such as
\"Maple Leaf\"). "
  (if (= 2 (length words))
      (let ((who (ignore-not-found (find-record 'Toot :name (first words)))))
        (if who
            (if (typep (second words) 'Toot-pattern-name)
                (if-let (pattern (find-record 'pattern :name
                                              (substitute #\Space #\- (second words))))
                  (progn (setf (Toot-pattern who) (pattern-id pattern))
                         (save-record who)
                         (broadcast (from-avatars (list :|doodled| (Toot-name who))))))
                (format nil "Can't understand color ~a" (third words)))
            (format nil "No Toot named ~a" (first words))))
      "Usage: #doodle-pattern WHO PATTERN-NAME"))

(define-operator-command server-list (words u r)
  "Enumerate the servers active in this cluster.

See `SERVER-LIST'

@subsection Usage

@verbatim
#server-list
@end verbatim

@subsection Example

@example
#server-list
@end example"
  (format nil "<ul>~{<li>~a</li>~}</ul>" (server-list)))

(define-operator-command at (words u r)
  "Issue an operator command on a particular server instance.

UNIMPLEMENTED. Remote code execution is not possible. Only works if
SERVER is `MACHINE-INSTANCE', i.e. the local machine, which is
identical to not using this command at all.

@subsection Usage

@verbatim
#at SERVER #OTHER-COMMAND OTHER-PARAMS
#at #each #OTHER-COMMAND OTHER-PARAMS
@end verbatim

@subsection Examples

@example
#at game1.test.tootsville.net #ws-stats
#at #each #git-pull
@end example

For a list of servers, see `SERVER-LIST'.

To issue a command on every server, send @code{#at #each #OTHER-COMMAND}.
"
  (if (string-equal (machine-instance) (first words))
      (parse-operator-command (format nil "~{~a~^ ~}" (rest words)))
      (if (string-equal "#each" (first words))
          (parse-operator-command (format nil "#at ~a ~{~a~^ ~}" (machine-instance) (rest words)))
          (format nil "Can't execute ~s at ~a" (rest words) (first words)))))

(define-operator-command gc (words u r)
  "Perform immediate garbage collection.

@subsection Usage

@verbatim
#gc
#gc #full
@end verbatim

Examples:

@example
#gc
#gc #full
@end example

Returns the same report as `TOOTSVILLE-USER::MEM'

"
  (cond
    ((and (= 1 (length words))
          (string-equal "#full" (first words)))
     (sb-ext:gc :full t))
    ((= 0 (length words))
     (sb-ext:gc))
    (t (private-admin-message "#gc"
                              "Usage: #gc [#full]")
       (return-from tootsville-user::gc)))
  (Tootsville-user::mem))

(define-operator-command doc (words u r)
  "Obtain documentation string in raw form about a symbol.

@subsection Usage

@verbatim
#doc [PACKAGE] SYMBOL [TYPE]
@end verbatim

TYPE can  be VARIABLE,  FUNCTION, STRUCTURE,  TYPE, SETF,  or T.  If not
supplied, defaults to FUNCTION.

PACKAGE is optional and defaults to TOOTSVILLE-USER.

@subsection Examples

@example
#doc cdr
#doc doc function
#doc Tootsville ws-stats
#doc Tootsville ws-bandwidth-by-source function
@end example

This is based upon `DOCUMENTATION', qv."
  (format nil
          "~{~a~^ ~}: ~a" words
          (let ((doc-types '(variable function structure type setf t)))
            (case (length words)
              (1 (documentation (find-symbol (string-upcase (first words)) 
                                             :Tootsville-user)
                                'function))
              (2 (if (member (second words) doc-types :test 'string-equal)
                     (documentation (find-symbol (string-upcase (first words)) :Tootsville-user) 
                                    (find (second words) doc-types :test 'string-equal))
                     (documentation (find-symbol (second words)
                                                 (find-package (string-upcase (first words))))
                                    'function)))
              (3 (documentation (find-symbol (second words)
                                             (find-package (string-upcase (first words))))
                                (find (third words) doc-types :test 'string-equal)))))))


(define-operator-command apropos (words u r)
  "Runs `APROPOS' for a remote user.

@subsection Usage

@verbatim
#apropos EXPRESSION
@end verbatim

@subsection Example

@verbatim
#apropos apropos
@end verbatim"
  (with-output-to-string (*standard-output*)
    (apropos (first words) :tootsville-user)))

(define-operator-command script (words u r)
  "Push a new function into for the #$ operator command.


UNIMPLEMENTED.

@subsection Usage

@verbatim
#script TITLE SOURCE TEXT ...
@end verbatim

@subsection Example

@verbatim
#script simply-string \"simply\"
@end verbatim

"
  (error 'unimplemented))

(define-operator-command uptime (words u r)
  "Gives the uptime of the server software."
  (let ((uptime (- (get-universal-time*) *started*)))
    (format nil "The server ~:(~a~) has been up for ~a (precisely ~:d sec)"
            (machine-instance) (human-duration uptime) (round uptime))))

(define-operator-command whatabout (words user _)
  "Searches for related item templates.

Replies with an admin (parrot) message with the item numbers and names
of related item templates. Template names and descriptions are searche

If WORD is a single word beginning with @code{#}, instead searches for
item templates tagged with WORD.

See also `TOOTSVILLE-USER::WHATIS' to get details about a specific
item.

@subsection Usage

@verbatim
#whatabout WORD(S)
#whatabout #TAG
@end verbatim

@subsection Example

@verbatim
#whatabout bucket
#whatabout #cute
@end verbatim

"
  (unless (plusp (length words))
    (return "Give some search term to look for"))
  (if (and (= 1 (length words))
           (char= #\# (char (first words) 0)))
      (let ((tag (subseq (first words) 1)))
        (if-let (results (mapcar
                          (lambda (template)
                            (list (item-template-id template)
                                  (item-template-name template)))
                          (mapcar
                           (lambda (item-tag)
                             (find-record 'item-template :id item-tag-item))
                           (find-records 'item-tag :tag tag))))
                (format nil "Item templates tagged ~a: <ul>~{<li>~{~d. ~a~}</li>~}</ul>"
                        tag results)
                (format nil "No templates found tagged ~a" tag)))
  (let ((term (join #\Space words)))
    (if-let (results (mapcar (lambda (template)
                               (list (item-template-id template)
                                     (item-template-name template)))
                             (remove-if-not (lambda (template)
                                              (or (search term (item-template-name template)
                                                          :test 'string-equal)
                                                  (search term (item-template-description template)
                                                          :test 'string-equal)))
                                            (find-records 'item-template))))
            (format nil "Item templates like <q>~/html/</q>: ~
<ul>~{<li>~{~d. ~/html/~}</li>~}</ul>"
                    term results)
            (format nil "No templates found like <Q>~/HTML/</Q>" results)))))

(define-operator-command describeitem (words user _)
  "Set description for an item.

@subsection Usage

To see the existing description of an item, use the
`TOOTSVILLE-USER::WHATIS' command.

To change the description of an item, use this command.

@verbatim
#describeitem ITEM-TEMPLATE-ID DESCRIPTION
#describeitem ITEM-TEMPLATE-ID #TAG
@end verbatim

When DESCRIPTION is a single word beginning with @code{#}, it is
interpreted instead as an `ITEM-TAG' to be associated with the item
template.

@subsection Examples

@verbatim
#describeitem 993 This is a lovely marble table 
       of which any grandmother would be proud.

#describeitem 993 #classical
@end verbatim
"
  (unless (<= 2 (length words))
    (return "Usage: item-template-id description-text"))
  (let ((item-template (find-record 'item-template :id (parse-number (first words)))))
    (if (and (= 2 (length words))
             (char= #\# (char (second words) 0)))
        (make-record 'item-tag :item (item-template-id item-template)
                     :tag (subseq (second words) 1))
        (setf (item-template-description item-template)
              (join #\Space (rest words))))
    (save-record item-template)
    (Tootsville-user::whatis (first words))))


(define-operator-command status (words u r)
  "Discover the general status of the host.

Reports a few interesting statistics in a row.

This gives a quick summary of what you would learn from a variety of
other operator commands, and also lists the operator commands you
would use to get more detailed information.

The intention is that operators can use @code{#status} to begin a
shift and see how things are going across the board.

TODO this should flag recent staff journal entries as well.

@subsection Usage

@verbatim
#status
@end verbatim
"
  (format nil "Status report for ~:(~a~)
<table>
<tr><th>#server-list</th><td>~:d server~:p</td></tr>
<tr><th>#mem</th><td>~a</td></tr>
<tr><th>#ws-stats</th><td>~:d socket~:p</td></tr>
<tr><th>#infinity-stats</th><td>~:d request~:p</td></tr>
<tr><th>#uptime</th><td>~a</td></tr>
<tr><th>#banhammer #list</th><td>~:d client~:p</td></tr>
</table>"
          (machine-instance)
          (length (server-list))
          (let ((s (with-output-to-string (*standard-output*) (room))))
            (subseq s (1+ (position #\: s)) (position #\Newline s)))
          (length (hunchensocket::clients *infinity-websocket-resource*))
          (+ *infinity-stream-requests* *infinity-rest-requests*)
          (human-duration (- (get-universal-time*) *started*))
          (length (hash-table-keys *banhammer*))))

(define-operator-command whatmusic (words u r)
"Discover available music

Searches the music available for a keyword --- both titles and
artists.

@subsection Usage

@verbatim
#whatmusic WORD[s]
@end verbatim

@subsection Examples

@verbatim
#whatmusic bensound
#whatmusic adventure
@end verbatim
"
  (unless (plusp (length words))
    (return "Give some search tepm to look for"))
  (let ((term (join #\Space words)))
    (if-let (results (mapcar
                      (lambda (music)
                        (list (music-moniker music)
                              (music-title music)
                              (music-artist music)))
                      (remove-if-not (lambda (music)
                                       (or (search term (music-title music)
                                                   :test 'string-equal)
                                           (search term (music-artist music)
                                                   :test 'string-equal)))
                                     (find-records 'music))))
            (format nil "Music like <q>~/html/</q>: ~
<ul>~{<li><b>~/html/</b>: <q>~/html/</q> by ~/html/</li>~}</ul>"
                    term results)
            (format nil "No music like <q>~/html/</q>" term))))

(define-operator-command setmusic (words u r)
  "Set the music for an area (or this area)

@subsection Usage

@verbatim
#setmusic MONIKER
#setmusic MONIKER LAT LONG [ALT [WORLD]]
@end verbatim

@subsection Examples

@verbatim
#setmusic adventure
#setmusic adventure 30 -20
#setmusic adventure 30 -20 0
#setmusic adventure 30 -20 0 CHOR
@end verbatim
"
  (error 'unimplemented))
