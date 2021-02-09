;; -*- lisp -*-
;;
;;; src/infinity/infinity.lisp is part of Tootsville
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

(defvar *infinity-ops* nil)

(defvar *infinity-stream-requests* 0)
(defvar *infinity-rest-requests* 0)
(defvar *infinity-users* (make-hash-table :test 'equalp))

(defun call-infinity-from-rest (method)
  "Call an Infinity-mode command METHOD from a REST call.

Used to create the REST endpoints mapping to METHOD."
  (let* ((json$ (hunchentoot:post-parameters*))
         (json (jonathan.decode:parse json$)))
    (with-user ()
      (v:info '(:infinity :rest) "REST request from ~a for command ~a"
              *user* method)
      (incf *infinity-rest-requests*)
      (funcall method json *user* nil))))

(defmacro with-http-errors-as-infinity-errors ((command) &body body)
  `(handler-case
       (progn ,@body)
     (error (c)
       (list 500 (list :|status| :false
                       :|from| "c"
                       :|command| ,command
                       :|error| (format nil "~a" c))))
     (http-client-error (c)
       (list (http-status-code c)
             (list :|status| :false
                   :|from| "c"
                   :|command| ,command
                   :|httpError| (http-status-code c)
                   :|error| (format nil "~a" c))))))

(defun average (list)
  (/ (reduce #'+ list) (length list)))

(defun infinity-stats ()
  (format nil "Infinity commands:
Handled ~:d stream request~:p and
~:d REST request~:p; total of ~:d request~:p,
for a total of ~:d user~:p.
Average user connected ~:d time~:p; 
most active user connected ~:d time~:p."
          *infinity-stream-requests* *infinity-rest-requests*
          (+ *infinity-stream-requests* *infinity-rest-requests*)
          (hash-table-count *infinity-users*)
          (floor (average (hash-table-values *infinity-users*)))
          (reduce #'max (hash-table-values *infinity-users*))))

(defun call-infinity-from-stream (json)
  "Call an Infinity-mode command from a stream of JSON packets.

Used by the WebSockets and direct TCP stream handlers."
  (let* ((command (getf json :|c|))
         (method (find-symbol (concatenate 'string "INFINITY-"
                                           (string-upcase (symbol-munger:camel-case->lisp-name command)))
                              :Tootsville))
         (data (ignore-errors (getf json :|d|))))
    (if (and (symbolp method) (not (eql 'nil method)) (fboundp method))
        (let ((*Toot* (or *Toot* (Toot *client*))))
          (v:info '(:infinity :stream) 
                  "Stream request from ~a for command ~a"
                  *client* method)
          (when *client*
            (setf (last-active *client*) (get-Unix-time)))
          (when *Toot*
            (setf (Toot-last-active *Toot*) (now))
            (when (zerop (random 10)) (save-record *Toot*)))
          (incf *infinity-stream-requests*)
          (with-http-errors-as-infinity-errors (command)
            (funcall method data *Toot* (world *client*))))
        (let ((c (or command "(No command sent)")))
          (v:warn '(:infinity :stream) "Unknown command from stream ~a: ~a"
                  *user* c)
          (list 404 (list :|from| "c"
                          :|status| :false
                          :|error| (format nil "Unrecognized command ~a"
                                           (limit-string-length c 100))))))))

(defmacro definfinity (name (lambda-list user-var plane-var) &body body)
  "Define an Infinity-mode “c” command NAME.

@cindex Infinity Mode Protocol

And now, let's talk about the Infinity Mode protocol.

@subsection History of Infinity Mode

In the Beginning,  Tootsville used a commercial  program called SmartFox
Server as  its chat server. There  were many problems with  this, and it
didn't last long.

However, the client program (Persephone) was written to use the SmartFox
client libraries,  which were very good.  So, we kept them,  and used an
AGPL chat server  program created by Bruce-Robert  Pocock, named Braque,
to replace  the server side.  Braque was renamed Appius  Claudius Cæcus,
because SmartFox Server was from Italy,  and Appius built the Via Appia,
a highway leaving Italy.

Appius  gained friends,  all  of which  were given  the  names of  other
Romans,  and  so  the  entire  software  suite  was  nicknamed  Romance,
like Romans.

In order to  negotiate a connection between Appius  and SmartFox client,
we had to provide a version identifier,  so we set the version number to
``infinity.''

With the adoption of the gossipnet for Romance II, we had to increment
the version from infinity, which brings us to ℵ@sub{0} (read: Alef-Null),
which is a particular kind of infinity that is not as big as some
other kinds of infinity, as silly as that mathematical construction
may sound (yes, that's real maths).

@subsection Wire protocols

There are two main wire protocols; RESTful POSTs and gossipnet.

@subsubsection RESTful POSTs

The REST POST interface is what you're really here to read about (on the
server  side). A  POST  is  submitted with  a  JSON object  representing
a command call.

This request can  be submitted to either the dedicated  endpoint, or the
general,   dispatching  endpoint.   The  dedicated   endpoint  will   be
slightly faster.

The   dedicated    endpoint   will    have   a    URL   of    the   form
/world/infinity/command-name,  with  the   command  name  in  lower-case
and hyphenated.

The  dispatching   endpoint  is   /world/infinity.  Submitting   to  the
dispatching endpoint requires a JSON object with two keys:

@table @code
@item c
The command name, in camelCase
@item d
The data to be submitted to that command.
@end table

In the  case of the  dedicated endpoint,  only the contents  of @code{d}
need to be submitted.

@subsection Datagram constructions

There are three datagram kinds used in Infinity Mode.

@enumerate

@item
The special @samp{logOK} packet type  is used only for acknowledging and
promulgating login events through the grid. This actually dates back all
the way to the SmartFox server's protocols, so it's an odd duck.

@item 
Commands that instigate an action are identified by a @samp{c}
attribute.

@item
Commands that provide information about the world, usually as a reaction
to another event,  are called Gatekeeper messages and  are identified by
a @samp{from} attribute.

@end enumerate

@subsection logOK datagrams

The login  process should be documented  at `WEBSOCKET-AUTHENTICATE' for
WebSockets, `TCP-STREAM-AUTHENTICATE' for direct TCP/IP server-to-server
streams,   and  `Tootsville.Gossip.createConnection'   for  peer-to-peer
WebRTC connections.

@subsection Command datagrams

Command  datagrams  may be  processed  through  either  a REST  POST  or
the Gossipnet. These represent an action or enquiry that a client is making.

Command datagrams are  identified by a @code{c} key,  which provides the
command name in @samp{lowerFirstCamelCase}.  This command name is mapped
to a function named @code{INFINITY-COMMAND-NAME} in hyphenated form.

Command  datagrams  usually have  a  @code{d}  key which  provides  some
additional data or parameters to the requested command.

In addition, there may be some of the following. Note that UUID's are the UUID's @emph{of a Toot character}, never the person who ``owns'' that Toot.

@table @code

@item r

Recipient. This can be an UUID  for a direct peer-to-peer command, or is
more often  just @code{$World}  for the game  server or  @code{$All} for
all listeners.

@item a

Author. The UUID of the originator of the packet.

@item u

User. The UUID  of the user who requested this  packet; usually the same
as @code{a}/Author.

@item s

Signature. Proof that the packet originated with @code{a}/Author.

@item v

Via. The history trail of a forwarded packet.

@end table

WRITEME

@subsection Gatekeeper datagrams

Gatekeeper datagrams are found either as the response to a REST POST, or
distributed along the Gossipnet. These  represent the state of the world
at a certain point in time.

Every   Gatekeeper   datagram   contains  the   keys   @code{from}   and
@code{status}.  The @code{from}  value uniquely  identifies the  type of
packet and determines what other  fields accompany it. The @code{status}
value  is a  Boolean, and  while  its meaning  varies by  packet, it  is
usually a good guess that if @code{status} is not @code{true}, there has
been some kind of request error and data is not available.

For a complete enumeration 

"
  (let* ((legacy-name (symbol-munger:lisp->camel-case (string name)))
         (docstring (if (stringp (first body))
                        (first body)
                        (format nil "Infinity mode command ~a" legacy-name)))
         (docstring-first (first-paragraph docstring))
         (docstring-rest (subseq docstring (length docstring-first)))
         (docstring-with-prefix (format nil "~a~2%@icindex ~a: ~a~2%Lisp ~a = JSON ~a~2%~a"
                                        docstring-first
                                        legacy-name docstring-first
                                        name legacy-name
                                        docstring-rest))
         (body (if (stringp (first body)) (rest body) body))
         (infinity-name (intern (concatenate 'string "INFINITY-" (string name))))
         (λ-list (if (eql '&rest (first lambda-list))
                     lambda-list
                     (cons '&key lambda-list))))
    (push (cons (string name) infinity-name) *infinity-ops*)
    (push (cons legacy-name infinity-name) *infinity-ops*)
    `(progn
       (defun ,infinity-name (d ,user-var ,plane-var)
         ,docstring-with-prefix
         (declare (ignorable ,user-var ,plane-var))
         (block nil (catch 'infinity
                      (destructuring-bind (,(first λ-list) ,@(mapcar (lambda (sym)
                                                                       (if (char= #\& (char (symbol-name sym) 0))
                                                                           sym
                                                                           (intern (symbol-munger:lisp->camel-case sym))))
                                                                     (rest λ-list)))
                          d
                        (let (,@(mapcar (lambda (var) (list var (intern (symbol-munger:lisp->camel-case var)))) (remove-if (lambda (sym)
                                                                                                                             (char= #\& (char (symbol-name sym) 0)))
                                                                                                                           (rest λ-list))))
                          ,@body)))))
       (defendpoint (POST ,(concatenate 'string "/world/infinity/" (string-downcase name))
                          "application/json")
         ,(format nil "~a~2%@subsection Infinity Mode command~2%See @ref{TOOTSVILLE INFINITY-~a}"
                  docstring-first name)
         (call-infinity-from-rest  ',infinity-name )))))

(defpackage Tootsville-User
  (:use :CL :CL-User :Bordeaux-Threads :Tootsville))

(defmacro define-operator-command (command (words user plane) &body body)
  (let* ((docstring (if (stringp (first body))
                        (first body)
                        (format nil "Undocumented operator command ~a" command)))
         (docstring-first (first-paragraph docstring))
         (docstring-rest (subseq docstring (length docstring-first)))
         (docstring-with-prefix (format nil "~a~2%@ocindex #~a: ~a~2%~a"
                                        docstring-first
                                        (string-downcase command)
                                        docstring-first
                                        docstring-rest))
         (body (if (stringp (first body)) (rest body) body))
         (command (if (find-symbol (string command) :CL)
                      (concatenate 'string "*" (string command))
                      command)))
    `(progn
       (defun ,(intern (string command) (find-package :Tootsville-User)) (&rest ,words)
         ,docstring-with-prefix
         (declare (ignorable ,words))
         (let ((,user *Toot*) (,plane (world *client*)))
           (declare (ignorable ,user ,plane))
           (if (or (null *Toot*) (and *Toot* (builder-Toot-p *Toot*)))
               (let ((reply (block nil (progn ,@body))))
                 (when (stringp reply)
                   (private-admin-message
                    ,(concatenate 'string #(#\#) (string-downcase command))
                    reply)))
               (private-admin-message "Builders Only"
                                      "# commands are only for Builder Toots.")))))))

(defendpoint (POST "/world/infinity" "application/json")
  "Dispatch an Infinity-mode JSON packet to its handler based on the @code{c} parameter.

See `DEFINFINITY' for a detailed discussion of this mode of operation."
  (with-posted-json (c d)
    (with-user ()
      (funcall (find-symbol (concatenate 'string "INFINITY-"
                                         (string-upcase (symbol-munger:camel-case->lisp-name c)))
                            :Tootsville)
               d *user* nil))))
