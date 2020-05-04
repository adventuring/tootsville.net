(in-package :Tootsville)

(defvar *infinity-ops* nil)

(defun call-infinity-from-rest (method)
  "Call an Infinity-mode command METHOD from a REST call.

Used to create the REST endpoints mapping to METHOD."
  (let* ((json$ (hunchentoot:post-parameters*))
         (json (jonathan.decode:parse json$)))
    (with-user ()
      (let ((*Toot* (or *Toot* (find-active-Toot-for-user))))
        (v:info '(:infinity :rest) "REST request from ~a for command ~a" 
                *user* method)
        (funcall method json *user* (user-plane *user*))))))

(defmacro with-http-errors-as-infinity-errors ((command) &body body)
  `(handler-case 
       (progn ,@body)
     (http-client-error (c)
       (list :|status| :false
             :|from| "c"
             :|command| ,command
             :|httpError| (http-status-code c)
             :|error| (format nil "~a" c)))))

(defun call-infinity-from-stream (json$)
  "Call an Infinity-mode command from a stream of JSON$ packets.

Used by the WebSockets and direct TCP stream handlers."
  (let* ((json-full (jonathan.decode:parse json$))
         (command (getf json-full :|c|))
         (method (find-symbol (concatenate 'string "INFINITY-" 
                                           (string-upcase (symbol-munger:camel-case->lisp-name command)))
                              :tootsville))
         (data (getf json-full :|d|)))
    (if (and (symbolp method) (not (eql 'nil method)))
        (with-user ()
          (let ((*Toot* (or *Toot* (find-active-Toot-for-user))))
            (v:info '(:infinity :stream) "Stream request from ~a for command ~a" 
                    *user* method)
            (with-http-errors-as-infinity-errors (command)
              (funcall method data *user* (user-plane *user*)))))
        (let ((c (or (getf json-full :|c|) "")))
          (v:warn '(:infinity :stream) "Unknown command from stream ~a: ~a"
                  *user* c)
          (list :|from| "c"
                :|status| :false
                :|error| (format nil "Unrecognized command ~a" 
                                 (subseq c 0 (min (length c) 100))))))))

(defun infinity-error (code reason)
  (throw 'infinity
    (list code (list :|status| :false
                     :|error| (symbol-munger:lisp->camel-case reason)))))

(defmacro definfinity (name (lambda-list user-var plane-var) &body body)
  "Define an Infinity-mode “c” command NAME.

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
“infinity.”

With the  adoption of the gossipnet  for Romance Ⅱ, we  had to increment
the  version from  infinity, which  brings us  to ℵ₀  (read: Alef-Null),
which is a particular kind of infinity  that is not as big as some other
kinds  of  infinity, as  silly  as  that mathematical  construction  may
sound (yes, that's real maths).

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
Commands    that    instigate    an    action    are    identified    by
a @samp{c} attribute.

@item
Commands that provide information about the world, usually as a reaction
to another event,  are called Gatekeeper messages and  are identified by
a @samp{from} attribute.

@end enumerate

XXX WRITEME

@subsection logOK datagrams 

XXX WRITEME

@subsection Command datagrams 

Command  datagrams  may be  processed  through  either  a REST  POST  or
the Gossipnet. These represent an action or enquiry that a client is making.

XXX WRITEME

@subsection Gatekeeper datagrams 

Gatekeeper datagrams are found either as the response to a REST POST, or
distributed along the Gossipnet. These  represent the state of the world
at a certain point in time.

XXX WRITEME

"
  (let ((legacy-name (symbol-munger:lisp->camel-case (string name)))
        (docstring (when (stringp (first body)) (first body)))
        (body (if (stringp (first body)) (rest body) body))
        (infinity-name (intern (concatenate 'string "INFINITY-" (string name))))
        (λ-list (if (eql '&rest (first lambda-list))
                    lambda-list
                    (cons '&key lambda-list))))
    (push (cons (string name) infinity-name) *infinity-ops*)
    (push (cons legacy-name infinity-name) *infinity-ops*)
    `(progn
       (defun ,infinity-name (d ,user-var ,plane-var)
         ,docstring
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
         ,docstring
         (call-infinity-from-rest  ',infinity-name )))))

(defpackage Tootsville-User
  (:use :CL :CL-User :Bordeaux-Threads :Tootsville))

(defmacro define-operator-command (command (words user plane) &body body)
  (let ((docstring (if (stringp (first body)) (first body)
                       (format nil "Undocumented operator command ~a" command)))
        (body (if (stringp (first body)) (rest body) body))
        (command (if (find-symbol (string command) :CL)
                     (concatenate 'string "*" (string command))
                     command)))
    `(progn
       (defun ,(intern (string command) (find-package :Tootsville-User)) (&rest ,words)
         ,docstring
         (declare (ignorable ,words))
         (let ((,user *user*) (,plane (user-plane *user*)))
           (declare (ignorable ,user ,plane))
           (let ((reply (block nil (progn ,@body))))
             (when (stringp reply)
               (private-admin-message
                ,(concatenate 'string #(#\#) (string-downcase command))
                reply)))))))) 

(defendpoint (POST "/world/infinity" "application/json")
  "Dispatch an Infinity-mode JSON packet to its handler based on the @code{c} parameter.

See `DEFINFINITY' for a detailed discussion of this mode of operation."
  (with-posted-json (c d)
    (with-user ()
      (let ((*Toot* (or *Toot* (find-active-Toot-for-user))))
        (funcall (find-symbol (concatenate 'string "INFINITY-"
                                           (string-upcase (symbol-munger:camel-case->lisp-name c)))
                              :Tootsville)
                 d *user* (user-plane *user*))))))
