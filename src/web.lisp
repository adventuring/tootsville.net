;;;; -*- lisp -*-
;;;
;;;; ./servers/src/web.lisp is part of Tootsville
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


(defun accepts-content-type-p (content-type)
  "Does the current Hunchentoot request Accept: CONTENT-TYPE?"
  (etypecase content-type
    (string (string-equal content-type (hunchentoot:header-in* :accept)))
    (cons (member (hunchentoot:header-in* :accept)
                  content-type
                  :test #'string-equal))))



(defun wants-json-p ()
  "Does the client request Accept JSON format?

Looks for  the canonical  \"Accept: application/json\", and  also checks
the request URI for \".js\" (which  is, of course, a subseq of \".json\"
as well.)"
  (or (search "application/json" (hunchentoot:header-in* :accept))
      (search ".js" (hunchentoot:request-uri*))))



(defun contents-to-bytes (contents)
  "Convert CONTENTS to a sequence of 8-bit bytes"
  (etypecase contents
    (string (flexi-streams:string-to-octets contents :external-format :utf-8))
    (vector contents)
    (list (flexi-streams:string-to-octets (jonathan:to-json contents)
                                          :external-format :utf-8))))

(defun encode-endpoint-reply (reply)
  "Handle the reply from an endpoint function gracefully."
  (let ((content-bytes #()))
    (cond
      ((stringp reply)
       (setf (hunchentoot:return-code*) 200
             content-bytes (flexi-streams:string-to-octets reply
                                                           :external-format :utf-8)))
      ((vectorp reply)
       (setf (hunchentoot:return-code*) 200
             content-bytes reply))
      ((and (listp reply) (not (numberp (first reply))))
       (setf (hunchentoot:return-code*) 200
             content-bytes (contents-to-bytes reply)))
      ((= 2 (length reply))
       (destructuring-bind (status contents) reply
         (check-type status http-response-status-number)
         (setf (hunchentoot:return-code*) status
               content-bytes (contents-to-bytes contents))))
      ((= 3 (length reply))
       (destructuring-bind (status headers contents) reply
         (check-type status http-response-status-number)
         (assert (every (lambda (x)
                          (or (stringp x) (symbolp x)))
                        headers)
                 (headers)
                 "Headers should be given as strings or symbols; got ~s"
                 headers)
         (loop for (header . value) on headers by #'cddr
            do (setf (hunchentoot:header-out header)
                     (atom-or-comma-list value)))
         (setf (hunchentoot:return-code*) status
               content-bytes (contents-to-bytes contents)))))
    content-bytes))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun apply-extension-to-template (template extension)
    "Create a clone of TEMPLATE with EXTENSION."
    (if template
        (let ((temp (append template (list extension))))
          (if (first temp)
              temp
              (rest temp)))
        (list "index" extension)))

  (defun without-sem (string)
    "The subset of STRING up to the first semicolon, if any."
    (if-let (sem (position #\; string))
      (subseq string 0 sem)
      string))

  (defun first-line (string)
    "The first line, or, lacking a shorter break, first 100 characters of STRING."
    (let ((newline (or (position #\newline string) 100)))
      (subseq string 0 (min newline 100 (length string)))))

  (defun defendpoint/make-endpoint-function (&key fname content-type
                                                  λ-list docstring body
                                                  (how-slow-is-slow .03))
    (let (($begin (gensym "BEGIN-"))
          ($elapsed (gensym "ELAPSED-")))
      `(defun ,fname (,@λ-list) ,docstring
              (let ((,$begin (get-internal-real-time)))
                (v:info '(,(make-keyword fname) :endpoint :endpoint-start)
                        ,(concatenate 'string "Starting: " (first-line docstring)))
                (setf (hunchentoot:content-type*) ,(add-charset (string-downcase content-type)))
                (unwind-protect
                     (let ((reply
                            (catch 'endpoint
                              (block endpoint
                                (with-timeout (,(* how-slow-is-slow 10))
                                  (block ,fname
                                    ,@body))))))
                       (let ((bytes (encode-endpoint-reply reply)))
                         (v:info '(,(make-keyword fname) :endpoint :endpoint-output)
                                 "Status: ~d; ~[~:;~:*~d header~:p; ~]~d octets"
                                 
                                 (hunchentoot:return-code*)
                                 (length (the list (hunchentoot:headers-out*)))
                                 (length (the vector bytes)))
                         bytes))
                  (let ((,$elapsed (/ (- (get-internal-real-time) ,$begin) internal-time-units-per-second)))
                    (v:info '(,(make-keyword fname) :endpoint :endpoint-finish)
                            ,(concatenate 'string "Finished: " (first-line docstring) " in ~,3fs")
                            (* 1.0 ,$elapsed))
                    (when (< ,how-slow-is-slow ,$elapsed)
                      (run-async  
                       (lambda ()
                         (v:error '(,(make-keyword fname) :endpoint :slow-query)
                                  "Slow query ~s took ~,3fs (>~,3fs allowed)"
                                  ',fname (* 1.0 ,$elapsed) ,how-slow-is-slow)
                         (rollbar:error!
                          (format nil "Slow query ~s took ~,3fs (>~,3fs allowed)"
                                  ',fname (* 1.0 ,$elapsed) ,how-slow-is-slow)))
                       :name "Report slow query"))))))))

  (defun after-slash (s)
    "Splits a string S at a slash. Useful for getting the end of a content-type."
    (if (find #\/ s)
        (subseq (string-downcase s) (1+ (position #\/ s)))
        (string-downcase s)))

  (defmacro check-arg-type (arg type &optional name)
    "Ensure that ARG  is of type TYPE, which is  called NAME. Signals back
to an HTTP client with a 400 error if this assertion is untrue.

This is basically just CHECK-TYPE for arguments passed by the user."
    `(unless (typep ,arg ',type)
       (return-from endpoint
         (list 400
               (list :content-type "application/json")
               (list :error
                     ,(format nil "Value provided for ~:(~a~) is not a valid ~a"
                              arg (or name (string-capitalize type)))
                     :expected-type ,(or name (string-capitalize type))
                     :argument-name ,(string-capitalize arg)
                     :provided-value(format nil "~s" ,arg))))))

  (defvar *extensions-for-content-types*
    '(
      :application/ecmascript "es"
      :application/epub+zip "epub"
      :application/java-archive "jar"
      :application/javascript "js"
      :application/json "json"
      :application/msword "doc"
      :application/octet-stream "bin"
      :application/pdf "pdf"
      :application/postscript "ps"
      :application/rtf "rtf"
      :application/tar "tar"
      :application/vnd.amazon.ebook "azw"
      :application/vnd.apple.installer+xml "mpkg"
      :application/vnd.mozilla.xul+xml "xul"
      :application/vnd.ms-fontobject "eot"
      :application/vnd.oai.openapi "json"
      :application/vnd.oasis.opendocument.presentation "odp"
      :application/vnd.oasis.opendocument.spreadsheet "ods"
      :application/vnd.oasis.opendocument.text "odt"
      :application/vnd.openxmlformats-officedocument.wordprocessingml.document "docx"
      :application/x-7z-compressed "7z"
      :application/x-abiword "abw"
      :application/x-bcpio "bcpio"
      :application/x-bzip "bz"
      :application/x-bzip2 "bz2"
      :application/x-compress "z"
      :application/x-cpio "cpio"
      :application/x-csh "csh"
      :application/x-gzip "gz"
      :application/x-latex "tex"
      :application/x-rar-compressed "rar"
      :application/x-sh "sh"
      :application/x-shar "shar"
      :application/x-texinfo "texi"
      :application/x-troff "roff"
      :application/x-troff-man "man"
      :application/x-troff-me "me"
      :application/x-troff-ms "ms"
      :application/xhtml+xml "xhtml"
      :application/xml "xml"
      :application/zip "zip"
      :audio/3gpp "3gp"            ; * same as video, use care
      :audio/3gpp2 "3g2"           ; * same as audio, use care
      :audio/aac "aac"
      :audio/basic "au"
      :audio/midi "midi"
      :audio/ogg "oga"
      :audio/wav "wav"
      :audio/webm "weba"
      :audio/x-aiff "aiff"
      :audio/x-mpegurl "m3u"
      :font/otf "otf"
      :font/ttf "ttf"
      :font/woff "woff"
      :font/woff2 "woff2"
      :image/gif "gif"
      :image/jpeg "jpg"
      :image/jpeg "jpg"
      :image/png "png"
      :image/png "png"
      :image/png "png"
      :image/svg "svg"
      :image/svg "svg"
      :image/tiff "tiff"
      :image/webp "webp"
      :image/x-icon "ico"
      :image/x-xbitmap "xbm"
      :image/x-xpixmap "xpm"
      :message/rfc822 "mbox"
      :text/calendar "ics"
      :text/css "css"
      :text/csv "csv"
      :text/html "html"
      :text/plain "txt"
      :text/richtext "rtx"
      :text/tab-separated-values "tsv"
      :text/x-vcard "vcf"
      :video/3gpp "3gp"
      :video/3gpp2 "3g2"
      :video/mp4 "mp4"
      :video/mpeg "mpeg"
      :video/ogg "ogv"
      :video/quicktime "qt"
      :video/webm "webm"
      :video/x-ms-asf "asf"
      :video/x-msvideo "avi"
      :video/x-sgi-movie "movie"
      :x-world/x-vrml "vrml"
      ))

  (defun extension-for-content-type (content-type)
    (getf *extensions-for-content-types*
          (make-keyword (string-upcase (without-sem content-type)))))

  (defun name-for-content-type (content-type)
    (or (extension-for-content-type content-type)
        (after-slash content-type)))

  (defun atom-or-comma-list (value)
    (cond
      ((atom value) value)
      ((= 1 (length value)) (first value))
      (t (format nil "~{~a~^, ~}" value))))

  (defun add-charset (content-type)
    "Adds the ;charset=UTF-8 type to the end of text and JS/JSON CONTENT-TYPEs"
    (if (member content-type
                '("text/plain" "text/html"
                  "application/javascript"
                  "application/json")
                :test 'string=)
        (concatenate 'string content-type "; charset=utf-8")
        content-type))

  (assert (equal (add-charset "text/html")
                 "text/html; charset=utf-8"))
  (assert (equal (add-charset "text/plain")
                 "text/plain; charset=utf-8"))
  (assert (equal (add-charset "application/javascript")
                 "application/javascript; charset=utf-8"))
  (assert (equal (add-charset "application/json")
                 "application/json; charset=utf-8"))
  (assert (equal (add-charset "image/png")
                 "image/png"))

  (defun constituentp (ch)
    "Is character CH a constituent character of a Lisp name {without escaping it}?"
    (let ((cc (char-code (char-upcase ch))))
      (or (< #xa0 cc)
          (<= (char-code #\A) cc (char-code #\Z))
          (<= (char-code #\0) cc (char-code #\9))
          (find ch "-/!?." :test #'char=))))

  (defun make-endpoint-function-name (method uri accept-type)
    "Create the name of the endpoint function for METHOD, URI, and ACCEPT-TYPE."
    (intern (format nil "ENDPOINT-~a-~a→~a"
                    method
                    (remove-if-not #'constituentp uri)
                    (etypecase accept-type
                      (null #\?)
                      (string (name-for-content-type accept-type))
                      (symbol (name-for-content-type (string accept-type)))))))

  (defun lambda-list-as-variables (λ-list)
    "Convert Λ-LIST into variables for an endpoint function."
    (if λ-list
        (cons 'list (mapcar (lambda (var)
                              (list 'quote var))
                            λ-list))
        'nil))

  (defmacro defendpoint ((method uri &optional content-type (how-slow-is-slow .03))
                         &body body)
    "Define an HTTP endpoint accessing URI via METHOD and accepting CONTENT-TYPE."
    (let* ((method (make-keyword (string-upcase method)))
           (content-type (make-keyword (string-upcase content-type)))
           (fname (make-endpoint-function-name method uri content-type))
           (template (parse-uri-as-template uri))
           (λ-list (mapcar (lambda (s)
                             (intern (symbol-name s) (symbol-package fname)))
                           (remove-if-not #'symbolp template)))
           (docstring (concatenate 
                       'string 
                       (if (and (consp body) (stringp (first body)))
                           (first body)
                           (format nil
                                   "Undocumented endpoint for ~a ~a → ~s"
                                   method uri content-type))
                       (format nil "~2%@subsection{Web Service Endpoint}
~2%This is a web service ~
endpoint accessed by the HTTP method ~a at the URI template ~a. ~
~:[The returned content-type is not specified.~;~
It returns a content-type of ~:*~(~a~).~]~2%~
~[~*There are no URI parameters.~
~;~{~a~} is a parameter from the URI.~
~:;The URI includes parameters: ~{~a~^, ~}.~]~
~2%It will report a slow response if it takes longer than ~f seconds
\(~:d milliseconds) to complete."
                               method uri content-type (length λ-list) λ-list 
                               how-slow-is-slow (round (* 1000 how-slow-is-slow))))))
      `(progn
         ,(defendpoint/make-endpoint-function
              :fname fname
            :content-type content-type
            :λ-list λ-list
            :docstring docstring
            :body body
            :how-slow-is-slow how-slow-is-slow)
         ,(when-let (extension (extension-for-content-type (string content-type)))
            `(add-or-replace-endpoint ',fname ,method
                                      ',(apply-extension-to-template template extension)
                                      ,content-type))
         (add-or-replace-endpoint ',fname ,method ',template ,content-type)
         ;; (format *trace-output* "~2& ★ New endpoint: ~a ~a → ~a~% All
         ;;         endpoints:  ~{~%  •~s~}"   method  uri  content-type
         ;;         (enumerate-endpoints))
         ))))



(defendpoint (get "/" text/html)
  "GET on the root redirects to the main web page for the cluster (eg, @url{https://Tootsville.org/})"
  (list 307 (list :location (format nil "https://www.~a.org/" (let ((cluster (cluster-name)))
                                                                (if (search "tootsville" cluster)
                                                                    cluster
                                                                    "test.tootsville.org")))) ""))

(defendpoint (get "/favicon" image/png)
  "Get the Tootsville logo as a PNG"
  (list 307 '(:location "https://Jumbo.Tootsville.org/Assets/Icons/favicon.png") ""))

(defendpoint (get "/favicon/ico")
  "Get the Tootsville logo in Windows Icon format"
  (list 307 '(:location "https://Jumbo.Tootsville.org/Assets/Icons/favicon.ico") ""))

(defendpoint (get "/favicon" image/gif)
  "Get the Tootsville logo as a GIF"
  (list 307 '(:location "https://Jumbo.Tootsville.org/Assets/Icons/favicon.gif") ""))



;;; Print-Object method for Hunchentoot requests


(defmethod print-object ((request hunchentoot:request) stream)
  "Print a Hunchentoot Request object nicely"
  (print-unreadable-object (request stream :type t)
    (princ (hunchentoot:request-method request) stream)
    (write-char #\Space stream)
    (princ (hunchentoot:request-uri request) stream)))



(defun query-string->plist (query-string)
  "Split an HTTP QUERY-STRING into a  PList.

Probably a duplicate of something done in Hunchentoot or Drakma?"
  (mapcan (lambda (pair)
            (destructuring-bind (key value)
                (split-sequence #\= pair)
              (list
               (make-keyword (substitute #\- #\_
                                         (string-upcase key)))
               value)))
          (split-sequence #\& query-string)))

(defun query-params ()
  "Get parameters from the query string of the current Hunchentoot request."
  (let ((uri (hunchentoot:request-uri*)))
    (when-let (qq (position #\? uri))
      (let* ((query-string (subseq uri qq)))
        (query-string->plist query-string)))))
