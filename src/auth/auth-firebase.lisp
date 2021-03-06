;;;; -*- lisp -*-
;;;
;;;; src/auth/auth-firebase.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018-2021  The
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

(defparameter *google-account-keys-refresh* (* 20 60)
  "How  often (in  sec)  to  refresh the  Google  account  keys?
 
These are used in Firebase  authentication verification, but only in the
event  Cache-Control:  max-age  is  not   set  on  the  keys,  which  it
usually is.")

(defun subheader-field (header-assoc label)
  (when header-assoc
    (let* ((label* (concatenate 'string label ":"))
           (len (length label*))
           (finds
             (mapcar
              (compose #'second (curry #'split-sequence #\:))
              (remove-if-not
               (lambda (section)
                 (and (> (length section) len)
                      (string-equal label* section
                                    :end2 len)))
               (mapcar
                (curry #'string-trim +whitespace+)
                (split-sequence #\, (cdr header-assoc)))))))
      (case (length finds)
        (0 nil)
        (1 (string-trim +whitespace+ (first finds)))
        (otherwise (warn "Multiple sub-header hits in ~:(~a~) for ~(~a~)"
                         (car header-assoc) label)
         (string-trim +whitespace+ (first finds)))))))

(defpost subheader-field-parses ()
  (equal "123"
         (subheader-field '(:cache-control . "max-age: 123, foo: bar")
                          "max-age")))

(defun compute-next-keys-update (headers-alist)
  (timestamp+ (now)
              (or (when-let ((n (subheader-field (assoc :cache-control
                                                        headers-alist)
                                                 "max-age")))
                    (parse-integer n))
                  *google-account-keys-refresh*)
              :sec))

(defun bytes-json (json-bytes)
  (jonathan.decode:parse
   (map 'string #'code-char json-bytes)))

(defun http-is-success-p (http-status)
  (typep http-status 'http-response-success-status-number))

(let ((keys nil)
      (keys-update-next (timestamp- (now) 1 :year)))
  (defun get-google-account-keys ()
    (when (timestamp< (now) keys-update-next)
      (return-from get-google-account-keys keys))
    (multiple-value-bind
          (json-bytes http-status headers-alist reply-uri)
        (drakma:http-request
         "https://www.googleapis.com/robot/v1/metadata/x509/securetoken@system.gserviceaccount.com"
         :accept "application/json")
      (when (http-is-success-p http-status)
        (assert (string-ends ".googleapis.com"
                             (puri:uri-host (puri:parse-uri reply-uri)))
                (reply-uri)
                "Google Firebase keys query did not come from GoogleAPIs.com ~
 — could this be a man-in-the-middle attack?")
        (setf keys (bytes-json json-bytes)
              keys-update-next
              (compute-next-keys-update headers-alist))
        (v:info '(:auth :firebase :google-account-keys)
                "Fetched ~r Google account key~:p; next refetch ~a"
                (/ (length keys) 2)
                keys-update-next)
        keys))))

(defun extract-certificate-base64 (string)
  "Base64-decode the certificate in STRING between BEGIN CERTIFICATE header and END CERTIFICATE footer lines."
  (let ((soh (search "-----BEGIN CERTIFICATE-----" string))
        (sof (search "-----END CERTIFICATE-----" string :from-end t)))
    (assert (and soh sof (< soh sof)))
    (let* ((hi (position #\Newline (subseq string soh)))
           (fi (- sof 1)))
      (assert (and hi fi (< hi fi)))
      (base64:base64-string-to-usb8-array (subseq string hi fi)))))

(defpost certificate-extraction ()
  (equalp #(105 106 154)
          (extract-certificate-base64
           "-----BEGIN CERTIFICATE-----
aWqa
-----END CERTIFICATE-----")))

(defun base64-from-uri-form (token)
  (substitute #\+ #\-
              (substitute #\/ #\_ token)))

(defun pad-to-multiple-of-8 (string)
  (let ((rem (mod (length string) 8)))
    (if (zerop rem)
        string
        (concatenate 'string string
                     (make-string (- 8 rem) :initial-element #\=)))))

(defun base64-decode% (string)
  (base64:base64-string-to-usb8-array
   (base64-from-uri-form
    (pad-to-multiple-of-8 string))))

(defun check-firebase-id-token (token)
  (v:warn :login "Token from Firebase: ~s" token)
  (handler-case
      (multiple-value-bind (claims header digest)
          (cljwt-custom:unpack token)
        (let ((google-account-keys (get-google-account-keys)))
          (extract-certificate-base64
           (extract google-account-keys
                    (make-keyword (gethash "kid" header))))
          ;; FIXME: JWT verification
          #+ (or)
          (multiple-value-bind (payload-claims payload-header)
              (handler-bind
                  ((cljwt-custom:invalid-rs256-signature
                     (lambda (c)
                       (declare (ignore c))
                       (invoke-restart 'continue))))
                (cljwt-custom:verify token
                                     (crypto:make-cipher
                                      :rc5 :mode :ces
                                      :key (base64-decode% digest))
                                     (gethash "alg" header)
                                     :fail-if-unsecured t
                                     :fail-if-unsupported t)))
          (when (gethash "exp" header)
            (assert (> (gethash "exp" header) (timestamp-to-unix (now))) (token)
                    "Credential token has expired"))
          (when (gethash "iat" header)
            (assert (< (gethash "iat" header) (timestamp-to-unix (now)))
                    (token)
                    "Credential token will be issued in the future. ~
 You must be punished for violating causality."))
          (when (gethash "auth_time" header)
            (assert (< (gethash "auth_time" header) (timestamp-to-unix (now)))
                    (token)
                    "Credential token  is from  a future  user authentication. ~
 You must be punished for violating causality."))
          (assert (string= (gethash "aud" header)
                           (config :firebase :project-id))
                  (token)
                  "Credential  token  was  not  for   us  (we  are  not  ~
 its audience)")
          (let ((credentials
                  (list :credentials (append (hash-table-plist (extract claims "firebase" "identities"))
                                             (list "firebase" (list (gethash "sub" claims))))
                        :email (gethash "email" claims)
                        :email-verified-p (gethash "email_verified" claims)
                        :name (gethash "name" claims)
                        :picture (gethash "picture" claims))))
            (v:info :Firebase "Credentials from Firebase: ~S" credentials)
            credentials)))
    ;; (cl-base64:bad-base64-character (c)
    ;;   (v:warn :Firebase "Error extracting credentials, ~S ~:*~A" c)
    ;;   nil)
    ))
