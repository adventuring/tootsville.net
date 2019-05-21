;;;; -*- lisp -*-
;;;
;;;; src/endpoints/slash-gossip.lisp is part of Tootsville
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

(defendpoint (get "/gossip/ice-servers" "application/json")
  "Obtain STUN/TURN server credentials for ICE"
  (with-user ()
    (list 200 ()
          (ice-credentials))))

(defendpoint (post "/gossip/offers" "application/sdp")
  "Provide a new offer. Body is an SDP offer. Reply will be an offer URI."
  (with-user ()
    (let ((sdp (jonathan.encode:%to-json
                (getf (jonathan.decode:parse
                                        ; no idea why I have to do it twice XXX
                       (jonathan.decode:parse
                        (map 'string #'code-char (hunchentoot:raw-post-data))))
                      :|offer|))))
      (enqueue-sdp-offer sdp)
      (list 202 (list :location "/gossip/offers")
            sdp))))

(defendpoint (get "/gossip/offers" "application/sdp")
  "Ask for any, arbitrary offer to potentially accept."
  (list 200 (dequeue-sdp-offer)))


