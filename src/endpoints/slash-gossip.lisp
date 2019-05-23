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
  "Obtain STUN/TURN server credentials for ICE."
  (with-user ()
    (list 200 ()
          (ice-credentials))))

(defendpoint (post "/gossip/offers" "application/sdp")
  "Provide a new offer. Body is an SDP offer. Reply will be an offer URI.

The offer URI will  be needed to retrieve the answer  to your offer from
whatever peer  may accept it. There  is no guarantee that  an offer will
be accepted."
  (with-user ()
    (let ((uri 
           (format nil "/gossip/answers/~a"
                   (enqueue-sdp-offer (raw-post-string)))))
      (list 201
            (list :location uri)
            (list :|location| uri)))))

(defendpoint (get "/gossip/offers" "application/json")
  "Ask for any, arbitrary offer to potentially accept.

Returns a JSON object with UUID (for answering) and SDP description."
  (with-user ()
    (list 200 () (dequeue-sdp-offer))))

(defendpoint (post "/gossip/answers/:uuid" "application/sdp")
  "Post an answer to a received SDP block"
  (make-record 'gossip-initiation :uuid uuid :answer (raw-post-string))
  (list 202 () #()))

(defendpoint (get "/gossip/answers/:uuid" "application/sdp" 31)
  "Read back the answer to an offer posted previously. 

COMET-type call may sleep up to 30s"
  (with-user ()
    (dotimes (_ 3000)
      (if-let ((record (find-record 'gossip-initiation 
                                    :uuid (uuid:make-uuid-from-string uuid))))
        (return-from endpoint (list 200 () (gossip-initiation-answer record)))
        (sleep 1/100)))
    (list 204 () #())))



