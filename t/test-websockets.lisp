;;;; -*- lisp -*-
;;;
;;;; t/test-websockets.lisp is part of Tootsville
;;;
;;;; Copyright © 2008-2017 Bruce-Robert Pocock; © 2018-2024 The
;;;; Corporation for Inter-World Tourism and Adventuring (CIWTA.org); © 2025
;;;; Interworldly Adventuring, LLC
;;;
;;;; This program is Free Software: you can redistribute it and/or
;;;; modify it under the terms of the GNU Affero General Public License
;;;; as published by the Free Software Foundation; either version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;;; Affero General Public License for more details.
;;;
;;;; You should have received a copy of the GNU Affero General Public
;;;; License along with this program. If not, see
;;;; <https://www.gnu.org/licenses/>.

(in-package :Tootsville.test)

;;; WebSocket communication tests

(in-suite websockets-tests)

(test websocket-stats-reset
  "Test WebSocket statistics reset"
  (let ((original-connections *ws-connections*)
        (original-sign-ins *ws-sign-ins*))
    (Tootsville::ws-stats-reset-all)
    (is (zerop *ws-connections*) "Should reset connection count")
    (is (zerop *ws-sign-ins*) "Should reset sign-in count")
    (setf *ws-connections* original-connections
          *ws-sign-ins* original-sign-ins)))

(test websocket-bandwidth-tracking
  "Test WebSocket bandwidth tracking"
  (let ((test-command "test-command")
        (test-source "test-client"))
    (incf (gethash test-command *ws-traffic-commands* 0) 100)
    (incf (gethash test-source *ws-traffic-from* 0) 50)
    (incf *ws-traffic-other* 25)
    
    (let ((bandwidth-report (Tootsville::ws-bandwidth-by-source)))
      (is (stringp bandwidth-report) "Should return string report")
      (is (search "test-command" bandwidth-report) "Should include command traffic")
      (is (search "test-client" bandwidth-report) "Should include source traffic"))))

(test websocket-connection-limits
  "Test WebSocket connection limits"
  (is (= 5 Tootsville::+pre-login-max-time+) "Should have 5 second pre-login timeout")
  (is (= 10 Tootsville::+pre-login-max-commands+) "Should allow 10 pre-login commands")
  (is (= 300 Tootsville::+ws-idle-seconds+) "Should have 300 second idle timeout"))

(test websocket-message-handling
  "Test WebSocket message handling"
  (with-test-database
    (let* ((test-message (list :type "ping" :payload (list :timestamp 123456789)))
           (result (Tootsville::handle-message test-message)))
      (is (not (null result)) "Should handle ping message"))))

(test websocket-authentication-flow
  "Test WebSocket authentication flow"
  (with-test-database
    (let* ((credentials (make-test-credentials))
           (auth-message (list :type "authenticate" :payload credentials))
           (result (Tootsville::handle-authentication auth-message)))
      (is (not (null result)) "Should process authentication message"))))

(test websocket-client-management
  "Test WebSocket client management"
  (with-test-database
    (let* ((client-id (uuid:make-v4-uuid))
           (client (Tootsville::create-client client-id)))
      (is (not (null client)) "Should create client")
      (is (uuid:uuid= client-id (Tootsville::client-id client)) "Should have correct client ID"))))

(test websocket-message-broadcasting
  "Test WebSocket message broadcasting"
  (with-test-database
    (let* ((message (list :type "broadcast" :payload "Hello World"))
           (clients (list (make-test-user) (make-test-user)))
           (result (Tootsville::broadcast-message message clients)))
      (is (not (null result)) "Should broadcast message to clients"))))

(test websocket-message-unicasting
  "Test WebSocket message unicasting"
  (with-test-database
    (let* ((message (list :type "private" :payload "Secret message"))
           (target-client (make-test-user))
           (result (Tootsville::unicast-message message target-client)))
      (is (not (null result)) "Should unicast message to target client"))))

(test websocket-connection-lifecycle
  "Test WebSocket connection lifecycle"
  (with-test-database
    (let* ((client-id (uuid:make-v4-uuid))
           (client (Tootsville::create-client client-id)))
      (is (not (null client)) "Should create client")
      (Tootsville::connect-client client)
      (is (Tootsville::client-connected-p client) "Should mark client as connected")
      (Tootsville::disconnect-client client)
      (is (not (Tootsville::client-connected-p client)) "Should mark client as disconnected"))))

(test websocket-error-handling
  "Test WebSocket error handling"
  (with-test-database
    (let ((invalid-message nil))
      (signals error (Tootsville::handle-message invalid-message) "Should handle nil message")
      
      (let ((malformed-message (list :invalid "format")))
        (signals error (Tootsville::handle-message malformed-message) "Should handle malformed message")))))

(test websocket-rate-limiting
  "Test WebSocket rate limiting"
  (with-test-database
    (let* ((client (make-test-user))
           (message (list :type "spam" :payload "repeated message")))
      (dotimes (i 20)
        (Tootsville::send-message client message))
      (is (Tootsville::client-rate-limited-p client) "Should rate limit excessive messages"))))

(test websocket-message-validation
  "Test WebSocket message validation"
  (with-test-database
    (let ((valid-message (list :type "valid" :payload "data" :timestamp 123456789)))
      (is (Tootsville::valid-message-p valid-message) "Should validate correct message format")
      
      (let ((invalid-message (list :type "invalid")))
        (is (not (Tootsville::valid-message-p invalid-message)) "Should reject invalid message format")))))

(test websocket-performance
  "Test WebSocket performance under load"
  (with-test-database
    (let ((start-time (get-internal-real-time))
          (clients (loop for i from 1 to 100 collect (make-test-user))))
      (dolist (client clients)
        (Tootsville::send-message client (list :type "performance" :payload "test")))
      (let ((end-time (get-internal-real-time)))
        (is (< (- end-time start-time) 1000) "Should handle 100 messages within reasonable time")))))

(test websocket-memory-management
  "Test WebSocket memory management"
  (with-test-database
    (let ((clients (loop for i from 1 to 50 collect (make-test-user))))
      (dolist (client clients)
        (Tootsville::connect-client client))
      (Tootsville::cleanup-disconnected-clients)
      (is (<= (length (Tootsville::active-clients)) 50) "Should manage client memory properly"))))

(test websocket-security
  "Test WebSocket security measures"
  (with-test-database
    (let* ((malicious-message (list :type "inject" :payload "<script>alert('xss')</script>"))
           (sanitized-message (Tootsville::sanitize-message malicious-message)))
      (is (not (search "<script>" (getf sanitized-message :payload))) "Should sanitize malicious content"))))

(test websocket-compression
  "Test WebSocket message compression"
  (with-test-database
    (let* ((large-message (list :type "large" :payload (make-string 10000 :initial-element #\x)))
           (compressed (Tootsville::compress-message large-message))
           (decompressed (Tootsville::decompress-message compressed)))
      (is (< (length (getf compressed :payload)) (length (getf large-message :payload))) "Should compress large messages")
      (is (equal (getf large-message :payload) (getf decompressed :payload)) "Should decompress correctly"))))

(test websocket-heartbeat
  "Test WebSocket heartbeat mechanism"
  (with-test-database
    (let* ((client (make-test-user))
           (heartbeat (list :type "ping" :payload (list :timestamp (get-universal-time)))))
      (Tootsville::send-heartbeat client)
      (is (Tootsville::client-alive-p client) "Should mark client as alive after heartbeat")
      
      (Tootsville::mark-client-idle client)
      (is (Tootsville::client-idle-p client) "Should mark idle client"))))

(test websocket-reconnection
  "Test WebSocket reconnection handling"
  (with-test-database
    (let* ((client (make-test-user))
           (session-data (list :user (getf client :uuid) :world "CHOR")))
      (Tootsville::store-session-data client session-data)
      (Tootsville::disconnect-client client)
      
      (let ((reconnected-client (Tootsville::reconnect-client (getf client :uuid))))
        (is (not (null reconnected-client)) "Should allow client reconnection")
        (is (equal session-data (Tootsville::restore-session-data reconnected-client)) "Should restore session data")))))

(test websocket-load-balancing
  "Test WebSocket load balancing"
  (with-test-database
    (let ((servers (list "server1" "server2" "server3"))
          (client (make-test-user)))
      (let ((assigned-server (Tootsville::assign-server client servers)))
        (is (member assigned-server servers :test #'string=) "Should assign client to available server")))))

(test websocket-monitoring
  "Test WebSocket monitoring and metrics"
  (with-test-database
    (let ((metrics (Tootsville::collect-websocket-metrics)))
      (is (listp metrics) "Should collect metrics")
      (is (getf metrics :active-connections) "Should track active connections")
      (is (getf metrics :total-messages) "Should track total messages")
      (is (getf metrics :error-rate) "Should track error rate"))))

(test websocket-graceful-shutdown
  "Test WebSocket graceful shutdown"
  (with-test-database
    (let ((clients (loop for i from 1 to 10 collect (make-test-user))))
      (dolist (client clients)
        (Tootsville::connect-client client))
      
      (Tootsville::initiate-graceful-shutdown)
      (is (zerop (length (Tootsville::active-clients))) "Should disconnect all clients gracefully"))))
