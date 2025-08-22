;;;; -*- lisp -*-
;;;
;;;; t/test-auth.lisp is part of Tootsville
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

;;; Authentication and authorization tests

(in-suite auth-tests)

(test user-authentication
  "Test user authentication functionality"
  (with-test-database
    (let* ((user (make-test-user))
           (credentials (make-test-credentials))
           (auth-result (Tootsville::authenticate-user (getf user :email) "password")))
      (is (not (null auth-result)) "Should authenticate user")
      (is (getf auth-result :authenticated) "Should mark user as authenticated")
      (is (getf auth-result :session-token) "Should provide session token"))))

(test user-registration
  "Test user registration functionality"
  (with-test-database
    (let* ((registration-data (list :email "newuser@example.com" :password "securepassword" :name "New User"))
           (registration-result (Tootsville::register-user registration-data)))
      (is (not (null registration-result)) "Should register user")
      (is (getf registration-result :user-id) "Should provide user ID")
      (is (getf registration-result :success) "Should mark registration as successful"))))

(test password-validation
  "Test password validation"
  (is (Tootsville::validate-password "SecurePass123!") "Should validate strong password")
  (is (not (Tootsville::validate-password "weak")) "Should reject weak password")
  (is (not (Tootsville::validate-password "")) "Should reject empty password")
  (is (not (Tootsville::validate-password "123456789")) "Should reject common password"))

(test session-management
  "Test session management"
  (with-test-database
    (let* ((user (make-test-user))
           (session (Tootsville::create-session (getf user :uuid))))
      (is (not (null session)) "Should create session")
      (is (getf session :session-id) "Should have session ID")
      (is (getf session :user-id) "Should have user ID")
      (is (getf session :expires-at) "Should have expiration time")
      
      (let ((validated-session (Tootsville::validate-session (getf session :session-id))))
        (is (not (null validated-session)) "Should validate session"))
      
      (Tootsville::invalidate-session (getf session :session-id))
      (is (null (Tootsville::validate-session (getf session :session-id))) "Should invalidate session"))))

(test token-management
  "Test token management"
  (with-test-database
    (let* ((user (make-test-user))
           (token (Tootsville::generate-access-token (getf user :uuid))))
      (is (not (null token)) "Should generate access token")
      (is (stringp (getf token :token)) "Should have token string")
      (is (getf token :expires-at) "Should have expiration time")
      
      (let ((validated-token (Tootsville::validate-access-token (getf token :token))))
        (is (not (null validated-token)) "Should validate access token"))
      
      (Tootsville::revoke-access-token (getf token :token))
      (is (null (Tootsville::validate-access-token (getf token :token))) "Should revoke access token"))))

(test permission-system
  "Test permission system"
  (with-test-database
    (let* ((user (make-test-user))
           (permission "read:world")
           (role "player"))
      (Tootsville::assign-role (getf user :uuid) role)
      (Tootsville::grant-permission (getf user :uuid) permission)
      
      (is (Tootsville::has-permission (getf user :uuid) permission) "Should have granted permission")
      (is (Tootsville::has-role (getf user :uuid) role) "Should have assigned role")
      
      (Tootsville::revoke-permission (getf user :uuid) permission)
      (is (not (Tootsville::has-permission (getf user :uuid) permission)) "Should revoke permission"))))

(test role-based-access-control
  "Test role-based access control"
  (with-test-database
    (let* ((admin-user (make-test-user :email "admin@example.com"))
           (regular-user (make-test-user :email "user@example.com")))
      (Tootsville::assign-role (getf admin-user :uuid) "admin")
      (Tootsville::assign-role (getf regular-user :uuid) "player")
      
      (is (Tootsville::can-access-resource (getf admin-user :uuid) "admin:panel") "Admin should access admin panel")
      (is (not (Tootsville::can-access-resource (getf regular-user :uuid) "admin:panel")) "Regular user should not access admin panel"))))

(test two-factor-authentication
  "Test two-factor authentication"
  (with-test-database
    (let* ((user (make-test-user))
           (totp-secret (Tootsville::generate-totp-secret (getf user :uuid))))
      (is (not (null totp-secret)) "Should generate TOTP secret")
      (is (stringp (getf totp-secret :secret)) "Should have secret string")
      (is (getf totp-secret :qr-code) "Should have QR code")
      
      (let ((totp-code (Tootsville::generate-totp-code (getf totp-secret :secret))))
        (is (Tootsville::validate-totp-code (getf totp-secret :secret) totp-code) "Should validate TOTP code")))))

(test oauth-integration
  "Test OAuth integration"
  (with-test-database
    (let* ((provider "google")
           (oauth-token "test-oauth-token")
           (oauth-result (Tootsville::authenticate-oauth provider oauth-token)))
      (is (not (null oauth-result)) "Should authenticate via OAuth")
      (is (getf oauth-result :provider) "Should have provider information")
      (is (getf oauth-result :user-id) "Should have user ID"))))

(test rate-limiting
  "Test authentication rate limiting"
  (with-test-database
    (let* ((user (make-test-user))
           (client-ip "192.168.1.1"))
      (dotimes (i 10)
        (Tootsville::record-login-attempt client-ip))
      
      (is (Tootsville::is-rate-limited client-ip) "Should rate limit after multiple attempts")
      
      (Tootsville::reset-rate-limit client-ip)
      (is (not (Tootsville::is-rate-limited client-ip)) "Should reset rate limit"))))

(test account-lockout
  "Test account lockout functionality"
  (with-test-database
    (let* ((user (make-test-user))
           (wrong-password "wrongpassword"))
      (dotimes (i 5)
        (Tootsville::record-failed-login (getf user :uuid)))
      
      (is (Tootsville::is-account-locked (getf user :uuid)) "Should lock account after failed attempts")
      
      (Tootsville::unlock-account (getf user :uuid))
      (is (not (Tootsville::is-account-locked (getf user :uuid))) "Should unlock account"))))

(test password-reset
  "Test password reset functionality"
  (with-test-database
    (let* ((user (make-test-user))
           (reset-token (Tootsville::generate-password-reset-token (getf user :uuid))))
      (is (not (null reset-token)) "Should generate reset token")
      (is (stringp (getf reset-token :token)) "Should have token string")
      (is (getf reset-token :expires-at) "Should have expiration time")
      
      (let ((reset-result (Tootsville::reset-password (getf reset-token :token) "newpassword")))
        (is (not (null reset-result)) "Should reset password")
        (is (getf reset-result :success) "Should mark reset as successful")))))

(test audit-logging
  "Test authentication audit logging"
  (with-test-database
    (let* ((user (make-test-user))
           (action "login")
           (audit-entry (Tootsville::log-auth-action (getf user :uuid) action "192.168.1.1")))
      (is (not (null audit-entry)) "Should create audit entry")
      (is (getf audit-entry :user-id) "Should include user ID")
      (is (getf audit-entry :action) "Should include action")
      (is (getf audit-entry :ip-address) "Should include IP address")
      (is (getf audit-entry :timestamp) "Should include timestamp"))))

(test security-policies
  "Test security policy enforcement"
  (with-test-database
    (let* ((user (make-test-user))
           (policy (list :min-password-length 8 :require-special-chars t :max-login-attempts 3)))
      (Tootsville::set-security-policy (getf user :uuid) policy)
      
      (let ((user-policy (Tootsville::get-security-policy (getf user :uuid))))
        (is (equal policy user-policy) "Should set and retrieve security policy"))
      
      (is (not (Tootsville::validate-password-against-policy "weak" policy)) "Should reject weak password")
      (is (Tootsville::validate-password-against-policy "StrongPass123!" policy) "Should accept strong password"))))

(test multi-factor-authentication
  "Test multi-factor authentication"
  (with-test-database
    (let* ((user (make-test-user))
           (mfa-setup (Tootsville::setup-mfa (getf user :uuid))))
      (is (not (null mfa-setup)) "Should setup MFA")
      (is (getf mfa-setup :enabled) "Should enable MFA")
      (is (getf mfa-setup :backup-codes) "Should provide backup codes")
      
      (let ((mfa-verification (Tootsville::verify-mfa (getf user :uuid) "123456")))
        (is (not (null mfa-verification)) "Should verify MFA code")))))

(test session-timeout
  "Test session timeout functionality"
  (with-test-database
    (let* ((user (make-test-user))
           (session (Tootsville::create-session (getf user :uuid) :timeout 60)))
      (is (not (null session)) "Should create session with timeout")
      (is (getf session :expires-at) "Should have expiration time")
      
      (is (Tootsville::is-session-expired session) "Should check session expiration")
      
      (Tootsville::extend-session (getf session :session-id) 120)
      (let ((extended-session (Tootsville::get-session (getf session :session-id))))
        (is (> (getf extended-session :expires-at) (getf session :expires-at)) "Should extend session timeout")))))

(test authentication-error-handling
  "Test authentication error handling"
  (with-test-database
    (let ((invalid-credentials (list :email "nonexistent@example.com" :password "wrong")))
      (signals authentication-error (Tootsville::authenticate-user (getf invalid-credentials :email) (getf invalid-credentials :password)) "Should signal error for invalid credentials")
      
      (let ((invalid-registration (list :email "invalid-email" :password "weak")))
        (signals validation-error (Tootsville::register-user invalid-registration) "Should signal validation error for invalid registration")))))

(test authentication-performance
  "Test authentication performance"
  (with-test-database
    (let ((start-time (get-internal-real-time)))
      (dotimes (i 100)
        (let ((user (make-test-user :email (format nil "perf~a@example.com" i))))
          (Tootsville::hash-password "testpassword")))
      (let ((end-time (get-internal-real-time)))
        (is (< (- end-time start-time) 1000) "Should hash 100 passwords within reasonable time")))))

(test authentication-security
  "Test authentication security measures"
  (with-test-database
    (let* ((user (make-test-user))
           (password "testpassword")
           (hashed-password (Tootsville::hash-password password)))
      (is (not (equal password hashed-password)) "Should hash password securely")
      (is (Tootsville::verify-password password hashed-password) "Should verify password correctly")
      (is (not (Tootsville::verify-password "wrongpassword" hashed-password)) "Should reject wrong password"))))

(test authentication-cleanup
  "Test authentication cleanup"
  (with-test-database
    (let* ((user (make-test-user))
           (session (Tootsville::create-session (getf user :uuid))))
      (Tootsville::cleanup-expired-sessions)
      (Tootsville::cleanup-expired-tokens)
      (Tootsville::cleanup-failed-logins)
      (is (<= (length (Tootsville::active-sessions)) 1) "Should cleanup expired sessions"))))


