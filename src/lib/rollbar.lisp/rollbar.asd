(cl:in-package :cl-user)
(require 'asdf)
(asdf:defsystem :rollbar
  :description "CL support for reporting to Rollbar"
  :author "Bruce-Robert Pocock"
  :version "0.1"
  :maintainer "Bruce-Robert Pocock"
  :mailto "brpocock+rollbar-lisp@star-hope.org"
  :licence "BSD"
  :long-name "Rollbar access from Common Lisp"

  :depends-on (:alexandria :drakma :jonathan :trivial-backtrace)

  :encoding :utf-8

  :components ((:file "rollbar")))
