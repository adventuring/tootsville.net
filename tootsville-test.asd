(in-package :cl-user)
(defpackage Tootsville-test-asd
  (:use :cl :asdf))
(in-package :Tootsville-test-asd)

(defsystem Tootsville-test
  :author "Bruce-Robert Pocock <BRFennPocock@star-hope.org>"
  :license "AGPL-3.0"
  :depends-on (:Tootsville
               :fiveam
               :fiveam-matchers
               :mock)
  :components ((:module "t"
                        :components
                        ((:file "test-suite")
                         (:file "test-users")
                         (:file "test-websockets")
                         (:file "test-items")
                         (:file "test-terrain")
                         (:file "test-world")
                         (:file "test-metronome")
                         (:file "test-utils")
                         (:file "test-auth"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
