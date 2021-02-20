;;;; -*- lisp -*-
;;;
;;;; tootsville.asd is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  © 2018-2021  The
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

(cl:in-package :cl-user)

(defpackage Tootsville-ASD
  (:use :cl :asdf))
(in-package :Tootsville-ASD)



(defsystem Tootsville
  :version "0.6.12"
  :author "Bruce-Robert Pocock <BRPocock@ciwta.org>"
  :license "AGPL v3+"
  :bug-tracker "https://github.com/adventuring/tootsville.net/issues"
  :description
  "The server software monolith for REST services of Tootsville.org"
  :long-description
  "The  REST  services  for  Tootsville.org, while  running  on  several
hostnames, are  handled from  a proxied HTTP  server. This  provides the
REST services for the front-end."
  :depends-on (
               
               ;; systems from Quicklisp
               :bordeaux-threads
               :cl-base64
               :cl-dbi
               :cl-memcached
               :cl-ppcre
               :cl-smtp
               :cljwt-custom
               :clouchdb
               :cxml
               :darts.lib.email-address
               :dbd-mysql
               :drakma
               :envy
               :fare-memoization
               :hunchentoot
               :hunchensocket
               :jonathan
               :lparallel
               :pngload
               :swank
               :symbol-munger
               :trivial-backtrace
               :trivial-ldap
               :trivial-signal
               :uiop
               :uuid
                                        ;:zeromq

               ;; Systems that travel bundled with Tootsville

               :dreamhost
               :oliphaunt
               :rollbar
               :thread-pool-taskmaster
               :twilio
               )
  :components ((:module
                "src"
                :components ((:file "lib/Chœrogryllum/Chœrogryllum")
                             (:file "package")
                             (:file "utils" :depends-on ("package-post"))
                             (:module "types" :depends-on ("utils")
                              :components ((:file "binary")
                                           (:file "color+pattern")
                                           (:file "date+time")
                                           (:file "http-types")
                                           (:file "string-characteristics")
                                           (:file "uri-types")
                                           (:file "toot-names")
                                           (:file "to-json")
                                           (:file "world-types")))
                             (:file "config" :depends-on ("package-post" "types"))
                             (:file "view" :depends-on ("config"))
                             (:file "browser" :depends-on ("config"))
                             (:file "users" :depends-on ("utils" "db"))
                             (:file "contacts" :depends-on ("utils"))
                             (:file "items" :depends-on ("utils"))
                             (:file "passport" :depends-on ("users"))
                             (:file "staff-journal" :depends-on ("users"))
                             (:file "sms" :depends-on ("users"))
                             (:file "toots" :depends-on ("utils" "users"))
                             (:file "players" :depends-on ("utils" "users"))
                             (:file "errors" :depends-on ("package-post"))
                             (:file "terrain" :depends-on ("package-post"))
                             (:file "world" :depends-on ("terrain" "weather/weather" "types"))
                             (:file "weather/weather" :depends-on ("package-post"))
                             (:file "weather/sun-moon" :depends-on ("package-post"))
                             (:file "version" :depends-on ("package-post" "config"))
                             (:file "logging" :depends-on ("package-post" "version"))
                             (:file "write-docs-2" :depends-on ("package-post" "infinity"))
                             (:file "power-on-self-test" :depends-on ("package"))
                             (:file "package-post" :depends-on ("power-on-self-test"))
                             (:file "command-line" :depends-on ("main" "logging" "write-docs-2"))
                             (:file "endpoint" :depends-on ("package-post"))
                             (:file "cassandra" :depends-on ("package-post"))
                             (:file "quaestor" :depends-on ("package-post" "metronome" "types"))
                             (:file "web"
                              :depends-on ("view" "players" "errors" "config" "endpoint"))
                             (:file "http-error" :depends-on ("web"))
                             (:file "redirect" :depends-on ("web"))
                             (:file "metronome" :depends-on ("package-post"))
                             (:file "gossip" :depends-on ("db"))
                             (:file "http-status-messages" :depends-on ("package-post"))
                             (:file "acceptor" :depends-on ("types" "endpoint" "web" "auth"
                                                                    "http-status-messages"))
                             (:file "websockets" :depends-on ("acceptor"))
                             (:file "tcp-stream" :depends-on ("websockets"))
                             (:file "messaging" :depends-on ("websockets" "tcp-stream" "characters"))
                             (:file "main" :depends-on ("config" "view" "package-post" "acceptor"))
                             (:module "db"
                              :depends-on ("package-post")
                              :components ((:file "memcached")
                                        ; (:file "zeromq")
                                           (:file "maria" :depends-on ("memcached"))
                                           (:file "generic-db" :depends-on ("memcached"))
                                           (:file "db-central" :depends-on ("maria" "generic-db"))
                                           (:file "friendly" :depends-on ("db-central"))))
                             (:file "lib/twilio/twilio-simple")
                             (:module "characters"
                              :depends-on ()
                              :components ((:file "robots")
                                           (:file "characters" :depends-on ("robots"))
                                           (:file "robo-toot" :depends-on ("characters"))
                                           (:module "personalities"
                                            :depends-on ("robo-toot")
                                            :components ((:file "toot-personality")
                                                         (:file "holiday-special-personality")
                                                         (:file "shaddow-personality")))
                                           (:module "named"
                                            :depends-on ("robo-toot" "personalities")
                                            :components ((:file "zap")
                                                         (:file "flora")
                                                         (:file "superstar")
                                                         (:file "lil-mc")
                                                         (:file "cupid")
                                                         (:file "moo")
                                                         (:file "dottie")
                                                         (:file "sparkle")
                                                         (:file "doodle")
                                                         (:file "picasso")
                                                         (:file "harmony")
                                                         (:file "props")
                                                         (:file "rad")
                                                         (:file "chaos")
                                                         (:file "smudge")
                                                         (:file "sploot")
                                                         (:file "nevermind")
                                                         (:file "shade")
                                                         (:file "jack")
                                                         (:file "snowcone")
                                                         (:file "mayor-louis")
                                                         (:file "welduh")))))
                             
                             (:module "auth"
                              :depends-on ("package-post" "users")
                              :components ((:file "auth-firebase")))
                             (:module
                              "endpoints"
                              :depends-on ("web" "terrain" "weather/weather" "db")
                              :components ((:file "slash-login")
                                           (:file "slash-version")
                                           (:file "slash-maintenance")
                                           (:file "slash-meta-game")
                                           (:file "slash-gossip")
                                           (:file "slash-toots")
                                           (:file "slash-users")
                                           (:file "slash-world")
                                           (:module
                                            "gossip"
                                            :depends-on ("slash-gossip")
                                            :components ((:file "twilio")
                                                         (:module
                                                          "alexa"
                                                          :components ((:file "alexa")
                                                                       (:file "info" :depends-on ("alexa"))
                                                                       (:file "chat" :depends-on ("alexa"))
                                                                       (:file "clock" :depends-on ("alexa"))))))))
                             (:module
                              "infinity"
                              :depends-on ("endpoints")
                              :components ((:file "infinity")
                                           (:file "legacy-commands" :depends-on ("infinity"))
                                           (:file "legacy-ops" :depends-on ("infinity"))
                                           (:file "modern-ops" :depends-on ("infinity"))
                                           (:file "game-actions" :depends-on ("infinity"))
                                           (:file "tootsville-commands" :depends-on ("infinity"))
                                           (:file "new-commands-20" :depends-on ("infinity"))))))))
