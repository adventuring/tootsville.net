(asdf:defsystem :oliphaunt
  :description "Various utilities that I use in different projects"
  :author "Bruce-Robert Fenn Pocock"
  :version "2.0.5"
  :maintainer "Bruce-Robert Fenn Pocock"
  :mailto "brpocock+oliphaunt-lisp@star-hope.org"
  :licence "AGPLv3" ; if this poses a problem, ask me for a waiver.
  :long-name "Bruce-Robert Fenn Pocock's collection of various utility functions"

  :depends-on (

               :alexandria
               :apply-argv
               :bordeaux-threads
               :buildapp
               #+romance-with-physics  :cl-bullet2l
               :cl-fad
               :cl-oauth
               :cl-readline
               :cl-unicode
               :cffi
               :fare-memoization
               :langutils
               :local-time
               :parse-number
               :prepl
               :split-sequence
               :sqlite
               :st-json
               :swank
               :trivial-garbage
               :trivial-gray-streams
               :usocket
               #+old-wordnet :wordnet

               )

  :encoding :utf-8

  :serial t

  :components
  ((:file "package")
   (:file "control-utils")
   (:file "repl-glue")
   (:file "hash-table")
   (:file "string-utils")
   (:file "json-utils")
   (:file "edn-utils")
   (:file "edn")
   (:file "extract")
   (:file "latex-utils")
   (:file "latin-utils")
   (:file "html-utils")
   (:file "math-utils")
   (:file "sql-utils")
   (:file "time-utils")
   (:file "plist-utils")
   (:file "i18n+l10n")
   (:file "class-graph")
   (:file "system-utils")))
