(define-operator-command quick-reload (words u r)
  "Quicklisp reload of the Tootsville package from disk."
  (asdf:load-asd (asdf:system-source-file :Tootsville))
  (ql:quickload :Tootsville)
  (return (format nil "Now running Tootsville.net server version ~a"
                  (asdf:component-version (asdf:find-system :Tootsville)))))

(define-operator-command push-script (words u r)
  "Instruct clients to load a new script file"
  (broadcast (list :|from| "newScript"
                   :|status| t
                   :|script| (first words))))
