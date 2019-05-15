Rollbar.com is  a service  for collecting  automated telemetry  (ie, bug
reports, mostly) through their web service.

This package is  a completely independent attempt  to re-implement their
general “per-language”  APIs in such  a way as  to be useful  for Common
Lisp programs.  It is not  particularly “authorized” and  definitely not
“supported” by Rollbar,  but they have kindly  added it to their list of
third-party libraries, so it's not unknown to them.

Automatic telemetry
-------------------

In general,  you should be able  to establish a dynamic  extent in which
Rollbar is your effective “debugger” and have conditions reported to the
servers without much additional tooling on your part.

      (rollbar:with-configuration
        (:access-token "…"
         :environment "development"
         :code-version "v1.0"
         :framework "my-cool-framework"
         :server (machine-instance))
        (rollbar:with-rollbar-for-debugger ()
           (main)))

See  also  `rollbar:classify-error-level`  to customize  how  conditions
are classified.


“Manual” telemetry
------------------

See:  `rollbar:notify`  for  the   general  case,  or  `rollbar:debug!`,
`info!`, `warning!`, `error!`, and `critical!`  to skip right to various
levels.

    (rollbar:notify :info "Hello, World")
      
    (handler-case
        (error "I'm so sad")
      (error (condition)
        (rollbar:error! "Good-bye, Cruel World"
                        :condition condition)))

