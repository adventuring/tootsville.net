(defun mail-only (address)
  "Given a nice e-mail address like \"Name\" <user@domain>, returns just the user@domain bit."
  (if (and (find #\< address :test #'char=)
           (find #\> address :test #'char=)
           (< (position #\< address :test #'char=)
              (position #\> address :test #'char=)))
      (first (split-sequence #\>
                             (second (split-sequence #\<
                                                     address))))
      address))

(assert (equal (mail-only "\"John Doe\" <jdoe@example.com>") "jdoe@example.com"))
