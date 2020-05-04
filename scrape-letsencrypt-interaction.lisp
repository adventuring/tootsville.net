;;; https://lispblog.xach.com/post/189499356038/working-with-letsencrypts-certbot-for-a-lisp

(in-package :Tootsville)

(defun scrape-letsencrypt-interaction (file)
  (let (secret path)
    (with-open-file (stream file)
      (labels ((next-line ()
                 (let ((line (read-line stream nil)))
                   (when (null line)
                     (unless (and secret path)
                       (error "Didn't find a secret and path anywhere in ~S"
                              file))
                     (return-from scrape-letsencrypt-interaction
                       (values secret path)))
                   line))
               (skip-past (string)
                 (loop
                   (let ((line (next-line)))
                     (when (search string line)
                       (return)))))
               (read-trimmed-line ()
                 (string-trim '(#\Return)
                              (next-line)))
               (substring-after (string target)
                 (let ((pos (search string target)))
                   (unless pos
                     (error "Could not find ~S in ~S" string target))
                   (subseq target pos))))
        (loop
          (skip-past "Create a file containing")
          (next-line)
          (setf secret (read-trimmed-line))
          (skip-past "make it available")
          (next-line)
          (setf path (substring-after ".well-known" (read-trimmed-line))))))))
          