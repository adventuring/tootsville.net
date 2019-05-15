(in-package :oliphaunt)

(defmacro mapplist ((key value) plist &body body)
  "Map  over the  key/value pairs  of  a plist,  appending the  results.
Typically used to rebuild a plist by returning lists with new key/value pairs."
  `(loop for (,key ,value) on ,plist by #'cddr
      appending (progn ,@body)))

(defun plist-keys (plist)
  "Return the keys of a plist"
  (mapplist (key _) plist
    (list key)))

(defun plist-values (object)
  (mapplist (_ value) object
    (list value)))

(defun plist-p (object)
  "Guesses whether OBJECT  is a plist. The  heuristic tests that this  is a list of  an even number of  objects, and the
positions which would be the keys of a plist are all keywords. This isn't technically the precise definition of a plist,
but it's an extremely useful approximation."
  (and (consp object)
       (evenp (length object))
       (every #'keywordp (plist-keys object))))


(defun clean-plist (plist &key (test #'identity))
  "Clean a plist by removing key/value pairs when the value does not satisfy TEST.

The default TEST is `IDENTITY', which causes key/value pairs when the value is NIL."
  (check-type test funcallable)
  (mapplist (key value) plist
    (when (funcall test value)
      (list key value))))

(defun groups-of (list count)
  "Batch the given list into groups, each of which are COUNT in length."
  (do ((i 0 (1+ i))
       (l list (cdr l))
       group result)
      ((null l)
       (when group (push (nreverse group) result))
       (nreverse result))
    (push (first l) group)
    (when (= (length group) count)
      (push (nreverse group) result)
      (setf group nil))))

(defun group-by (list &key (test #'eql) (key #'identity))
  "Group elements of a list by some attribute.

Given a TEST (which  must be a test method which  `MAKE-HASH-TABLE' will accept as a test; ie,  `EQ', `EQL', `EQUAL', or
`EQUALP'), group together all of the elements of LIST which (after transformation by KEY)
"
  (let ((hash (make-hash-table :test test)))
    (dolist (el list)
      (push el (gethash (funcall key el) hash)))
    (loop for key being the hash-key of hash using (hash-value val)
       collect (cons key val))))



(defmacro interleave (&rest sets)
  "Interleave elements from each set: (a b c) (1 2 3) ⇒ (a 1 b 2 c 3)"
  (let ((gensyms
         (loop for i below (length sets)
            collecting (gensym (or (and (consp (elt sets i))
                                        (princ-to-string (car (elt sets i))))
                                   (princ-to-string (elt sets i)))))))
    `(loop
        ,@(loop for i below (length sets)
             appending (list 'for (elt gensyms i) 'in (elt sets i)))
        ,@(loop for i below (length sets)
             appending (list 'collect (elt gensyms i))))))
