(in-package :oliphaunt)



(defgeneric extract-key-path% (collection key more-keys)
  (:method ((collection cons) (key symbol) more-keys)
    (declare (ignore more-keys))
    (getf collection key))
  (:method (collection (key integer) more-keys)
    (declare (ignore more-keys))
    (elt collection key))
  (:method ((collection hash-table) key more-keys)
    (declare (ignore more-keys))
    (gethash key collection))
  (:method ((collection array) (indices cons) more-keys)
    (declare (ignore more-keys))
    (apply #'aref collection indices))
  (:method ((collection null) key more-keys)
    (values))
  (:method ((object t) (key function) more-keys)
    (declare (ignore more-keys))
    (funcall function object))
  (:method :around (collection key (more-keys cons))
           (extract-key-path% (call-next-method)
                              (first more-keys)
                              (rest more-keys))))

(defun extract (collection key &rest more-keys)
  "Extract the  item identified  by KEY  from COLLECTION.  If MORE-KEYS,
then extract an item from each subsequently nested collection.

 • For a list with a symbol key, uses GETF
 • For a sequence and integer, uses ELT
 • For a hash-table, uses GETHASH
 • For an array, uses AREF"
  (extract-key-path% collection key more-keys))

(assert (= 4 (extract '(:a (:b (:c (:d 4)))) :a :b :c :d)))
(assert (= 4 (extract '(:a (1 2 3 4)) :a 3)))
(let ((h (make-hash-table :test 'equalp)))
  (setf (gethash "monkey" h) "George")
  (assert (string= "George" (extract `(:a (:b ,h)) :a :b "monkey"))))
