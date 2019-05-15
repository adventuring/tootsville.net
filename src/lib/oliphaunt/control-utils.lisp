(in-package :oliphaunt)

;; flow control convenience functions

(defmacro until (test &body body)
  `(do () (,test) ,@body))

(defmacro while (test &body body)
  `(do () ((not ,test)) ,@body))

(defmacro doseq ((element sequence) &body body)
  `(loop for ,element across ,sequence do (progn ,@body)))

(defmacro any (predicate sequence)
  `(notevery (complement ,predicate) ,sequence))

(defmacro forever (&body body)
  (let* ((block-tag (if (symbolp (first body))
                        (first body)
                        'forever))
         (loop-tag (gensym block-tag)))
    `(block ,block-tag
       (tagbody
          ,loop-tag
          ,@body
          (go ,loop-tag)))))

(defmacro eval-once (form)
  `(let ((v ',(gensym "ONCE")))
     (if (boundp v)
         (symbol-value v)
         (set v ,form))) )

(defmacro ignore-warnings ((&rest warnings-to-ignore) &body body)
  `(handler-bind
       ((,(or warnings-to-ignore 'warning) #'muffle-warning))
     ,@body))



(defun extreme (function list)
  (reduce (lambda (a b)
            (if (funcall function a b) a b))
          list))


(defmacro modincf (place base)
  "Increment a setf'able place, and return true whenever it wraps
around modulo base.

Put in a more sane way: return true every `base' times it is called
with the same `place'"
  `(zerop (setf ,place (mod (1+ ,place) ,base))))

(defun make-t-every-n-times (base)
  "Returns a function which, every \"N\" times that it is called,
 returns true."
  (let ((private 0))
    (lambda ()
      (modincf private base))))




(define-condition todo-item (error)
  ((note :initarg :note :reader todo-note)))

(defmethod todo (&optional (string "TODO: This function is not yet implemented")
                 &rest keys)
  (declare (ignore keys))
  (restart-case
      (error 'todo-item :note string)
    (return-nil ()
      :report (lambda (s)
                (format s "Return NIL"))
      nil)
    (return-t ()
      :report (lambda (s)
                (format s "Return T"))
      t)
    (return-value (value)
      :report (lambda (s)
                (format s "Return some other value"))
      value)))



(defmacro with-do-over-restart ((&optional (tag :do-over)
                                           (label "Retry this form")
                                           &rest format-args) &body body)
  (with-gensyms (do-over-tag)
    `(tagbody ,do-over-tag
        (restart-case
            (progn ,@body)
          (,tag () :report (lambda (s)
                             (format s ,label ,@format-args))
                (go ,do-over-tag))))))



(defun strings-list-p (list)
  (and (consp list)
       (every #'stringp list)))

(deftype strings-list ()
  '(satisfies string-list-p))

(defun boolbool (generalized-boolean)
  "Cast a generalized Boolean value to precisely T or NIL"
  (if generalized-boolean t nil))

(defun yesno$ (bool)
  "Cast a boolean as the string Yes or No"
  (if bool "Yes" "No"))

(deftype funcallable ()
  '(or (and symbol (satisfies fboundp))
    function))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun null-if (thing other &key (test #'eql))
    "As  with SQL's  NULLIF, returns  THING if  THING  is not  equal to  OTHER  (under TEST);  but, if  THING is  OTHER,
returns NIL.

OTHER may be a set, in which case, NIL is returned for any THING which matches (under TEST) any member of that set.

If it matters: TEST is always called with OTHER, then THING."
    (check-type test funcallable)
    (if (atom other)
        (if (funcall test other thing)
            nil
            thing)
        (if (some (curry test thing) other)
            nil
            thing))))


(defun base-type-of (sequence)
  "Identifies the essential type of a sequence: list, string, or vector. Returns NIL for any other type of object."
  (cond
    ((consp sequence) 'list)
    ((typep sequence 'string) 'string)
    ((typep sequence 'vector) 'vector)))

(defun coerce-vector (old-vector new-type)
  "Create a vector of NEW-TYPE and copy each element of OLD-VECTOR into it."
  (check-type old-vector vector)
  (let* ((length (length old-vector))
         (new-vector (make-array length :element-type new-type :fill-pointer length)))
    (loop for i from 0 below length
       do (setf (aref new-vector i) (aref old-vector i)))
    new-vector))

(defun take (count source)
  "Take COUNT elements from SOURCE.

If SOURCE is a  sequence, returns a sequence of a  similar type (list, string, or vector) with  the first COUNT elements
from SOURCE.

If SOURCE is a function or a symbol, calls that function COUNT times, collecting the results into a list."
  (check-type count (integer 0 *))
  (if (zerop count)
      nil
      (cond ((consp source)
             (loop for element in source repeat count collect element))
            ((typep source 'sequence)
             (subseq source 0 count))
            (t (loop repeat count collect (funcall source))))))

(defun take-if (count predicate source)
  "As `TAKE', but discards elements which do not satisfy PREDICATE.

eg: (take-if 5 #'digit-char-p \" a 1 b 2 c 3 d 4 e 5 f 6 g 7\") ⇒ \"12345\""
  (check-type count (integer 0 *))
  (check-type predicate funcallable)
  (if (zerop count)
      nil
      (cond
        ((consp source)
         (loop for element in source
            with taken = 0
            while (< taken count)
            when (funcall predicate element)
            collect element))
        ((typep source 'sequence)
         (coerce (let ((first (elt source 0))
                       (rest (subseq source 1)))
                   (if (funcall predicate first)
                       (concatenate (base-type-of source)
                                    (vector first)
                                    (take-if (1- count) predicate rest))
                       (take-if count predicate rest)))
                 (base-type-of source)))
        (t (let ((this (funcall source)))
             (if (funcall predicate this)
                 (cons this (take-if (1- count) predicate source))
                 (take-if count predicate (cdr source))))))))



(defun repeat (count item)
  (make-list count :initial-element item))

(define-compiler-macro repeat (&whole form count item)
  (if (and (numberp count)
           (constantp count)
           (constantp item))
      (cons 'list (make-list count :initial-element item))
      form))
