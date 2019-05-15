(defpackage :class-graph
  (:use :cl :oliphaunt))
(in-package :class-graph)

(defvar *seen*)
(defvar *dot*)

(defun print-relations (class depth)
  (unless (or (not (plusp depth))
              (gethash class *seen*))
    (setf (gethash class *seen*) t)
    (mapcar (lambda (subclass)
              (format *dot* "  \"~:(~A~)\" -> \"~:(~A~)\";~%"
                      (class-name class)
                      (class-name subclass))
              (print-relations subclass (1- depth)))
            (closer-mop:class-direct-subclasses class))))

(defun graph-class (class &key (filename
                                (substitute #\: #\/
                                            (string (class-name class))))
                               (depth 3)
                    &aux (*seen* (make-hash-table :test 'equal)))
  (with-open-file (*dot* (make-pathname :name filename :type "dot")
                         :direction :output :if-exists :supersede
                         :external-format :utf-8)
    (format *dot* "digraph {~%  rankdir=LR; node [shape=box]~%  \"~:(~A~)\" [style=filled,fillcolor=yellow]~%" (class-name class))
    (print-relations class (1- depth))
    (mapcar (lambda (superclass)
              (format *dot* "  \"~:(~A~)\" -> \"~:(~A~)\";~%"
                      (class-name superclass)
                      (class-name class)))
            (closer-mop:class-direct-superclasses class))
    (mapcar (lambda (slot)
              (format *dot* "  \"~:(~A~)\" -> \"~:(~A~)\" [style: dotted];
 \"~:(~A~)\" -> \"~:(~A~)\" [style: dashed; color: blue];~%"
                      (class-name class)
                      (closer-mop:slot-definition-name slot)
                      (closer-mop:slot-definition-name slot)
                      (closer-mop:slot-definition-type slot)))
            (closer-mop:class-direct-slots class))
    (format *dot* "}~%")))
