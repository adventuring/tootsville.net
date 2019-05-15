(in-package :oliphaunt)

(defun range (min max)
  (loop for i from min upto max collecting i))

#+sbcl
(setf (symbol-value '∞)
      sb-ext:double-float-positive-infinity)
