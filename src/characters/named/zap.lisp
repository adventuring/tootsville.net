(defmethod initialize-robot (robot (Toot-name (eql :zap)))
  (initialize-robo-Toot robot))

(defmethod robo-Toot-heard :after (robot (listener-name (eql :zap)) speaker (mode null) heard)
  (when (find "Zap" (last heard))
    (setf (gethash speaker (robo-Toot-mode robo-Toot)) :how-are-you)
    (robo-Toot-say robot "Hello, ~a. How are you?" (Toot-name speaker))))
  
  

