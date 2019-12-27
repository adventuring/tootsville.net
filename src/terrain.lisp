;;;; -*- lisp -*-
;;;
;;;; src/terrain.lisp is part of Tootsville
;;;
;;;; Copyright  © 2008-2017  Bruce-Robert  Pocock;  © 2018-2020  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This  program is  Free  Software: you  can  redistribute it  and/or
;;;; modify it under the terms of  the GNU Affero General Public License
;;;; as published by  the Free Software Foundation; either  version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

(in-package :Tootsville)

(define-constant +habitat-colors+
    '(((65 0 145). :shaddow)
      ((150 150 150). :rocky)
      ((239 14 78). :swamp)
      ((38 152 65). :grassland)
      ((236 237 138). :desert)
      ((145 82 0). :savannah)
      ((35 239 14). :forest)
      ((150 138 237). :ocean)
      ((255 255 255). :ice))
  :test 'equalp
  :documentation "The color triplets which represent each type of habitat
 in the PNG habitat map.")

(defvar *global-heightmap%)
(defvar *global-heightmap-x%)
(defvar *global-heightmap-y%)
(defvar *features%)

(deftype kind-of-habitat ()
  "The various kinds of habitat that exist in the world.

@table @b
@item Shaddow
@item Rocky
@item Swamp
@item Grassland
@item Desert
@item Savannah
@item Forest
@item Ocean
@item Ice
@item Moon
@item Pink Moon 
@item Moon Base
@item City
@item Farm
@item Manatee City
@item Beachside
@item Space
@item Asteroid Field
@end itemize"
  '(member :shaddow :rocky :swamp :grassland
    :desert :savannah :forest :ocean :ice
    :moon :pink-moon :moon-base :city :farm
    :manatee-city :beachside :space :asteroid-field))

(deftype map-places ()
  "A symbol representing one of the planes in which the game takes place.

@table @code
@item CHOR
Choerogryllum
@item MOON
The Moon
@item OTHM
The Other Moon
@item PINK
The Pink Moon
@item ORBIT
In space, in transit between Choerogryllum and The Moon.
@end table"
  '(member :chor :moon :othm :pink :orbit))

(deftype even-coordinate ()
  '(and integer (satisfies divisible-by-200-p)))



;; Reading map data from images


(defvar *habitat-map*
  (pngload:load-file (asdf:system-relative-pathname :Tootsville
                                                    "data/Tootanga-map"
                                                    :type "png"))
  "The Tootanga map contains color-coded pixels representing the various
habitat areas of the game. Each pixel represents a 200m by 200m area; thus,
the entire map area (800 by 600 pixels) represents a playable game area of
160 by 120 km.")

(defvar *elevation-map*
  (pngload:load-file (asdf:system-relative-pathname :Tootsville
                                                    "data/Tootanga-elevation"
                                                    :type "png"))
  "The Tootangan elevation map provides a logarithmic altitude map of the
approximate/net altitude of each 200 by 200 meter area of the game.")

(defpost check-map-heights ()
  "Ensure that both maps are 600px high."
  (= 600 (pngload:height *habitat-map*) (pngload:height *elevation-map*)))

(defpost check-map-widths ()
  "Ensure that both maps are 800px wide."
  (= 800 (pngload:width *habitat-map*) (pngload:width *elevation-map*)))

(defun habitat<-pixel (r g b)
  "Which habitat type does the given color triplet represent?"
  (let ((c (or (assoc (list r g b) +habitat-colors+ :test #'equalp)
               (error "palette mismatch in habitat map on ~a"
                      (color24-name (color24-rgb r g b))))))
    (cdr c)))

(defun get-9-terrain-tiles (latitude longitude)
  "Returns 9 tiles of terrain centered on LATITUDE LONGITUDE as a 3 by 3 array"
  (check-type latitude even-coordinate)
  (check-type longitude even-coordinate)
  (let ((x-offset (/ latitude 200))
        (y-offset (/ longitude 200))
        (elevation (make-array '(3 3) :element-type '(unsigned-byte 8)))
        (habitat (make-array '(3 3) :element-type 'symbol :initial-element :ocean)))
    (when (or (< x-offset 0) (< y-offset 0)
              (> x-offset 799) (> y-offset 599))
      (error 'unimplemented))
    (dotimes (ix 3)
      (dotimes (iy 3)
        (setf (aref elevation ix iy)
              (aref (pngload:data *elevation-map*) (+ x-offset ix) (+ y-offset iy) 2)
              
              (aref habitat ix iy)
              (habitat<-pixel
               (aref (pngload:data *habitat-map*) (+ x-offset ix) (+ y-offset iy) 0)
               (aref (pngload:data *habitat-map*) (+ x-offset ix) (+ y-offset iy) 1)
               (aref (pngload:data *habitat-map*) (+ x-offset ix) (+ y-offset iy) 2)))))
    (list elevation habitat)))



;;; Adding features

(defun terrain/connect-streams ()
  "Connect up to any stream in an adjacent tile.

If no adjacent tile has yet been spawned, small chance of creating a new
 stream.  If streams  enter  from more  than one  side,  connect up  as
 a branching stream. "
  ;; (error 'unimplemented)
  )

(defun terrain/add-cactus ()
  "Add a cactus"
  (error 'unimplemented))

(defun terrain/add-tree ()
  "Add a random tree or bush."
  (let* ((latitude (1+ *global-heightmap-x%))
         (longitude (1+ *global-heightmap-y%))
         (tree (create-item
                (item-template-id
                 (find-record
                  'item-template 
                  :name (random-elt '("Decorative Plant"
                                      "Little Plant" "Plant" "Spider Plant"
                                      "Succulent Plant" "Yucca plant"))))))
         (scale (+ (/ (random 150) 100.0) 1.5)))
    (setf (item-latitude tree) latitude
          (item-longitude tree) longitude
          (item-x tree) (- (/ (random 2000) 100.0) 10.0)
          (item-y tree) (- (/ (random 2000) 100.0) 10.0)
          (item-avatar-scale-x tree) scale
          (item-avatar-scale-y tree) scale
          (item-avatar-scale-z tree) scale))
  )

(defun terrain/add-mushrooms ()
  "Add a cluster of mushrooms or similar."
  (error 'unimplemented))

(defun terrain/add-log ()
  "Adds a fallen log or similar feature."
  (error 'unimplemented))

(defun terrain/add-flowers ()
  "Add a random cluster of appropriate flowers or herbs."
  (error 'unimplemented))

(defun terrain/stream-present-p ()
  "Does a stream bisect the currently-active space?

Should return true  if a body of water exists  which enters the space
 from any  side and  bisects the  space into  two disjoint  land areas.
 Terminus of  a stream  or completely underwater  are not  “streams” by
 this definition."
  (error 'unimplemented))

(defun point-underwater-p (latitude longitude)
  "Is the point underwater? TODO"
  (declare (ignore latitude longitude))
  (error 'unimplemented))

(defun find-random-point-if (function)
  "Find a random point within the space for which FUNCTION is true.

Returns (LIST LATITUDE LONGITUDE)"
  (loop
     for latitude = (/ (random 20000) 100)
     for longitude = (/ (random 20000) 100)
     until (funcall function latitude longitude)
     finally (return (list latitude longitude))))

(defun terrain/add-small-pond ()
  "Create a pool of water smaller than the tile and contained within it. TODO"
  (if (terrain/stream-present-p)
      (destructuring-bind (latitude longitude)
          (find-random-point-if #'point-underwater-p)
        (error 'unimplemented))
      (error 'unimplemented)))

(defun terrain/add-shaddow-bush ()
  "Add a Shaddow bush to the area"
  (error 'unimplemented))

(defun terrain/add-shaddow-stalagmite ()
  "Add a Shaddow stalagmite to the area"
  (error 'unimplemented))

(defun terrain/add-shaddow-pit ()
  "Add a Shaddow pit to the area"
  (error 'unimplemented))


;;; Per-habitat generation rules.

(defgeneric generate-terrain-features (contour habitat)
  (:documentation "Generate the terrain features based upon the contour map
 and habitat type.  Methods of this function specialize upon the habitat
 type."))

(defmethod generate-terrain-features :before (contour habitat)
  "Connect streams, regardless of the habitat type."
  (terrain/connect-streams))

(defmethod generate-terrain-features (contour (habitat (eql :shaddow)))
  (loop repeat (random 100) do (terrain/add-shaddow-stalagmite))
  (loop repeat (random 100) do (terrain/add-shaddow-bush))
  (loop repeat (random 25) do (terrain/add-shaddow-pit)))

(defmethod generate-terrain-features (contour (habitat (eql :swamp)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :ocean)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :grassland)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :forest)))
  (loop repeat (random 200) do (terrain/add-tree))
  (loop repeat (random 10) do (terrain/add-mushrooms))
  (loop repeat (random 10) do (terrain/add-log))
  (loop repeat (random 5) do (terrain/add-flowers))
  (loop repeat (random 5) do (terrain/add-small-pond)))

(defmethod generate-terrain-features (contour (habitat (eql :desert)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :savannah)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :rocky)))
  (error 'unimplemented))

(defmethod generate-terrain-features (contour (habitat (eql :ice)))
  (error 'unimplemented))



(defgeneric generate-terrain-contour (9-elevations habitat
                                      latitude longitude scale)
  (:documentation "Generate the contour for a tile area"))

(defun copy-terrain-edge-horz (start-latitude longitude end-latitude
                               dest-latitude dest-longitude)
  (loop for xi from start-latitude to end-latitude
     for yi = longitude
     for xj from dest-latitude to (+ dest-latitude (- start-latitude end-latitude))
     for yj = dest-longitude
     do (setf (global-heightmap-corner xj yj) (global-heightmap-corner xi yi))))

(defun copy-terrain-edge-vert (latitude start-longitude end-longitude
                               dest-latitude dest-longitude)
  (loop for yi from start-longitude to end-longitude
     for xi = latitude
     for yj from dest-longitude to (+ dest-longitude (- start-longitude
                                                        end-longitude))
     for xj = dest-latitude
     do (setf (global-heightmap-corner xj yj) (global-heightmap-corner xi yi))))

(defun generate-terrain-blank-edge-horz (start-latitude longitude 
                                         end-latitude base-elevation)
  (loop for xi from start-latitude to end-latitude
     for yi = longitude
     do (setf (global-heightmap-corner xi yi) base-elevation)))

(defun generate-terrain-blank-edge-vert (latitude start-longitude 
                                         end-longitude base-elevation)
  (loop for yi from start-longitude to end-longitude
     for xi = latitude
     do (setf (global-heightmap-corner xi yi) base-elevation)))

(defun fill-blank-contour (latitude longitude base-elevation)
  (check-type latitude even-coordinate)
  (check-type longitude even-coordinate)
  (dotimes (xi 200)
    (dotimes (yi 200)
      (setf (global-heightmap-corner (+ xi latitude) (+ yi longitude))
            (+ base-elevation (- (random 5) 2))))))

(defun smoothe-contour-200×200 (latitude longitude &optional (repeats 3))
  (check-type latitude even-coordinate)
  (check-type longitude even-coordinate)
  (dotimes (i repeats)
    (dotimes (xi 200)
      (dotimes (yi 200)
        (setf (global-heightmap-corner (+ xi latitude) (+ yi longitude))
              (floor (+ (* 4 (global-heightmap-corner (+ xi latitude)
                                                      (+ yi longitude)))
                        (global-heightmap-corner (+ xi latitude -1)
                                                 (+ yi longitude))
                        (global-heightmap-corner (+ xi latitude 1)
                                                 (+ yi longitude))
                        (global-heightmap-corner (+ xi latitude)
                                                 (+ yi longitude -1))
                        (global-heightmap-corner (+ xi latitude)
                                                 (+ yi longitude 1)))
                     8))))))

(defun generate-blank-contour (9-elevations latitude longitude)
  (if (terrain-exists-p :chor (1- latitude) longitude)
      (copy-terrain-edge-vert (1- latitude) longitude (+ longitude 200)
                              latitude longitude)
      (generate-terrain-blank-edge-vert (1- latitude) (1- longitude)
                                        (+ longitude 200)
                                        (aref 9-elevations 0 1)))
  (if (terrain-exists-p :chor (+ latitude 200) longitude)
      (copy-terrain-edge-vert (+ latitude 200) longitude (+ longitude 200)
                              latitude longitude)
      (generate-terrain-blank-edge-vert (+ latitude 200) (1- longitude)
                                        (+ longitude 200)
                                        (aref 9-elevations 2 1)))
  (if (terrain-exists-p :chor latitude (1- longitude))
      (copy-terrain-edge-horz (1- latitude) (1- longitude) (+ latitude 200)
                              latitude longitude)
      (generate-terrain-blank-edge-horz (1- latitude) (1- longitude)
                                        (+ latitude 200)
                                        (aref 9-elevations 1 0)))
  (if (terrain-exists-p :chor latitude (+ longitude 200))
      (copy-terrain-edge-horz (1- latitude) (+ longitude 200) (+ latitude 200)
                              latitude longitude)
      (generate-terrain-blank-edge-horz (1- latitude) (+ longitude 200)
                                        (+ latitude 200)
                                        (aref 9-elevations 1 2)))
  (fill-blank-contour latitude longitude (aref 9-elevations 1 1))
  (smoothe-contour-200×200 latitude longitude 30))


(defun (setf global-heightmap-corner) (elevation latitude longitude)
  (setf (aref *global-heightmap%
              (- latitude *global-heightmap-x%)
              (- longitude *global-heightmap-y%))
        elevation))

(defun global-heightmap-corner (latitude longitude)
  (check-type latitude even-coordinate)
  (check-type longitude even-coordinate)
  (aref *global-heightmap%
        (- latitude *global-heightmap-x%)
        (- longitude *global-heightmap-y%)))

(defmethod generate-terrain-contour (9-elevations habitat latitude longitude
                                     (scale (eql 0)))
  ;; (format *trace-output* "~& Initial blank slate:")
  ;; (dump-global-heightmap latitude longitude)
  (generate-blank-contour 9-elevations latitude longitude)
  ;; (format *trace-output* "~& With neighbouring elevations:")
  ;; (dump-global-heightmap latitude longitude)
  (call-next-method 9-elevations habitat latitude longitude 8))

(defmethod generate-terrain-contour (9-elevations habitat latitude longitude
                                     (scale (eql 1)))
  t)

(defgeneric habitat-elevation-roughness (habitat)
  (:documentation "How much relative roughness to the contour for this habitat?")
  (:method (habitat) 1)
  (:method ((habitat (eql :desert))) 1/100)
  (:method ((habitat (eql :grasslands))) 1/6)
  (:method ((habitat (eql :savannah))) 1/12)
  (:method ((habitat (eql :ocean))) 1/2))

(defun shift-contour-point (latitude longitude shift)
  "Shift a point on the contour map vertically"
  (check-type latitude even-coordinate)
  (check-type longitude even-coordinate)
  (setf (global-heightmap-corner latitude longitude)
        (min #xff (max 0 (+ (global-heightmap-corner latitude longitude)
                            shift)))))

(defmethod generate-terrain-contour (9-elevations habitat latitude longitude step)
  (let ((scale (elt '(0 1 2 5 10 25 50 100 200)
                    step)))
    (dotimes (xi (floor 200 scale))
      (dotimes (yi (floor 200 scale))
        (let ((shift (round (* (sqrt (* (random (let ((r (floor scale 5)))
                                                  (if (zerop r) scale r)))
                                        (habitat-elevation-roughness habitat)))
                               (signum (1- (random 3)))))))
          (dotimes (xj scale)
            (dotimes (yj scale)
              (shift-contour-point (+ latitude (* xi scale) xj)
                                   (+ longitude (* yi scale) yj)
                                   shift))))))
    (smoothe-contour-200×200 latitude longitude)
    
    ;; (format  *trace-output*   "~&  After  contour   randomization  on
    ;; ~D×~:*~D     square~p:"     scale      (floor     200     scale))
    ;; (dump-global-heightmap latitude longitude)
    )
  
  (generate-terrain-contour 9-elevations habitat latitude longitude (1- step)))

(defun dump-global-heightmap (latitude longitude)
  (check-type latitude even-coordinate)
  (check-type longitude even-coordinate)
  (format *trace-output* "~& ┎────────────────────────────────────────────────────────────────┒~
~{~% ┃ ~{~2,'0x~^ ~} ┃~}~
~% ┖────────────────────────────────────────────────────────────────┚"
          (loop for yi from longitude upto (+ longitude 200) by 5
             collect (loop for xi from latitude upto (+ latitude 200) by 10
                        collect (global-heightmap-corner xi yi)))))



;;; Spawn new, never-before-seen terrain block
;;
;;; Terrain blocks are 200m×200m and  can potentially be on Chœrogryllum
;;; (:chor), in the  near-Chœrogryllum orbit (:Oribt), or  on one of
;;; the moons (:Moon, :othm, :pink).

(defgeneric spawn-terrain (place latitude longitude))

(defmethod spawn-terrain ((place (eql :chor)) (latitude integer) (longitude integer))
  (check-type latitude even-coordinate)
  (check-type longitude even-coordinate)
  (assert (<= -80000 latitude 80000))
  (assert (<= -60000 longitude 60000))
  (let ((*global-heightmap% (make-array (list 202 202) :element-type '(unsigned-byte 8)))
        (*global-heightmap-x% (1- latitude))
        (*global-heightmap-y% (1- longitude))
        (*features%))
    (destructuring-bind (elevation habitat) 
        (get-9-terrain-tiles latitude longitude)
      (verbose:info :terrain "~& Generating map at (~:d,~:d) ~
in habitat ~:(~A~) with elevations ~S"
                    latitude longitude (aref habitat 1 1) elevation)
      (let* ((contour (generate-terrain-contour elevation habitat 
                                                latitude longitude 1))
             (features (generate-terrain-features contour (aref habitat 1 1)))))
      (format *trace-output* "~& Final rough map:")
      (dump-global-heightmap latitude longitude)
      ))
  ;; apply sea level
  ;; save to map database
  
  )



(defun terrain-db-key (place latitude longitude)
  (check-type place map-places)
  (check-type latitude even-coordinate)
  (check-type longitude even-coordinate)
  (format nil "World/~:(~a~)/~x×~x" place latitude longitude))

(defun terrain-exists-p (place latitude longitude)
  "If terrain has been previously defined at the tile given, return it.

Use `TERRAIN' generally instead."
  (clouchdb:get-document (terrain-db-key place latitude longitude)))

(defun terrain (place latitude longitude)
  "Obtain the terrain tile in PLACE at LATITUDE,LONGITUDE

PLACE is one of :chor, :Moon, :othm, :pink, :orbit.

LATITUDE and LONGITUDE must be aligned to 200m increments."
  (or (terrain-exists-p place latitude longitude)
      (spawn-terrain place latitude longitude)))
