(defpackage :xmas.ship (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object :xmas.matrix-stack :xmas.deftest))
(in-package :xmas.ship)

(defstruct ring-buffer
  (index 0 :type array-index)
  (vector (make-array 10 :element-type t) :type simple-vector))

(defstruct p
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (r 0.0 :type single-float)
  (s 0.0 :type single-float))

(defun ring-buffer-retract (r)
  (let ((v (ring-buffer-vector r))
        (i (ring-buffer-index r)))
  (setf (ring-buffer-index r) (mod (1- i) (length v)))))

(defun ring-buffer-first-element (r)
  (let ((v (ring-buffer-vector r))
        (i (ring-buffer-index r)))
    (svref v i)))

(defun ring-buffer-do (r fn)
  (let ((v (ring-buffer-vector r))
        (i (ring-buffer-index r)))
    (loop repeat (length v) for j upfrom 0 do
         (funcall fn (svref v (mod (+ i j) (length v)))))))

(defmacro do-ring-buffer ((var buffer) &body body)
  (with-gensyms (lambda)
    `(let ((,lambda (lambda (,var) ,@body)))
       (declare (dynamic-extent ,lambda))
       (ring-buffer-do ,buffer ,lambda))))

(defclass positioned-sprite (sprite)
  ((angle :accessor angle :initform 0.0 :initarg :angle)
   (distance :accessor distance :initform 200.0 :initarg :distance)))

(defclass ship (positioned-sprite)
  ())

(defclass bolt (positioned-sprite)
  ((rotation-speed :accessor rotation-speed :initarg :rotation-speed)
   (forward-speed :accessor forward-speed :initarg :forward-speed)))

(defun ship-scale (ship)
  (* 0.5 (scale-y ship)))

(defun (setf ship-scale) (v ship)
  (setf (scale-x ship) (* 1.5 v)
        (scale-y ship) (* 2.0 v)))

(defconstant pif (coerce pi 'single-float))
(defconstant 2pif (* 2.0 pif))

(defstruct star
  (angle    (random 2pif)  :type single-float)
  (distance (random 200.0) :type single-float)
  (max-scale (+ 0.5 (random 1.0)) :type single-float))

(defun make-stars (count)
  (coerce (loop repeat count collect (make-star))
          'vector))
(defun make-star-sprites (count)
  (coerce (loop repeat count collect
               (make-instance 'sprite :sprite-frame (get-frame "star-32.png")))
          'vector))

(defun update-stars (stars sprites dt)
  (loop for i below (length stars)
     for star = (aref stars i)
     for sprite = (aref sprites i) do
       (let* ((pct (/ (star-distance star) 300.0))
              (speed (lerp (ease :in-sine pct) 1.0 20.0))
              (dist (mod (+ (star-distance star) (* dt speed 20.0)) 300.0))
              (pct (/ dist 300.0))
              (rot (lerp (ease :out-quad pct) -0.5 1.0))
              (angle (mod (+ (star-angle star) (* dt rot)) 2pif))
              (scale (* (star-max-scale star) (lerp (ease :in-sine pct) 0.15 1.0))))
         (declare (type single-float pct speed dist angle dt)
                  (optimize (speed 3) (safety 1)))
         (setf
          (star-distance star) dist
          (star-angle star) angle
          (opacity sprite) (ease :out-quad pct)
          (x sprite) (+ 250.0 (* dist (sin angle)))
          (y sprite) (+ 250.0 (* dist (cos angle)))
          (scale-x sprite) scale
          (scale-y sprite) scale))))

(defun radians-angle-from-center (x y)
  (- (atan (- y 250.0) (- 250.0 x))
     (load-time-value (* pif 0.5))))

(defun project-ship-position (radians radius)
  (values (+ 250.0 (* (sin radians) radius))
          (+ 250.0 (* (cos radians) radius))))

(defun rad->sprite-deg (rad)
  (* -360.0 (/ rad 2pif)))

(defun update-ship-position-from-mouse (mouse-x mouse-y ship)
  (setf (angle ship) (radians-angle-from-center mouse-x mouse-y)))

(defun update-ship-position (ship)
  (setf (distance ship) (clamp (distance ship) 75.0 200.0))
  (multiple-value-bind (x y)
      (project-ship-position (angle ship) (distance ship))
    (setf (x ship) x
          (y ship) y
          (rotation ship)
          (+ 180.0 (rad->sprite-deg (radians-angle-from-center x y)))
          (ship-scale ship) (/ (distance ship) (* 1.5 125.0)))))

(defun make-trails (count)
  (coerce
   (loop repeat count for i upfrom 0 for pct = (- 1.0 (/ i count)) collect
        (let ((sprite (make-instance 'sprite
                       :sprite-frame (get-frame "triangle-32.png")
                       :opacity (+ 0.1 (* 0.8 pct))
                       :scale-x 1.5 :scale-y 2.0)))
          (run-action sprite (hue-cycle-with-offset 3.0 pct) :repeat :forever)
          sprite))
   'vector))

(defun make-trails-ring-buffer (count)
  (let ((vec (make-array count :element-type t)))
    (map-into vec (lambda (_) (declare (ignore _)) (make-p)) vec)
    (make-ring-buffer :vector vec)))

(defun update-ship-trails (ship trails ring-buffer)
  (ring-buffer-retract ring-buffer)
  (let* ((p (ring-buffer-first-element ring-buffer))
         (i 0))
    (setf (p-x p) (x ship)
          (p-y p) (y ship)
          (p-r p) (rotation ship)
          (p-s p) (ship-scale ship))
    (do-ring-buffer (p ring-buffer)
      (let ((sprite (svref trails i)))
        (setf (x sprite) (p-x p)
              (y sprite) (p-y p)
              (rotation sprite) (p-r p)
              (ship-scale sprite) (p-s p)
              i (1+ i))))))

(defun speed-for-ship-distance (distance)
  (lerp (ease :in-quad (/ distance 150.0)) 120.0 360.0))

(defun adjustable-array-remove-unordered (array idx)
  (rotatef (aref array idx) (aref array (1- (length array))))
  (decf (fill-pointer array)))

(defun move-bolts (bolts dt)
  (let ((removes nil))
    (loop for bolt across bolts
       for idx upfrom 0
       for dist = (speed-for-ship-distance (distance bolt))
       do
         (incf (angle bolt) (* (rotation-speed bolt) dt))
         (decf (distance bolt) (* (forward-speed bolt) dist dt))
         (if (<= (distance bolt) 10.0)
             (push idx removes)
             (multiple-value-bind (x y)
                 (project-ship-position (angle bolt) (distance bolt))
               (setf (x bolt) x
                     (y bolt) y
                     (rotation bolt)
                     (+ 180.0 (rad->sprite-deg (radians-angle-from-center x y)))
                     (ship-scale bolt) (/ (distance bolt) (* 1.5 125.0))))))
    (dolist (idx removes)
      (remove-from-parent (aref bolts idx))
      (adjustable-array-remove-unordered bolts idx))))

(defun add-bolt (sprites ship bolts)
  (let ((bolt (make-instance 'bolt
                             :sprite-frame (get-frame "bolt.png")
                             :rotation-speed 0.0
                             :forward-speed 2.0
                             :angle (angle ship)
                             :distance (distance ship))))
    (run-action bolt (hue-cycle-with-offset 0.05 (random 1.0)) :repeat :forever)
    (vector-push-extend bolt bolts)
    (add-child sprites bolt)))

(defvar *star-bolt-hue* 0.0)

(defun add-star-bolt (sprites ship bolts)
  (let* ((bolt (make-instance 'bolt
                              :sprite-frame (get-frame "star-32.png")
                              :rotation-speed 4.0
                              :forward-speed 0.10 
                              :angle (angle ship)
                              :distance (distance ship)))
         (rot (random 360.0))
         (sub (make-instance 'sprite
                             :sprite-frame (get-frame "star-32.png")
                             :rotation rot
                             :x 16.0 :y 16.0
                             :scale-x 0.75 :scale-y 0.75
                             :anchor-x 1.0 :anchor-y 1.25)) 
         (sub2 (make-instance 'sprite
                             :sprite-frame (get-frame "star-32.png")
                             :rotation (mod (+ 180.0 rot) 360.0)
                             :x 16.0 :y 16.0
                             :scale-x 0.75 :scale-y 0.75
                             :anchor-x 1.0 :anchor-y 1.25))
         (hue *star-bolt-hue*)
         (hue2 (mod (+ hue 0.005) 1.0))
         (cycle-len 0.25))
    (setf *star-bolt-hue* (mod (+ 0.005 *star-bolt-hue*) 1.0))
    (run-action sub (rotate-by 0.5 360.0) :repeat :forever)
    (run-action sub (hue-cycle-with-offset  cycle-len hue2) :repeat :forever)
    (run-action sub2 (rotate-by 0.5 -360.0) :repeat :forever)
    (run-action sub2 (hue-cycle-with-offset  cycle-len hue2) :repeat :forever)
    (run-action sub2 (list (move-by 0.3 10.0 10.0)
                           (move-by 0.3 -10.0 -10.0))
                :repeat :forever)
    (run-action bolt (hue-cycle-with-offset cycle-len hue) :repeat :forever)
    (add-child bolt sub)
    (add-child bolt sub2)
    (vector-push-extend bolt bolts)
    (add-child sprites bolt)))

(deftest ship (:width 500 :height 500 :expandable t :preserve-aspect-ratio t)
  :init
  (texture-packer-add-frames-from-file "./res/ship/sprites.json")
  stack := (make-matrix-stack)
  root := (make-instance 'sprite-batch-node
                         :texture (texture-frame-texture
                                   (get-frame "soft-ball-32.png")))
  started := nil
  keys := (make-hash-table :test 'eql)
  just-pressed := (make-hash-table :test 'eql)
  mouse-x := 250.0
  mouse-y := 0.0
  mouse-moved := nil
  globe := (make-instance
            'sprite
            :x 250.0 :y 250.0
            :scale-x 2.0 :scale-y 2.0
            :sprite-frame (get-frame "soft-ball-32.png"))
  ship := (make-instance
           'ship
           :opacity 0.8
           :x 250.0 :y 100.0
           :scale-x 1.5 :scale-y 2.0
           :sprite-frame (get-frame "triangle-32.png"))
  bolts := (make-array 16 :adjustable t :fill-pointer 0)
  star-count := 600
  stars := (make-stars star-count)
  star-sprites := (make-star-sprites star-count)
  (loop for sprite across star-sprites do
       (run-action sprite (rotate-by 1.0 360.0) :repeat :forever))
  trail-count := 30
  trails := (make-trails trail-count)
  ring-buffer := (make-trails-ring-buffer trail-count)
  ring-buffer-initialized := nil
  (add-children root star-sprites)
  (add-children root (reverse trails))
  (add-children root (list globe ship))
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (update-stars stars star-sprites dt)
  (cond
    (mouse-moved (update-ship-position-from-mouse mouse-x mouse-y ship))
    ((gethash :left keys)  (decf (angle ship) (* 3.0 dt)))
    ((gethash :right keys) (incf (angle ship) (* 3.0 dt))))
  (cond
    ((gethash :up keys)
     (decf (distance ship) (* (speed-for-ship-distance (distance ship)) dt)))
    ((gethash :down keys)
     (incf (distance ship) (* (speed-for-ship-distance (distance ship)) dt))))
  (update-ship-position ship) 
  (when (gethash #\s keys)
    (add-bolt root ship bolts))
  (when (gethash #\a just-pressed)
    (add-star-bolt root ship bolts))
  (move-bolts bolts dt)
  (unless ring-buffer-initialized
    (setf ring-buffer-initialized t)
    (do-ring-buffer (p ring-buffer)
      (setf (p-x p) (x ship)
            (p-y p) (y ship)
            (p-r p) (rotation ship)
            (p-s p) (ship-scale ship))))
  (update-ship-trails ship trails ring-buffer)
  (let ((*matrix-stack* stack))
    (visit-with-xform root))
  (setf mouse-moved nil)
  (clrhash just-pressed)
  :on-event
  (case (car event)
    (:mousemove (setf mouse-x (cadr event)
                      mouse-y (cddr event)
                      mouse-moved t))
    (:keydown
     (unless (gethash (cdr event) keys)
       (setf (gethash (cdr event) just-pressed) t))
     (setf (gethash (cdr event) keys) t))
    (:keyup (setf (gethash (cdr event) keys) nil))))

(run-test 'ship)
