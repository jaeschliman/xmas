(defpackage :xmas.ship (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object :xmas.matrix-stack :xmas.deftest))
(in-package :xmas.ship)

(defclass physics ()
  ((velocity-x :accessor velocity-x :initarg :velocity-x)
   (velocity-y :accessor velocity-y :initarg :velocity-y)
   (acceleration-x :accessor acceleration-x :initarg :acceleration-x)
   (acceleration-y :accessor acceleration-y :initarg :acceleration-y))
  (:default-initargs
   :velocity-x 0.0 :velocity-y 0.0
   :acceleration-x 0.0 :acceleration-y 0.0))

(defconstant pif (coerce pi 'single-float))
(defconstant 2pif (* 2.0 pif))

(defstruct star
  (angle (random 2pif))
  (distance (random 200.0)))

(defun make-stars (count)
  (coerce (loop repeat count collect (make-star))
          'vector))
(defun make-star-sprites (count)
  (coerce (loop repeat count collect
               (make-instance
                'sprite
                :scale-x 0.2
                :scale-y 0.2
                :sprite-frame (get-frame "star-32.png") ))
          'vector))

(defun update-stars (stars sprites dt)
  (loop for i below (length stars)
     for star = (aref stars i)
     for sprite = (aref sprites i) do
       (let* ((pct (/ (star-distance star) 300.0))
              (speed (lerp 20.0 1.0 pct))
              (dist (+ (star-distance star) (* dt speed 20.0)))
              (angle (mod (+ (star-angle star) dt) 2pif)))
         (declare (type single-float pct speed dist angle dt)
                  (optimize (speed 3) (safety 1)))
         (when (> dist 300.0) (setf dist (mod dist 300.0)))
         (setf
          (star-distance star) dist
          (star-angle star) angle
          (opacity sprite) (ease :out-quad pct)
          (x sprite) (+ 250.0 (* dist (sin angle)))
          (y sprite) (+ 250.0 (* dist (cos angle)))))))

(defun radians-angle-from-center (x y)
  (- (atan (- y 250.0) (- 250.0 x))
     (load-time-value (* pif 0.5))))

(defun project-ship-position (radians &optional (radius 200.0))
  (values (+ 250.0 (* (sin radians) radius))
          (+ 250.0 (* (cos radians) radius))))

(defun rad->sprite-deg (rad)
  (* -360.0 (/ rad 2pif)))

(defun update-ship-position (mouse-x mouse-y ship)
  (multiple-value-bind (x y)
      (project-ship-position (radians-angle-from-center mouse-x mouse-y))
    (setf (x ship) x
          (y ship) y
          (rotation ship)
          (+ 180.0 (rad->sprite-deg (radians-angle-from-center x y))))))

(deftest ship (:width 500 :height 500)
  :init
  (texture-packer-add-frames-from-file "./res/ship/sprites.json")
  stack := (make-matrix-stack)
  root := (make-instance 'sprite-batch-node
                         :texture (texture-frame-texture
                                   (get-frame "soft-ball-32.png")))
  started := nil
  mouse-x := 250.0
  mouse-y := 0.0
  globe := (make-instance
            'sprite
            :x 250.0 :y 250.0
            :scale-x 2.0 :scale-y 2.0
            :sprite-frame (get-frame "soft-ball-32.png"))
  ship := (make-instance
           'sprite
           :x 250.0 :y 100.0
            :scale-x 1.5 :scale-y 2.0
            :sprite-frame (get-frame "triangle-32.png"))
  star-count := 150
  stars := (make-stars star-count)
  star-sprites := (make-star-sprites star-count)
  (add-children root star-sprites)
  (add-children root (list globe ship))
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (update-stars stars star-sprites dt)
  (update-ship-position mouse-x mouse-y ship)
  (let ((*matrix-stack* stack))
    (visit-with-xform root))
  :on-event
  (case (car event)
    (:mousemove (setf mouse-x (cadr event)
                      mouse-y (cddr event)))))

(run-test 'ship)
