(defpackage :xmas.ship (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object :xmas.matrix-stack :xmas.deftest))
(in-package :xmas.ship)

(defstruct ring-buffer
  (index 0 :type array-index)
  (vector (make-array 10 :element-type t) :type simple-vector))

(defstruct p
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (r 0.0 :type single-float))

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

(defclass ship (node)
  ((angle :accessor angle :initform 0.0)
   (distance :accessor distance :initform 200.0)))

(defconstant pif (coerce pi 'single-float))
(defconstant 2pif (* 2.0 pif))

(defstruct star
  (angle    (random 2pif)  :type single-float)
  (distance (random 200.0) :type single-float))

(defun make-stars (count)
  (coerce (loop repeat count collect (make-star))
          'vector))
(defun make-star-sprites (count)
  (coerce (loop repeat count collect
               (make-instance
                'sprite
                :scale-x 0.2
                :scale-y 0.2
                :sprite-frame (get-frame "star-32.png")))
          'vector))

(defun update-stars (stars sprites dt)
  (loop for i below (length stars)
     for star = (aref stars i)
     for sprite = (aref sprites i) do
       (let* ((pct (/ (star-distance star) 300.0))
              (speed (lerp 20.0 1.0 pct))
              (dist (mod (+ (star-distance star) (* dt speed 20.0)) 300.0))
              (pct (/ dist 300.0))
              (angle (mod (+ (star-angle star) dt) 2pif))
              (scale (lerp 1.5 0.15 pct)))
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
  (multiple-value-bind (x y)
      (project-ship-position (angle ship) (distance ship))
    (setf (x ship) x
          (y ship) y
          (rotation ship)
          (+ 180.0 (rad->sprite-deg (radians-angle-from-center x y))))))

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
          (p-r p) (rotation ship))
    (do-ring-buffer (p ring-buffer)
      (let ((sprite (svref trails i)))
        (setf (x sprite) (p-x p)
              (y sprite) (p-y p)
              (rotation sprite) (p-r p)
              i (1+ i))))))

(deftest ship (:width 500 :height 500)
  :init
  (texture-packer-add-frames-from-file "./res/ship/sprites.json")
  stack := (make-matrix-stack)
  root := (make-instance 'sprite-batch-node
                         :texture (texture-frame-texture
                                   (get-frame "soft-ball-32.png")))
  started := nil
  keys := (make-hash-table :test 'eql)
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
  star-count := 300
  stars := (make-stars star-count)
  star-sprites := (make-star-sprites star-count)
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
  (update-ship-position ship) 
  (unless ring-buffer-initialized
    (setf ring-buffer-initialized t)
    (do-ring-buffer (p ring-buffer)
      (setf (p-x p) (x ship)
            (p-y p) (y ship)
            (p-r p) (rotation ship))))
  (update-ship-trails ship trails ring-buffer)
  (let ((*matrix-stack* stack))
    (visit-with-xform root))
  (setf mouse-moved nil)
  :on-event
  (case (car event)
    (:mousemove (setf mouse-x (cadr event)
                      mouse-y (cddr event)
                      mouse-moved t))
    (:keydown (setf (gethash (cdr event) keys) t))
    (:keyup   (setf (gethash (cdr event) keys) nil))))

(run-test 'ship)
