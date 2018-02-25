(defpackage :xmas.blob-editor (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object :xmas.matrix-stack :xmas.spotlight-node :xmas.deftest))
(in-package :xmas.blob-editor)

(defstruct p
  (x 0.0 :type single-float)
  (y 0.0 :type single-float))

(defun draw-circle (x y radius r g b a)
  (let* ((count 30)
         (2pi (load-time-value (coerce (* 2.0 pi) 'single-float)))
         (step (/ 2pi count)))
    (xmas.render-buffer::with-colored-2d-triangle-fan ()
      (macrolet ((v (x y r g b a)
                   `(xmas.render-buffer::vert ,x ,y ,r ,g ,b ,a)))
        (v x y r g b a)
        (loop repeat (1+ count) with angle = 2pi
           for x1 = (+ x (* (sin angle) radius))
           for y1 = (+ y (* (cos angle) radius)) do
             (v x1 y1 r g b a)
             (decf angle step))))))

(defun catmul-romm (u x0 x1 x2 x3)
  (let* ((u2 (* u u))
         (u3 (* u2 u)))
    (declare (optimize (speed 3) (safety 1))
             (type single-float u u2 u3 x0 x1 x2 x3))
    (* (+ (* 2 x1)
          (* (+ (- x0) x2) u)
          (* (+ (* 2.0 x0)
                (- (* 5.0 x1))
                (* 4.0 x2)
                (- x3))
             u2)
          (* (+ (- x0) (* 3.0 x1) (- (* 3.0 x2)) x3)
             u3))
       0.5)))

(defun cmr-points (f c0 a b c1)
  (loop for i from 0 to 30
     with x0 = (p-x c0)
     with x1 = (p-x a)
     with x2 = (p-x b)
     with x3 = (p-x c1)
     with y0 = (p-y c0)
     with y1 = (p-y a)
     with y2 = (p-y b)
     with y3 = (p-y c1)
     for u = (/ i 30.0)
     do (funcall f
         (catmul-romm u x0 x1 x2 x3)
         (catmul-romm u y0 y1 y2 y3))))

(defun cmr-vector (f points)
  (let ((len (length points)))
    (macrolet ((@ (i) `(aref points ,i)))
      (cmr-points f (@ (- len 3)) (@ (- len 2)) (@ (1- len)) (@ 0))
      (cmr-points f (@ (- len 2)) (@ (1- len)) (@ 0) (@ 1))
      (cmr-points f (@ (1- len)) (@ 0) (@ 1) (@ 2))
      (loop for i from 1 to (- len 3)
         for c0 = (@ (1- i))
         for a  = (@ i)
         for b  = (@ (1+ i))
         for c1 = (@ (+ i 2))
         do (cmr-points f c0 a b c1))
      (let ((p (@ (- len 2))))
        (funcall f (p-x p) (p-y p))))))

(defun draw-blob-catmull-romm (vector)
  (xmas.render-buffer::with-colored-2d-triangle-fan ()
    (flet ((draw (x y) (xmas.render-buffer::vert x y 255 0 0 255)))
      (declare (dynamic-extent #'draw))
      (xmas.render-buffer::vert 400.0 400.0  255 0 0 255)
      (cmr-vector #'draw vector))))

(defun draw-blob-simple (vector)
  (xmas.render-buffer::with-colored-2d-triangle-fan ()
    (macrolet ((v (x y r g b a)
                 `(xmas.render-buffer::vert ,x ,y ,r ,g ,b ,a)))
      (v 400.0 400.0 255 0 0 255)
      (loop for p across vector do
           (v (p-x p) (p-y p) 255 0 0 255))
      (let ((p (aref vector 0)))
        (v (p-x p) (p-y p) 255 0 0 255)))))

(defun draw-blob (vector)
  (cond
    ((> (length vector) 3)
     (draw-blob-catmull-romm vector))
    ((> (length vector) 2)
     (draw-blob-simple vector))))

(defun draw-points (vector)
  (let (toggle)
    (loop for p across vector do
         (setf toggle (not toggle))
         (draw-circle (p-x p) (p-y p) 10.0 255 255 (if toggle 255 128) 128))))

(defun lerp-points (a b result amt)
  (setf (p-x result) (lerp amt (p-x a) (p-x b))
        (p-y result) (lerp amt (p-y a) (p-y b))))

(defun draw-blob-lerp (a b scratch amt)
  (do () ((>= (length scratch) (length a)))
    (vector-push-extend (make-p :x 0.0 :y 0.0) scratch))
  (setf (fill-pointer scratch) (length a))
  (assert (= (length a) (length b) (length scratch)))
  (loop for i below (length a) do
       (lerp-points (aref a i) (aref b i) (aref scratch i) amt))
  (draw-blob scratch))

(defun draw-curr-point (p)
  (draw-circle (p-x p) (p-y p) 8.0 0 128 255 255))

(defun resort-points (vector)
  (let (cx cy)
    (loop for p across vector
       for x = (p-x p) for y = (p-y p)
       minimizing x into minx
       maximizing x into maxx
       minimizing y into miny
       maximizing y into maxy
       finally (setf cx (* 0.5 (+ minx maxx))
                     cy (* 0.5 (+ miny maxy))))
    (flet ((angle (p)
             (atan (- (p-y p) cy)
                   (- (p-x p) cx))))
      (sort vector '< :key #'angle))))

(defun add-point (vector x y)
  (vector-push-extend (make-p :x x :y y) vector)
  (resort-points vector))

(defun maybe-find-curr-point (vector x y)
  (labels ((dsq (p)
             (let ((a (- (p-x p) x))
                   (b (- (p-y p) y)))
               (+ (* a a) (* b b))))
           (close-enough (p)
             (<= (dsq p) 100.0)))
    (find-if #'close-enough vector)))

(defun adjustable-vector-remove-at-index (idx vector)
  (cond ((= (length vector) 1)
         (setf (fill-pointer vector) 0))
        (t
         (rotatef (aref vector idx)
                  (aref vector (1- (length vector))))
         (decf (fill-pointer vector)))))

(defun adjustable-vector-remove-1 (item vector)
  (when-let (idx (position item vector))
    (adjustable-vector-remove-at-index idx vector)))

(deftest blob-editor (:width 800 :height 800)
  :init
  frame-count := 3
  dragging := nil
  curr-point := nil
  frames := (make-array frame-count :adjustable t)
  curr-frame := 0
  (map-into frames
            (lambda (_)
              (declare (ignore _))
              (make-array 16 :element-type 'p :adjustable t :fill-pointer 0))
            frames)
  points := (aref frames curr-frame)
  draw-points := t
  lerp-points := (make-array 16 :element-type 'p :adjustable t :fill-pointer 0)
  step := 0
  paused := nil
  speed := 30.0
  :update
  (if paused
      (draw-blob points)
      (progn
        (incf step (* dt speed))
        (when (> step 100.0)
          (setf step (- step 100.0)
                curr-frame (mod (1+ curr-frame) (length frames))
                curr-point nil
                points (aref frames curr-frame)))
        (draw-blob-lerp
         (aref frames curr-frame)
         (aref frames (mod (1+ curr-frame) (length frames)))
         lerp-points
         (* step 0.01))))
  (when draw-points
    (draw-points points))
  (when curr-point
    (draw-curr-point curr-point))
  :on-event
  (case (car event)
    (:mousedown
     (when curr-point (setf dragging t)))
    (:mouseup
     (when curr-point (setf dragging nil)))
    (:mousedrag
     (when curr-point
       (setf (p-x curr-point) (cadr event)
             (p-y curr-point) (cddr event)
             points (resort-points points))))
    (:mousemove
     (setf curr-point (maybe-find-curr-point points (cadr event) (cddr event))))
    (:click (let ((x (cadr event))
                  (y (cddr event)))
              (map-into frames (lambda (points)
                                 (add-point points x y))
                        frames)
              (setf points (aref frames curr-frame))))
    (:keypress
     (case (cdr event)
       (#\t (setf draw-points (not draw-points)))
       (#\x (when curr-point
              (when-let (idx (position curr-point points))
                (map-into frames (lambda (points)
                                   (adjustable-vector-remove-at-index idx points)
                                   (resort-points points))
                          frames))
              (setf curr-point nil)))
       (#\p (setf paused (not paused)))
       (:left
        (setf curr-point nil
              curr-frame (mod (1- curr-frame) (length frames))
              points (aref frames curr-frame)))
       (:right
        (setf curr-point nil
              curr-frame (mod (1+ curr-frame) (length frames))
              points (aref frames curr-frame)))
       (:up (incf speed 5.0))
       (:down
        (decf speed 5.0)
        (setf speed (max speed 0.0)))))))

(run-test 'blob-editor)
