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

(defun draw-points (vector)
  (cond
    ((> (length vector) 3)
     (draw-blob-catmull-romm vector))
    ((> (length vector) 2)
     (draw-blob-simple vector)))
  (loop for p across vector do
       (draw-circle (p-x p) (p-y p) 10.0 255 255 255 128)))

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

(deftest blob-editor (:width 800 :height 800)
  :init
  dragging := nil
  curr-point := nil
  points := (make-array 1024 :element-type 'p :adjustable t :fill-pointer 0)
  :update
  (draw-points points)
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
     (unless dragging
       (setf curr-point (maybe-find-curr-point points (cadr event) (cddr event)))))
    (:click (let ((x (cadr event))
                  (y (cddr event)))
              (setf points (add-point points x y))))))

(run-test 'blob-editor)
