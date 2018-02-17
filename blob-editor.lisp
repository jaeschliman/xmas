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

(defun draw-points (vector)
  (when (> (length vector) 2)
    (xmas.render-buffer::with-colored-2d-triangle-fan ()
      (macrolet ((v (x y r g b a)
                   `(xmas.render-buffer::vert ,x ,y ,r ,g ,b ,a)))
        (v 400.0 400.0 0 255 0 255)
        (loop for p across vector do
             (v (p-x p) (p-y p) 255 0 0 255))
        (let ((p (aref vector 0)))
          (v (p-x p) (p-y p) 255 0 0 255)))))
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
