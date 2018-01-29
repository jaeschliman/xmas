(defpackage :xmas.spotlight-node (:use :cl :alexandria
                                       :xmas.action
                                       :xmas.draw
                                       :xmas.node
                                       :xmas.texture
                                       :xmas.deftest))
(in-package :xmas.spotlight-node)


(defclass spotlight-node (node)
  ((radius :accessor spotlight-node-radius :initarg :radius)
   (texture :initarg :texture)))

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defmethod draw ((self spotlight-node))
  (draw-node-color self)
  (with-slots (radius texture) self
    (flet ((rect (x y w h) (xmas.render-buffer::draw-rect x y w h)))
      (let* ((-r (- radius))
             (d  (* 2.0 radius))
             (width d)
             (height d)
             (hw radius)
             (hh radius)
             (full-width (content-width self))
             (full-height (content-height self))
             (w (* 0.5 (- full-width width)))
             (h (* 0.5 (- full-height height))))
        (draw-texture-at texture -r -r width height)
        (rect (+ 0 hw)   (- 0 hh)   w          height)
        (rect (- 0 hw w) (- 0 hh)   w          height)
        (rect (- 0 hw w) (+ 0 hh)   full-width h)
        (rect (- 0 hw w) (- 0 hh h) full-width h)))))

(defun make-inverted-circle-image (width height)
  (xmas.vecto-texture:vecto-image (:width width :height height)
    (vecto:set-rgb-fill 1.0 1.0 1.0)
    (vecto:centered-circle-path (* 0.5 width) (* 0.5 height) (1- (* 0.5 (min width height))))
    (vecto:clip-path)
    (vecto:rectangle 0 0 width height)
    (vecto:fill-path)))


(deftest spotlight-node (:width 500 :height 500)
  :init
  started := nil
  width := 100
  height := 100
  background := (get-texture "./bayarea.png")
  texture := (make-texture-from-rgba-vector 
              (make-inverted-circle-image width height)
              width height)
  mouse-x := 250
  mouse-y := 250
  root := (make-instance 'spotlight-node
                         :x mouse-x :y mouse-y
                         :content-width 1000 :content-height 1000
                         :texture texture
                         :radius 0.0
                         :color (vector 0.0 0.0 0.0))
  (run-action root (list
                    (lerp-slot-to 1.0 'radius (* 250 (sqrt 2.0)))
                    (delay 1.0)
                    (lerp-slot-to 1.0 'radius 0.0)
                    (delay 0.5))
              :repeat :forever)
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (xmas.render-buffer::set-color 1.0 1.0 1.0 1.0)
  (draw-texture-at background 25.0 25.0 450.0 450.0)
  (setf (x root) mouse-x
        (y root) mouse-y)
  (visit root)
  :on-event
  (case (car event)
    (:mousemove (setf mouse-x (cadr event)
                      mouse-y (cddr event)))))

(run-test 'spotlight-node)
