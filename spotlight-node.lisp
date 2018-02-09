(defpackage :xmas.spotlight-node (:use :cl :alexandria
                                       :xmas.matrix
                                       :xmas.matrix-stack
                                       :xmas.action
                                       :xmas.draw
                                       :xmas.node
                                       :xmas.texture
                                       :xmas.deftest)
            (:export
             #:spotlight-node))
(in-package :xmas.spotlight-node)


(defclass spotlight-node (node)
  ((radius :accessor spotlight-node-radius :initarg :radius)
   (texture :initarg :texture))
  (:default-initargs
   :texture (make-texture-from-rgba-vector
             (make-inverted-circle-image 100 100)
             100 100)))

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defmethod draw ((self spotlight-node))
  (draw-node-color self)
  (with-slots (radius texture) self
    (flet ((rect (x y w h) (xmas.render-buffer::draw-rect x y w h)))
      (declare (dynamic-extent (function rect)))
      (let* ((d (* 2.0 radius))
             (width d)
             (height d)
             (hw radius)
             (hh radius)
             (full-width (content-width self))
             (full-height (content-height self))
             (w (* 0.5 (- full-width width)))
             (h (* 0.5 (- full-height height)))
             (cx (* 0.5 full-width))
             (cy (* 0.5 full-height)))
        (draw-texture-at texture (- cx radius) (- cy radius) width height)
        (rect (+ cx hw)   (- cy hh)   w          height)
        (rect (- cx hw w) (- cy hh)   w          height)
        (rect (- cx hw w) (+ cy hh)   full-width h)
        (rect (- cx hw w) (- cy hh h) full-width h)))))

(defun %draw-texture-at (tex x y w h matrix)
  (let ((tx1 0.0) (tx2 1.0)
        (ty1 1.0) (ty2 0.0))
    (multiple-value-bind (llx lly ulx uly urx ury lrx lry)
        (four-corners x y
                      (+ x w)
                      (+ y h)
                      matrix)
      (xmas.render-buffer::%draw-quad
       llx lly ulx uly urx ury lrx lry
       tx1 ty1
       tx2 ty2))))

(defmethod draw-with-xform ((self spotlight-node) matrix)
  (draw-node-color self)
  (with-slots (radius texture) self
    (flet ((rect (x y w h)
             (multiple-value-bind (llx lly ulx uly urx ury lrx lry)
                 (four-corners x y (+ x w) (+ y h) matrix)
               (xmas.render-buffer::%draw-quad
                llx lly ulx uly urx ury lrx lry
                0.0 0.0 0.0 0.0))))
      (declare (dynamic-extent (function rect)))
      (let* ((d (* 2.0 radius))
             (width d)
             (height d)
             (hw radius)
             (hh radius)
             (full-width (content-width self))
             (full-height (content-height self))
             (w (* 0.5 (- full-width width)))
             (h (* 0.5 (- full-height height)))
             (cx (* 0.5 full-width))
             (cy (* 0.5 full-height)))
        (when-let (id (texture-id texture))
          (xmas.render-buffer::with-textured-2d-quads (id)
            (%draw-texture-at texture (- cx radius) (- cy radius) width height matrix))
          ;;TODO: write with-untextured-2d-quads
          (xmas.render-buffer::with-textured-2d-quads (0)
            (rect (+ cx hw)   (- cy hh)   w          height)
            (rect (- cx hw w) (- cy hh)   w          height)
            (rect (- cx hw w) (+ cy hh)   full-width h)
            (rect (- cx hw w) (- cy hh h) full-width h)))))))

(defun make-inverted-circle-image (width height)
  (xmas.vecto-texture:vecto-image (:width width :height height)
    (vecto:set-rgb-fill 1.0 1.0 1.0)
    (vecto:centered-circle-path (* 0.5 width) (* 0.5 height)
                                (1- (* 0.5 (min width height))))
    (vecto:clip-path)
    (vecto:rectangle 0 0 width height)
    (vecto:fill-path)))


(deftest spotlight-node (:width 500 :height 500)
  :init
  matrix-stack := (make-matrix-stack)
  started := nil
  width := 100
  height := 100
  background := (get-texture "./bayarea.png")
  texture := (make-texture-from-rgba-vector
              (make-inverted-circle-image width height)
              width height)
  mouse-x := 250.0
  mouse-y := 250.0
  root := (make-instance 'spotlight-node
                         :x mouse-x :y mouse-y
                         :content-width 1000.0 :content-height 1000.0
                         :texture texture
                         :radius 0.0
                         :anchor-x 0.5 :anchor-y 0.5
                         :color (vector 0.0 0.0 0.0))
  (run-action root (list
                    (lerp-slot-to 1.0 'radius (* 250.0 (sqrt 2.0)))
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
  (let ((*matrix-stack* matrix-stack))
    (visit-with-xform root))
  :on-event
  (case (car event)
    (:mousemove (setf mouse-x (cadr event)
                      mouse-y (cddr event)))))

;; (run-test 'spotlight-node)
