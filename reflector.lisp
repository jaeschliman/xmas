(defpackage :xmas.reflector (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object :xmas.draw :xmas.lfont-reader :xmas.deftest))
(in-package :xmas.reflector)

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defclass reflector-node (node)
  ((reflection-count :initarg :reflection-count :initform 4)))

(defmethod visit ((self reflector-node))
  (when (not (visible self))
    (return-from visit))
  (xmas.render-buffer::push-matrix)
  (let ((ax (anchor-x self))
        (ay (anchor-y self)))
    (assert (zerop ax))
    (assert (zerop ay))
    (xmas.render-buffer::translate-scale-rotate
     (x self) (y self)
     (if (flip-x self) (* -1.0 (scale-x self)) (scale-x self))
     (if (flip-y self) (* -1.0 (scale-y self)) (scale-y self))
     (rotation self)))
  (when (children self)
    (let* ((count (slot-value self 'reflection-count ))
           (rot (/ 360.0 count)))
      (loop repeat count do
           (loop for child across (children self) do
                (visit child))
           (xmas.render-buffer::translate-scale-rotate 0.0 0.0 1.0 1.0 rot))))
  (xmas.render-buffer::pop-matrix))

(defclass rect (node) ())

(defmethod draw ((self rect))
  (draw-node-color self)
  (xmas.render-buffer::draw-rect 0 0 (content-width self) (content-height self)))

(deftest reflector-node (:width 500 :height 500)
  :init
  started := nil
  root := (make-instance 'reflector-node :x 250 :y 250 :reflection-count 5)
  colors := (circular-list
             (list 1.0 0.0 0.0)
             (list 0.0 1.0 0.0)
             (list 0.0 0.0 1.0))
  r1 := (make-instance 'rect :content-width 20.0 :content-height 20.0
                       :x 50 :y 20 :color (apply 'vector (pop colors)))
  r2 := (make-instance 'rect :content-width 20.0 :content-height 20.0
                       :x 50 :y 0 :color (apply 'vector (pop colors)))
  (run-action r1 (list
                  (move-by-x 3.0 -100)
                  (move-by-x 3.0 100))
              :repeat :forever)
  (run-action r2 (list
                  (move-by-y 3.0 -100)
                  (move-by-y 3.0 100))
              :repeat :forever)
  (run-action r1 (list
                  (apply #'tint-to 1.0 (pop colors))
                  (apply #'tint-to 1.0 (pop colors))
                  (apply #'tint-to 1.0 (pop colors)))
              :repeat :forever)
  (run-action r2 (list
                  (apply #'tint-to 1.0 (pop colors))
                  (apply #'tint-to 1.0 (pop colors))
                  (apply #'tint-to 1.0 (pop colors)))
              :repeat :forever)
  (run-action r1 (rotate-by 3.0 360) :repeat :forever)
  (run-action r2 (rotate-by 1.5 -360) :repeat :forever)
  (run-action root (rotate-by 10.0 -360) :repeat :forever)
  (add-children root (list r1 r2))
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (visit root))

(run-test 'reflector-node)
