(defpackage :xmas.reflector (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object :xmas.draw :xmas.lfont-reader :xmas.deftest))
(in-package :xmas.reflector)

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defclass reflector-node (node)
  ((reflection-count :initarg :reflection-count :initform 4)))

(defmethod visit ((self reflector-node))
 (let* ((count (slot-value self 'reflection-count))
        (rot (/ 360.0 count))
        (original-rotation (rotation self)))
      (loop repeat count 
           for i upfrom 0 do
           (setf (rotation self) (+ original-rotation (* i rot)))
           (call-next-method))
      (setf (rotation self) original-rotation)))

(defclass rect (node) ())

(defmethod draw ((self rect))
  (draw-node-color self)
  (xmas.render-buffer::draw-rect 0 0 (content-width self) (content-height self)))

(defun make-rect (x y colors)
  (let ((big (make-instance 'rect :content-width 20.0 :content-height 20.0
                            :x x :y y :color (apply 'vector (first colors))))
        (small (make-instance 'rect :content-width 5.0 :content-height 5.0
                              :x 25 :y 25 :color (apply 'vector (first colors)))))
    (flet ((cycle (node)
             (run-action node (list
                               (apply #'tint-to 1.0 (pop colors))
                               (apply #'tint-to 1.0 (pop colors))
                               (apply #'tint-to 1.0 (pop colors)))
                         :repeat :forever)))
      (cycle big)
      (cycle small)
      (add-child big small)
      big)))

(deftest reflector-node (:width 500 :height 500)
  :init
  started := nil
  root := (make-instance 'reflector-node :x 250 :y 250 :reflection-count 5)
  colors := (circular-list
             (list 1.0 0.0 0.0)
             (list 0.0 1.0 0.0)
             (list 0.0 0.0 1.0))
  r1 := (make-rect 50 20 colors)
  r2 := (make-rect 50 0 colors)
  r3 := (make-rect 50 40 colors)
  (run-action r1 (list
                  (move-by-x 3.0 -100)
                  (move-by-x 3.0 100))
              :repeat :forever)
  (run-action r2 (list
                  (move-by-y 3.0 -100)
                  (move-by-y 3.0 100))
              :repeat :forever)
  
  (run-action r1 (rotate-by 3.0 360) :repeat :forever)
  (run-action r2 (rotate-by 1.5 -360) :repeat :forever)
  (run-action r3 (rotate-by 3.0 -360) :repeat :forever)
  (run-action root (rotate-by 10.0 -360) :repeat :forever)
  (add-children root (list r1 r2 r3))
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (visit root))

(run-test 'reflector-node)
