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
                            :opacity 0.5
                            :x x :y y :color (apply 'vector (first colors))))
        (reflector  (make-instance 'reflector-node :x 25.0 :y 25.0 :reflection-count 12))
        (reflector2 (make-instance 'reflector-node :x 25.0 :y 25.0 :reflection-count 4))
        (med (make-instance 'rect :content-width 10.0 :content-height 10.0
                            :opacity 0.5
                            :x 25.0 :y 75.0 :color (apply 'vector (first colors))))
        (small (make-instance 'rect :content-width 5.0 :content-height 5.0
                              :opacity 0.8
                              :x 55.0 :y 15.0 :color (apply 'vector (first colors)))))
    (flet ((cycle (node)
             (run-action node (list
                               (apply #'tint-to 1.77 (pop colors))
                               (apply #'tint-to 1.77 (pop colors))
                               (apply #'tint-to 1.77 (pop colors))
                               (apply #'tint-to 1.77 (pop colors))
                               (apply #'tint-to 1.77 (pop colors))
                               (apply #'tint-to 1.77 (pop colors)))
                         :repeat :forever)))
      (cycle big)
      (cycle med)
      (cycle small)
      (run-action med (rotate-by 6.0 360.0) :repeat :forever)
      (run-action small (rotate-by 0.5 360) :repeat :forever)
      (run-action small (list
                         (move-by-x 1.5 -50 :ease :in-out-sine)
                         (move-by-x 1.5 50 :ease :in-out-sine))
                  :repeat :forever)
      (run-action med (list
                       (move-by-y 3.0 -100 :ease :in-out-sine)
                       (move-by-y 3.0 100 :ease :in-out-sine))
                  :repeat :forever)
      (let ((scale-offs 0.50)
            (scale-dur  0.65)
            (scale-node big))
        (run-action scale-node (list
                                (scale-x-to scale-dur (+ 1.0 scale-offs))
                                (scale-x-to scale-dur (- 1.0 scale-offs)))
                    :repeat :forever)
        (run-action scale-node (list
                                (scale-y-to scale-dur (- 1.0 scale-offs))
                                (scale-y-to scale-dur (+ 1.0 scale-offs)))
                    :repeat :forever))
      (add-child big reflector)
      (add-child reflector med)
      (add-child med reflector2)
      (add-child reflector2 small)
      big)))

(deftest reflector-node (:width 500 :height 500 :should-clear nil)
  :init
  started := nil
  root := (make-instance 'reflector-node :x 250.0 :y 250.0 :reflection-count 5)
  wash := (make-instance 'rect :content-width 500.0 :content-height 500.0 :color (vector 0.3 0.3 0.3) :opacity (/ 1.0 10.0))
  colors := (circular-list
             (list 1.0 0.0 0.0)
             (list 1.0 1.0 0.0)
             (list 0.0 1.0 0.0)
             (list 0.0 1.0 1.0)
             (list 0.0 0.0 1.0)
             (list 1.0 0.0 1.0))
  r1 := (make-rect 50.0 20.0 colors)
  r2 := (make-rect 50.0 0.0 colors)
  r3 := (make-rect 50.0 40.0 colors)
  (run-action r1 (list
                  (move-by-x 6.0 -100)
                  (move-by-x 6.0 100))
              :repeat :forever)
  (run-action r2 (list
                  (move-by-y 6.0 -100)
                  (move-by-y 6.0 100))
              :repeat :forever)
  (run-action r1 (rotate-by 3.0 360) :repeat :forever)
  (run-action r2 (rotate-by 1.5 -360) :repeat :forever)
  (run-action r3 (rotate-by 3.0 -360) :repeat :forever)
  (run-action root (rotate-by 3.0 -360) :repeat :forever)
  (add-children root (list wash r1 r2 r3))
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (visit root))

(run-test 'reflector-node)

(when nil
  (cl-user::with-current-window-contents (self)
    (with-slots (root) self
      (setf (slot-value  root 'reflection-count) 5))))
