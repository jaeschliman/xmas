(defpackage :xmas.cross-the-river (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object :xmas.draw))
(in-package :xmas.cross-the-river)

;;------------------------------------------------------------
;; boilerplate visit/draw code
;; TODO: move these out into common file (will require updating tests)
(defgeneric draw (node))
(defmethod draw ((self node)))

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defmethod draw ((self sprite))
  (draw-node-color self)
  (let* ((frame (sprite-frame self))
         (frame-width (texture-frame-width frame))
         (frame-height (texture-frame-height frame))
         (width
          (if (texture-frame-rotated frame) frame-height frame-width))
         (height
          (if (texture-frame-rotated frame) frame-width frame-height))
         (offs-x (* 0.5 (- (content-width self) width)))
         (offs-y (* 0.5 (- (content-height self) height))))
    (xmas.draw:draw-texture-frame-at
     frame offs-x offs-y
     frame-width
     frame-height)))

(defmethod visit ((self node))
  (when (not (visible self))
    (return-from visit))
  (xmas.render-buffer::push-matrix)
  (let ((ax (anchor-x self))
        (ay (anchor-y self)))
    (if (or nil (and (zerop ax) (zerop ay)))
        (xmas.render-buffer::translate-scale-rotate
         (x self) (y self)
         (if (flip-x self) (* -1.0 (scale-x self)) (scale-x self))
         (if (flip-y self) (* -1.0 (scale-y self)) (scale-y self))
         (rotation self))
        (xmas.render-buffer::translate-scale-rotate-translate
         (x self) (y self)
         (if (flip-x self) (* -1.0 (scale-x self)) (scale-x self))
         (if (flip-y self) (* -1.0 (scale-y self)) (scale-y self))
         (rotation self)
         (* -1.0 ax (content-width self))
         (* -1.0 ay (content-height self)))))
  (draw self)
  (when (children self)
    (loop for child across (children self) do
         (visit child)))
  (xmas.render-buffer::pop-matrix))

;; end boilerplate
;;------------------------------------------------------------

(defclass repeater-node (node)
  ((texture :accessor texture :initarg :texture)))

(defclass wavy-node (node)
  ((texture :accessor texture :initarg :texture)
   (overlap :accessor overlap :initarg :overlap))
  (:default-initargs
   :overlap 0.8))

(defmethod initialize-instance ((self wavy-node) &key)
  (call-next-method)
  (let* ((tex (texture self))
         (h (* (overlap self) (texture-height tex)))
         (count (ceiling (height self) h))
         (y 0)
         (odd 1.0))
    (dotimes (i count)
         (let ((node (make-instance 'repeater-node
                                    :texture tex
                                    :content-width  (+ (width self) 60.0)
                                    :content-height (texture-height tex)
                                    :x (- (+ 20.0 (random 20.0) ))
                                    :y y
                                    :z-order (- count i)))
               (dur (+ 1.5 (random 1.5)))
               (h-amt 20.0)
               (v-amt 2.0))
           (run-action node (list (move-by dur (* h-amt odd) (* v-amt odd -1)
                                           :ease :in-out-sine)
                                  (move-by dur (* h-amt odd -1) (* v-amt odd)
                                           :ease :in-out-sine))
                       :repeat :forever )
           (add-child self node)
           (incf y h)
           (setf odd (* -1.0 odd))))))

(defmethod draw ((self repeater-node))
  (let* ((texture (texture self))
         (tw (texture-width texture))
         (u1 (coerce (/ (width self) tw) 'float)))
    (draw-texture-at-tex-coords texture 0.0 0.0
                                (width self) (height self)
                                0.0 0.0 u1 1.0)))

(xmas.deftest:deftest cross-the-river (:width 500 :height 500)
  :tags sketch
  :init
  started := nil
  tex := (get-texture "./res/cross-the-river/wave.png" :wrap :repeat)
  node := (make-instance 'wavy-node
                         :texture tex
                         :content-width 500.0
                         :content-height 300.0
                         :y 100
                         :overlap 0.5)

  :update
  (unless started
    (setf started t)
    (on-enter node))
  (visit node))

(xmas.deftest:run-test 'cross-the-river)
