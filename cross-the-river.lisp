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

(defclass image (node)
  ((texture :accessor texture :initarg :texture))
  (:default-initargs :anchor-x 0.5 :anchor-y 0.5))

(defmethod initialize-instance ((self image) &key texture)
  (call-next-method)
  (setf (content-width self) (texture-width texture)
        (content-height self) (texture-height texture)))

(defmethod draw ((self image))
  (draw-node-color self)
  (let ((texture (texture self)))
    (draw-texture-at texture 0.0 0.0
                     (texture-width texture)
                     (texture-height texture))))

;; end boilerplate
;;------------------------------------------------------------

(defclass repeater-node (node)
  ((texture :accessor texture :initarg :texture)))

(defclass wavy-node (node)
  ((texture :accessor texture :initarg :texture)
   (overlap :accessor overlap :initarg :overlap)
   (waves   :accessor waves   :initform nil))
  (:default-initargs
   :overlap 0.8))

(defmethod initialize-instance ((self wavy-node) &key)
  (call-next-method)
  (let* ((tex (texture self))
         (h (* (overlap self) (texture-height tex)))
         (count (ceiling (height self) h))
         (y 0)
         (odd 1.0)
         (waves (make-array count :element-type t)))
    (setf (waves self) waves)
    (dotimes (i count)
      (flet ((make-node ()
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
                             :repeat :forever)
                 node)))
        (setf (aref waves i) (make-node))
        (setf odd (* -1.0 odd))
        (incf y h)))))

(defmethod on-enter ((self wavy-node))
  (call-next-method)
  (map nil #'on-enter (waves self)))

(defmethod on-exit ((self wavy-node))
  (call-next-method)
  (map nil #'on-exit (waves self)))

(defmethod draw ((self repeater-node))
  (let* ((texture (texture self))
         (tw (texture-width texture))
         (u1 (coerce (/ (width self) tw) 'float)))
    (draw-texture-at-tex-coords texture 0.0 0.0
                                (width self) (height self)
                                0.0 0.01 u1 1.0)))

(defmethod draw ((self wavy-node))
  (loop
     with waves = (waves self)
     for i from (1- (length waves)) downto 0 do
       (visit (aref waves i))))

(xmas.deftest:deftest cross-the-river (:width 500 :height 500)
  :tags sketch
  :init
  started := nil
  tex := (get-texture "./res/cross-the-river/wave.png" :wrap :repeat)
  root := (make-instance 'node)
  north-shore := (make-instance 'image
                                :anchor-x 0.0
                                :anchor-y 1.0
                                :y 500.0
                                :texture (get-texture "./res/cross-the-river/north-shore.png"))
  node := (make-instance 'wavy-node
                         :texture tex
                         :content-width 500.0
                         :content-height 250.0
                         :y 100
                         :overlap 0.5)

  south-shore := (make-instance 'image
                                :anchor-x 0.0
                                :anchor-y 0.0
                                :texture (get-texture "./res/cross-the-river/south-shore.png"))
  (add-child root north-shore)
  (add-child root node)
  (add-child root south-shore)
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (visit root))

(xmas.deftest:run-test 'cross-the-river)
