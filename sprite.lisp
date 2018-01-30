(defpackage :xmas.sprite (:use :cl :alexandria :xmas.texture :xmas.node)
            (:export
             #:sprite
             #:sprite-frame
             #:stop-animation
             #:run-animation))
(in-package :xmas.sprite)

(defclass sprite (node)
  ((sprite-frame :accessor sprite-frame :initarg :sprite-frame))
  (:default-initargs
   :anchor-x 0.5 :anchor-y 0.5
   :content-width 0.0 :content-height 0.0))

(defmethod initialize-instance ((self sprite)
                                &key sprite-frame content-width content-height
                                  &allow-other-keys)
  (call-next-method)
  (let ((width (texture-frame-width sprite-frame))
        (height (texture-frame-height sprite-frame))
        (rotated (texture-frame-rotated sprite-frame)))
    (when (zerop content-width)
      (setf (content-width self) (if rotated height width)))
    (when (zerop content-height)
      (setf (content-height self) (if rotated width height)))))

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
