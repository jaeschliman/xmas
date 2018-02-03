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

(defmethod draw-with-xform ((self sprite) xform)
  (when-let* ((frame (sprite-frame self))
              (id (texture-id (texture-frame-texture frame))))
    (xmas.render-buffer::with-textured-2d-quads (id)
      (let ((flip-x (flip-x self))
            (flip-y (flip-y self))
            (tx1 (texture-frame-tx1 frame))
            (tx2 (texture-frame-tx2 frame))
            (ty1 (texture-frame-ty1 frame))
            (ty2 (texture-frame-ty2 frame))
            (rotated (texture-frame-rotated frame)))
        (multiple-value-bind (llx lly ulx uly urx ury lrx lry)
            (node-four-corners self xform)
          (if rotated
              (progn 
                (when flip-y (rotatef tx1 tx2))
                (when flip-x (rotatef ty1 ty2))
                (xmas.render-buffer::%draw-quad
                 llx lly lrx lry urx ury ulx uly 
                 tx1 ty1
                 tx2 ty2))
              (progn
                (when flip-x (rotatef tx1 tx2))
                (when flip-y (rotatef ty1 ty2))
                (xmas.render-buffer::%draw-quad
                 llx lly ulx uly urx ury lrx lry
                 tx1 ty1
                 tx2 ty2))))))))
