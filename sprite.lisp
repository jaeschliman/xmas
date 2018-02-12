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

(defun %draw-sprite (self frame xform)
  (declare (type sprite self)
           (type texture-frame frame)
           (type xmas.matrix::m4 xform)
           (optimize (speed 3) (safety 1)))
  (let* ((flip-x (flip-x self))
         (flip-y (flip-y self))
         (frame-width (texture-frame-width frame))
         (frame-height (texture-frame-height frame))
         (rotated (texture-frame-rotated frame))
         (width (if rotated frame-height frame-width))
         (height (if rotated frame-width frame-height))
         (offs-x (* 0.5 (- (content-width self) width)))
         (offs-y (* 0.5 (- (content-height self) height)))
         (tx1 (texture-frame-tx1 frame))
         (tx2 (texture-frame-tx2 frame))
         (ty1 (texture-frame-ty1 frame))
         (ty2 (texture-frame-ty2 frame)))
    (multiple-value-bind (llx lly ulx uly urx ury lrx lry)
        (four-corners offs-x offs-y (+ offs-x width) (+ offs-y height) xform)
      (when flip-y
        (rotatef (values lly llx) (values uly ulx))
        (rotatef (values lry lrx) (values ury urx)))
      (when flip-x
        (rotatef (values llx lly) (values lrx lry))
        (rotatef (values ulx uly) (values urx ury)))
      (if rotated
          ;; d c
          ;; a b
          (xmas.render-buffer::%draw-quad
           llx lly lrx lry urx ury ulx uly 
           tx1 ty1
           tx2 ty2)
          ;; a d
          ;; b c
          (xmas.render-buffer::%draw-quad
           ulx uly llx lly lrx lry urx ury
           tx1 ty1
           tx2 ty2)))))

(defmethod draw-with-xform ((self sprite) xform)
  (when-let* ((frame (sprite-frame self))
              (id (texture-id (texture-frame-texture frame))))
    (xmas.render-buffer::with-colored-textured-2d-quads (id)
      (let ((c (color self)))
        (xmas.render-buffer::%draw-quad-color
         (svref c 0) (svref c 1) (svref c 2) (opacity self)))
     (%draw-sprite self frame xform))))
