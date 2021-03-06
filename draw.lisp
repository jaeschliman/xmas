(defpackage xmas.draw (:use :cl :alexandria :xmas.texture)
            (:export
             #:draw-texture-frame
             #:draw-texture
             #:draw-texture-frame-at
             #:draw-texture-at
             #:draw-texture-at-tex-coords))
(in-package xmas.draw)

(defun draw-texture (texture)
  (when (texture-id texture)
    (xmas.render-buffer::simple-draw-gl-texture-no-color
     (texture-id texture)
     (texture-width texture)
     (texture-height texture))))

(defun draw-texture-at (texture x y w h)
  (when-let (id (texture-id texture))
    (xmas.render-buffer::draw-gl-texture-at id x y w h))) 

(defun draw-texture-at-tex-coords (texture x y w h u0 v0 u1 v1)
  (when-let (id (texture-id texture))
    (xmas.render-buffer::draw-gl-texture-at-tex-coords id x y w h u0 v0 u1 v1)))

(defun draw-texture-frame (frame x y &optional outset)
  (when-let (id (texture-id (texture-frame-texture frame)))
    (let ((w (texture-frame-width frame))
          (h (texture-frame-height frame)))
      (when outset
        (decf x 0.5)
        (decf y 0.5)
        (incf w 1.0)
        (incf h 1.0))
      (if (texture-frame-rotated frame)
          (xmas.render-buffer::simple-draw-gl-with-tex-coords-rotated
           id x y w h
           (texture-frame-tx1 frame)
           (texture-frame-ty1 frame)
           (texture-frame-tx2 frame)
           (texture-frame-ty2 frame))
          (xmas.render-buffer::simple-draw-gl-with-tex-coords
           id x y w h
           (texture-frame-tx1 frame)
           (texture-frame-ty1 frame)
           (texture-frame-tx2 frame)
           (texture-frame-ty2 frame))))))

(defun draw-texture-frame-at (frame x y w h &optional outset)
  (when-let (id (texture-id (texture-frame-texture frame)))
    (when outset
      (decf x 0.5)
      (decf y 0.5)
      (incf w 1.0)
      (incf h 1.0))
    (if (texture-frame-rotated frame)
        (xmas.render-buffer::draw-gl-with-tex-coords-rotated
         id x y w h
         (texture-frame-tx1 frame)
         (texture-frame-ty1 frame)
         (texture-frame-tx2 frame)
         (texture-frame-ty2 frame))
        (xmas.render-buffer::draw-gl-with-tex-coords
         id x y w h
         (texture-frame-tx1 frame)
         (texture-frame-ty1 frame)
         (texture-frame-tx2 frame)
         (texture-frame-ty2 frame)))))
