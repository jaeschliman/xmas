(defpackage xmas.tmx-renderer (:use :cl :alexandria)
            (:export
             #:draw-tmx-layer
             #:tmx-renderer
             #:make-tmx-renderer
             #:tmx-renderer-width
             #:tmx-renderer-height
             #:tmx-renderer-tile-width
             #:tmx-renderer-tile-height
             #:tmx-renderer-layer
             #:tmx-renderer-frames
             #:tmx-renderer-map
             #:make-tileset-texture-frames
             #:tmx-renderer-from-file
             #:tmx-renderer-tile-properties
             #:tmx-renderer-tile-at-point
             #:draw-tmx-renderer))
(in-package xmas.tmx-renderer)

(defun draw-tmx-layer (at-x at-y tile-width tile-height layer frames)
  (let* ((cols (xmas.tmx-reader:layer-width layer))
         (rows (xmas.tmx-reader:layer-height layer))
         (start-x (+ (- at-x (/ (* tile-width cols) 2.0)) (/ tile-width 2.0)))
         (start-y (- (+ at-y (/ (* tile-height rows) 2.0)) (/ tile-width 2.0))))
    (dotimes (row rows)
      (dotimes (col cols)
        (when-let (frame (aref frames (xmas.tmx-reader:layer-gid-at layer col row)))
          (let ((x (+ start-x (* col tile-width)))
                (y (- start-y (* row tile-height))))
            (xmas.draw:draw-texture-frame frame x y t)))))))

(defun make-tileset-texture-frames (tileset)
  (let* ((texture (xmas.texture:get-texture (xmas.tmx-reader:tileset-source tileset)))
         (tile-width (coerce (xmas.tmx-reader:tileset-tile-width tileset) 'float))
         (tile-height (coerce (xmas.tmx-reader:tileset-tile-height tileset) 'float))
         (texture-width (xmas.texture:texture-width texture))
         (texture-height (xmas.texture:texture-height texture))
         (cols (floor texture-width tile-width))
         (rows (floor texture-height tile-height))
         ;;TODO: read this from input
         (first-gid 1)
         (vec (make-array (1+ (* rows cols)) :element-type t :initial-element nil))
         (idx first-gid))
    ;; (format t "got ~S elements ~%" (length vec))
    ;; (format t "texture width = ~S ~%" texture-width)
    ;; (format t "texture height = ~S ~%" texture-height)
    ;; (format t "tile-width = ~S ~%" tile-width)
    ;; (format t "tile-height = ~S ~%" tile-height)
    (prog1 vec
      (dotimes (row rows)
        (dotimes (col cols)
          (setf (aref vec idx)
                (xmas.texture:texture-frame texture
                                       (+ (* col tile-width) 0.5)
                                       (+ (* row tile-height) 0.5)
                                       (- tile-width 1.0)
                                       (- tile-height 1.0)
                                       :flipped nil))
          (incf idx))))))

(defstruct tmx-renderer
  width height tile-width tile-height layer frames map)


(defun tmx-renderer-from-file (path)
  (let* ((map (xmas.tmx-reader:read-tilemap path))
         (tileset (first (xmas.tmx-reader:map-tilesets map)))
         (frames (make-tileset-texture-frames tileset))
         (layer (first (xmas.tmx-reader:map-layers map)))
         (tile-width (xmas.tmx-reader:tileset-tile-width tileset))
         (tile-height (xmas.tmx-reader:tileset-tile-height tileset))
         (width (* tile-width (xmas.tmx-reader:layer-width layer)))
         (height (* tile-height (xmas.tmx-reader:layer-height layer))))
    (make-tmx-renderer
     :width width :height height
                       :tile-width tile-width :tile-height tile-height
                       :layer layer :frames frames :map map)))

(defun tmx-renderer-tile-properties (tmx)
  (xmas.tmx-reader:map-tile-properties (tmx-renderer-map tmx)))

(defun tmx-renderer-tile-at-point (tmx x y &optional (default 0))
  (let* ((w (tmx-renderer-width tmx))
         (h (tmx-renderer-height tmx))
         (tw (tmx-renderer-tile-width tmx))
         (th (tmx-renderer-tile-height tmx))
         (height-in-tiles (floor h th))
         (layer (tmx-renderer-layer tmx)))
    (cond ((or (<= x 0.0) (>= x w)) default)
          ((or (<= y 0.0) (>= y h)) default)
          (t (let ((tx (floor x tw))
                   (ty (- height-in-tiles (floor y th) 1)))
               (xmas.tmx-reader:layer-gid-at layer tx ty))))))

(defun draw-tmx-renderer (x y renderer)
  (draw-tmx-layer x y
                  (tmx-renderer-tile-width renderer)
                  (tmx-renderer-tile-height renderer)
                  (tmx-renderer-layer renderer)
                  (tmx-renderer-frames renderer)))
