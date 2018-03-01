(defpackage :xmas.lfont-reader (:use :cl :alexandria :xmas.texture :xmas.draw :xmas.node)
            (:export
             #:lfont-from-file
             #:lfont-draw-string
             #:lfont
             #:make-lfont
             #:lfont-chars
             #:lfont-texture
             #:draw-string))
(in-package :xmas.lfont-reader)

;;TODO: move this into a util file
(defun file-pathname-relative-to-file (path file)
  (let ((dir (make-pathname :directory (pathname-directory (truename path)))))
    (merge-pathnames file dir)))

(defstruct lfont
  (chars (make-hash-table :test 'eql))
  (texture nil))

(defun lfont-from-file (path)
  (with-input-from-file (s path)
    (let* ((*read-eval* nil)
           (header (read s))
           (image-path (file-pathname-relative-to-file path (getf header :image)))
           (texture (get-texture image-path))
           (count (getf header :count))
           (font (make-lfont))
           (table (lfont-chars font)))
      (prog1 font
        (loop repeat count 
           for entry = (read s)
           for char = (getf entry :char)
           for x = (float (getf entry :x))
           for y = (float (getf entry :y))
           for width = (float (getf entry :width))
           for height = (float (getf entry :height))
           for frame = (texture-frame texture x y width height)
           do
             (setf (gethash char table) frame))))))


(defun lfont-draw-string (lfont string x y &key (letter-spacing 1.0))
  (let ((table (lfont-chars lfont))
        (left  x))
    (loop for char across string
       for (retract advance frame) = (gethash char table)
       when frame do
         (let ((width (texture-frame-width frame))
               (height (texture-frame-height frame)))
           (decf left (ceiling retract))
           (draw-texture-frame-at frame left y width height)
           (incf left (+ letter-spacing (floor advance)))))))

(defun draw-string (lfont string x y matrix &key
                                              (letter-spacing 1.0)
                                              (r 1.0)
                                              (g 1.0)
                                              (b 1.0)
                                              (a 1.0))
  (when-let ((id (texture-id (lfont-texture lfont))))
    (let ((table (lfont-chars lfont))
          (left  x))
      (xmas.render-buffer::with-colored-textured-2d-quads (id)
        (loop for char across string
           for (retract advance frame) = (gethash char table)
           when frame do
             (let ((width (texture-frame-width frame))
                   (height (texture-frame-height frame)))
               (decf left (ceiling retract))
               (multiple-value-bind (llx lly ulx uly urx ury lrx lry)
                   (four-corners left y (+ left width) (+ y height) matrix)
                 (let ((tx1 (texture-frame-tx1 frame))
                       (ty1 (texture-frame-ty1 frame))
                       (tx2 (texture-frame-tx2 frame))
                       (ty2 (texture-frame-ty2 frame)))
                   (xmas.render-buffer::%draw-quad
                    llx lly ulx uly urx ury lrx lry
                    tx1 ty2 tx2 ty1)
                   (xmas.render-buffer::%draw-quad-color r g b a)))
               (incf left (+ letter-spacing (floor advance)))))))))
