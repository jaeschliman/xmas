(defpackage :xmas.lfont-reader (:use :cl :alexandria :xmas.texture xmas.draw)
            (:export
             #:lfont-from-file
             #:lfont-draw-string))
(in-package :xmas.lfont-reader)

;;TODO: move this into a util file
(defun file-pathname-relative-to-file (path file)
  (let ((dir (make-pathname :directory (pathname-directory (truename path)))))
    (merge-pathnames file dir)))

(defstruct lfont
  (chars (make-hash-table :test 'eql)))

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
           for x = (getf entry :x)
           for y = (getf entry :y)
           for width = (getf entry :width)
           for height = (getf entry :height)
           for frame = (texture-frame texture x y width height)
           do
             (setf (gethash char table) frame))))))


(defun lfont-draw-string (lfont string x y &key (letter-spacing 1.0))
  (let ((table (lfont-chars lfont))
        (left  x))
    (loop for char across string
       for frame = (gethash char table)
       when frame do
         (let ((width (texture-frame-width frame))
               (height (texture-frame-height frame)))
           (draw-texture-frame-at frame left y width height)
           (incf left (+ letter-spacing width))))))
