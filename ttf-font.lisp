(defpackage :xmas.ttf-font (:use :cl :alexandria :xmas.texture :xmas.vecto-texture :xmas.lfont-reader)
            (:export
             #:make-font))
(in-package :xmas.ttf-font)

(defun font-scale (font size)
  (float (/ size (zpb-ttf:units/em font))))

(defun advance-width (font size char)
  (let ((glyph (zpb-ttf:find-glyph char font))
        (scale (font-scale font size)))
    (* scale (zpb-ttf:advance-width glyph))))

(defun retract-width (font size char)
  (let* ((scale (font-scale font size))
         (str (make-array 1 :element-type 'character :initial-element char))
         (bb  (zpb-ttf:string-bounding-box str font :kerning nil)))
    (declare (dynamic-extent str))
    (* scale (zpb-ttf:xmin bb))))

(defun glyph-width (font size char)
  (let* ((scale (font-scale font size))
         (str (make-array 1 :element-type 'character :initial-element char))
         (bb  (zpb-ttf:string-bounding-box str font :kerning nil))
         (width (- (zpb-ttf:xmax bb) (min (zpb-ttf:xmin bb) 0.0)))
         (glyph (zpb-ttf:find-glyph char font)))
    (declare (dynamic-extent str))
    (* scale (max width (zpb-ttf:advance-width glyph)
                  ))))

(defun draw-string-glyphs (string font size)
  (vecto:with-graphics-state
    (vecto:translate 0 1)
    (loop for idx below (length string)
       for char = (aref string idx)
       for width = (ceiling (glyph-width font size char))
       for str = (subseq string idx (1+ idx)) do
         (vecto:translate 2 0)
         (vecto:draw-string 0 0 str)
         (vecto:translate (+ 2 width) 0.0))))

(defun calculate-width (string font size)
  (loop for idx below (length string)
     for char = (aref string idx)
     sum (+ 4.0 (ceiling (glyph-width font size char)))))

(defparameter *default-font-chars* "0123456789ABCDEFGHIJKLMNOPQRSTUVXZYabcdefghijklmnopqrstuvwxyz .,!?")

(defun make-font (&key font size (characters *default-font-chars*))
  (let* ((font (zpb-ttf:open-font-loader font))
         (scale (font-scale font size))
         (em-bounding-box (zpb-ttf:string-bounding-box characters font :kerning nil))
         (bounding-box (map 'vector (lambda (s) (* s scale)) em-bounding-box))
         (xmin (zpb-ttf:xmin bounding-box))
         (xmax (zpb-ttf:xmax bounding-box))
         (ymin (zpb-ttf:ymin bounding-box))
         (ymax (zpb-ttf:ymax bounding-box))
         (width (max
                 (calculate-width characters font size)
                 (ceiling (if (< xmin 0) (+ xmax (abs xmin)) xmax))))
         (height (+ 2 (ceiling (if (< ymin 0) (+ ymax (abs ymin)) ymax))))
         (vertical-padding (if (< ymin 0.0) (abs ymin) 0.0))
         (image-width (ceiling width))
         (image-height (ceiling height))
         (bounding-boxes nil)
         (image nil))
    (unwind-protect
         (progn
           (loop
              with left = 2.0
              for char across characters
              for advance = (advance-width font size char)
              for retract = (retract-width font size char)
              for width = (ceiling (glyph-width font size char)) do
                (push (list char left 0.0 width height retract advance) bounding-boxes)
                (incf left (+ width 4.0)))
           (setf image
                 (vecto-image (:width image-width :height image-height)
                   (vecto:set-font font size)
                   (vecto:set-rgb-fill 1.0 1.0 1.0)
                   (vecto:translate 0.0 vertical-padding)
                   (draw-string-glyphs characters font size))))
      (zpb-ttf:close-font-loader font))
    (let* ((font (make-lfont))
           (texture (make-texture-from-rgba-vector image image-width image-height))
           (table (lfont-chars font)))
      (prog1 font
        (loop for entry in bounding-boxes
           for (char x y width height retract advance) = entry
           for frame = (texture-frame texture x y width height) do
             (setf (gethash char table) (list retract advance frame)))))))
