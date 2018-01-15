(defpackage :xmas.lfont-writer (:use :cl :alexandria))
(in-package :xmas.lfont-writer)

(defun font-scale (font size)
  (float (/ size (zpb-ttf:units/em font))))

(defun glyph-width (font size char)
  (let ((glyph (zpb-ttf:find-glyph char font))
        (scale (font-scale font size)))
    (* scale (zpb-ttf:advance-width glyph))))

(defun my-draw-string (string font size)
  (vecto:with-graphics-state
    (vecto:translate 0 1)
    (loop for idx below (length string)
       for char = (aref string idx)
       for str = (subseq string idx (1+ idx)) do
         (vecto:translate 1 0)
         (vecto:draw-string 0 0 str)
         (vecto:translate (1+ (ceiling (glyph-width font size char))) 0.0))))

(defun calculate-width (string font size)
  (loop for idx below (length string)
     for char = (aref string idx)
     sum (+ 2.0 (ceiling (glyph-width font size char)))))

(defun save-font-output (path image-path size bounding-boxes)
  (with-output-to-file (s path :if-exists :supersede)
    (prin1 `(:version 0 :image ,image-path
                      :height ,size :count ,(length bounding-boxes))
           s)
    (terpri s)
    (dolist (b bounding-boxes)
      (prin1 b s)
      (terpri s))))

(defun write-font-string (string
                          &key
                            font
                            size
                            image-output-path
                            font-output-path
                            output-folder)
  (let* ((font (zpb-ttf:open-font-loader font))
         (scale (font-scale font size))
         (em-bounding-box (zpb-ttf:string-bounding-box string font :kerning nil))
         (bounding-box (map 'vector (lambda (s) (* s scale)) em-bounding-box))
         (xmin (zpb-ttf:xmin bounding-box))
         (xmax (zpb-ttf:xmax bounding-box))
         (ymin (zpb-ttf:ymin bounding-box))
         (ymax (zpb-ttf:ymax bounding-box))
         (width (max
                 (calculate-width string font size)
                 (ceiling (if (< xmin 0) (+ xmax (abs xmin)) xmax))))
         (height (+ 2 (ceiling (if (< ymin 0) (+ ymax (abs ymin)) ymax))))
         (vertical-padding (if (< ymin 0.0) (abs ymin) 0.0))
         (bounding-boxes nil)
         (dir (make-pathname :directory output-folder)))
    (unwind-protect
         (progn
           (loop
              with left = 1.0
              for char across string
              for width = (ceiling (glyph-width font size char))
              do (push (list :char char :x left :y 0.0 :width width :height height)
                       bounding-boxes)
                (incf left (+ width 2.0)))
           (vecto:with-canvas (:width (ceiling width) :height (ceiling height))
             (vecto:set-font font size)
             (vecto:set-rgb-fill 1.0 1.0 1.0)
             (vecto:translate 0.0 vertical-padding)
             (my-draw-string string font size)
             (vecto:save-png (merge-pathnames image-output-path dir))))
      (zpb-ttf:close-font-loader font))
    (save-font-output (merge-pathnames font-output-path dir) image-output-path height bounding-boxes)
    t))

(when nil
  (write-font-string "0123456789ABCDEFGHIJKLMNOPQRSTUVXZYabcdefghijklmnopqrstuvwxyz .,!?"
                     :font "./res/ttf/OpenSans-Light.ttf"
                     :size 22.0
                     :image-output-path "OpenSans-Light.png"
                     :font-output-path "OpenSans-Light.lfont"
                     :output-folder "./res/lfont/"))

(when nil
  (write-font-string "0123456789ABCDEFGHIJKLMNOPQRSTUVXZYabcdefghijklmnopqrstuvwxyz .,!?"
                     :font "./res/ttf/november.ttf"
                     :size 22.0
                     :image-output-path "november.png"
                     :font-output-path "november.lfont"
                     :output-folder "./res/lfont/"))
