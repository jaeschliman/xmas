(defpackage :xmas.vecto-texture (:use :cl :alexandria :xmas.texture)
            (:export
             #:vecto-image))
(in-package :xmas.vecto-texture)

(defmacro vecto-image ((&key width height) &body body)
  (with-gensyms (result)
    `(let (,result)
       (vecto:with-canvas (:width ,width :height ,height)
         (progn ,@body)
         (setf ,result (zpng:image-data (vecto::image vecto::*graphics-state*))))
       ,result)))
