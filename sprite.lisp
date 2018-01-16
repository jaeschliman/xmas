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
   :content-width nil :content-height nil
   :anchor-x 0.5 :anchor-y 0.5))

(defmethod initialize-instance ((self sprite)
                                &key sprite-frame content-width content-height
                                  &allow-other-keys)
  (call-next-method)
  (let ((width (texture-frame-width sprite-frame))
        (height (texture-frame-height sprite-frame))
        (rotated (texture-frame-rotated sprite-frame)))
    (unless content-width
      (setf (content-width self) (if rotated height width)))
    (unless content-height
      (setf (content-height self) (if rotated width height)))))
