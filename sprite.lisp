(defpackage :xmas.sprite (:use :cl :alexandria :xmas.texture :xmas.node)
            (:export
             #:sprite
             #:sprite-frame
             #:stop-animation
             #:run-animation))
(in-package :xmas.sprite)

(defclass sprite (node)
  ((sprite-frame :reader sprite-frame :initarg :sprite-frame))
  (:default-initargs
   :content-width nil :content-height nil))

(defmethod (setf sprite-frame) (v (self sprite))
  (setf (slot-value self 'sprite-frame) v)
  (setf (content-width self) (texture-frame-width v))
  (setf (content-height self) (texture-frame-height v)))

(defmethod initialize-instance ((self sprite)
                                &key sprite-frame content-width content-height
                                  &allow-other-keys)
  (call-next-method)
  (unless content-width
    (setf (content-width self) (texture-frame-width sprite-frame)))
  (unless content-height
    (setf (content-height self) (texture-frame-height sprite-frame))))
