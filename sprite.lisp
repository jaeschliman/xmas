(defpackage :sprite (:use :cl :alexandria :texture :node)
            (:export
             #:sprite
             #:sprite-frame
             #:stop-animation
             #:run-animation))
(in-package :sprite)

(defclass sprite (node)
  ((sprite-frame :accessor sprite-frame :initarg :sprite-frame)))

(defun sprite-width (sprite)
  (* (scale-x sprite) (texture-frame-width (sprite-frame sprite))))

(defun sprite-height (sprite)
  (* (scale-y sprite) (texture-frame-height (sprite-frame sprite))))

(defmethod width ((self sprite))
  (sprite-width self))

(defmethod height ((self sprite))
  (sprite-height self))
