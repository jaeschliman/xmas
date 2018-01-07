(in-package :action)

(defstruct (sprite-animation (:include finite-time-action))
  frames)

(defmethod update ((self sprite-animation) time)
  (let* ((frames (sprite-animation-frames self))
         (target (sprite-animation-target self))
         (count (length frames))
         (max-index (1- count))
         (index (floor (* time count)))
         (frame (aref frames (max 0 (min max-index index)))))
    (setf (sprite:sprite-frame target) frame)))

(defact sprite-animation-action (frame-delay frames)
  (let* ((frames (coerce frames 'vector))
         (duration (* frame-delay (length frames))))
    (make-sprite-animation :duration duration :frames frames)))
