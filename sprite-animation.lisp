(in-package :sprite)

(defmethod stop-animation ((self sprite))
  (stop-all-actions self :tag 'animation))

(defmethod run-animation ((self sprite) name &key repeat)
  (stop-animation self)
  (when-let (animation (xmas.animation-manager:get-animation name))
    (run-action self animation :repeat repeat :tag 'animation)))
