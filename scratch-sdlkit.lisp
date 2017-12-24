(ql:quickload :cl-autowrap)
(ql:quickload :sdl2kit) ;; fails to compile :(
(ql:quickload :sdl2kit-examples)

(sdl2.kit:define-start-function cube-test (&key (w 800) (h 500))
  (sdl2:gl-set-attr :stencil-size 8)
  (sdl2:gl-set-attr :context-profile-mask 1)
  (sdl2:gl-set-attr :context-major-version 3)
  (sdl2:gl-set-attr :context-minor-version 3)
  (make-instance 'kit.sdl2.test:cube-window :w w :h h))

(sdl2:make-this-thread-main #'cube-test)
