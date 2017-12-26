(in-package :cl-user)

(require :cocoa)

(defmacro progmain ((&rest bindings) &body b)
  ;;eventually do something with bindings. get later.
  (declare (ignorable bindings))
  `(ccl::queue-for-event-process (lambda () ,@b )))

(ql:quickload :alexandria)
(ql:quickload :queues.simple-cqueue)
(ql:quickload :bordeaux-threads)
(progmain ()
  (ql:quickload :cl-opengl)
  (ql:quickload :cl-glu)
  (ql:quickload :cl-glut)
  (load "./runloop.lisp")
  (load "./render-buffer.lisp")
  (load "./matrix.lisp")
  (load "./scratch-ccl.lisp")
  (load "./texture.lisp")
  (load "./node.lisp")
  (load "./action.lisp")
  (load "./action-manager.lisp")
  )

