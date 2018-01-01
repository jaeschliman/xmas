(in-package :cl-user)

(require :cocoa)

(defmacro progmain ((&rest bindings) &body b)
  ;;eventually do something with bindings. get later.
  (declare (ignorable bindings))
  `(ccl::queue-for-event-process (lambda () ,@b )))

(ql:quickload :alexandria)
(ql:quickload :queues.simple-cqueue)
(ql:quickload :bordeaux-threads)
(ql:quickload :cl-json)
(ql:quickload :xmls)
(ql:quickload :chipz)
(ql:quickload :cl-base64)
(progmain ()
  (ql:quickload :cl-opengl)
  (ql:quickload :cl-glu)
  (ql:quickload :cl-glut) ;; am I even using this?
  (load "./runloop.lisp")
  (load "./render-buffer.lisp")
  (load "./matrix.lisp")
  (load "./node.lisp")
  (load "./action.lisp")
  (load "./action-manager.lisp")
  (load "./node-lifecycle.lisp")
  (load "./display.lisp")
  (load "./texture.lisp")
  (load "./texture-packer.lisp")
  (load "./scratch-ccl.lisp") ;; really should be called "runtime-ccl"
  (load "./tmx-reader.lisp")
  )

