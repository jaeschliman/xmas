(defpackage :display (:use :cl :alexandria)
            (:export
             #:display
             #:native-view
             #:native-window
             #:contents
             #:display-width
             #:display-height
             #:display-fps
             #:display-runloop
             #:display-gl-queue
             #:display-renderbuffer
             #:display-scratch-matrix
             #:display-action-manager
             #:display-drain-gl-queue
             #:display-enqueue-for-gl
             #:display-texture-manager))

(in-package :display)

(defclass display ()
  ((native-view :accessor native-view :initform nil)
   (native-window :accessor native-window :initform nil)
   (contents :accessor contents :initarg :contents)
   (width :accessor display-width :initarg :width)
   (height :accessor display-height :initarg :height)
   (fps :reader display-fps :initarg :fps)
   (runloop :accessor display-runloop :initform nil)
   (gl-queue :accessor display-gl-queue :initform (queues:make-queue :simple-cqueue))
   (renderbuffer :accessor display-renderbuffer
                 :initform (render-buffer::make-render-buffer))
   (scratch-matrix :accessor display-scratch-matrix
                   :initform (matrix:make-matrix))
   (action-manager :accessor display-action-manager
                   :initform (action-manager:make-manager))
   (texture-manager :accessor display-texture-manager)))

(defun display-drain-gl-queue (display)
  (loop
     with queue = (display-gl-queue display)
     for thunk = (queues:qpop queue)
     while thunk
     do (funcall thunk)))

(defun display-enqueue-for-gl (display thunk)
  (queues:qpush (display-gl-queue display) thunk))
