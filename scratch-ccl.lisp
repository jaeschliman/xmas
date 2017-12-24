(in-package :cl-user)
(ql:quickload :alexandria)

(defmacro progmain ((&rest bindings) &body b)
  ;;eventually do something with bindings. get later.
  (declare (ignorable bindings))
  `(ccl::queue-for-event-process (lambda () ,@b )))

(defgeneric draw (contents))

(require :cocoa)
(progmain ()
  (ql:quickload :cl-opengl)
  (ql:quickload :cl-glu)
  (ql:quickload :cl-glut))

(defclass display ()
  ((native-view :accessor native-view :initform nil)
   (native-window :accessor native-window :initform nil)
   (contents :accessor contents :initarg :contents)))

(defgeneric contents-will-mount (contents display)
  (:method ((anything t) display)
    (declare (ignorable display))
    (values)))

(defgeneric contents-will-unmount (contents display)
  (:method ((anything t) display)
    (declare (ignorable display))
    (values)))

(defun cleanup-display (display)
  (contents-will-unmount (contents display) display)
  (setf (native-view display) nil
        (native-window display) nil))

(defclass my-window (ns:ns-window)
  ((display :accessor display))
  (:metaclass ns:+ns-object))

(defclass my-view (ns:ns-opengl-view)
  ((contents :accessor my-view-contents :initarg :contents :initform nil))
  (:metaclass ns:+ns-object))

(defun shape-gl-2d (width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height)
  (gl:matrix-mode :modelview))
  
(defun reshape-window (self)
  (let* ((f (#/frame self))
         (w (pref f :<NSR>ect.size.width))
         (h (pref f :<NSR>ect.size.height)))
    (shape-gl-2d w h)))

(objc:defmethod (#/prepareOpenGL :void) ((self my-view))
  (#_glClearColor 0.05 0.05 0.05 0.0)
  (reshape-window self))

(objc:defmethod (#/windowWillClose: :void) ((self my-window) notification)
  (declare (ignorable notification))
  (cleanup-display (display self)))

(objc:defmethod (#/drawRect: :void) ((self my-view) (a-rect :ns-rect))
  (declare (ignorable a-rect))
  (with-slots (contents) self
    (when contents (draw contents)))
  (#_glFlush))

(defmethod redisplay ((self display))
  (alexandria:when-let ((view (native-view self)))
    (progmain ()
      (#/setNeedsDisplay: view #$YES))))

(defun display-contents (contents &key
                                    (width 250)
                                    (height 250)
                                    (title "untitled")
                                    (expandable nil))
  (let ((result (make-instance 'display :contents contents)))
    (progmain ()
      (let* ((w (gui::new-cocoa-window :class (find-class 'my-window)
                                       :title title
                                       :width width
                                       :height height
                                       :expandable expandable))
             (w-content-view (#/contentView w))
             (w-frame (#/frame w-content-view))
             (glview (make-instance 'my-view
                                    :contents contents
                                    :with-frame w-frame
                                    :pixel-format (#/defaultPixelFormat ns:ns-opengl-view))))
        (#/addSubview: w-content-view glview)
        (#/setDelegate: w w)
        (setf (native-view result) glview
              (native-window result) w
              (display w) result)

        (contents-will-mount contents result)
        (#/setLevel: w 100)
        (#/orderFront: w nil)))

    result))

(defstruct my-thing (x 10) (y 10) (thread nil))

(defmethod draw ((self my-thing))
  (gl:clear-color 0.0 1.0 0.0 1.0)
  (gl:clear :color-buffer)
  (gl:color 0 0 1 0.5)
  (let ((x (my-thing-x self))
        (y (my-thing-y self)))
    (gl:rect x y (+ x 10) (+ y 10))))


(defmethod contents-will-mount ((self my-thing) display)
  (let* ((fn
          (lambda ()
            (loop do
                 (sleep 1)
                 (incf (my-thing-x self) 1)
                 (incf (my-thing-y self) 1)
                 (redisplay display))))
         (proc (ccl::process-run-function "my thing loop" fn)))
    (setf (my-thing-thread self) proc)))

(defmethod contents-will-unmount ((self my-thing) display)
  (declare (ignorable display))
  (alexandria:when-let (proc (my-thing-thread self))
    (ccl::process-kill proc)))

(display-contents (make-my-thing))
