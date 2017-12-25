(in-package :cl-user)
(ql:quickload :alexandria)

(defmacro progmain ((&rest bindings) &body b)
  ;;eventually do something with bindings. get later.
  (declare (ignorable bindings))
  `(ccl::queue-for-event-process (lambda () ,@b )))

(defclass display ()
  ((native-view :accessor native-view :initform nil)
   (native-window :accessor native-window :initform nil)
   (contents :accessor contents :initarg :contents)
   (width :accessor display-width :initarg :width)
   (height :accessor display-height :initarg :height)
   (fps :reader display-fps :initarg :fps)
   (runloop :accessor display-runloop :initform nil)
   (renderbuffer :accessor display-renderbuffer
                 :initform (render-buffer::make-render-buffer))))

(defgeneric handle-event (contents event)
  (:method (contents event) (declare (ignorable contents event))))

(defgeneric step-contents (contents dt)
  (:method (contents dt) (declare (ignorable contents dt))))

(defgeneric mount-contents (contents display)
  (:method (contents (display display))
    (setf (display-runloop display)
          (runloop:make-runloop
           :name "runloop"
           :step (display-fps display)
           :function
           (lambda (dt)
             (render-buffer::with-writes-to-render-buffer
                 ((display-renderbuffer display))
               (step-contents contents dt)))
           :event-handler
           (lambda (event)
             (handle-event contents event))))))

(defgeneric unmount-contents (contents display)
  (:method (contents (display display))
    (runloop:kill-runloop (display-runloop display))))

(defgeneric draw (contents display)
  (:method (contents (display display))
    (declare (ignorable contents))
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer)
    (render-buffer::with-reads-from-render-buffer ((display-renderbuffer display))
      (render-buffer::run!))))

(require :cocoa)
(progmain ()
  (ql:quickload :cl-opengl)
  (ql:quickload :cl-glu)
  (ql:quickload :cl-glut))

(defvar *running-displays* nil)
(defvar *running-displays-loop* nil)

(defun start-update-loop ()
  (unless *running-displays-loop*
    (setf *running-displays-loop*
          (ccl::process-run-function
           "display loop"
           (lambda ()
             (loop do
                  (sleep (/ 1.0 60.0))
                  (progmain ()
                    (dolist (display *running-displays*)
                      (alexandria:when-let (view (native-view display))
                        (#/setNeedsDisplay: view #$YES))))))))))

(defun stop-update-loop ()
  (when *running-displays-loop*
    (ccl::process-kill *running-displays-loop*)
    (setf *running-displays-loop* nil)))

(defun add-running-display (display)
  (unless *running-displays*
    (start-update-loop))
  (push display *running-displays*))

(defun remove-running-display (display)
  (setf *running-displays* (remove display *running-displays*))
  (unless *running-displays*
    (stop-update-loop)))

(defgeneric contents-will-mount (contents display)
  (:method ((anything t) display)
    (declare (ignorable display))
    (values)))

(defgeneric contents-will-unmount (contents display)
  (:method ((anything t) display)
    (declare (ignorable display))
    (values)))

(defun init-display (display)
  (add-running-display display))

(defun cleanup-display (display)
  (remove-running-display display)
  (contents-will-unmount (contents display) display)
  (unmount-contents (contents display) display)
  (setf (native-view display) nil
        (native-window display) nil))

(defclass my-window (ns:ns-window)
  ((display :accessor display))
  (:metaclass ns:+ns-object))

(defclass my-view (ns:ns-opengl-view)
  ((contents :accessor my-view-contents :initarg :contents :initform nil)
   (display  :accessor my-view-display  :initarg :display  :initform nil))
  (:metaclass ns:+ns-object))

(defun prepare-gl-2d (width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height)
  (gl:matrix-mode :modelview))
  
(defun reshape-window (self)
  (let* ((f (#/frame self))
         (w (pref f :<NSR>ect.size.width))
         (h (pref f :<NSR>ect.size.height)))
    (prepare-gl-2d w h)))

(objc:defmethod (#/prepareOpenGL :void) ((self my-view))
  (#_glClearColor 0.05 0.05 0.05 0.0)
  (reshape-window self))

(objc:defmethod (#/windowWillClose: :void) ((self my-window) notification)
  (declare (ignorable notification))
  (cleanup-display (display self)))

(objc:defmethod (#/drawRect: :void) ((self my-view) (a-rect :ns-rect))
  (declare (ignorable a-rect))
  (with-slots (contents display) self
    (when contents (draw contents display)))
  (#_glFlush))

(objc:defmethod (#/acceptsFirstResponder :<BOOL>)
    ((self my-view))
  #$YES)

(objc:defmethod (#/canBecomeKeyView :<BOOL>)
    ((self my-view))
  #$YES)

(defun %translate-keycode (ns-event)
  (case (#/keyCode ns-event)
    (123 :left)
    (124 :right)
    (125 :down)
    (126 :up)
    (t   (let* ((nss (#/characters ns-event))
                (s   (ccl::lisp-string-from-nsstring nss)))
           (when (length s)
             (aref s 0))))))

(defun %enqueue-key-event (myview ns-event event)
  (let ((key (%translate-keycode ns-event))
        (runloop (display-runloop (my-view-display myview))))
    (runloop:enqueue-runloop-event runloop (cons event key))))

(objc:defmethod (#/keyDown: :void) ((self my-view) event)
  (%enqueue-key-event self event :keydown))

(objc:defmethod (#/keyUp: :void) ((self my-view) event)
  (%enqueue-key-event self event :keyup)
  (%enqueue-key-event self event :keypress))

(defmethod redisplay ((self display))
  (alexandria:when-let ((view (native-view self)))
    (progmain ()
      (#/setNeedsDisplay: view #$YES))))

(defun display-contents (contents &key
                                    (width 250)
                                    (height 250)
                                    (title "untitled")
                                    (expandable nil)
                                    (fps (/ 1.0 60.0)))
  (let ((result (make-instance 'display
                               :fps fps
                               :contents contents
                               :width width
                               :height height)))
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
                                    :display  result
                                    :with-frame w-frame
                                    :pixel-format (#/defaultPixelFormat ns:ns-opengl-view))))
        (#/addSubview: w-content-view glview)
        (#/setDelegate: w w)
        (setf (native-view result) glview
              (native-window result) w
              (display w) result)
        (init-display result)
        (contents-will-mount contents result)
        (mount-contents contents result)
        (#/setLevel: w 100)
        (#/orderFront: w nil)))

    result))
