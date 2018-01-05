(in-package :cl-user)

(defgeneric handle-event (contents event)
  (:method (contents event) (declare (ignorable contents event))))

(defgeneric step-contents (contents dt)
  (:method (contents dt) (declare (ignorable contents dt))))

(defgeneric mount-contents (contents display)
  (:method (contents (display display:display))
    (let ((scratch-matrix (display:display-scratch-matrix display))
          (texture-manager (display:display-texture-manager display))
          (action-manager (display:display-action-manager display)))
      (setf (display:display-runloop display)
            (runloop:make-runloop
             :name "runloop"
             :step (display:display-fps display)
             :function
             (lambda (dt)
               (let ((matrix:*tmp-matrix* scratch-matrix)
                     (texture:*texture-manager* texture-manager)
                     (action-manager:*action-manager* action-manager))
                 (render-buffer::with-writes-to-render-buffer
                     ((display:display-renderbuffer display))
                   (action-manager:update-actions action-manager dt)
                   (step-contents contents dt))))
             :event-handler
             (lambda (event)
               (let ((matrix:*tmp-matrix* scratch-matrix)
                     (texture:*texture-manager* texture-manager)
                     (action-manager:*action-manager* action-manager))
                 (handle-event contents event))))))))

(defgeneric unmount-contents (contents display)
  (:method (contents (display display:display))
    (declare (ignorable contents))
    (when (display:display-runloop display)
      (runloop:kill-runloop (display:display-runloop display)))))

(defgeneric draw (contents display)
  (:method (contents (display display:display))
    (declare (ignorable contents))
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer)
    (gl:enable :texture-2d)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (render-buffer::with-reads-from-render-buffer ((display:display-renderbuffer display))
      (render-buffer::run!))))

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
                      (alexandria:when-let (view (display:native-view display))
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
  (let ((matrix:*tmp-matrix* (display:display-scratch-matrix display))
        (texture:*texture-manager* (display:display-texture-manager display))
        (action-manager:*action-manager* (display:display-action-manager display)))
    (contents-will-unmount (display:contents display) display)
    (unmount-contents (display:contents display) display))
  (setf (display:native-view display) nil
        (display:native-window display) nil))

(defclass my-window (ns:ns-window)
  ((display :accessor display))
  (:metaclass ns:+ns-object))

(defclass my-view (ns:ns-opengl-view)
  ((contents :accessor my-view-contents :initarg :contents :initform nil)
   (display  :accessor my-view-display  :initarg :display  :initform nil)
   (requires-resize? :accessor my-view-requires-resize? :initform nil))
  (:metaclass ns:+ns-object))

(defun prepare-gl-2d (width height)
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height)
  (gl:matrix-mode :modelview))

(defun nsview-size (self)
  (let* ((f (#/frame self))
         (w (pref f :<NSR>ect.size.width))
         (h (pref f :<NSR>ect.size.height)))
    (values w h)))

(defun reshape-window (self)
  (multiple-value-bind (w h) (nsview-size self)
    (prepare-gl-2d w h)))

(objc:defmethod (#/prepareOpenGL :void) ((self my-view))
  (#_glClearColor 0.05 0.05 0.05 0.0)
  (reshape-window self)
  (display:display-drain-gl-queue (my-view-display self)))

(objc:defmethod (#/windowWillClose: :void) ((self my-window) notification)
  (declare (ignorable notification))
  (cleanup-display (display self)))

(objc:defmethod (#/windowDidResize: :void) ((self my-window) notification)
  (declare (ignorable notification))
  (let ((view (#/contentView self)))
    (setf (my-view-requires-resize? view) t)
    (multiple-value-bind (w h) (nsview-size view)
      (runloop:enqueue-runloop-event (display:display-runloop (display self))
                                     (cons :resize (cons w h))))))

(objc:defmethod (#/drawRect: :void) ((self my-view) (a-rect :ns-rect))
  (declare (ignorable a-rect))
  (when (my-view-requires-resize? self)
    (reshape-window self))
  (display:display-drain-gl-queue (my-view-display self))
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
        (runloop (display:display-runloop (my-view-display myview))))
    (runloop:enqueue-runloop-event runloop (cons event key))))

(objc:defmethod (#/keyDown: :void) ((self my-view) event)
  (%enqueue-key-event self event :keydown))

(objc:defmethod (#/keyUp: :void) ((self my-view) event)
  (%enqueue-key-event self event :keyup)
  (%enqueue-key-event self event :keypress))

(defmethod redisplay ((self display:display))
  (alexandria:when-let ((view (display:native-view self)))
    (progmain ()
      (#/setNeedsDisplay: view #$YES))))

(defun display-contents (contents &key
                                    (width 250)
                                    (height 250)
                                    (title "untitled")
                                    (expandable nil)
                                    (fps (/ 1.0 60.0)))
  (let* ((result (make-instance 'display:display
                                :fps fps
                                :contents contents
                                :width width
                                :height height))
         (texture-manager (texture:make-texture-manager :display result))
         (package *package*))
    (setf (display:display-texture-manager result)
          texture-manager)
    (progmain ()
      (let* ((*package* package)
             (w (gui::new-cocoa-window :class (find-class 'my-window)
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
        (#/setContentView: w glview)
        (#/setDelegate: w w)
        (setf (display:native-view result) glview
              (display:native-window result) w
              (display w) result)
        (init-display result)
        (let ((matrix:*tmp-matrix* (display:display-scratch-matrix result))
              (texture:*texture-manager* (display:display-texture-manager result))
              (action-manager:*action-manager* (display:display-action-manager result)))
          (contents-will-mount contents result)
          (mount-contents contents result))
        (#/setLevel: w 100)
        (#/makeFirstResponder: w glview)
        (#/makeKeyAndOrderFront: w nil)))

    result))
