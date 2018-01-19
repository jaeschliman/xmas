(in-package :cl-user)

(defvar *display-closed-hook* nil)

(defgeneric handle-event (contents event)
  (:method (contents event) (declare (ignorable contents event))))

(defgeneric step-contents (contents dt)
  (:method (contents dt) (declare (ignorable contents dt))))

(defgeneric call-with-contents (contents fn)
  (:method (contents fn)
    (declare (ignore contents))
    (funcall fn)))

(defgeneric runloop-bindings-alist (contents)
  (:method (contents)
    (declare (ignore contents))
    nil))

(defgeneric mount-contents (contents display)
  (:method (contents (display xmas.display:display))
    (let ((scratch-matrix (xmas.display:display-scratch-matrix display))
          (texture-manager (xmas.display:display-texture-manager display))
          (action-manager (xmas.display:display-action-manager display))
          (animation-manager (xmas.display:display-animation-manager display))
          (package *package*))
      (setf (xmas.display:display-runloop display)
            (xmas.runloop:make-runloop
             :name "runloop"
             :step (xmas.display:display-fps display)
             :bindings (runloop-bindings-alist contents)
             :function
             (lambda (dt)
               (let ((*package* package)
                     (xmas.matrix:*tmp-matrix* scratch-matrix)
                     (xmas.texture:*texture-manager* texture-manager)
                     (xmas.action-manager:*action-manager* action-manager)
                     (xmas.animation-manager:*animation-manager* animation-manager)
                     (loop-body
                        (lambda ()
                          (xmas.render-buffer::with-writes-to-render-buffer
                              ((xmas.display:display-renderbuffer display))
                            (xmas.action-manager:update-actions action-manager dt)
                            (step-contents contents dt)))))
                 (declare (dynamic-extent loop-body))
                 (call-with-contents contents loop-body)))
             :event-handler
             (lambda (event)
               (let ((*package* package)
                     (xmas.matrix:*tmp-matrix* scratch-matrix)
                     (xmas.texture:*texture-manager* texture-manager)
                     (xmas.action-manager:*action-manager* action-manager)
                     (xmas.animation-manager:*animation-manager* animation-manager))
                 (let ((handler (lambda () (handle-event contents event))))
                   (declare (dynamic-extent handler))
                   (call-with-contents contents handler)))))))))

(defgeneric unmount-contents (contents display)
  (:method (contents (display xmas.display:display))
    (declare (ignorable contents))
    (when (xmas.display:display-runloop display)
      (xmas.runloop:kill-runloop (xmas.display:display-runloop display)))))

(defgeneric draw (contents display)
  (:method (contents (display xmas.display:display))
    (declare (ignorable contents))
    (gl:clear-color 0.0 0.0 0.0 1.0)
    (gl:clear :color-buffer)
    (gl:enable :texture-2d)
    (gl:enable :blend)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (xmas.render-buffer::with-reads-from-render-buffer ((xmas.display:display-renderbuffer display))
      (xmas.render-buffer::run!))))

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
                      (alexandria:when-let (view (xmas.display:native-view display))
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
  (let ((xmas.matrix:*tmp-matrix* (xmas.display:display-scratch-matrix display))
        (xmas.texture:*texture-manager* (xmas.display:display-texture-manager display))
        (xmas.action-manager:*action-manager* (xmas.display:display-action-manager display))
        (xmas.animation-manager:*animation-manager* (xmas.display:display-animation-manager display))
        (contents (xmas.display:contents display)))
    (call-with-contents
     contents
     (lambda ()
       (contents-will-unmount contents display)
       (unmount-contents contents display))))
  (setf (xmas.display:native-view display) nil
        (xmas.display:native-window display) nil))


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

(defun resize-gl-2d (viewport-x viewport-y viewport-width viewport-height width height)
  (gl:viewport viewport-x viewport-y viewport-width viewport-height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:ortho-2d 0 width 0 height)
  (gl:matrix-mode :modelview))

(defun nsview-size (self)
  (let* ((f (#/frame self))
         (w (pref f :<NSR>ect.size.width))
         (h (pref f :<NSR>ect.size.height)))
    (values w h)))


(defun aspect-fit (screen-w screen-h content-w content-h)
  (let* ((screen-ratio (/ screen-w screen-h))
         (content-ratio (/ content-w content-h))
         (new-w (if (> screen-ratio content-ratio)
                    (* content-w (/ screen-h content-h))
                    screen-w))
         (new-h (if (> screen-ratio content-ratio)
                    screen-h
                    (* content-h (/ screen-w content-w))))
         (offs-x (/ (- screen-w new-w) 2.0))
         (offs-y (/ (- screen-h new-h) 2.0)))
    (values offs-x offs-y new-w new-h)))

(defun reshape-window (self)
  (multiple-value-bind (w h) (nsview-size self)
    (let* ((display (my-view-display self))
           (width (xmas.display:display-width display))
           (height (xmas.display:display-height display)))
      (with-slots (xmas.display:size-to-fit xmas.display:preserve-aspect-ratio) display
        (cond
          (xmas.display:preserve-aspect-ratio
           (multiple-value-bind (x y w h) (aspect-fit w h width height)
             (resize-gl-2d x y w h width height)))
          (xmas.display:size-to-fit
           (resize-gl-2d 0 0 w h width height))
          (t
           (resize-gl-2d 0 0 w h w h)))))))

(objc:defmethod (#/prepareOpenGL :void) ((self my-view))
  (#_glClearColor 0.05 0.05 0.05 0.0)
  (reshape-window self)
  (xmas.display:display-drain-gl-queue (my-view-display self)))

(objc:defmethod (#/windowWillClose: :void) ((self my-window) notification)
  (declare (ignorable notification))
  (cleanup-display (display self))
  (alexandria:when-let (hook (xmas.display:display-closed-hook (display self)))
    (funcall hook)))

(objc:defmethod (#/windowDidResize: :void) ((self my-window) notification)
  (declare (ignorable notification))
  (let ((view (#/contentView self)))
    (setf (my-view-requires-resize? view) t)
    (multiple-value-bind (w h) (nsview-size view)
      (xmas.runloop:enqueue-runloop-event (xmas.display:display-runloop (display self))
                                     (cons :resize (cons w h))))))

(objc:defmethod (#/drawRect: :void) ((self my-view) (a-rect :ns-rect))
  (declare (ignorable a-rect))
  (when (my-view-requires-resize? self)
    (reshape-window self))
  (xmas.display:display-drain-gl-queue (my-view-display self))
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

;;TODO: have some pooled event struct.
;;      would require a queue back to the main thread to return events after use.

(defun %enqueue-key-event (myview ns-event event)
  (let ((key (%translate-keycode ns-event))
        (runloop (xmas.display:display-runloop (my-view-display myview))))
    (xmas.runloop:enqueue-runloop-event runloop (cons event key))))

(objc:defmethod (#/keyDown: :void) ((self my-view) event)
  (%enqueue-key-event self event :keydown))

(objc:defmethod (#/keyUp: :void) ((self my-view) event)
  (%enqueue-key-event self event :keyup)
  (%enqueue-key-event self event :keypress))

(defun %translate-mouse-coords (myview ns-event)
  ;;TODO: take aspect/stretch scaling into account
  (let* ((p1 (#/locationInWindow ns-event))
         (p2 (#/convertPoint:fromView: myview p1 +null-ptr+))
         (x  (ns:ns-point-x p2))
         (y  (ns:ns-point-y p2)))
    (values x y)))

(defun %enqueue-mouse-event (myview ns-event event)
  (multiple-value-bind (x y) (%translate-mouse-coords myview ns-event)
    (let ((runloop (xmas.display:display-runloop (my-view-display myview))))
      (xmas.runloop:enqueue-runloop-event runloop (cons event (cons x y))))))

(objc:defmethod (#/mouseDown: :void)
    ((self my-view) event)
  (%enqueue-mouse-event self event :mousedown))

(objc:defmethod (#/mouseUp: :void)
    ((self my-view) event)
  (%enqueue-mouse-event self event :mouseup)
  (let* ((count (#/clickCount event))
         (type (case count
                 (1 :click)
                 (2 :doubleclick)
                 (3 :tripleclick)
                 (t nil))))
    (when type
      (%enqueue-mouse-event self event type))))

(objc:defmethod (#/mouseMoved: :void)
    ((self my-view) event)
  (%enqueue-mouse-event self event :mousemove))

(objc:defmethod (#/mouseDragged: :void)
    ((self my-view) event)
  (%enqueue-mouse-event self event :mousedrag))

(defmethod redisplay ((self xmas.display:display))
  (alexandria:when-let ((view (xmas.display:native-view self)))
    (progmain ()
      (#/setNeedsDisplay: view #$YES))))

(defun default-pixel-format ()
  (#/defaultPixelFormat ns:ns-opengl-view))

;; TODO: a working anti-aliased pixel format

(defun display-contents (contents &key
                                    (width 250)
                                    (height 250)
                                    (title "untitled")
                                    (expandable nil)
                                    (fps (/ 1.0 60.0))
                                    (size-to-fit nil)
                                    (preserve-aspect-ratio nil))
  (let* ((result (make-instance 'xmas.display:display
                                :fps fps
                                :contents contents
                                :width width
                                :height height
                                :size-to-fit size-to-fit
                                :action-manager (xmas.action-manager:make-manager)
                                :animation-manager (xmas.animation-manager:make-manager)
                                :preserve-aspect-ratio preserve-aspect-ratio
                                :closed-hook *display-closed-hook*))
         (texture-manager (xmas.texture:make-texture-manager :display result)))
    (setf (xmas.display:display-texture-manager result)
          texture-manager)
    (progmain (*package*)
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
                                    :pixel-format (default-pixel-format))))
        (#/setContentView: w glview)
        (#/setDelegate: w w)
        (#/setAcceptsMouseMovedEvents: w #$YES)
        (setf (xmas.display:native-view result) glview
              (xmas.display:native-window result) w
              (display w) result)
        (init-display result)
        (let ((xmas.matrix:*tmp-matrix* (xmas.display:display-scratch-matrix result))
              (xmas.texture:*texture-manager* (xmas.display:display-texture-manager result))
              (xmas.action-manager:*action-manager* (xmas.display:display-action-manager result))
              (xmas.animation-manager:*animation-manager* (xmas.display:display-animation-manager result)))
          (call-with-contents
           contents
           (lambda ()
             (contents-will-mount contents result)
             (mount-contents contents result))))
        (#/setLevel: w 100)
        (#/makeFirstResponder: w glview)
        (#/makeKeyAndOrderFront: w nil)))

    result))
