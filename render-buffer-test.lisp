(in-package :render-buffer)

(definstr set-color (r g b a)
  (gl:color r g b a))

(definstr draw-rect (x y w h)
  (gl:rect x y (+ x w) (+ y h)))

(definstr simple-draw-gl-texture (id w h)
  (gl:bind-texture :texture-2d id)
  (gl:color 1 1 1)
  (let* ((x (- (/ w 2)))
         (y (- (/ h 2)))
         (x2 (+ x w))
         (y2 (+ y h)))
    (gl:with-primitive :quads
      (gl:tex-coord 0  1)
      (gl:vertex    x  y  0)
      (gl:tex-coord 1  1)
      (gl:vertex    x2 y  0)
      (gl:tex-coord 1  0)
      (gl:vertex    x2 y2 0)
      (gl:tex-coord 0  0)
      (gl:vertex    x  y2 0))))

(definstr push-matrix ()
  (gl:push-matrix))

(definstr pop-matrix ()
  (gl:pop-matrix))

(definstr-vec mult-matrix (vec)
  (cffi:with-foreign-object (matrix '%gl:float 16)
    (let ((i 0))
      (do-vec! (val)
        (setf (cffi:mem-aref matrix '%gl:float i) val)
        (incf i))
      (%gl:mult-matrix-f matrix))))

;; test method for node transforms
;; (definstr-vec mult-matrix-noop (vec)
;;   (cffi:with-foreign-object (matrix '%gl:float 16)
;;     (let ((i 0))
;;       (do-vec! (val)
;;         (setf (cffi:mem-aref matrix '%gl:float i) val)
;;         (incf i))
;;       ;;(%gl:mult-matrix-f matrix)
;;       )))

(definstr translate-scale-rotate (x y sx sy r)
  (gl:translate x y 0.0)
  (gl:scale sx sy 1.0)
  (gl:rotate r 0.0 0.0 1.0))

(defun draw-texture (texture)
  (when (texture:texture-id texture)
    (simple-draw-gl-texture (texture:texture-id texture)
                            (texture:texture-width texture)
                            (texture:texture-height texture))))


(defstruct bouncy-box
  (x (random 230))
  (y (random 230))
  (dx (* 2 (- 50 (random 100))))
  (dy (* 2 (- 50 (random 100))))
  (r (/ (random 100) 100.0))
  (g (/ (random 100) 100.0))
  (b (/ (random 100) 100.0))
  (a (/ (random 100) 100.0)))

(defun update-bouncing-box (box maxx maxy dt)
  (with-slots (x y dx dy) box
    (incf x (* dx dt))
    (incf y (* dy dt))
    (cond ((> x maxx) (setf x maxx dx (* dx -1)))
          ((< x 0) (setf x 0 dx (* dx -1))))
    (cond ((> y maxy) (setf y maxy dy (* dy -1)))
          ((< y 0) (setf y 0 dy (* dy -1))))))

(defun draw-bouncing-box (box)
  (with-slots (x y r g b a) box
    (set-color r g b a)
    (draw-rect x y 20 20)))

(defstruct test3
  (boxes (loop repeat 4000 collect (make-bouncy-box)))
  width
  height)

(defmethod cl-user::contents-will-mount ((self test3) display)
  (with-slots (boxes width height) self
    (setf width  (display:display-width display)
          height (display:display-height display))
    (dolist (box boxes)
      (setf (bouncy-box-x box) (random (- width 20))
            (bouncy-box-y box) (random (- height 20))))))

(defmethod cl-user::step-contents ((self test3) dt)
  (with-slots (boxes width height) self
    (let ((maxx (- width 20))
          (maxy (- height 20)))
      (dolist (box boxes)
        (update-bouncing-box box maxx maxy dt)))
    (map nil 'draw-bouncing-box boxes)))

(defmethod cl-user::contents-will-unmount ((self test3) display)
  (declare (ignorable self display)))

(defmethod cl-user::handle-event ((self test3) event)
  (case (car event)
    (:resize
     (let ((w (cadr event))
           (h (cddr event)))
       (setf (test3-width self) w
             (test3-height self) h)))
    (t (format t "got unhandled event: ~S~%" event))))

(defvar *my-random-state* (make-random-state t))

(let ((*random-state* *my-random-state*))
  (cl-user::display-contents (make-test3)
                             :width 500
                             :height 500
                             :expandable t))

(defstruct test4
  alien)

(defmethod cl-user::contents-will-mount ((self test4) display)
  (setf (test4-alien self) (texture:load-texture-on-display display #P"./alien.png"))
  (unless (test4-alien self)
    (format t "missing texture!")))

(defmethod cl-user::step-contents ((self test4) dt)
  (declare (ignorable dt))
  (draw-texture (test4-alien self)))

(cl-user::display-contents (make-test4))


(defstruct sprite
  (x 0.0)
  (y 0.0)
  (sx 1.0)
  (sy 1.0)
  (r 0.0)
  (texture nil)
  (dx 0.0)
  (dy 0.0)
  (dsx 0.0)
  (dsy 0.0)
  (dr 0.0))

(defun update-sprite (sprite maxx maxy dt)
  (declare (type sprite sprite))
  (with-slots (x y dx dy r dr) sprite
    (incf x (* dx dt))
    (incf y (* dy dt))
    (incf r (* dr dt))
    (setf r (mod r 360.0))
    (cond ((< x 0) (setf x 0 dx (* -1 dx)))
          ((> x maxx) (setf x maxx dx (* -1 dx))))
    (cond ((< y 0) (setf y 0 dy (* -1 dy)))
          ((> y maxy) (setf y maxy dy (* -1 dy))))))

(defun draw-sprite (sprite)
  (push-matrix)
  (translate-scale-rotate (sprite-x sprite) (sprite-y sprite)
                          (sprite-sx sprite) (sprite-sy sprite)
                          (sprite-r sprite))
  (draw-texture (sprite-texture sprite))
  (pop-matrix))

(defstruct test5
  width
  height
  texture
  sprites)

(defmethod cl-user::contents-will-mount ((self test5) display)
  (let ((width (display:display-width display))
        (height (display:display-height display))
        (texture (texture:load-texture-on-display display #P"./alien.png")))
    (unless texture
      (format t "missing texture!"))
    (setf (test5-width self) width)
    (setf (test5-height self) height)
    (setf (test5-texture self) texture)
    (setf (test5-sprites self)
          (loop repeat 4000 collect
               (make-sprite :x (random width)
                            :y (random height)
                            :dx (- (random 100) 50)
                            :dy (- (random 100) 50)
                            :r (random 360)
                            :dr (- (random 100) 50)
                            :sx 0.5
                            :sy 0.5
                            :texture texture)))))


(defmethod cl-user::step-contents ((self test5) dt)
  (declare (ignorable dt))
  (let ((maxx (test5-width self)) (maxy (test5-height self)))
    (dolist (sprite (test5-sprites self))
      (update-sprite sprite maxx maxy dt)))
  (dolist (sprite (test5-sprites self))
    (draw-sprite sprite)))

(defmethod cl-user::handle-event ((self test5) event)
  (case (car event)
    (:resize
     (let ((w (cadr event))
           (h (cddr event)))
       (setf (test5-width self) w
             (test5-height self) h)))
    (t (format t "got unhandled event: ~S~%" event))))

(cl-user::display-contents (make-test5) :width 500 :height 500 :expandable t)

(defstruct test6
  (matrix (matrix:make-matrix))
  (scratch-matrix (matrix:make-matrix)))

(defmethod cl-user::contents-will-mount ((self test6) display)
  (declare (ignorable display))
  (let ((m (test6-matrix self)))
    (matrix:into-matrix (m)
      (matrix:load-identity)
      (matrix:translate 125.0 125.0))))

(defmethod cl-user::step-contents ((self test6) dt)
  (declare (ignorable dt))
  (let ((matrix:*tmp-matrix* (test6-scratch-matrix self)))
    (matrix:into-matrix ((test6-matrix self))
      (matrix:rotate (* 40 dt))))
  (set-color 0.0 1.0 0.0 0.5)
  (draw-rect 10.0 10.0 20.0 20.0)
  (push-matrix)
  (mult-matrix (matrix:unwrap-matrix (test6-matrix self)))
  (draw-rect 10.0 10.0 20.0 20.0)
  (draw-rect 30.0 30.0 20.0 20.0)
  (pop-matrix))

(cl-user::display-contents (make-test6))

(defmethod draw ((self node:node))
  (apply 'set-color (node:color self))
  (draw-rect -20.0 -20.0 40.0 40.0))

;; the fast method:
(defmethod visit ((self node:node))
  (push-matrix)
  (translate-scale-rotate (node:x self) (node:y self)
                          (node:scale-x self) (node:scale-y self)
                          (node:rotation self))
  (draw self)
  (when (node:children self)
    (loop for child across (node:children self) do
         (visit child)))
  (pop-matrix))

;; the also-fast method:
;; (defmethod visit ((self node:node))
;;   (push-matrix)
;;   (translate-scale-rotate (node:x self) (node:y self)
;;                           (node:scale-x self) (node:scale-y self)
;;                           (node:rotation self))
;;   (node:node-transform self) ;; <- note this addition
;;   (draw self)
;;   (pop-matrix))

;; this method also performs fairly well (some minor stuttering),
;; seeming to indicate that the bottleneck is actually in
;; %gl:mult-matrix-f
;; (defmethod visit ((self node:node))
;;   (push-matrix)
;;   (translate-scale-rotate (node:x self) (node:y self)
;;                           (node:scale-x self) (node:scale-y self)
;;                           (node:rotation self))
;;   (mult-matrix-noop (matrix:unwrap-matrix (node:node-transform self)))
;;   (draw self)
;;   ;;visit children here.
;;   (pop-matrix))

;; the slow as sin method.
;; (defmethod visit ((self node:node))
;;   (push-matrix)
;;   (mult-matrix (matrix:unwrap-matrix (node:node-transform self)))
;;   (draw self)
;;   (when (node:children self)
;;     (loop for child across (node:children self) do
;;          (visit child)))
;;   (pop-matrix))

(defstruct test7
  nodes
  width
  height
  root-node)

(defmethod cl-user::contents-will-mount ((self test7) display)
  (with-slots (nodes width height root-node) self
    (let* ((w (display:display-width display))
           (h (display:display-height display))
           (w/2 (/ w 2))
           (h/2 (/ h 2))
           (diagonal (sqrt (+ (* w w) (* h h))))
           (d/2 (/ diagonal 2)))
      (setf width w
            height h
            root-node (make-instance 'node:node
                                     :x w/2
                                     :y h/2)
            nodes
            (loop
               repeat 4000 collect
                 (make-instance 'node:node
                                :x (coerce (- (random diagonal) d/2) 'float)
                                :y (coerce (- (random diagonal) d/2)'float)
                                :rotation (coerce (random 360) 'float)
                                :color (list
                                        (/ (random 100) 100.0)
                                        (/ (random 100) 100.0)
                                        (/ (random 100) 100.0)
                                        (/ (random 100) 100.0))))))
    (dolist (child nodes)
      (node:add-child root-node child))))

(defmethod cl-user::step-contents ((self test7) dt)
  (declare (ignorable dt))
  (dolist (node (cons (test7-root-node self) (test7-nodes self)))
    (let ((r (node:rotation node)))
      (incf r (* dt 100))
      (setf r (mod r 360))
      (setf (node:rotation node) r)))
  (set-color 0.0 1.0 1.0 0.4)
  (visit (test7-root-node self)))

(cl-user::display-contents (make-test7) :width 500 :height 500)


(defstruct test8
  node
  started)

(defmethod cl-user::contents-will-mount ((self test8) display)
  (let ((node (make-instance 'node:node
                             :x (/ (display:display-width display) 2)
                             :y (/ (display:display-height display) 2)))
        (node2 (make-instance 'node:node
                              :x 20.0
                              :y 20.0)))
    (node:run-action node
                     (action:repeat-forever
                      (action:rotate-by 5.0 360.0)))
    (node:run-action node2
                     (action:repeat-forever
                      (action:rotate-by 2.5 -360.0)))
    (node:add-child node node2)
    (setf (test8-node self) node)))

(defmethod cl-user::step-contents ((self test8) dt)
  (declare (ignorable dt))
  (unless (test8-started self)
    (setf (test8-started self) t)
    (node:on-enter (test8-node self)))
  (visit (test8-node self)))

(cl-user::display-contents (make-test8))
