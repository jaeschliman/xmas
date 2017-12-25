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
      (gl:vertex    x  y2 0))) )

(defun draw-texture (texture)
  (when (cl-user::texture-id texture)
    (simple-draw-gl-texture (cl-user::texture-id texture)
                            (cl-user::texture-width texture)
                            (cl-user::texture-height texture))))


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
    (setf width (cl-user::display-width display)
          height (cl-user::display-height display))
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
  (setf (test4-alien self) (cl-user::load-texture display #P"./alien.png"))
  (unless (test4-alien self)
    (format t "missing texture!")))

(defmethod cl-user::step-contents ((self test4) dt)
  (declare (ignorable dt))
  (draw-texture (test4-alien self)))

(cl-user::display-contents (make-test4))
