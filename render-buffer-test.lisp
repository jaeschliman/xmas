(in-package :render-buffer)

(definstr set-color (r g b a)
  (gl:color r g b a))

(definstr draw-rect (x y w h)
  (gl:rect x y (+ x w) (+ y h)))

(defstruct bouncy-box
  (x (random 230))
  (y (random 230))
  (dx (- 50 (random 100)))
  (dy (- 50 (random 100)))
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
