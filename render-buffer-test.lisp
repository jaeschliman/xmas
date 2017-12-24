(in-package :render-buffer)

(definstr set-color (r g b a)
  (gl:color r g b a))

(definstr draw-rect (x y w h)
  (gl:rect x y (+ x w) (+ y h)))


(defmacro runloop ((&key (sleep 1) (name "test runloop")) &body body)
  (once-only (sleep name)
    `(ccl::process-run-function
      ,name
      (lambda ()
        (loop do
             (sleep ,sleep)
             ,@body)))))

(defun stoploop (proc) (ccl::process-kill proc))

(defstruct test1
  loop (x 0) (y 0) (buffer (make-render-buffer)))

(defmethod cl-user::draw ((self test1))
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer)
  (with-reads-from-render-buffer ((test1-buffer self))
    (run!)))

(defmethod cl-user::contents-will-mount ((self test1) display)
  (declare (ignorable display))
  (with-slots (loop x y buffer) self
    (setf loop
          (runloop ()
            (incf x)
            (incf y)
            (with-writes-to-render-buffer (buffer)
              (set-color 1.0 1.0 0.0 1.0)
              (draw-rect x y 20 20))))))

(defmethod cl-user::contents-will-unmount ((self test1) display)
  (declare (ignorable display))
  (stoploop (test1-loop self)))

;; (cl-user::display-contents (make-test1))


(defstruct test2
  (x 0)
  (y 5)
  (dx 10)
  (dy 10)
  (buffer (make-render-buffer))
  update-loop)

(defmethod cl-user::draw ((self test2))
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer)
  (with-reads-from-render-buffer ((test2-buffer self))
    (run!)))

(defun update-bouncing-box (box dt)
  (with-slots (x y dx dy) box
    (incf x (* dx dt))
    (incf y (* dy dt))
    (cond ((> x 230) (setf x 230 dx (* dx -1)))
          ((< x 0) (setf x 0 dx (* dx -1))))
    (cond ((> y 230) (setf y 230 dy (* dy -1)))
          ((< y 0) (setf y 0 dy (* dy -1))))))

(defun draw-bouncing-box (box)
  (with-slots (x y r g b a) box
    (set-color r g b a)
    (draw-rect x y 20 20)))

(defmethod cl-user::contents-will-mount ((self test2) display)
  (declare (ignorable display))
  (with-slots (x y buffer update-loop) self
    (setf update-loop
          (runloop (:sleep (/ 1.0 40.0))
            (update-bouncing-box self (/ 1.0 60.0))
            (with-writes-to-render-buffer (buffer)
              (set-color 1.0 1.0 0.0 1.0)
              (draw-rect x y 20 20))))))

(defmethod cl-user::contents-will-unmount ((self test2) display)
  (declare (ignorable display))
  (stoploop (test2-update-loop self)))

;; (cl-user::display-contents (make-test2))

(defstruct bouncy-box
  (x (random 230))
  (y (random 230))
  (dx (- 50 (random 100)))
  (dy (- 50 (random 100)))
  (r (/ (random 100) 100.0))
  (g (/ (random 100) 100.0))
  (b (/ (random 100) 100.0))
  (a (/ (random 100) 100.0)))

(defstruct test3
  (boxes (loop repeat 4000 collect (make-bouncy-box)))
  (buffer (make-render-buffer))
  update-loop)

(defmethod cl-user::draw ((self test3))
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer)
  (with-reads-from-render-buffer ((test3-buffer self))
    (run!)))

(defmethod cl-user::contents-will-mount ((self test3) display)
  (declare (ignorable display))
  (with-slots (boxes buffer update-loop) self
    (setf update-loop
          (runloop:make-runloop
           :function
           (lambda (dt)
             (dolist (box boxes)
               (update-bouncing-box box dt))
             (with-writes-to-render-buffer (buffer)
               (map nil 'draw-bouncing-box boxes)))))))

(defmethod cl-user::contents-will-unmount ((self test3) display)
  (declare (ignorable display))
  (runloop:kill-runloop (test3-update-loop self)))

(defvar *my-random-state* (make-random-state t))

;; (let ((*random-state* *my-random-state*))
;;   (cl-user::display-contents (make-test3)))
