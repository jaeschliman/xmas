(defpackage :platformer (:use :cl :alexandria :node :action :texture :texture-packer :display))
(in-package :platformer)

(defmacro with-struct ((prefix &rest slots) var &body body)
  (once-only (var)
    `(symbol-macrolet
         ,(loop for slot in slots collect
               (list slot (list (symbolicate prefix slot) var)))
       ,@body)))

;; FIXME: eps is a hack, need to properly 'arrive' at dest.
(defmacro move-towards! (place dest speed dt &optional (eps 0.1))
  (once-only (dest speed dt)
    `(let* ((curr ,place)
            (delta (* ,speed ,dt (if (< ,dest curr) -1 1)))
            (new-val (+ curr delta)))
       (if (< (abs (- curr ,dest)) ,eps)
           (setf ,place ,dest)
           (setf ,place new-val)))))

(define-modify-macro clampf (min max) clamp)

(defclass sprite (node)
  ((sprite-frame :accessor sprite-frame :initarg :sprite-frame)))

(defun sprite-width (sprite)
  (* (scale-x sprite) (texture-frame-width (sprite-frame sprite))))

(defun sprite-height (sprite)
  (* (scale-y sprite) (texture-frame-height (sprite-frame sprite))))

(defclass physics-sprite (sprite)
  ((velocity-x :accessor velocity-x :initarg :velocity-x)
   (velocity-y :accessor velocity-y :initarg :velocity-y)
   (acceleration-x :accessor acceleration-x :initarg :acceleration-x)
   (acceleration-y :accessor acceleration-y :initarg :acceleration-y))
  (:default-initargs
   :velocity-x 0.0 :velocity-y 0.0
   :acceleration-x 0.0 :acceleration-y 0.0))

(defclass player (physics-sprite)
  ((can-jump :accessor can-jump :initform nil)
   (jump-power :accessor jump-power :initform 100.0)))

(defclass tmx-node (node)
  ((tmx :accessor tmx :initarg :tmx)))

(defgeneric width (node))
(defgeneric height (node))
(defmethod width ((self sprite))
  (sprite-width self))
(defmethod height ((self sprite))
  (sprite-height self))
(defmethod width ((self player))
  (* 0.6 (sprite-width self)))
(defmethod height ((self player))
  (* 0.9 (sprite-height self)))

(defun right (sprite)
  (+ (x sprite) (/ (width sprite) 2.0)))

(defun (setf right) (x sprite)
  (setf (x sprite) (- x (/ (width sprite) 2.0))))

(defun left (sprite)
  (- (x sprite) (/ (width sprite) 2.0)))

(defun (setf left) (x sprite)
  (setf (x sprite) (+ x (/ (width sprite) 2.0))))

(defun bottom (sprite)
  (- (y sprite) (/ (height sprite) 2.0)))

(defun (setf bottom) (y sprite)
  (setf (y sprite) (+ y (/ (height sprite) 2.0))))

(defun top (sprite)
  (+ (y sprite) (/ (height sprite) 2.0)))

(defun (setf top) (y sprite)
  (setf (y sprite) (- y (/ (height sprite) 2.0))))

(defgeneric draw (node))
(defmethod draw ((self node)))

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defmethod draw ((self sprite))
  (draw-node-color self)
  (render-buffer::draw-texture-frame (sprite-frame self) 0.0 0.0))

(defmethod draw ((self tmx-node))
  (let* ((r (tmx self))
         (x (/ (render-buffer::tmx-renderer-width r) 2.0))
         (y (/ (render-buffer::tmx-renderer-height r) 2.0)))
    (render-buffer::draw-tmx-renderer x y r)))

(defmethod visit ((self node))
  (when (not (visible self))
    (return-from visit))
  (render-buffer::push-matrix)
  (render-buffer::translate-scale-rotate
   (x self) (y self)
   (if (flip-x self) (* -1.0 (scale-x self)) (scale-x self))
   (if (flip-y self) (* -1.0 (scale-y self)) (scale-y self))
   (rotation self))
  (draw self)
  (when (node:children self)
    (loop for child across (node:children self) do
         (visit child)))
  (render-buffer::pop-matrix))

(defstruct pf
  started
  (keys (make-hash-table :test 'eql))
  root
  tmx
  player
  tmx-node
  )

(defun tile-at-point (tmx x y)
  (render-buffer::tmx-renderer-tile-at-point tmx x y))

(defmethod collide-with-tile (self side tile)
  (declare (ignore self side tile)))

(defun top-for-tile (tile x y)
  (case tile
    (2 (+ 1.0 (mod x 32.0) (* 32.0 (floor y 32.0))))
    (3 (+ 1.0 (- 32.0 (mod x 32.0)) (* 32.0 (floor y 32.0))))
    (t (+ 1.0 (* 32.0 (+ 1.0 (floor y 32.0)))))))

(defun bottom-for-tile (tile x y)
  (case tile
    (t (1- (* 32.0 (+ 0.0 (floor y 32.0)))))))

(defun left-for-tile (tile x y)
  (declare (ignore y))
  (case tile
    ((2 3) x)
    (t (1- (* 32.0 (floor x 32.0))))))

(defun right-for-tile (tile x y)
  (declare (ignore y))
  (case tile
    ((2 3) x)
    (t (1+ (* 32.0 (1+ (floor x 32.0)))))))

(defun move-sprite-up-if-hitting-tiles-on-bottom  (pf dt)
  (declare (ignore dt))
  (with-struct (pf- player tmx) pf
    (let ((y (bottom player)) (hit nil))
      (labels
          ((check (x)
             (let ((tile (tile-at-point tmx x y)))
               (unless (zerop tile)
                 (maxf y (top-for-tile tile x y))
                 (setf hit tile)
                 )))
           (run-checks ()
             (check (x player))
             (check (left player))
             (check (right player))))
        (prog ()
         :loop (run-checks)
         (unless (= (bottom player) y)
           (setf (bottom player) y)
           (go :loop))
         (when hit
           (collide-with-tile player 'bottom hit)))))))

(defun move-sprite-down-if-hitting-tiles-on-top (pf dt)
  (declare (ignore dt))
  (with-struct (pf- player tmx) pf
    (let ((y (top player)) (hit nil))
      (labels
          ((check (x)
             (let ((tile (tile-at-point tmx x y)))
               (unless (zerop tile)
                 (minf y (bottom-for-tile tile x y))
                 (setf hit tile))))
           (run-checks ()
             (check (x player))
             (check (left player))
             (check (right player))))
        (prog ()
         :loop (run-checks)
         (unless (= (top player) y)
           (setf (top player) y)
           (go :loop))
         (when hit
           (collide-with-tile player 'top hit)))))))

(defun move-sprite-left-if-hitting-tiles-on-right (pf dt)
  (declare (ignore dt))
  (with-struct (pf- player tmx) pf
    (let ((x (right player)) (hit nil))
      (labels
          ((check (y)
             (let ((tile (tile-at-point tmx x y)))
               (unless (zerop tile)
                 (minf x (left-for-tile tile x y))
                 (setf hit tile))))
           (run-checks ()
             (check (y player))
             (check (+ (y player) 10))
             (check (- (y player) 10))
             (check (top player))
             (check (bottom player))))
        (prog ()
         :loop
         (run-checks)
         (unless (= (right player) x)
           (setf (right player) x)
           (go :loop))
         (when hit
           (collide-with-tile player 'right hit)))))))

(defun move-sprite-right-if-hitting-tiles-on-left (pf dt)
  (declare (ignore dt))
  (with-struct (pf- player tmx) pf
    (let ((x (left player)) (hit nil))
      (labels ((check (y)
                 (let ((tile (tile-at-point tmx x y)))
                   (unless (zerop tile)
                     (maxf x (right-for-tile tile x y))
                     (setf hit tile))))
               (run-checks ()
                 (check (y player))
                 (check (+ (y player) 10))
                 (check (- (y player) 10))
                 (check (top player))
                 (check (bottom player))))
        (prog ()
         :loop
         (run-checks)
         (unless (= (left player) x)
           (setf (left player) x)
           (go :loop))
         (when hit
           (collide-with-tile player 'left hit)))))))

(defun update-player-physics (pf player dt)
  (incf (x player) (* dt (velocity-x player)))
  (move-sprite-left-if-hitting-tiles-on-right pf dt)
  (move-sprite-right-if-hitting-tiles-on-left pf dt)
  (incf (velocity-x player) (* dt (acceleration-x player)))

  (incf (y player) (* dt (velocity-y player)))
  (move-sprite-up-if-hitting-tiles-on-bottom pf dt)
  (move-sprite-down-if-hitting-tiles-on-top pf dt)
  (incf (velocity-y player) (* dt (acceleration-y player))))

(defmethod collide-with-tile ((self player) side tile)
  (when (and (eql tile 1) (eq side 'left))
    (format t "BANG~%"))
  (case side
    (bottom (setf (can-jump self) t
                  (velocity-y self) -100.0))
    ((left right)
     (case tile
       ;;do nothing
       ((2 3))
       (t (setf (velocity-x self) 0))))
    (top
     (setf (velocity-y self) (* -0.5 (velocity-y self))
           (jump-power self) 0))))

(defun move-player (pf dt)
  (let* ((jump-vel 225.0)
         (jump-power 100.0)
         (jump-drain-speed 100.0)
         (rise-gravity -800.0)
         (fall-gravity -1000.0)
         (hz-max-vel 200.0)
         (hz-accel-rate 300.0)
         (hz-decel-rate (* hz-accel-rate 2.25))
         (hz-floating-vel 200.0)
         (hz-floating-decel-rate 50.0))
    (with-struct (pf- player tmx keys) pf
      (flet ((key-down (key) (gethash key keys))
             (key-up   (key) (null (gethash key keys))))
        ;;use faster gravity for falling
        (if (> (velocity-y player) 0.0)
            (setf (acceleration-y player) rise-gravity)
            (setf (acceleration-y player) fall-gravity
                  (jump-power player) 0.0))
        ;;use jump-power to allow keypress to float player longer
        (when (and (key-down #\s)
                   (or (can-jump player) (> (jump-power player) 10.0)))
          (when (can-jump player)
            (setf (jump-power player) jump-power))
          (move-towards! (jump-power player) 0.0 jump-drain-speed dt)
          (setf (velocity-y player) (* jump-vel (/ (jump-power player) 80.0))
                (can-jump player) nil))
        (when (key-up #\s)
          (setf (jump-power player) 0.0))
        (when (key-down #\a)
          (setf hz-max-vel (* 2.0 hz-max-vel)
                hz-accel-rate (* 2.0 hz-accel-rate)))
        (cond ((can-jump player) ;;on the ground
               (when (key-down :left)
                 (move-towards! (velocity-x player) (- hz-max-vel) hz-accel-rate dt))
               (when (key-down :right)
                 (move-towards! (velocity-x player) hz-max-vel hz-accel-rate dt))
               (when (and (key-up :left) (key-up :right))
                 (move-towards! (velocity-x player) 0.0 hz-decel-rate dt 5.0)))
              (t ;;floating
               (when (key-down :left)
                 ;;only adjust speed if not already moving faster.
                 (when (> (velocity-x player) (- hz-floating-vel))
                   (move-towards! (velocity-x player)
                                  (- hz-floating-vel) hz-accel-rate dt)))
               (when (key-down :right)
                 (when (< (velocity-x player) hz-floating-vel)
                   (move-towards! (velocity-x player)
                                  hz-floating-vel hz-accel-rate dt)))
               (when (and (key-up :left) (key-up :right))
                 (move-towards! (velocity-x player)
                                0.0 hz-floating-decel-rate dt))))
        (when (< (abs (velocity-x player)) 2.0)
          (setf (velocity-x player) 0.0))
        (when (key-down :left)
          (setf (flip-x player) t))
        (when (key-down :right)
          (setf (flip-x player) nil))
        (update-player-physics pf player dt)
        (when (< (y player) 0.0)
          (setf (y player) 0.0
                (velocity-y player) 0.0))
        (clampf (x player) 0.0 15000)
        (clampf (y player) 0.0 1500)))))

(defun move-camera (pf dt)
  (declare (ignore dt))
  (with-struct (pf- root player) pf
    (let* ((center (+ (- (x root)) 250))
           (right-edge (+ center 100))
           (left-edge (- center 100)))
      (cond
        ((> (x player) right-edge)
         (decf (x root) (- (x player) right-edge )))
        ((and t (< (x player) left-edge))
         (incf (x root) (- left-edge (x player)))))
      (when (> (x root) 0.0)
        (setf (x root) 0.0)))))

(defun get-frame (sprites path)
  (texture-packer-get-frame sprites path))

(defmethod cl-user::contents-will-mount ((self pf) display)
  (declare (ignore display))
  (with-struct (pf- root tmx tmx-node player) self
    (let* ((sprites (texture-packer-from-file "./res/test.json"))
           (frame  (get-frame sprites "pickle.png")))
      (setf root (make-instance 'node)
            tmx (render-buffer::tmx-renderer-from-file
                                     "./res/platformer/dev.tmx")
            tmx-node (make-instance 'tmx-node
                               :tmx tmx)
            player (make-instance 'player
                                  :x 80.0
                                  :acceleration-y -100.0
                                  :sprite-frame frame))
      (setf (bottom player) 32.0)
      (add-child root tmx-node)
      (add-child root player))))


(defmethod cl-user::step-contents ((self pf) dt)
  (with-struct (pf- root started player) self
    (unless started
      (setf started t)
      (on-enter root))
    (move-player self dt)
    (move-camera self dt)
    (visit root)))

(defmethod cl-user::handle-event ((self pf) event)
  (let ((info (cdr event)) (keys (pf-keys self)))
    (case (car event)
      (:keydown (setf (gethash info keys) t))
      (:keyup   (setf (gethash info keys) nil)))))

(cl-user::display-contents (make-pf) :width 500 :height 500)
