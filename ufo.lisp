(defpackage :xmas.ufo (:use :cl :alexandria :xmas.node :xmas.action :xmas.texture :xmas.display))
(in-package :xmas.ufo)

(defmacro with-struct ((prefix &rest slots) var &body body)
  (once-only (var)
    `(symbol-macrolet
         ,(loop for slot in slots collect
               (list slot (list (symbolicate prefix slot) var)))
       ,@body)))

(defmacro move-towards! (place dest speed dt)
  (once-only (dest speed dt)
    `(let* ((curr ,place)
            (delta (* ,speed ,dt (if (< ,dest curr) -1 1)))
            (new-val (incf curr delta)))
       (setf ,place new-val))))

(define-modify-macro clampf (min max) clamp)

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defclass sprite (node)
  ((texture :accessor texture :initarg :texture)))

(defclass physics-sprite (sprite)
  ((velocity-x :accessor velocity-x :initarg :velocity-x)
   (velocity-y :accessor velocity-y :initarg :velocity-y)
   (acceleration-x :accessor acceleration-x :initarg :acceleration-x)
   (acceleration-y :accessor acceleration-y :initarg :acceleration-y))
  (:default-initargs
   :velocity-x 0.0 :velocity-y 0.0
   :acceleration-x 0.0 :acceleration-y 0.0))

(defclass abductable (physics-sprite)
  ((is-being-abducted :accessor is-being-abducted :initform nil)))

(defun update-physics-sprite (sprite dt)
  (incf (x sprite) (* dt (velocity-x sprite)))
  (incf (y sprite) (* dt (velocity-y sprite)))
  (incf (velocity-x sprite) (* dt (acceleration-x sprite)))
  (incf (velocity-y sprite) (* dt (acceleration-y sprite))))

(defclass rect (node)
  ((width :accessor rect-width :initarg :width)
   (height :accessor rect-height :initarg :height)
   (texture :accessor texture :initarg :texture)))

(defun sprite-with-file (path)
  (let ((texture (get-texture path)))
    (make-instance 'sprite :texture texture)))

(defgeneric draw (node))

(defmethod draw ((self node)))

(defmethod draw ((self sprite))
  (draw-node-color self)
  (xmas.render-buffer::draw-texture (texture self)))

(defmethod draw ((self rect))
  (draw-node-color self)
  (when-let (id (texture-id (texture self)))
    (xmas.render-buffer::simple-draw-gl-texture-no-color
     id (rect-width self) (rect-height self))))

(defmethod visit ((self node))
  (when (not (visible self))
    (return-from visit))
  (xmas.render-buffer::push-matrix)
  (xmas.render-buffer::translate-scale-rotate
   (x self) (y self)
   (scale-x self) (scale-y self)
   (rotation self))
  (draw self)
  (when (xmas.node:children self)
    (loop for child across (xmas.node:children self) do
         (visit child)))
  (xmas.render-buffer::pop-matrix))

(defstruct ufo-game
  player
  started
  root-node
  player-moving
  building1 building2 cows beam
  (keys (make-hash-table :test 'eql))
  (speed 50))

(defun add-cow (ufo-game)
  (let* ((width 500)
         (cow (make-instance 'abductable
                             :x (+ width 100)
                             :y (+ 30.0 (random 7))
                             :texture (get-texture "./cow.png"))))
    (with-struct (ufo-game- root-node cows) ufo-game
      (setf (rotation cow) -0.5)
      (run-action cow (list (rotate-by 0.1 5.0)
                            (rotate-by 0.1 -5.0))
                  :repeat :forever)
      (add-child root-node cow)
      (push cow cows))))

(defun remove-cow (ufo-game cow)
  (with-struct (ufo-game- cows) ufo-game
    (remove-from-parent cow)
    (setf cows (remove cow cows))))

(defun add-cow-after-random-delay (ufo-game)
  (with-struct (ufo-game- root-node) ufo-game
    (let ((action (run-sequence
                   (callfunc (lambda ()
                               (add-cow ufo-game)))
                   (delay (+ 2.0 (random 3)))
                   (callfunc (lambda ()
                               (add-cow-after-random-delay ufo-game) )))))
      (run-action root-node action))))

(defmethod cl-user::contents-will-mount ((self ufo-game) display)
  (let* ((width (display-width display))
         (height (display-height display))
         (center-x (/ width 2.0))
         (center-y (/ height 2.0))
         (ufo (make-instance 'physics-sprite
                             :z-order 1
                             :texture (get-texture "./ufo.png")))
         (beam (sprite-with-file "./beam.png"))
         (root (make-instance 'node))
         (doom (sprite-with-file "./ufo.png"))
         (buildings1 (sprite-with-file "./buildings.png"))
         (buildings2 (sprite-with-file "./buildings.png"))
         (sky (make-instance 'rect
                             :texture (get-texture "./pixel.png")
                             :width width :height height
                             :color (vector 0.0 1.0 1.0)))
         (cows nil))
    (setf (ufo-game-player self) ufo
          (ufo-game-root-node self) root
          (scale-x ufo) 0.2
          (scale-y ufo) 0.2
          (rotation ufo) -10.0
          (x sky) center-x
          (y sky) center-y

          (x ufo) center-x
          (y ufo) center-y

          (y beam) (- (/ -700.0 2.0) 60.0)
          (x beam) (- 10.0)
          (color beam) (vector 0.0 1.0 0.0)
          (visible beam) nil

          (x doom) center-x
          (y doom) center-y
          (color doom) (vector 1.0 0.0 0.0)
          (opacity doom) 0.8

          (x buildings1) center-x
          (y buildings1) 125
          (x buildings2) (+ center-x width)
          (y buildings2) 125

          (ufo-game-building1 self) buildings1
          (ufo-game-building2 self) buildings2
          (ufo-game-cows self) cows
          (ufo-game-beam self) beam)

    (add-cow-after-random-delay self)

    (add-child root doom)

    ;; -- leave the sky out for now. it looks good dark
    ;; (add-child root sky)
    (add-child root buildings1)
    (add-child root buildings2)

    (add-child root ufo)
    (add-child ufo beam)

    (run-action beam
                (list (fade-in 0.6)
                      (fade-out 0.6))
                :repeat :forever)

    (run-action
     ufo
     (list (rotate-by 0.6 20.0 :ease :in-out-sine)
           (rotate-by 0.6 -20.0 :ease :in-out-sine))
     :repeat :forever)))

(defun adjust-game-speed (ufo-game dt)
  (move-towards! (ufo-game-speed ufo-game) 150 5 dt))

(defun adjust-beam-opacity (ufo-game dt)
  (declare (ignorable dt))
  (with-struct (ufo-game- beam) ufo-game
    (when (visible beam)
      (setf (opacity beam) (+ 0.3 (* 0.4 (opacity beam)))))))

(defun move-buildings (ufo-game dt)
  (let* ((speed (* 0.5 (ufo-game-speed ufo-game)))
         (width 500)
         (b1 (ufo-game-building1 ufo-game))
         (b2 (ufo-game-building2 ufo-game))
         (min-x (- (/ width 2.0)))
         (new-x (- (x b1) (* speed dt))))
    (when (< new-x min-x)
      (incf new-x width))
    (setf (x b1) new-x
          (x b2) (+ new-x width))))

(defun move-cows (ufo-game dt)
  (let* ((speed (- (ufo-game-speed ufo-game)))
         (min-y 30.0)
         (min-x -50.0))
    (dolist (cow (ufo-game-cows ufo-game))
      (unless (is-being-abducted cow)
        (setf (velocity-x cow) speed))
      (update-physics-sprite cow dt)
      (when (< (x cow) min-x)
        (remove-cow ufo-game cow))
      (when (< (y cow) min-y)
        (setf (acceleration-y cow) 0
              (velocity-y cow) 0
              (y cow) min-y)))))

(defun start-abducting-cow (cow)
  (setf (is-being-abducted cow) t
        (velocity-y cow) 0.0
        (acceleration-x cow) 0.0
        (acceleration-y cow) 0.0
        (color cow) (vector 0.0 1.0 0.0))
  (stop-all-actions cow)
  (run-action cow
              (list (delay 1.0)
                    (rotate-by 5.0 -180.0 :ease :in-sine))))

(defun stop-abducting-cow (cow)
  (setf (is-being-abducted cow) nil
        (velocity-x cow) -100.0
        (acceleration-y cow) -500.0
        (color cow) (vector 1.0 1.0 1.0))
  (stop-all-actions cow))

(defun hover-over-cows (ufo-game dt)
  (declare (ignorable dt))
  (with-struct (ufo-game- player cows beam) ufo-game
    (let ((x (x player))
          (y (y player))
          (beam-radius (expt 150.0 2))
          (got-em-radius (expt 10.0 2)))
      (flet ((dsq (cow)
               (+ (expt (- x (x cow)) 2) (expt (- y (y cow)) 2))))
        (setf (visible beam) nil)
        (dolist (cow cows)
          (let ((dsq (dsq cow)))
            (cond
              ((< dsq got-em-radius)
               (remove-cow ufo-game cow))
              ((and (<= (y cow) y)
                    (< (abs (- x (x cow))) 60)
                    (< dsq beam-radius))
               (setf (visible beam) t)
               (unless (is-being-abducted cow)
                 (start-abducting-cow cow))
               (move-towards! (velocity-x cow) 0.0 40.0 dt)
               (move-towards! (scale-x cow) 0.3 0.25 dt)
               (move-towards! (scale-y cow) 0.3 0.25 dt)
               (move-towards! (y cow) y 40 dt)
               (move-towards! (x cow) x 100 dt))
              ((is-being-abducted cow)
               (stop-abducting-cow cow)))))))))

(defun move-ufo (ufo-game dt)
  (with-struct (ufo-game- player keys) ufo-game
    (flet ((key-down (k) (gethash k keys))
           (key-up   (k) (null (gethash k keys))))
      (let* ((max-vel 250.0)
             (min-vel (- max-vel))
             (accel-rate 350.0)
             (decel-rate 40.0))
        (if (key-down :left)
            (move-towards! (velocity-x player) min-vel accel-rate dt)
            (when (key-up :right)
              (move-towards! (velocity-x player) 0.0 decel-rate dt)))
        (if (key-down :right)
            (move-towards! (velocity-x player) max-vel accel-rate dt)
            (when (key-up :left)
              (move-towards! (velocity-x player) 0.0 decel-rate dt)))
        (if (key-down :down)
            (move-towards! (velocity-y player) min-vel accel-rate dt)
            (when (key-up :up)
              (move-towards! (velocity-y player) 0.0 decel-rate dt)))
        (if (key-down :up)
            (move-towards! (velocity-y player) max-vel accel-rate dt)
            (when (key-up :down)
              (move-towards! (velocity-y player) 0.0 decel-rate dt)))
        (clampf (velocity-x player) min-vel max-vel)
        (clampf (velocity-y player) min-vel max-vel)
        (update-physics-sprite player dt)
        (let ((x (x player)) (y (y player)))
          (when (or (< x 50.0) (> x 450.0))
            (setf (velocity-x player) 0.0
                  (acceleration-x player) 0.0))
          (when (or (< y 100.0) (> y 450.0))
            (setf (velocity-y player) 0.0
                  (acceleration-y player) 0.0)))
        (clampf (x player) 50.0 450.0)
        (clampf (y player) 100.0 450.0)))))

(defmethod cl-user::step-contents ((self ufo-game) dt)
  (declare (ignorable dt))
  (unless (ufo-game-started self)
    (setf (ufo-game-started self) t)
    (on-enter (ufo-game-root-node self)))
  (adjust-game-speed self dt)
  (move-ufo self dt)
  (move-buildings self dt)
  (move-cows self dt)
  (hover-over-cows self dt)
  (adjust-beam-opacity self dt)
  (visit (ufo-game-root-node self)))

(defmethod cl-user::handle-event ((self ufo-game) event)
  (let ((info (cdr event)) (keys (ufo-game-keys self)))
    (case (car event)
      (:keydown (setf (gethash info keys) t))
      (:keyup   (setf (gethash info keys) nil)))))


(cl-user::display-contents (make-ufo-game)
                           :width 500 :height 500
                           :expandable t
                           :preserve-aspect-ratio t)
