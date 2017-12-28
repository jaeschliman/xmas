(defpackage :ufo (:use :cl :alexandria :node :action :texture :display))
(in-package :ufo)

(defmacro with-struct ((prefix &rest slots) var &body body)
  (once-only (var)
    `(symbol-macrolet
         ,(loop for slot in slots collect
               (list slot (list (symbolicate prefix slot) var)))
       ,@body)))

(defclass sprite (node)
  ((texture :accessor texture :initarg :texture)))

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
  (apply 'render-buffer::set-color (color self))
  (render-buffer::draw-texture (texture self)))

(defmethod draw ((self rect))
  (apply 'render-buffer::set-color (color self))
  (when-let (id (texture-id (texture self)))
    (render-buffer::simple-draw-gl-texture-no-color
     id (rect-width self) (rect-height self))))

(defmethod visit ((self node))
  (render-buffer::push-matrix)
  (render-buffer::translate-scale-rotate
   (x self) (y self)
   (scale-x self) (scale-y self)
   (rotation self))
  (draw self)
  (when (node:children self)
    (loop for child across (node:children self) do
         (visit child)))
  (render-buffer::pop-matrix))

(defstruct ufo-game
  player
  started
  root-node
  player-moving
  building1 building2 cows)

(defmethod cl-user::contents-will-mount ((self ufo-game) display)
  (let* ((width (display-width display))
         (height (display-height display))
         (center-x (/ width 2.0))
         (center-y (/ height 2.0))
         (ufo (sprite-with-file "./ufo.png"))
         (root (make-instance 'node))
         (doom (sprite-with-file "./ufo.png"))
         (buildings1 (sprite-with-file "./buildings.png"))
         (buildings2 (sprite-with-file "./buildings.png"))
         (sky (make-instance 'rect 
                             :texture (get-texture "./pixel.png")
                             :width width :height height
                             :color '(0.0 1.0 1.0 1.0)))
         (cows (loop repeat 5 collect
                    (make-instance 'sprite
                                   :x (random (+ width 100))
                                   :y (+ 30.0 (random 7))
                                   :texture (get-texture "./cow.png")))))
    (setf (ufo-game-player self) ufo
          (ufo-game-root-node self) root
          (scale-x ufo) 0.2
          (scale-y ufo) 0.2
          (rotation ufo) -10.0
          (x sky) center-x
          (y sky) center-y
          (x ufo) center-x
          (y ufo) center-y
          (x doom) center-x
          (y doom) center-y

          (x buildings1) center-x
          (y buildings1) 125
          (x buildings2) (+ center-x width)
          (y buildings2) 125

          (ufo-game-building1 self) buildings1
          (ufo-game-building2 self) buildings2
          (ufo-game-cows self) cows)

    (setf (color doom) '(1.0 0.0 0.0 0.8))
    (add-child root doom)

    ;; -- leave the sky out for now. it looks good dark
    ;; (add-child root sky)
    (add-child root buildings1)
    (add-child root buildings2)

    (dolist (cow cows)
      (add-child root cow)
      (setf (rotation cow) -0.5)
      (run-action cow (list (rotate-by 0.1 5.0)
                            (rotate-by 0.1 -5.0))
                  :repeat :forever))

    (add-child root ufo)
    
    (run-action
     ufo
     (list (rotate-by 0.6 20.0 :ease :in-out-sine)
           (rotate-by 0.6 -20.0 :ease :in-out-sine))
     :repeat :forever)))

(defun move-buildings (ufo-game dt)
  (let* ((speed 50)
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
  (let* ((speed 100)
         (width 500)
         (delx (* dt speed))
         (min-x -50.0))
    (dolist (cow (ufo-game-cows ufo-game))
      (decf (x cow) delx)
      (when (< (x cow) min-x)
        (incf (x cow) (+ width 100.0))))))

(defmethod cl-user::step-contents ((self ufo-game) dt)
  (declare (ignorable dt))
  (unless (ufo-game-started self)
    (setf (ufo-game-started self) t)
    (on-enter (ufo-game-root-node self)))
  (move-buildings self dt)
  (move-cows self dt)
  (visit (ufo-game-root-node self)))

(defmethod cl-user::handle-event ((self ufo-game) event)
  (when (eq (car event) :keydown)
    (with-struct (ufo-game- player player-moving) self
        (when (not player-moving)
          (let ((key (cdr event))
                (amt 40.0))
            (flet ((move (x y)
                     (setf player-moving t)
                     (run-action
                      player
                      (list
                       (move-by 0.2 x y :ease :in-out-sine)
                       (callfunc (lambda () (setf player-moving nil)))))))
              (case key
                (:left  (move (- amt) 0.0))
                (:right (move amt 0.0))
                (:up    (move 0.0 amt))
                (:down  (move 0.0 (- amt))))))))))


(cl-user::display-contents (make-ufo-game) :width 500 :height 500)
