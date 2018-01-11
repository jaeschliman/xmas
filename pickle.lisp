(defpackage :xmas.pickle
  (:use :cl :alexandria :xmas.node :xmas.action :xmas.texture :xmas.texture-packer :xmas.display)
  (:shadow #:get-frame))
(in-package :xmas.pickle)

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

(defclass sprite (node)
  ((sprite-frame :accessor sprite-frame :initarg :sprite-frame)))

(defun sprite-width (sprite)
  (* (scale-x sprite) (texture-frame-width (sprite-frame sprite))))

(defun sprite-height (sprite)
  (* (scale-y sprite) (texture-frame-height (sprite-frame sprite))))

(defun sprites-overlap? (a b)
  (let* ((ax1 (- (x a) (/ (sprite-width a) 2.0)))
         (ay1 (- (y a) (/ (sprite-height a) 2.0)))
         (bx1 (- (x b) (/ (sprite-width b) 2.0)))
         (by1 (- (y b) (/ (sprite-height b) 2.0)))
         (ax2 (+ (sprite-width a) ax1))
         (ay2 (+ (sprite-height a) ay1))
         (bx2 (+ (sprite-width b) bx1))
         (by2 (+ (sprite-height b) by1)))
    (if (or (> bx1 ax2)
            (< bx2 ax1)
            (> by1 ay2)
            (< by2 ay1))
        nil
        t)))

(defclass physics-sprite (sprite)
  ((velocity-x :accessor velocity-x :initarg :velocity-x)
   (velocity-y :accessor velocity-y :initarg :velocity-y)
   (acceleration-x :accessor acceleration-x :initarg :acceleration-x)
   (acceleration-y :accessor acceleration-y :initarg :acceleration-y))
  (:default-initargs
   :velocity-x 0.0 :velocity-y 0.0
   :acceleration-x 0.0 :acceleration-y 0.0))

(defun update-physics-sprite (sprite dt)
  (incf (x sprite) (* dt (velocity-x sprite)))
  (incf (y sprite) (* dt (velocity-y sprite)))
  (incf (velocity-x sprite) (* dt (acceleration-x sprite)))
  (incf (velocity-y sprite) (* dt (acceleration-y sprite))))

(defclass player (physics-sprite)
  ((can-jump :accessor can-jump :initform nil)))

(defclass tomato (physics-sprite)
  ((did-splat :accessor did-splat :initform nil)))

(defclass jewel (physics-sprite)
  ((done-bouncing :accessor done-bouncing :initform nil)))

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defgeneric draw (node))

(defmethod draw ((self node)))

(defmethod draw ((self sprite))
  (draw-node-color self)
  (xmas.draw::draw-texture-frame (sprite-frame self) 0.0 0.0))

(defmethod visit ((self node))
  (when (not (visible self))
    (return-from visit))
  (xmas.render-buffer::push-matrix)
  (xmas.render-buffer::translate-scale-rotate
   (x self) (y self)
   (if (flip-x self) (* -1.0 (scale-x self)) (scale-x self))
   (if (flip-y self) (* -1.0 (scale-y self)) (scale-y self))
   (rotation self))
  (draw self)
  (when (xmas.node:children self)
    (loop for child across (xmas.node:children self) do
         (visit child)))
  (xmas.render-buffer::pop-matrix))

(defstruct pickle-game
  (keys (make-hash-table :test 'eql))
  started
  root
  player
  sprites
  cats
  tomatoes
  width height
  jewels)

(defun get-frame (sprites path)
  (texture-packer-get-frame sprites path))

(defun add-tomato (pickle-game)
  (with-struct (pickle-game- sprites tomatoes root width height) pickle-game
    (let ((tomato (make-instance
                'tomato
                :sprite-frame (get-frame sprites "tomato.png")
                :x (random width)
                :y (+ 100.0 height)
                :velocity-y -200.0
                :acceleration-y -200.0)))
      (add-child root tomato)
      (push tomato tomatoes))))

(defun keep-adding-tomatoes (pickle-game)
  (with-struct (pickle-game- root) pickle-game
    (run-action root
                (list (delay 1.5)
                      (callfunc
                       (lambda ()
                         (add-tomato pickle-game)
                         (keep-adding-tomatoes pickle-game)))))))

(defun remove-tomato (pickle-game tomato)
  (with-struct (pickle-game- tomatoes) pickle-game
    (setf tomatoes (remove tomato tomatoes))
    (remove-from-parent tomato)))

(defun splat-tomato (pickle-game tomato)
  (unless (did-splat tomato)
    (setf (did-splat tomato) t)
    (with-struct (pickle-game- sprites root) pickle-game
      (setf (velocity-y tomato) 0.0)
      (setf (acceleration-y tomato) 0.0)
      (setf (sprite-frame tomato) (get-frame sprites "tomato splat.png"))
      (run-action tomato (rotate-by 0.2 20.0))
      (run-action tomato (list
                          (delay 0.1)
                          (fade-out 0.3)
                          (callfunc
                           (lambda ()
                             (remove-tomato pickle-game tomato))))))))

(defun move-tomatoes (pickle-game dt)
  (with-struct (pickle-game- tomatoes sprites) pickle-game
    (dolist (tomato tomatoes)
      (update-physics-sprite tomato dt)
      (when (< (y tomato) 75.0)
        (setf (y tomato) 75.0)
        (splat-tomato pickle-game tomato)))))

(defun reset-player-sprite-frame (pickle-game)
  (with-struct (pickle-game- player sprites) pickle-game
    (setf (sprite-frame player)
          (if (can-jump player)
              (get-frame sprites "pickle.png")
              (get-frame sprites "pickle jump.png")))))

(defun collide-player-with-tomatoes (pickle-game dt)
  (declare (ignore dt))
  (with-struct (pickle-game- tomatoes player sprites) pickle-game
    (dolist (tomato tomatoes)
      (when (and (not (did-splat tomato))
                 (sprites-overlap? tomato player))
        (splat-tomato pickle-game tomato)
        (setf (sprite-frame player) (get-frame sprites "pickle mad.png"))
        (run-action player (list (delay 0.4)
                                 (callfunc
                                  (lambda () (reset-player-sprite-frame pickle-game)))))))))


(defun add-jewel (pickle-game)
  (with-struct (pickle-game- sprites jewels root width height) pickle-game
    (let ((jewel (make-instance
                'jewel
                :sprite-frame (get-frame sprites "jewel.png")
                :x (random width)
                :y (+ 100.0 height)
                :scale-x 1.35
                :scale-y 1.35
                :velocity-y -200.0
                :acceleration-y -200.0)))
      (run-action jewel (rotate-by 0.75 -360) :repeat :forever)
      (add-child root jewel)
      (push jewel jewels))))

(defun keep-adding-jewels (pickle-game)
  (with-struct (pickle-game- root) pickle-game
    (run-action root
                (list (delay 2.0)
                      (callfunc
                       (lambda ()
                         (add-jewel pickle-game)
                         (keep-adding-jewels pickle-game)))))))

(defun remove-jewel (pickle-game jewel)
  (with-struct (pickle-game- jewels) pickle-game
    (setf jewels (remove jewel jewels))
    (remove-from-parent jewel)))

(defun move-jewels (pickle-game dt)
  (with-struct (pickle-game- jewels) pickle-game
    (dolist (jewel jewels)
      (update-physics-sprite jewel dt)
      (when (< (y jewel) 75.0)
        (setf (y jewel) 75.0)
        (setf (velocity-y jewel) (* -0.5 (velocity-y jewel)))
        (when (< (abs (velocity-y jewel)) 200.0)
          (unless (done-bouncing jewel)
            (setf (done-bouncing jewel) t)
            (run-action jewel (list (fade-out 2.0)
                                    (callfunc
                                     (lambda ()
                                       (remove-jewel pickle-game jewel)))))))))))

(defun collide-player-with-jewels (pickle-game dt)
  (declare (ignore dt))
  (with-struct (pickle-game- jewels player sprites) pickle-game
    (dolist (jewel jewels)
      (when (sprites-overlap? player jewel)
        (remove-jewel pickle-game jewel)
        (setf (sprite-frame player) (get-frame sprites "pickle blink.png"))
        (run-action player (list (delay 0.4)
                                 (callfunc
                                  (lambda () (reset-player-sprite-frame pickle-game)))))))))

(defun add-cat (pickle-game)
  (with-struct (pickle-game- sprites cats root width height) pickle-game
    (let ((cat (make-instance
                'physics-sprite
                :sprite-frame (get-frame sprites "throwcat.png")
                :x (random width)
                :y (+ 100.0 height)
                :scale-x 1.35
                :scale-y 1.35
                :velocity-y -200.0
                :acceleration-y -200.0)))
      (run-action cat (rotate-by 0.75 -360) :repeat :forever)
      (add-child root cat)
      (push cat cats))))

(defun keep-adding-cats (pickle-game)
  (with-struct (pickle-game- root) pickle-game
    (run-action root
                (list (delay 8.5)
                      (callfunc
                       (lambda ()
                         (add-cat pickle-game)
                         (keep-adding-cats pickle-game)))))))

(defun remove-cat (pickle-game cat)
  (with-struct (pickle-game- cats) pickle-game
    (setf cats (remove cat cats))
    (remove-from-parent cat)))

(defun move-cats (pickle-game dt)
  (with-struct (pickle-game- cats width) pickle-game
    (dolist (cat cats)
      (update-physics-sprite cat dt)
      (when (< (y cat) 75.0)
        (setf (y cat) 75.0)
        (setf (velocity-y cat) 0.0)
        (setf (acceleration-y cat) 0.0)
        (setf (velocity-x cat) 100.0)
        (stop-all-actions cat)
        (setf (rotation cat) 0.0)
        (run-action
         cat
         (list (rotate-by 0.2 -5.0)
               (rotate-by 0.2  5.0))
         :repeat :forever))
      (when (= (y cat) 75.0)
        (move-towards! (velocity-x cat) 200.0 100.0 dt))
      (when (> (x cat) (+ 100.0 width))
        (remove-cat pickle-game cat)))))

(defmethod cl-user::contents-will-mount ((self pickle-game) display)
  (with-struct (pickle-game- root sprites player width height) self
    (setf width (display-width display)
          height (display-height display)
          root (make-instance 'node)
          sprites (texture-packer-from-file "./res/test.json")
          player (make-instance
                  'player
                  :sprite-frame (get-frame sprites "pickle.png"))
          (can-jump player) t
          (scale-x player) 1.25
          (scale-y player) 1.25
          (x player) (/ width 2.0)
          (y player) 100)
    (add-child root player)
    (keep-adding-cats self)
    (keep-adding-tomatoes self)
    (keep-adding-jewels self)))

(defun move-player (pickle-game dt)
  (with-struct (pickle-game- player keys sprites) pickle-game
    (let* ((max-vel 300.0)
           (accel-rate 300.0)
           (decel-rate (* accel-rate 0.75))
           (rise-gravity -800.0)
           (fall-gravity -1000.0)
           (jump-vel 400.0))
      (flet ((key-down (key) (gethash key keys))
             (key-up   (key) (null (gethash key keys))))
        (if (key-down :left)
            (move-towards! (velocity-x player) (- max-vel) accel-rate dt)
            (when (key-up :right)
              (move-towards! (velocity-x player) 0.0 decel-rate dt)))
        (if (key-down :right)
            (move-towards! (velocity-x player) max-vel accel-rate dt)
            (when (key-up :left)
              (move-towards! (velocity-x player) 0.0 decel-rate dt)))
        (when (key-down :left)
          (setf (flip-x player) t))
        (when (key-down :right)
          (setf (flip-x player) nil))
        (if (> (velocity-y player) 0.0 )
            (setf (acceleration-y player) rise-gravity)
            (setf (acceleration-y player) fall-gravity))
        (when (and (key-down :up) (can-jump player))
          (setf (velocity-y player) jump-vel
                (can-jump player) nil
                (sprite-frame player) (get-frame sprites "pickle jump.png")))
        (when (key-down :down)
          (setf (sprite-frame player) (get-frame sprites "pickle mad.png")))))
    (update-physics-sprite player dt)
    (when (or (> (x player) 500) (< (x player) 0))
      (clampf (x player) 0 500)
      (setf (velocity-x player) 0))
    (when (< (y player) 100.0)
      (when (null (can-jump player))
        (setf (sprite-frame player) (get-frame sprites "pickle.png")))
      (setf (can-jump player) t)
      (setf (y player) 100.0)
      (setf (velocity-y player) 0.0))))

(defmethod cl-user::step-contents ((self pickle-game) dt)
  (declare (ignorable dt))
  (with-struct (pickle-game- started root) self
    (unless started
      (setf started t)
      (on-enter root))
    (move-player self dt)
    (move-cats self dt)
    (move-tomatoes self dt)
    (move-jewels self dt)
    (collide-player-with-tomatoes self dt)
    (collide-player-with-jewels self dt)
    (visit root)))

(defmethod cl-user::handle-event ((self pickle-game) event)
  (let ((info (cdr event)) (keys (pickle-game-keys self)))
    (case (car event)
      (:keydown (setf (gethash info keys) t))
      (:keyup   (setf (gethash info keys) nil)))))

(cl-user::display-contents (make-pickle-game) :width 500 :height 500)
