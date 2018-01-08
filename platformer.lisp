(defpackage :platformer (:use :cl :alexandria :node :sprite :action :texture :texture-packer :display :xmas.animation-manager))
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

(defstruct tile
  shape
  gid
  material
  type)

(defun tile-from-properties (gid plist)
  (let ((result (make-tile :gid gid)))
    (prog1 result
      (let ((*read-eval* nil))
        (when-let (string (getf plist :shape))
          (setf (tile-shape result) (read-from-string string)))
        (when-let (string (getf plist :material))
          (setf (tile-material result) (read-from-string string)))
       (when-let (string (getf plist :type))
          (setf (tile-type result) (read-from-string string))) ))))

(defun tile-lookup-table-from-tmx-renderer (tmx)
  (let* ((props (render-buffer::tmx-renderer-tile-properties tmx))
         (result (make-array (length props))))
    (prog1 result
      (loop for plist across props
         for idx upfrom 0 do
           (setf (aref result idx) (tile-from-properties idx plist))))))

(defclass image (node)
  ((texture :accessor texture :initarg :texture)))

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
   (jump-power :accessor jump-power :initform 100.0)
   (jumping :accessor jumping :initform nil)
   (standing-on :accessor standing-on :initform nil)
   (state :accessor state :initform nil :initarg :state)))

(defgeneric leave-state (object state next-state))
(defgeneric enter-state (object state prev-state))

(defun set-state (player next-state)
  (let ((prev-state (state player)))
    (unless (eq prev-state next-state)
      (leave-state player prev-state next-state)
      (setf (state player) next-state)
      (enter-state player next-state prev-state))))

(defclass tmx-node (node)
  ((tmx :accessor tmx :initarg :tmx)))

(defmethod width ((self player))
  24.0
  ;;(* 0.6 (sprite-width self))
  )
(defmethod height ((self player))
  64
  ;;(* 0.8 (sprite-height self))
  )

(defgeneric draw (node))
(defmethod draw ((self node)))

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defmethod draw ((self image))
  (draw-node-color self)
  (render-buffer::draw-texture (texture self)))

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
  tile-table
  background
  )

(defun tile-at-point (tmx x y)
  (render-buffer::tmx-renderer-tile-at-point tmx x y))

(defmethod collide-with-tile (self side tile)
  (declare (ignore self side tile)))

(defun top-for-tile (tile x y)
  (case (tile-shape tile)
    (slope-left  (+ 1.0 (mod x 32.0) (* 32.0 (floor y 32.0))))
    (slope-right (+ 1.0 (- 32.0 (mod x 32.0)) (* 32.0 (floor y 32.0))))
    (block       (+ 1.0 (* 32.0 (+ 1.0 (floor y 32.0)))))))

(defun bottom-for-tile (tile x y)
  (declare (ignore x))
  (case (tile-shape tile)
    (t (1- (* 32.0 (+ 0.0 (floor y 32.0)))))))

(defun left-for-tile (tile x y)
  (declare (ignore y))
  (ecase (tile-shape tile)
    ((slope-left slope-right) x) ;;so we can get 'pushed up' to correct position
    (block (1- (* 32.0 (floor x 32.0))))))

(defun right-for-tile (tile x y)
  (declare (ignore y))
  (ecase (tile-shape tile)
    ((slope-left slope-right) x)
    (block (1+ (* 32.0 (1+ (floor x 32.0)))))))

(defun move-sprite-up-if-hitting-tiles-on-bottom  (pf sprite dt)
  (declare (ignore dt))
  (with-struct (pf- tmx tile-table) pf
    (let ((y (bottom sprite)) (hit nil))
      (labels
          ((check (x)
             (let ((tile (tile-at-point tmx x y)))
               (unless (zerop tile)
                 (maxf y (top-for-tile (aref tile-table tile) x y))
                 (setf hit tile))))
           (run-checks ()
             (check (x sprite))
             (check (left sprite))
             (check (right sprite))))
        (prog ()
         :loop (run-checks)
         (unless (= (bottom sprite) y)
           (setf (bottom sprite) y)
           (go :loop))
         (when hit
           (let ((tile (aref tile-table hit)))
             (collide-with-tile sprite 'bottom tile)
             (return tile))))))))

(defun move-sprite-down-if-hitting-tiles-on-top (pf sprite dt)
  (declare (ignore dt))
  (with-struct (pf- tmx tile-table) pf
    (let ((y (top sprite)) (hit nil))
      (labels
          ((check (x)
             (let ((tile (tile-at-point tmx x y)))
               (unless (zerop tile)
                 (minf y (bottom-for-tile (aref tile-table tile) x y))
                 (setf hit tile))))
           (run-checks ()
             (check (x sprite))
             (check (left sprite))
             (check (right sprite))))
        (prog ()
         :loop (run-checks)
         (unless (= (top sprite) y)
           (setf (top sprite) y)
           (go :loop))
         (when hit
           (collide-with-tile sprite 'top (aref tile-table hit))))))))

(defun move-sprite-left-if-hitting-tiles-on-right (pf sprite dt)
  (declare (ignore dt))
  (with-struct (pf- tmx tile-table) pf
    (let ((x (right sprite)) (hit nil))
      (labels
          ((check (y)
             (let ((tile (tile-at-point tmx x y)))
               (unless (zerop tile)
                 (minf x (left-for-tile (aref tile-table tile) x y))
                 (setf hit tile))))
           (run-checks ()
             (check (y sprite))
             (check (+ (y sprite) 10))
             (check (- (y sprite) 10))
             (check (top sprite))
             (check (bottom sprite))))
        (prog ()
         :loop
         (run-checks)
         (unless (= (right sprite) x)
           (setf (right sprite) x)
           (go :loop))
         (when hit
           (collide-with-tile sprite 'right (aref tile-table hit))))))))

(defun move-sprite-right-if-hitting-tiles-on-left (pf sprite dt)
  (declare (ignore dt))
  (with-struct (pf- tmx tile-table) pf
    (let ((x (left sprite)) (hit nil))
      (labels ((check (y)
                 (let ((tile (tile-at-point tmx x y)))
                   (unless (zerop tile)
                     (maxf x (right-for-tile (aref tile-table tile) x y))
                     (setf hit tile))))
               (run-checks ()
                 (check (y sprite))
                 (check (+ (y sprite) 10))
                 (check (- (y sprite) 10))
                 (check (top sprite))
                 (check (bottom sprite))))
        (prog ()
         :loop
         (run-checks)
         (unless (= (left sprite) x)
           (setf (left sprite) x)
           (go :loop))
         (when hit
           (collide-with-tile sprite 'left (aref tile-table hit))))))))

(defun update-sprite-physics (pf sprite dt)
  (let (standing-on)
    (incf (x sprite) (* dt (velocity-x sprite)))
    (move-sprite-left-if-hitting-tiles-on-right pf sprite dt)
    (move-sprite-right-if-hitting-tiles-on-left pf sprite dt)
    (incf (velocity-x sprite) (* dt (acceleration-x sprite)))
    (clampf (velocity-x sprite) -1000 1000)

    (incf (y sprite) (* dt (velocity-y sprite)))
    (setf standing-on (move-sprite-up-if-hitting-tiles-on-bottom pf sprite dt))
    (move-sprite-down-if-hitting-tiles-on-top pf sprite dt)
    (incf (velocity-y sprite) (* dt (acceleration-y sprite)))
    (clampf (velocity-y sprite) -1000 1000)
    standing-on))

(defmethod collide-with-tile ((self player) side tile)
  (when (eq side 'bottom)
    (setf (can-jump self) t))
  (case (tile-material tile)
    (rubber
     (let ((bounce -1.1))
       (case side
         ;;bounce
         ((left right) (setf (velocity-x self) (* bounce (velocity-x self))))
         (top (setf (velocity-y self) (* bounce (velocity-y self))))
         (bottom
          (let* ((vel (velocity-y self))
                 (new-vel (if (< (abs vel) 150.0) -100.0 (* bounce vel))))
            (setf (velocity-y self) new-vel))))))
    (t
     (case side
       (bottom
        (case (tile-shape tile)
          (slope-left
           ;;so the player will stick to the ground when walking
           (when (< (velocity-x self) 0)
             (setf (acceleration-y self) -1000)))
          (slope-right
           ;;so the player will stick to the ground when walking
           (when (> (velocity-x self) 0)
             (setf (acceleration-y self) -1000)))
          (block
              ;;keep the player on the ground, but allow running to
              ;;to float off of slopes a bit
              (setf (velocity-y self) -100))))
       ((left right)
        (case (tile-shape tile)
          ;;do nothing
          ((slope-left slope-right))
          (block (setf (velocity-x self) 0))))
       (top
        ;;richochet
        (setf (velocity-y self) (* -0.5 (velocity-y self))
              (jumping self) nil
              (jump-power self) 0))))))

(defmethod leave-state ((self player) state next-state)
  (declare (ignore state next-state))
  ;;(format t "leaving ~A for ~A~%" state next-state)
  )
(defmethod enter-state ((self player) state prev-state)
  ;; (format t "entering ~A from ~A~%" state prev-state)
  (declare (ignore prev-state))
  (flet ((set-frame (f)
           (stop-animation self)
           (setf (sprite-frame self) (get-frame f)))
         (animate (name)
           (run-animation self name :repeat :forever)))
    (case state
      (standing
       (set-frame "pickle.png"))
      (walking
       (animate 'pickle-walk))
      (running
       (animate 'pickle-run))
      (floating
       (set-frame "pickle float.png"))
      (falling
       (set-frame "pickle fall.png"))
      (jumping
       (set-frame "pickle jump.png"))
      (t
       (set-frame "pickle.png")))))

(defun hz-accel-rate-for-tile (tile)
  (case (tile-material tile)
    (brick 300.0)
    (ice   150.0)
    (t 300.0)))

(defun hz-decel-rate-for-tile (tile)
  (case (tile-material tile)
    (brick (* 300.0 2.25))
    (ice   (* 300.0 0.05))
    (t     (* 300.0 2.25))))

(defun update-state (player pf dt)
  (declare (ignore dt))
  (with-struct (pf- tmx keys) pf
    (flet ((key-down (key) (gethash key keys))
           (key-up   (key) (null (gethash key keys))))
      (let ((state (state player))
            (left (key-down :left))
            (right (key-down :right))
            (fast (key-down #\a))
            (jump (key-down #\s))
            (on-ground (standing-on player)))
        (cond
          (on-ground
           (cond
             (jump 'jumping)
             ((or left right) (if fast 'running 'walking))
             (t 'standing)))
          ((eq state 'jumping)
           (cond
             (jump (if (< (velocity-y player) 0.0) 'falling 'jumping))
             (t    'floating)))
          (t (if (< (velocity-y player) 0.0) 'falling 'floating)))))))

(defun maybe-jump-player (pf dt)
  (with-struct (pf- player tmx keys) pf
    (labels ((key-down (key) (gethash key keys))
             (key-up   (key) (null (gethash key keys)))
             (use-floating-jump  () nil))
      (cond
        ((use-floating-jump)
         (let ((jump-vel 225.0)
               (jump-power 100.0)
               (jump-drain-speed 100.0))
           ;;use jump-power to allow keypress to float player longer
           (when (and (key-down #\s)
                      (or (can-jump player) (> (jump-power player) 10.0)))
             (when (can-jump player)
               (setf (jump-power player) jump-power
                     (jumping player) t))
             (move-towards! (jump-power player) 0.0 jump-drain-speed dt)
             (setf (velocity-y player) (* jump-vel (/ (jump-power player) 80.0))
                   (can-jump player) nil))
           (when (key-up #\s)
             (setf (jump-power player) 0.0
                   (jumping player) nil))))
        (t
         (let ((initial-vel 275)
               (holding-gravity -200)
               (jump-power 0.5))
           (when (and (key-down #\s) (can-jump player))
             (setf (can-jump player) nil
                   (jumping player) t
                   (velocity-y player) initial-vel
                   (jump-power player) jump-power))
           (when (and (key-down #\s) (jumping player))
             (setf (acceleration-y player) holding-gravity)
             (decf (jump-power player) dt))
           (when (or (key-up #\s) (< (jump-power player) 0.0))
             (setf (jumping player) nil))))))))

(defun move-player (pf dt)
  (let* ((rise-gravity -800.0)
         (fall-gravity -1200.0)
         (hz-max-vel 200.0))
    (with-struct (pf- player tmx keys) pf
      (flet ((key-down (key) (gethash key keys))
             (key-up   (key) (null (gethash key keys))))

        ;;use faster gravity for falling
        (if (> (velocity-y player) 0.0)
            (setf (acceleration-y player) rise-gravity)
            (setf (acceleration-y player) fall-gravity
                  (jump-power player) 0.0
                  (jumping player) nil))

        (maybe-jump-player pf dt)

        ;;horizontal motion
        (let ((tile (standing-on player)))
          (cond (tile ;;on the ground
                 (let* ((hz-accel-rate (hz-accel-rate-for-tile tile))
                        (hz-decel-rate (hz-decel-rate-for-tile tile)))
                   (when (key-down #\a)
                     (setf hz-max-vel (* 2.0 hz-max-vel)
                           hz-accel-rate (* 2.0 hz-accel-rate)))
                   (when (key-down :left)
                     (move-towards! (velocity-x player) (- hz-max-vel) hz-accel-rate dt))
                   (when (key-down :right)
                     (move-towards! (velocity-x player) hz-max-vel hz-accel-rate dt))
                   (when (and (key-up :left) (key-up :right))
                     (move-towards! (velocity-x player) 0.0 hz-decel-rate dt 5.0))))
                (t ;;floating
                 (let* ((hz-accel-rate 300.0)
                        (hz-floating-vel 200.0)
                        (hz-floating-decel-rate 50.0))
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
                                    0.0 hz-floating-decel-rate dt))))))
        (when (< (abs (velocity-x player)) 2.0)
          (setf (velocity-x player) 0.0))
        (when (key-down :left)
          (setf (flip-x player) t))
        (when (key-down :right)
          (setf (flip-x player) nil))
        (setf (can-jump player) nil)
        (setf (standing-on player) (update-sprite-physics pf player dt))
        (when (< (y player) 0.0)
          (setf (y player) 0.0
                (velocity-y player) 0.0))
        (clampf (x player) 0.0 15000)
        (clampf (y player) 0.0 1500)))))

(defun move-camera (pf dt)
  (declare (ignore dt))
  (with-struct (pf- root player background) pf
    (let* ((center-x (+ (- (x root)) 250))
           (right-edge (+ center-x 100))
           (left-edge (- center-x 100))
           (center-y (+ (- (y root)) 250))
           (top-edge (+ center-y 100))
           (bottom-edge (- center-y 50)))
      (cond
        ((> (x player) right-edge)
         (decf (x root) (- (x player) right-edge)))
        ((and t (< (x player) left-edge))
         (incf (x root) (- left-edge (x player)))))
      (when (> (x root) 0.0)
        (setf (x root) 0.0))
      (cond
        ((> (y player) top-edge)
         (decf (y root) (- (y player) top-edge)))
        ((and t (< (y player) bottom-edge))
         (incf (y root) (- bottom-edge (y player)))))
      (when (> (y root) 0.0)
        (setf (y root) 0.0))
      (setf (y background) (+ (- (y root)) 250))
      (setf (x background) (+ (- (x root)) 250)))))

(defun make-node-from-object-info (type initargs)
  (when-let* ((path (case type
                      (jewel "jewel.png")
                      (cat "throwcat.png")))
              (frame (get-frame path)))
    (apply #'make-instance 'sprite :sprite-frame frame initargs)))

(defmethod cl-user::contents-will-mount ((self pf) display)
  (declare (ignore display))
  (with-struct (pf- root tmx tmx-node player tile-table background) self
    (texture-packer-add-frames-from-file "./res/test.json")
    (add-animation 'pickle-walk (/ 1.0 7.5) '("pickle walk0.png" "pickle walk1.png"))
    (add-animation 'pickle-run (/ 1.0 15) '("pickle walk0.png" "pickle walk1.png"))
    (let* ((frame  (get-frame "pickle.png"))
           (objects nil))
      (setf root (make-instance 'node)
            background (make-instance 'image :x 250 :y 250
                                      :texture (get-texture "./res/platformer/sky.png"))
            tmx (render-buffer::tmx-renderer-from-file
                 "./res/platformer/dev.tmx")
            tmx-node (make-instance 'tmx-node
                                    :tmx tmx)
            player (make-instance 'player
                                  :x 80.0
                                  :acceleration-y -100.0
                                  :sprite-frame frame
                                  :state 'standing)
            tile-table (tile-lookup-table-from-tmx-renderer tmx))
      (let* ((map (render-buffer::tmx-renderer-map tmx))
             (layers (tmx-reader:map-layers map))
             (objects-layer (find :objects layers :key 'tmx-reader:layer-type))
             (data (tmx-reader:layer-data objects-layer)))
        (dolist (plist data)
          (let* ((tile (aref tile-table (getf plist :gid)))
                 (type (tile-type tile))
                 (initargs (rest (rest plist))))
            (when-let (object (make-node-from-object-info type initargs))
              (format t "built object for type: ~S ~A ~A ~%" type object (getf initargs :y))
              (push object objects)))))
      (setf (bottom player) 32.0)
      ;;should have a 'move-player-to-ground' function
      (setf (standing-on player)
            (update-sprite-physics self player (/ 1.0 60.0)))
      (add-child root background)
      (add-child root tmx-node)
      (dolist (sprite objects)
        (add-child root sprite))
      (add-child root player))))


(defmethod cl-user::step-contents ((self pf) dt)
  (with-struct (pf- root started player) self
    (unless started
      (setf started t)
      (on-enter root))
    (set-state player (update-state player self dt))
    (move-player self dt)
    (move-camera self dt)
    (visit root)))

(defmethod cl-user::handle-event ((self pf) event)
  (let ((info (cdr event)) (keys (pf-keys self)))
    (case (car event)
      (:keydown (setf (gethash info keys) t))
      (:keyup   (setf (gethash info keys) nil)))))

(cl-user::display-contents (make-pf) :width 500 :height 500
                           :expandable t
                           :preserve-aspect-ratio t)
