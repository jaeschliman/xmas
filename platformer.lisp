(defpackage :xmas.platformer (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object))
(in-package :xmas.platformer)

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

(defun make-output-string (&optional (length 0))
  (make-array length :element-type 'base-char :fill-pointer 0 :adjustable t))

(define-modify-macro clampf (min max) clamp)

(defstruct platformer
  started
  level
  (root (make-instance 'node))
  initial-level)

(defstruct level
  root
  player
  background
  tmx
  tmx-node
  tile-table
  object-manager
  name)

(defvar *runloop-bindings* (make-hash-table :test 'eq))

(defmacro defrunvar (name value)
  `(progn
     (defvar ,name)
     (setf (gethash ',name *runloop-bindings*) (lambda () ,value))))

(defrunvar *camera-x* 250.0)
(defrunvar *camera-y* 250.0)
(defrunvar *last-player-x* 0.0)
(defrunvar *last-player-y* 0.0)
(defrunvar *keys* (make-hash-table :test 'eql))
(defrunvar *just-pressed* (make-hash-table :test 'eql))
(defrunvar *font-22* (xmas.lfont-reader:lfont-from-file "./res/lfont/november.lfont"))
(defrunvar *jewel-count* 0)
(defrunvar *jewel-count-label* (make-output-string))
(defrunvar *next-level* nil)
(defrunvar *level-states* (make-hash-table :test 'equal))
(defrunvar *display-width* 0)
(defrunvar *display-height* 0)

(defmethod cl-user::runloop-bindings-alist ((self platformer))
  (let (result)
    (maphash (lambda (symbol fn)
               (push (cons symbol (funcall fn)) result))
             *runloop-bindings*)
    result))

(defun key-down (k) (gethash k *keys*))
(defun key-up (k) (null (key-down k)))
(defun just-pressed (k) (gethash k *just-pressed*))

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
  (let* ((props (xmas.tmx-renderer:tmx-renderer-tile-properties tmx))
         (result (make-array (length props))))
    (prog1 result
      (loop for plist across props
         for idx upfrom 0 do
           (setf (aref result idx) (tile-from-properties idx plist))))))

(defclass image (node)
  ((texture :accessor texture :initarg :texture)))

(defclass horizontal-scroller-image (image)
  ((speed :initarg :speed))
  (:default-initargs :speed 1.0))

(defclass physics ()
  ((velocity-x :accessor velocity-x :initarg :velocity-x)
   (velocity-y :accessor velocity-y :initarg :velocity-y)
   (acceleration-x :accessor acceleration-x :initarg :acceleration-x)
   (acceleration-y :accessor acceleration-y :initarg :acceleration-y))
  (:default-initargs
   :velocity-x 0.0 :velocity-y 0.0
   :acceleration-x 0.0 :acceleration-y 0.0))

(defclass player (physics sprite)
  ((can-jump :accessor can-jump :initform nil)
   (jump-power :accessor jump-power :initform 100.0)
   (jumping :accessor jumping :initform nil)
   (standing-on :accessor standing-on :initform nil)
   (state :accessor state :initform nil :initarg :state)))

(defclass jewel (game-sprite)
  ())

(defclass cat (game-sprite)
  ())

(defclass platform (game-sprite)
  ())

(defclass door (game-sprite)
  ((level :initarg :level)
   (marker :initarg :marker)))

(defmethod player-collision (player (jewel jewel))
  (let ((object (game-object jewel)))
    (remove-from-parent jewel)
    (setf (sprite object) nil)
    (incf *jewel-count*)
    (setf (fill-pointer *jewel-count-label*) 0)
    (with-output-to-string (s *jewel-count-label*)
      (format s "~S jewels" *jewel-count*))))

(defmethod player-collision (player (door door))
  (with-slots (level marker) door
    (when (and (just-pressed :up)
               (> (left player) (left door))
               (< (right player) (right door)))
      (format t "let's go to level: ~S : ~S !~%" level marker)
      ;; should really be called *next-room* at this point
      (setf *next-level* (list level :start-position marker)))))

(defmethod player-collision (player (platform platform))
  (when (and (< (velocity-y player) 0.0)
             (>= (bottom-for-y player *last-player-y*)
                 (top platform)))
    (setf (bottom player) (top platform)
          (velocity-y player) 0.0
          (standing-on player) platform
          (can-jump player) t)))

(defmethod wake-sprite ((cat cat) game-object)
  (declare (ignore game-object))
  (run-action cat (list (move-by 1.0 -20.0 0.0)
                        (move-by 1.0 20.0 0.0))
              :repeat :forever))

(defun should-sleep? (object manager left bottom right top)
  (declare (ignorable manager))
  (flet ((outside (obj) (or
                         (> (left obj) right)
                         (> (bottom obj) top)
                         (< (top obj) bottom)
                         (< (right obj) left))))
    (when (outside object)
      (if-let (sprite (sprite object))
        (when (outside sprite)
          t)
        t))))

(defun update-object-manager (level dt)
  (declare (ignorable dt))
  (with-struct (level- player object-manager) level
    (with-struct (game-object-manager- awake-objects object-qtree sprite-qtree) object-manager
      (let* ((offs-x (* *display-width* 0.75))
             (offs-y (* *display-height* 0.75))
             (x *camera-x*)
             (y *camera-y*)
             (left (- x offs-x))
             (bottom (- y offs-y))
             (right (+ x offs-x))
             (top (+ y offs-y))
             (wake (lambda (object) (wake object object-manager))))
        (declare (dynamic-extent wake))
        (qtree-query-collisions object-qtree left bottom right top wake)
        (loop with sleepers = nil
           for object across awake-objects do
             (when (should-sleep? object object-manager left bottom right top)
               (push object sleepers))
           finally
             (dolist (sleeper sleepers) (to-sleep sleeper object-manager)))
        (qtree-reset sprite-qtree
                     :x x :y y
                     :width (* 2.0 offs-x) :height (* 2.0 offs-y))
        (loop for object across awake-objects do
             (when-let (s (sprite object))
               (qtree-add sprite-qtree s)))))))

(defgeneric player-collision (player object)
  (:method (player object)
    (declare (ignore player object))))

(defun collide-player-with-objects (level dt)
  (declare (ignorable dt))
  (with-struct (level- player object-manager) level
    (with-struct (game-object-manager- sprite-qtree) object-manager
      (flet ((collide (object) (player-collision player object)))
        (qtree-query-collisions
         sprite-qtree (left player) (bottom player) (right player) (top player)
         #'collide)))))

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

(defgeneric draw (node))
(defmethod draw ((self node)))

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defmethod draw ((self image))
  (draw-node-color self)
  (xmas.draw:draw-texture (texture self)))

(defmethod draw ((self horizontal-scroller-image))
  (draw-node-color self)
  (let* ((texture (texture self))
         (width (texture-width texture))
         (height (texture-height texture))
         (speed (slot-value self 'speed))
         (offs-y (* height -0.5))
         (offs-x (+ (* width -0.5) (- width (mod (* speed (x self)) width)))))
    (xmas.draw:draw-texture-at texture (- offs-x width) offs-y width height)
    (xmas.draw:draw-texture-at texture offs-x offs-y width height)
    (xmas.draw:draw-texture-at texture (+ offs-x width) offs-y width height)))

(defmethod draw ((self sprite))
  (draw-node-color self)
  (let* ((frame (sprite-frame self))
         (frame-width (texture-frame-width frame))
         (frame-height (texture-frame-height frame))
         (width
          (if (texture-frame-rotated frame) frame-height frame-width))
         (height
          (if (texture-frame-rotated frame) frame-width frame-height))
         (offs-x (* 0.5 (- (content-width self) width)))
         (offs-y (* 0.5 (- (content-height self) height))))
    (xmas.draw:draw-texture-frame-at
     frame offs-x offs-y
     frame-width
     frame-height)))

(defmethod draw ((self tmx-node))
  (let* ((r (tmx self))
         (x (/ (xmas.tmx-renderer:tmx-renderer-width r) 2.0))
         (y (/ (xmas.tmx-renderer:tmx-renderer-height r) 2.0))
         (x-offs (* *display-width* 0.6))
         (y-offs (* *display-height* 0.6))
         (x1 (- *camera-x* x-offs))
         (x2 (+ *camera-x* x-offs))
         (y1 (- *camera-y* y-offs))
         (y2 (+ *camera-y* y-offs)))
    (xmas.tmx-renderer:draw-tmx-renderer-windowed
     x y r
     x1 y1
     x2 y2)))

(defmethod visit ((self node))
  (when (not (visible self))
    (return-from visit))
  (xmas.render-buffer::push-matrix)
  (let ((ax (anchor-x self))
        (ay (anchor-y self)))
    (if (or nil (and (zerop ax) (zerop ay)))
        (xmas.render-buffer::translate-scale-rotate
         (x self) (y self)
         (if (flip-x self) (* -1.0 (scale-x self)) (scale-x self))
         (if (flip-y self) (* -1.0 (scale-y self)) (scale-y self))
         (rotation self))
        (xmas.render-buffer::translate-scale-rotate-translate
         (x self) (y self)
         (if (flip-x self) (* -1.0 (scale-x self)) (scale-x self))
         (if (flip-y self) (* -1.0 (scale-y self)) (scale-y self))
         (rotation self)
         (* -1.0 ax (content-width self))
         (* -1.0 ay (content-height self)))))
  (draw self)
  (when (children self)
    (loop for child across (children self) do
         (visit child)))
  (xmas.render-buffer::pop-matrix))

(defun tile-at-point (tmx x y)
  (xmas.tmx-renderer:tmx-renderer-tile-at-point tmx x y))

(defmethod collide-with-tile (self side tile)
  (declare (ignore self side tile)))

(defun top-for-tile (sprite tile x y prev-y dt)
  (ecase (tile-shape tile)
    (slope-left  (+ 1.0 (mod x 32.0) (* 32.0 (floor y 32.0))))
    (slope-right (+ 1.0 (- 32.0 (mod x 32.0)) (* 32.0 (floor y 32.0))))
    (block (* 32.0 (+ 1.0 (floor y 32.0))))
    (platform
     (let* ((top (* 32.0 (+ 1.0 (floor y 32.0))))
            (vel (velocity-y sprite)))
       (cond
         ((and (<= vel 0.0)
               (>= prev-y top)
               (<= y top))
          top)
         (t y))))))

(defun bottom-for-tile (sprite tile x y dt)
  (declare (ignore x sprite))
  (case (tile-shape tile)
    (platform y)
    (t (1- (* 32.0 (+ 0.0 (floor y 32.0)))))))

(defun left-for-tile (sprite tile x y dt)
  (declare (ignore y sprite))
  (ecase (tile-shape tile)
    ((slope-left slope-right) x) ;;so we can get 'pushed up' to correct position
    (platform x)
    (block (+ -1.0 (* 32.0 (floor x 32.0))))))

(defun right-for-tile (sprite tile x y dt)
  (declare (ignore y sprite))
  (ecase (tile-shape tile)
    ((slope-left slope-right) x)
    (platform x)
    (block (* 32.0 (1+ (floor x 32.0))))))

(defun move-sprite-up-if-hitting-tiles-on-bottom  (level sprite prev-y dt)
  (with-struct (level- tmx tile-table) level
    (let ((y (bottom sprite)) (hit nil))
      (labels
          ((check (x)
             (let* ((gid (tile-at-point tmx x y))
                    (tile (aref tile-table gid)))
               (unless (null (tile-material tile))
                 ;; TODO: should really return a second value indicating whether
                 ;;       a hit was detected.
                 (let ((new-y (top-for-tile sprite tile x y prev-y dt)))
                   (unless (= new-y y)
                     (maxf y new-y)
                     (setf hit tile))))))
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
           (collide-with-tile sprite 'bottom hit)
           (return hit)))))))

(defun move-sprite-down-if-hitting-tiles-on-top (level sprite dt)
  (declare (ignore dt))
  (with-struct (level- tmx tile-table) level
    (let ((y (top sprite)) (hit nil))
      (labels
          ((check (x)
             (let* ((gid (tile-at-point tmx x y))
                    (tile (aref tile-table gid)))
               (unless (null (tile-material tile))
                 (minf y (bottom-for-tile sprite tile x y dt))
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
           (collide-with-tile sprite 'top hit)))))))

(defun move-sprite-left-if-hitting-tiles-on-right (level sprite dt)
  (declare (ignore dt))
  (with-struct (level- tmx tile-table) level
    (let ((x (right sprite)) (hit nil))
      (labels
          ((check (y)
             (let* ((gid (tile-at-point tmx x y))
                    (tile (aref tile-table gid)))
               (unless (null (tile-material tile))
                 (minf x (left-for-tile sprite tile x y dt))
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
           (collide-with-tile sprite 'right hit)))))))

(defun move-sprite-right-if-hitting-tiles-on-left (level sprite dt)
  (declare (ignore dt))
  (with-struct (level- tmx tile-table) level
    (let ((x (left sprite)) (hit nil))
      (labels ((check (y)
                 (let* ((gid (tile-at-point tmx x y))
                        (tile (aref tile-table gid)))
                   (unless (null (tile-material tile))
                     (maxf x (right-for-tile sprite tile x y dt))
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
           (collide-with-tile sprite 'left hit)))))))

(defun update-sprite-physics (level sprite dt)
  (let (standing-on)
    (incf (x sprite) (* dt (velocity-x sprite)))
    (if (> (velocity-x sprite) 0)
        (move-sprite-left-if-hitting-tiles-on-right level sprite dt)
        (move-sprite-right-if-hitting-tiles-on-left level sprite dt))

    (let ((prev-bottom (bottom sprite)))
      (incf (y sprite) (* dt (velocity-y sprite)))
      (setf standing-on 
            (if (> (velocity-y sprite) 0)
                (prog1 nil (move-sprite-down-if-hitting-tiles-on-top level sprite dt))
                (move-sprite-up-if-hitting-tiles-on-bottom level sprite prev-bottom dt))))
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
          ((block platform)
           (setf (velocity-y self) 0.0))))
       ((left right)
        (case (tile-shape tile)
          ;;do nothing
          ((slope-left slope-right))
          (block (setf (velocity-x self) 0))))
       (top
        (case (tile-shape tile)
          (platform ;;do nothing
           )
          ;;richochet
          (t
           (setf (velocity-y self) (* -0.5 (velocity-y self))
                 (jumping self) nil
                 (jump-power self) 0))))))))

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

(defun hz-accel-rate-for-material (material)
  (case material
    (brick 300.0)
    (ice   150.0)
    (t 300.0)))

(defun hz-decel-rate-for-material (material)
  (case material
    (brick (* 300.0 2.25))
    (ice   (* 300.0 0.05))
    (t     (* 300.0 2.25))))

(defun material-for-thing (it)
  (if (tile-p it)
      (tile-material it)
      'brick))

(defun update-state (player level dt)
  (declare (ignore dt))
  (with-struct (level- tmx) level
    (let ((state (state player))
          (left (key-down :left))
          (right (key-down :right))
          (fast (key-down #\a))
          (jump (key-down #\s))
          (just-pressed-jump (just-pressed #\s))
          (on-ground (standing-on player)))
      (cond
        (on-ground
         (cond
           (just-pressed-jump 'jumping)
           ((or left right) (if fast 'running 'walking))
           (t 'standing)))
        ((eq state 'jumping)
         (cond
           (jump (if (< (velocity-y player) 0.0) 'falling 'jumping))
           (t    'floating)))
        (t (if (< (velocity-y player) 0.0) 'falling 'floating))))))

(defun maybe-jump-player (level dt)
  (with-struct (level- player tmx) level
    (labels ((use-floating-jump  () nil))
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
           (when (and (just-pressed #\s) (can-jump player))
             (setf (can-jump player) nil
                   (jumping player) t
                   (jump-power player) jump-power)
             (maxf (velocity-y player) initial-vel))
           (when (and (key-down #\s) (jumping player))
             (setf (acceleration-y player) holding-gravity)
             (decf (jump-power player) dt))
           (when (or (key-up #\s) (< (jump-power player) 0.0))
             (setf (jumping player) nil))))))))

(defun move-player (level dt)
  (let* ((rise-gravity -800.0)
         (fall-gravity -1200.0)
         (hz-max-vel 200.0))
    (with-struct (level- player tmx) level
      (setf *last-player-x* (x player))
      (setf *last-player-y* (y player))

      ;;use faster gravity for falling
      (if (> (velocity-y player) 0.0)
          (setf (acceleration-y player) rise-gravity)
          (setf (acceleration-y player) fall-gravity
                (jump-power player) 0.0
                (jumping player) nil))

      (maybe-jump-player level dt)

      ;;horizontal motion
      (let ((thing (standing-on player)))
        (cond (thing ;;standing on something
               (let* ((material (material-for-thing thing))
                      (hz-accel-rate (hz-accel-rate-for-material material))
                      (hz-decel-rate (hz-decel-rate-for-material material)))
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

      (setf (standing-on player) (update-sprite-physics level player dt))

      (when (< (y player) 0.0)
        (setf (y player) 0.0
              (velocity-y player) 0.0))
      (clampf (x player) 0.0 15000)
      (clampf (y player) 0.0 1500))))

(defun update-sprite-velocity (sprite dt)
    (incf (velocity-x sprite) (* dt (acceleration-x sprite)))
    (clampf (velocity-x sprite) -1000 1000)
    (incf (velocity-y sprite) (* dt (acceleration-y sprite)))
    (clampf (velocity-y sprite) -1000 1000))

(defun move-camera (level dt)
  (declare (ignore dt))
  (with-struct (level- root player background) level
    (let ((w/2 (* 0.5 *display-width*))
          (h/2 (* 0.5 *display-height*)))
      (setf (x root) (- (- *camera-x* w/2))
            (y root) (- (- *camera-y* h/2)))
      (let* ((center-x *camera-x*)
             (right-edge (+ center-x 100))
             (left-edge (- center-x 100))
             (center-y *camera-y*)
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
        (setf *camera-x* (+ (- (x root)) w/2)
              *camera-y* (+ (- (y root)) h/2))
        (setf (x background) *camera-x*)
        (setf (y background) *camera-y*)))))

(defun make-node-from-object-info (type initargs)
  (when-let* ((path (case type
                      (jewel "jewel.png")
                      (cat "throwcat.png")
                      (door "door.png")
                      (platform "platform.png")))
              (frame (get-frame path)))
    (apply #'make-instance type :sprite-frame frame initargs)))

(defun make-game-object-from-object-info (type initargs)
  (when-let (sprite (make-node-from-object-info type initargs))
    (let ((obj (make-instance 'game-object
                :sprite sprite :x (getf initargs :x) :y (getf initargs :y))))
      (setf (game-object sprite) obj)
      obj)))

(defun load-game-objects-from-tmx (tmx object-manager tile-table)
  (let* ((map (xmas.tmx-renderer:tmx-renderer-map tmx))
         (layers (xmas.tmx-reader:map-layers map))
         (objects-layer (find :objects layers :key 'xmas.tmx-reader:layer-type))
         (data (xmas.tmx-reader:layer-data objects-layer)))
    (dolist (list data)
      (let ((object-type (car list))
            (plist (rest list)))
        (case object-type
          (:tile-sprite 
           (let* ((tile (aref tile-table (getf plist :gid)))
                  (type (tile-type tile))
                  (initargs (rest (rest plist))))
             (when-let (object (make-game-object-from-object-info type initargs))
               (game-object-manager-add-object object-manager object))))
          (:point
           (let* ((*read-eval* nil)
                  (name (read-from-string (getf plist :name))))
             (setf (gethash name (game-object-manager-points object-manager))
                   (list (getf plist :x) (getf plist :y)))))
          (t (warn "unhandled object type: ~S~%" object-type)))))))

(defmethod init-level ((self level) &key
                                      background-node
                                      tmx-file
                                      start-position
                                      game-object-manager)
  (with-struct (level- root tmx tmx-node player
                       tile-table background object-manager)
      self
    (let* ((frame (get-frame "pickle.png"))
           (already-loaded-objects (not (null game-object-manager))))
      (setf root (make-instance 'node)
            object-manager (or game-object-manager
                               (make-game-object-manager))
            (game-object-manager-sprite-node object-manager) root
            background background-node
            tmx (xmas.tmx-renderer:tmx-renderer-from-file tmx-file)
            tmx-node (make-instance 'tmx-node
                                    :tmx tmx)
            player (make-instance 'player
                                  :z-order 1
                                  :x 50.0
                                  :acceleration-y -100.0
                                  :sprite-frame frame
                                  :content-width 25.0
                                  :content-height 60.0
                                  :state 'standing)
            tile-table (tile-lookup-table-from-tmx-renderer tmx))
      (let* ((width (xmas.tmx-renderer:tmx-renderer-width tmx))
             (height (xmas.tmx-renderer:tmx-renderer-height tmx))
             (x (/ width 2.0))
             (y (/ height 2.0)))
        (unless already-loaded-objects
          (game-object-manager-set-active-area object-manager x y width height)
          (load-game-objects-from-tmx tmx object-manager tile-table)))
      (if-let (start (gethash start-position (game-object-manager-points object-manager)))
        (setf (x player) (first start)
              (bottom player) (second start))
        (progn
          (warn "starting position not found!")
          (setf (x player) 100.0)
          (setf (y player) 100.0)))

      ;;should have a 'move-player-to-ground' function
      (setf (standing-on player)
            (update-sprite-physics self player (/ 1.0 60.0)))
      (setf *camera-x* (+ (x player) 200)
            *camera-y*  (+ (top player) 300))
      (add-child root background)
      (add-child root tmx-node)
      (add-child root player))))

(defmethod update ((self level) dt)
  (with-struct (level- player object-manager ) self
    (set-state player (update-state player self dt))
    (move-player self dt)
    (update-object-manager self dt)
    (collide-player-with-objects self dt)
    (update-sprite-velocity player dt)
    (move-camera self dt)))

(defun get-level (name &key (start-position 'start))
  (when (stringp start-position)
    (setf start-position (symbolicate (string-upcase start-position))))
  (let ((level (make-level :name name))
        (manager (when (boundp '*level-states*)
                   (gethash name *level-states*))))
    (cond
      ((string= name "dev")
       (init-level level
                :background-node (make-instance
                                  'image :x 250 :y 250
                                  :texture (get-texture "./res/platformer/sky.png"))
                :tmx-file "./res/platformer/dev.tmx"
                :start-position start-position
                :game-object-manager manager))
      ((string= name "cave")
       (init-level level
                   :background-node
                   (make-instance
                    'horizontal-scroller-image
                    :x 250 :y 250
                    :speed 0.5
                    :texture (get-texture "./res/platformer/cave.png"))
                :tmx-file "./res/platformer/cave.tmx"
                :start-position start-position
                :game-object-manager manager))
      ((string= name "infinite")
       (init-level level
                :background-node (make-instance
                                  'image :x 250 :y 250
                                  :texture (get-texture "./res/platformer/sky.png"))
                :tmx-file "./res/platformer/infinite.tmx"
                :start-position start-position
                :game-object-manager manager)))
    level))


(defmethod cl-user::contents-will-mount ((self platformer) display)
  (setf *display-width*  (display-width display)
        *display-height* (display-height display))
  (setf (fill-pointer *jewel-count-label*) 0)
  (with-output-to-string (s *jewel-count-label*)
    (format s "~S jewels" *jewel-count*))
  (texture-packer-add-frames-from-file "./res/test.json")
  (add-animation 'pickle-walk (/ 1.0 7.5) '("pickle walk0.png" "pickle walk1.png"))
  (add-animation 'pickle-run (/ 1.0 15) '("pickle walk0.png" "pickle walk1.png"))
  (with-struct (platformer- level root initial-level)
      self
    (setf level (get-level initial-level))
    (add-child root (level-root level))))


(defmethod cl-user::step-contents ((self platformer) dt)
  (with-struct (platformer- started root level) self
    (unless started
      (setf started t)
      (on-enter root))
    (update level dt)
    (clrhash *just-pressed*)
    (visit root)
    (xmas.lfont-reader:lfont-draw-string *font-22* *jewel-count-label* 20.0 360.0)
    (when *next-level*
      (let ((mgr (level-object-manager level)))
        (setf (gethash (level-name level) *level-states*) mgr)
        (game-object-manager-sleep-all mgr))
      (remove-from-parent (level-root level))
      (setf level (apply #'get-level *next-level*))
      (add-child root (level-root level))
      (setf *next-level* nil))))

(defmethod cl-user::handle-event ((self platformer) event)
  (let ((info (cdr event)))
    (case (car event)
      (:keydown (unless (gethash info *keys*)
                  (setf (gethash info *just-pressed*) t))
                (setf (gethash info *keys*) t))
      (:keyup   (setf (gethash info *keys*) nil)))))

(cl-user::display-contents (make-platformer :initial-level "dev")
                           :width (* 1920 0.4)  ;;was: 890
                           :height (* 1080 0.4) ;;was: 500
                           :expandable t
                           :preserve-aspect-ratio t)
