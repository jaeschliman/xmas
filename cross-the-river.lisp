(defpackage :xmas.cross-the-river (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object :xmas.draw :xmas.lfont-reader))
(in-package :xmas.cross-the-river)

(defclass image (node)
  ((texture :accessor texture :initarg :texture))
  (:default-initargs :anchor-x 0.5 :anchor-y 0.5))

(defmethod initialize-instance ((self image) &key texture)
  (call-next-method)
  (setf (content-width self) (texture-width texture)
        (content-height self) (texture-height texture)))

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defmethod draw ((self image))
  (draw-node-color self)
  (let ((texture (texture self)))
    (draw-texture-at texture 0.0 0.0
                     (texture-width texture)
                     (texture-height texture))))

(defclass pickable-image (image)
  ((hovering :accessor hovering :initform nil)))

(defmethod draw ((self pickable-image))
  (when (hovering self)
    (xmas.render-buffer::set-color 1.0 1.0 1.0 1.0)
    (xmas.render-buffer::draw-rect 0.0 0.0 (content-width self) (content-height self)))
  (call-next-method))

(defclass repeater-node (node)
  ((texture :accessor texture :initarg :texture)))

(defclass wavy-node (node)
  ((texture :accessor texture :initarg :texture)
   (overlap :accessor overlap :initarg :overlap)
   (waves   :accessor waves   :initform nil)
   (boat    :reader   boat    :initarg :boat))
  (:default-initargs
   :overlap 0.8))

(defmethod initialize-instance ((self wavy-node) &key)
  (call-next-method)
  (let* ((tex (texture self))
         (tw (texture-width tex))
         (h (* (overlap self) (texture-height tex)))
         (count (ceiling (height self) h))
         (odd 1.0)
         (waves (make-array count :element-type t))
         (colors (circular-list (vector 0.25 0.5 1.0)
                       (vector 0.0 0.25 0.85)
                       (vector 0.0 0.8 0.85)))
         (xs (circular-list 0.0 (/ tw 2.0) (/ tw 3.0)))
         (delays (circular-list 1.0 0.0 1.0 1.3333))
         (scales (circular-list 0.95 1.05 1.05 0.95)))
    (setf (waves self) waves)
    (setf (parent (boat self)) self)
    (dotimes (i count)
      (labels ((tint (duration color)
                 (tint-to duration (svref color 0) (svref color 1) (svref color 2)))
               (make-node ()
                 (let* ((diff (abs (- (* 0.5 count) i)))
                        (pct  (/ (- (* 0.5 count) diff) (* 0.5 count)))
                        (ipct (- 1.0 pct))
                        (y (* h i))
                        ;; an attempt to make them bunch up in the center
                        ;; but it didn't end up looking so great
                        ;; (y (+ (* pct h count 0.5) (* ipct h i)))
                        (x (- (+ tw (pop xs) (random tw))))
                        (color (copy-seq (pop colors)))
                        (node (make-instance 'repeater-node
                                             :texture tex
                                             :content-width (+ (width self) (* 4.0 tw))
                                             :content-height (texture-height tex)
                                             :x x
                                             :y y
                                             :opacity 0.9
                                             :color color
                                             :z-order (coerce (- count i) 'single-float)))
                        (h-amt tw)
                        (v-amt (+ 3.0 (* pct 6.0)))
                        (v-dur (* 0.5 (+ 0.5 (random 0.5))))
                        (h-dur (+ (random 0.25) 0.25 (* ipct 1.0)))
                        ;; (wave-dur (+ 0.5 (* ipct 2.5)))
                        (wave-dur 3.0))
                   (run-action node (list (move-by-x h-dur h-amt)
                                          (callfunc (lambda () (setf (x node) x))))
                               :repeat :forever)
                   (run-action node (list (move-by-y v-dur (* v-amt odd -1)
                                                     :ease :in-out-sine)
                                          (move-by-y v-dur (* v-amt odd)
                                                     :ease :in-out-sine))
                               :repeat :forever)
                   (run-action node (list (scale-x-to wave-dur (pop scales)
                                                      :ease :in-out-sine)
                                          (scale-x-to wave-dur (pop scales)
                                                      :ease :in-out-sine))
                                   :repeat :forever)
                   (let ((dur 3.0))
                     (run-action node (list (delay (pop delays))
                                            (tint dur (pop colors))
                                            (delay (pop delays))
                                            (tint dur (pop colors))
                                            (delay (pop delays))
                                            (tint dur (pop colors)))
                                 :repeat :forever))
                   node)))
        (setf (aref waves i) (make-node))
        (setf odd (* -1.0 odd))))))

(defmethod on-enter ((self wavy-node))
  (call-next-method)
  (map nil #'on-enter (waves self))
  (on-enter (boat self)))

(defmethod on-exit ((self wavy-node))
  (call-next-method)
  (map nil #'on-exit (waves self))
  (on-exit (boat self)))

(defmethod draw ((self repeater-node))
  (let* ((texture (texture self))
         (tw (texture-width texture))
         (u1 (coerce (/ (width self) tw) 'float)))
    (draw-node-color self)
    (draw-texture-at-tex-coords texture 0.0 0.0
                                (width self) (height self)
                                0.0 0.01 u1 1.0)))

(defmethod draw ((self wavy-node))
  (loop
     with boat       = (boat self)
     with boat-y     = (y boat)
     with boat-drawn = nil
     with waves      = (waves self)
     with offs       = 10
     for i from (1- (length waves)) downto 0
     for wave = (aref waves i)
     do
       (visit wave)
       (unless boat-drawn
         (when (> boat-y (+ (y wave) offs))
           (visit boat)
           (setf boat-drawn t)))))

(defclass positioned (node)
  ((place :accessor place :initarg :place)
   (north-position :initarg :north-position)
   (south-position :initarg :south-position)) )

(defun get-position-for-place (positioned place)
  (let* ((slot (ecase place
                 (north 'north-position)
                 (south 'south-position)))
         (posn (slot-value positioned slot)))
    (values (getf posn :x) (getf posn :y))))

(defun update-position (self)
  (multiple-value-bind (x y) (get-position-for-place self (place self))
    (setf (x self) x (y self) y)))

(defmethod initialize-instance ((self positioned) &key)
  (call-next-method)
  (update-position self))

(defclass boat (image positioned)
  ((item :accessor item :initform nil)))

(defclass possesion (pickable-image positioned)
  ())

(defun try-swap-places (ctr item)
  (with-slots (boat root) ctr
    (if (eq (place item) 'boat)
        (let* ((shore (place boat)))
          (add-child root item)
          (setf (item boat) nil
                (place item) shore)
          (update-position item))
        (when (and (null (item boat)) (eq (place item) (place boat)))
           (add-child boat item)
            (setf (x item) 20.0
                  (y item) 10.0
                  (place item) 'boat
                  (item boat) item)))))

(defun new-mode (ctr)
  (with-slots (boat mode goat cabbage wolf) ctr
    (flet ((eating? (a b)
             (and (eq (place a) (place b))
                  (not (eq (place a) (place boat))))))
      (cond ((eq mode 'play)
             (cond ((and (eq (place goat) 'south)
                         (eq (place cabbage) 'south)
                         (eq (place wolf) 'south))
                    'win)
                   ((eating? goat cabbage) 'lose-goat)
                   ((eating? wolf goat) 'lose-wolf)))))))

(defun enter-mode (ctr new-mode)
  (with-slots (root boat mode goat cabbage wolf) ctr
    (case mode
      ((win lose-goat lose-wolf)
       (setf (visible root) t)
       (when (eq new-mode 'play)
         (when-let (item (item boat))
           (add-child root item)
           (setf (item boat) nil))
         (setf (place boat) 'north
               (place goat) 'north
               (place cabbage) 'north
               (place wolf) 'north)
         (map nil 'update-position (list boat goat cabbage wolf))))
      (play
       (case new-mode
        ((win lose-goat lose-wolf) (setf (visible root) nil)))))
    (setf mode new-mode)))

(xmas.deftest:deftest cross-the-river (:width 500 :height 500 :expandable t :preserve-aspect-ratio t)
  :tags sketch
  :init
  (set-default-texture-directory "./res/cross-the-river/")
  started := nil
  mode := 'play
  mouse-x := -1.0
  mouse-y := -1.0
  mouse-clicked := nil
  november  := (xmas.ttf-font:make-font :font "./res/ttf/november.ttf" :size 22)
  tex := (get-texture "wave.png" :wrap :repeat)
  root := (make-instance 'node)
  boat := (make-instance 'boat
                         :anchor-y 0.0
                         :x 150.0 :y 50.0
                         :north-position '(:x 250.0 :y 250.0)
                         :south-position '(:x 250.0 :y 20.0)
                         :texture (get-texture "boat.png")
                         :place 'north)
  north-shore := (make-instance 'image
                                :anchor-x 0.0
                                :anchor-y 1.0
                                :y 500.0
                                :texture (get-texture "north-shore.png"))
  node := (make-instance 'wavy-node
                         :texture tex
                         :content-width 500.0
                         :content-height 250.0
                         :boat boat
                         :y 100.0
                         :overlap (* 0.8 0.4 0.8))
  south-shore := (make-instance 'image
                                :anchor-x 0.0
                                :anchor-y 0.0
                                :texture (get-texture "south-shore.png"))
  wolf := (make-instance 'possesion
                         :place 'north
                         :scale-x 0.4 :scale-y 0.4
                         :anchor-y 0.0
                         :north-position '(:x 150.0 :y 400.0)
                         :south-position '(:x 150.0 :y 100.0)
                         :texture (get-texture "wolf.png"))
  goat := (make-instance 'possesion
                         :place 'north
                         :scale-x 0.4 :scale-y 0.4
                         :anchor-y 0.0
                         :north-position '(:x 250.0 :y 400.0)
                         :south-position '(:x 250.0 :y 100.0)
                         :flip-x t
                         :texture (get-texture "goat.png"))
  cabbage := (make-instance 'possesion
                            :place 'north
                            :scale-x 0.25 :scale-y 0.25
                            :anchor-y 0.0
                            :north-position '(:x 350.0 :y 400.0)
                            :south-position '(:x 350.0 :y 100.0)
                            :texture (get-texture "cabbage.png"))
  (add-children root (list north-shore node south-shore wolf goat cabbage))
  (run-action boat (list (rotate-by 0.5 -5.0)
                         (rotate-by 1.0 10.0)
                         (rotate-by 0.5 -5.0))
              :repeat :forever)
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (case mode
    (play
     (flet ((hover-or-click (item)
              (let ((over (node-contains-world-point-p item mouse-x mouse-y)))
                (if (and over mouse-clicked)
                    (try-swap-places self item)
                    (setf (hovering item) over))) ))
       (hover-or-click wolf)
       (hover-or-click goat)
       (hover-or-click cabbage)))
    (t
     (setf (hovering goat) nil
           (hovering cabbage) nil
           (hovering wolf) nil)))
  (when-let (new-mode (new-mode self))
    (unless (eq mode new-mode)
      (enter-mode self new-mode)))
  (visit root)
  (case mode
    (lose-goat
     (xmas.render-buffer::set-color 1.0 0.0 0.0 1.0)
     (lfont-draw-string november "The goat ate the cabbage!" 85 250))
    (lose-wolf
     (xmas.render-buffer::set-color 1.0 0.0 0.0 1.0)
     (lfont-draw-string november "The wolf ate the goat!" 100 250))
    (win
     (xmas.render-buffer::set-color 1.0 1.0 1.0 1.0)
     (lfont-draw-string november "You win!" 200 250)))
  (setf mouse-clicked nil)
  :on-event
  (case (car event)
    (:mousemove (setf mouse-x (cadr event)
                      mouse-y (cddr event)))
    (:click (setf mouse-clicked t))
    (:keypress
     (case mode
       (play
        (let ((shore (if (eq (place boat) 'north) 'south 'north)))
          (multiple-value-bind (x y) (get-position-for-place boat shore)
            (enter-mode self 'animation)
            (run-action boat
                        (list
                         (move-to 3.0 x y)
                         (callfunc (lambda ()
                                     (setf (place boat) shore)
                                     (enter-mode self 'play))))))))
       ((win lose-goat lose-wolf)
        (enter-mode self 'play))))))

(xmas.deftest:run-test 'cross-the-river)
