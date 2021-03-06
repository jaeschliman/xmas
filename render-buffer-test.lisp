(defpackage :xmas.render-buffer-tests (:use :cl :alexandria
                                            :xmas.action
                                            :xmas.animation-manager
                                            :xmas.display
                                            :xmas.draw
                                            :xmas.lfont-reader
                                            :xmas.matrix
                                            :xmas.matrix-stack
                                            :xmas.node
                                            :xmas.qtree
                                            :xmas.sprite
                                            :xmas.texture
                                            :xmas.texture-packer
                                            :xmas.tmx-reader
                                            :xmas.tmx-renderer
                                            :xmas.deftest))
(in-package :xmas.render-buffer-tests)

(defmacro with-struct ((prefix &rest slots) var &body body)
  (once-only (var)
    `(symbol-macrolet
         ,(loop for slot in slots collect
               (list slot (list (symbolicate prefix slot) var)))
       ,@body)))

(define-modify-macro clampf (min max) clamp)

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
    (xmas.render-buffer::set-color r g b a)
    (xmas.render-buffer::draw-rect x y 20 20)))

(deftest bouncy-balls (:width 500 :height 500 :expandable t)
  :tags draw-rect window events draw-heavy
  :init
  width  := (display-width display)
  height := (display-height display)
  boxes  := (loop repeat 4000 collect (make-bouncy-box))
  (dolist (box boxes)
    (setf (bouncy-box-x box) (random (- width 20))
          (bouncy-box-y box) (random (- height 20))))
  :update
  (let ((maxx (- width 20))
        (maxy (- height 20)))
    (dolist (box boxes)
      (update-bouncing-box box maxx maxy dt)))
  (map nil 'draw-bouncing-box boxes)
  :on-event
  (case (car event)
    (:resize
     (let ((w (cadr event))
           (h (cddr event)))
       (setf width w
             height h)))
    (t (format t "got unhandled event: ~S~%" event))))

(deftest draw-texture ()
  :tags texture load-time draw-texture
  :init
  alien := (get-texture #P "./alien.png")
  (assert (texture-width alien))
  (assert (texture-height alien))
  :update
  (draw-texture alien))

(defstruct simple-sprite
  (x 0.0)
  (y 0.0)
  (sx 1.0)
  (sy 1.0)
  (r 0.0)
  (texture nil)
  (dx 0.0)
  (dy 0.0)
  (dsx 0.0)
  (dsy 0.0)
  (dr 0.0))

(defun update-sprite (sprite maxx maxy dt)
  (declare (type sprite sprite))
  (with-slots (x y dx dy r dr) sprite
    (incf x (* dx dt))
    (incf y (* dy dt))
    (incf r (* dr dt))
    (setf r (mod r 360.0))
    (cond ((< x 0) (setf x 0 dx (* -1 dx)))
          ((> x maxx) (setf x maxx dx (* -1 dx))))
    (cond ((< y 0) (setf y 0 dy (* -1 dy)))
          ((> y maxy) (setf y maxy dy (* -1 dy))))))

(defun draw-sprite (sprite)
  (xmas.render-buffer::push-matrix)
  (xmas.render-buffer::translate-scale-rotate
   (simple-sprite-x sprite) (simple-sprite-y sprite)
   (simple-sprite-sx sprite) (simple-sprite-sy sprite)
   (simple-sprite-r sprite))
  (xmas.render-buffer::set-color 1.0 1.0 1.0 1.0)
  (draw-texture (simple-sprite-texture sprite))
  (xmas.render-buffer::pop-matrix))

(deftest draw-many-textures (:width 500 :height 500)
  :tags draw-heavy texture
  :init
  width   := (display-width display)
  height  := (display-height display)
  texture := (get-texture #P"./alien.png")
  sprites := (loop repeat 4000 collect
               (make-simple-sprite :x (random width)
                                   :y (random height)
                                   :dx (- (random 100) 50)
                                   :dy (- (random 100) 50)
                                   :r (random 360)
                                   :dr (- (random 100) 50)
                                   :sx 0.5
                                   :sy 0.5
                                   :texture texture))
  :update
  (dolist (sprite sprites)
    (update-sprite sprite width height dt))
  (dolist (sprite sprites)
    (draw-sprite sprite)))

(deftest matrix-translation-rotation ()
  :tags draw-rect matrix mult-matrix
  :init
  matrix := (make-matrix)
  scratch := (make-matrix)
  (into-matrix (matrix)
    (load-identity)
    (translate 125.0 125.0))
  :update
  (let ((*tmp-matrix* scratch))
    (into-matrix (matrix)
      (rotate (* 40 dt))))
  (xmas.render-buffer::set-color 0.0 1.0 0.0 0.5)
  (xmas.render-buffer::draw-rect 10.0 10.0 20.0 20.0)
  (xmas.render-buffer::draw-rect 115.0 115.0 20.0 20.0)
  (xmas.render-buffer::push-matrix)
  (xmas.render-buffer::mult-matrix (unwrap-matrix matrix))
  (xmas.render-buffer::draw-rect -10.0 -10.0 20.0 20.0)
  (xmas.render-buffer::draw-rect 10.0 10.0 20.0 20.0)
  (xmas.render-buffer::draw-rect 30.0 30.0 20.0 20.0)
  (xmas.render-buffer::pop-matrix))

(defun draw-node-color (node)
  (let ((c (color node)) (a (opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defclass debug-node (node)
  ())

(defclass rect (node)
  ()
  (:default-initargs :anchor-x 0.5 :anchor-y 0.5))

(defmethod draw ((self rect))
  (draw-node-color self)
  (xmas.render-buffer::draw-rect 0.0 0.0 (width self) (height self)))

(defmethod draw ((self debug-node))
  (draw-node-color self)
  (xmas.render-buffer::draw-rect -20.0 -20.0 40.0 40.0))

(deftest visit-and-draw-many-nodes (:width 500 :height 500)
  :tags node draw-heavy add-child rotation
  :init
  width     := (display-width display)
  height    := (display-height display)
  diagonal  := (sqrt (+ (* width width) (* height height)))
  d/2       := (/ diagonal 2)
  root-node := (make-instance 'node :x (/ width 2.0) :y (/ height 2.0))
  nodes     := (loop repeat 4000 collect
                    (make-instance 'debug-node
                                   :x (coerce (- (random diagonal) d/2) 'float)
                                   :y (coerce (- (random diagonal) d/2)'float)
                                   :rotation (coerce (random 360) 'float)
                                   :color (vector
                                           (/ (random 100) 100.0)
                                           (/ (random 100) 100.0)
                                           (/ (random 100) 100.0))
                                   :opacity (/ (random 100) 100.0)))
  (add-children root-node nodes)
  :update
  (dolist (node (cons root-node nodes))
    (let ((r (rotation node)))
      (incf r (* dt 100))
      (setf r (mod r 360))
      (setf (rotation node) r)))
  (xmas.render-buffer::set-color 0.0 1.0 1.0 0.4)
  (visit root-node))

;; (run-test 'visit-and-draw-many-nodes)

(deftest node-actions ()
  :tags node actions run-action repeat easing callfunc remove-from-parent
  :init
  started := nil
  node := (make-instance 'debug-node
                         :x (/ (display-width display) 2)
                         :y (/ (display-height display) 2))
  node2 := (make-instance 'debug-node
                          :color (vector 0.0 1.0 1.0)
                          :opacity 0.4
                          :x 20.0
                          :y 20.0)
  node3 := (make-instance 'debug-node
                          :color (vector 1.0 0.0 0.0)
                          :opacity 0.4
                          :x -20.0
                          :y -20.0)
  node4 := (make-instance 'debug-node
                          :color (vector 0.0 1.0 0.0)
                          :opacity 0.4
                          :x  20.0
                          :y -20.0)
  node5 := (make-instance 'debug-node
                          :color (vector 1.0 1.0 0.0)
                          :opacity 0.4
                          :x -20.0
                          :y  20.0)
  (run-action node (list (delay 1.0)
                         (rotate-by 1.5 180.0 :ease :in-out-quad)
                         (delay 1.0)
                         (rotate-by 1.25 -90.0 :ease :in-out-sine))
              :repeat :forever)
  (run-action node2 (rotate-by 2.5 -360)
              :repeat :forever)
  (run-action node3 (list (rotate-by 0.25 -60.0) (rotate-by 0.25 60.0))
              :repeat :forever)
  (run-action node4 (list (rotate-by 0.5 -60.0)
                          (rotate-by 0.25 -60.0)
                          (rotate-by 0.25 60.0))
              :repeat :forever)
  (run-action node5 (list (rotate-by 0.5 -60.0)
                          (rotate-by 0.5 60.0)
                          (rotate-by 0.5 -360.0)
                          (rotate-by 0.5 360.0))
              :repeat :forever)
  (add-child node node2)
  (add-child node node3)
  (add-child node node4)
  (add-child node node5)
  (run-action node (list
                    (callfunc (lambda () (format t " tick! ")))
                    (delay 2.0))
              :repeat :forever)
  (run-action node (list
                    (delay 1.0)
                    (callfunc (lambda () (format t " tock! ")))
                    (delay 1.0))
              :repeat :forever)
  (run-action node (list
                    (delay 3.0)
                    (callfunc (lambda () (remove-from-parent node2)))))
  :update
  (unless started
    (setf started t)
    (on-enter node))
  (visit node))

;; (run-test 'node-actions)

(deftest action-manager-test ()
  :tags node actions action-manager
  :init
  started := nil
  root := (make-instance 'debug-node
                         :x (/ (display-width display) 2)
                         :y (/ (display-height display) 2))
  (labels ((add-node (&aux (node (make-instance 'debug-node)))
             (run-action
              node
              (list (move-by 3.0 -250 -250)
                    (callfunc (lambda () (remove-from-parent node)))))
             (run-action
              root
              (list (delay 4.0)
                    (callfunc
                     (lambda ()
                       (add-child root node)
                       (add-node)))))))
    (add-node))
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (visit root))

;; (run-test 'action-manager-test)

(deftest texture-frame-test (:width 500 :height 500)
  :tags texture-frame edges
  :init
  tex := (get-texture "./bayarea.png")
  a := (texture-frame tex 0.0 0.0 250.0 250.0)
  b := (texture-frame tex 0.0 250.0 250.0 250.0)
  c := (texture-frame tex 250.0 0.0 250.0 250.0)
  d := (texture-frame tex 250.0 250.0 250.0 250.0)
  :update
  (xmas.render-buffer::push-matrix)
  (draw-texture-frame a 125.0 125.0)
  (draw-texture-frame b 125.0 (+ 250.0 125.0))
  (draw-texture-frame c (+ 250.0 125.0) 125.0)
  (draw-texture-frame d (+ 250.0 125.0) (+ 250.0 125.0))
  (xmas.render-buffer::pop-matrix))

;; (run-test 'texture-frame-test)

(deftest texture-packer-test (:width 500 :height 500)
  :tags texture-packer texture texture-frame file-format
  :init
  packer := (texture-packer-from-file "./res/test.json")
  (assert (xmas.texture-packer::texture-packer-file-texture packer))
  normal-frame := (texture-packer-get-frame packer "pickle.png")
  blink-frame  := (texture-packer-get-frame packer "pickle blink.png")
  jewel        := (texture-packer-get-frame packer "jewel.png")
  :update
  (draw-texture-frame normal-frame 125.0 250.0)
  (draw-texture-frame blink-frame 375.0 250.0)
  (draw-texture-frame jewel 250.0 250.0))

;; (run-test 'texture-packer-test)

(deftest sprite-frame-test (:width 500 :height 500)
  :tags texture-packer texture texture-frame file-format sprite
  :init
  packer := (texture-packer-from-file "./res/test.json")
  (assert (xmas.texture-packer::texture-packer-file-texture packer))
  normal-frame := (texture-packer-get-frame packer "pickle.png")
  blink-frame  := (texture-packer-get-frame packer "pickle blink.png")
  jewel-frame  := (texture-packer-get-frame packer "jewel.png")
  a := (make-instance 'sprite :x 125.0 :y 150.0 :sprite-frame normal-frame)
  b := (make-instance 'sprite :x 375.0 :y 150.0 :sprite-frame blink-frame)
  c := (make-instance 'sprite :x 250.0 :y 150.0 :sprite-frame jewel-frame)
  :update
  (draw-texture-frame normal-frame 125.0 250.0)
  (draw-texture-frame blink-frame 375.0 250.0)
  (draw-texture-frame jewel-frame 250.0 250.0)
  (visit a)
  (visit b)
  (visit c))

;; (run-test 'sprite-frame-test)

(deftest tilemap-test0 (:width 500 :height 500)
  :tags tmx file-format
  :init
  map     := (read-tilemap "./res/test-tilemap.tmx")
  tileset := (first (map-tilesets map))
  frames  := (make-tileset-texture-frames tileset)
  :update
  (loop
     with x = 100.0 with y = 250.0
     for frame across frames
     for i upfrom 0
     when frame
     do (draw-texture-frame frame (+ x (* i 50.0)) y)))

;; (run-test 'tilemap-test0)

(deftest tilemap-test-1 (:width 500 :height 500)
  :tags tmx file-format draw-tmx-layer
  :init
  map     := (read-tilemap "./res/test-tilemap.tmx")
  tileset := (first (map-tilesets map))
  frames  := (make-tileset-texture-frames tileset)
  layer   := (first (map-layers map))
  :update
  (draw-tmx-layer 250.0 250.0
     (tileset-tile-width tileset)
     (tileset-tile-height tileset)
     layer
     frames))

;; (run-test 'tilemap-test-1)

(deftest tmx-renderer-test (:width 500 :height 500)
  :tags tmx file-format tmx-renderer
  :init
  r := (tmx-renderer-from-file "./res/test-tilemap.tmx")
  :update
  (let* ((x (/ (tmx-renderer-width r) 2.0))
         (y (/ (tmx-renderer-height r) 2.0)))
    (draw-tmx-renderer x y r)))

;; (run-test 'tmx-renderer-test)

(deftest action-tags (:width 500 :height 500)
  :tags node action-manager actions action-tags
  :init
  started := nil
  n := (make-instance 'debug-node :x 250.0 :y 250.0)
  (run-action n (list (move-by 3.0 -100.0 0.0)
                      (move-by 3.0 100.0 0.0))
              :repeat :forever :tag 'moving)
  (run-action n (rotate-by 5.0 360.0)
              :repeat :forever :tag 'rotating)
  (run-action n (list (delay 3.0)
                      (callfunc (lambda ()
                                  (stop-all-actions n :tag 'moving)))))
  :update
  (unless started
    (setf started t)
    (on-enter n))
  (visit n))

;; (run-test 'action-tags)

(deftest animation (:width 500 :height 500)
  :tags file-format texture-packer animation anchor-point
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  (add-animation 'cat (/ 1.0 7.5) '("catwalk0.png" "catwalk1.png"))
  started := nil
  sprite := (make-instance 'sprite
                           :x 250.0
                           :y 250.0
                           :scale-x 2.0
                           :scale-y 2.0
                           :sprite-frame (get-frame "pickle.png")
                           :anchor-x 0.0
                           :anchor-y 0.0)
  (run-animation sprite 'cat :repeat :forever)
  (setf (rotation sprite) -15.0)
  :update
  (unless started
    (setf started t)
    (on-enter sprite))
  (visit sprite))

;; (run-test 'animation)

(defclass box (rect)
  ((vx :accessor box-vx :initarg :vx)
   (vy :accessor box-vy :initarg :vy)))

(defun draw-marker (rect)
  (xmas.render-buffer::draw-rect (x rect)
                                 (y rect)
                                 5.0 5.0))

(deftest quadtree (:width 500 :height 500)
  :tags mouse quadtree draw-rect
  :init
  started := nil
  qtree   := (qtree)
  mouse-x := 0.0
  mouse-y := 0.0
  root    := (make-instance 'node)
  nodes   := (loop repeat 100 collect
                (let ((n (make-instance
                          'box
                          :content-width 20.0
                          :content-height 20.0
                          :opacity 0.5
                          :x (random 500.0)
                          :y (random 500.0)
                          :vx (- (/ (random 100) 100.0) 0.5)
                          :vy (- (/ (random 100) 100.0) 0.5))))
                  (prog1 n
                    (add-child root n))))
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (qtree-reset qtree :x 250 :y 250 :width 500 :height 500)
  (dolist (box nodes)
    (incf (x box) (* 100 dt (box-vx box)))
    (incf (y box) (* 100 dt (box-vy box)))
    (when (or (< (x box) 0.0) (> (x box) 500.0))
      (setf (box-vx box) (* -1.0 (box-vx box)))
      (clampf (x box) 0.0 500.0))
    (when (or (< (y box) 0.0) (> (y box) 500.0))
      (setf (box-vy box) (* -1.0 (box-vy box)))
      (clampf (y box) 0.0 500.0))
    (qtree-add qtree box))
  (xmas.render-buffer::set-color 1.0 1.0 1.0 0.1)
  (qtree-map-nodes
   qtree
   (lambda (x y w h items)
     (declare (ignore items))
     (xmas.render-buffer::draw-rect
      (- x (/ w 2.0)) (- y (/ h 2.0)) (- w 4.0) (- h 4.0))))
  (visit root)
  (xmas.render-buffer::set-color 1.0 0.0 0.0 1)
  (xmas.render-buffer::draw-rect mouse-x mouse-y 5.0 5.0)
  (let ((x mouse-x) (y mouse-y))
    (xmas.render-buffer::set-color 0.0 1.0 0.0 1)
    (qtree-query qtree x y (+ x 5) (+ y 5) 'draw-marker)
    (xmas.render-buffer::set-color 0.0 0.0 1.0 1)
    (qtree-query-collisions
     qtree x y (+ x 5) (+ y 5) 'draw-marker))
  :on-event
  (when (eq (car event) :mousemove)
    (setf mouse-x (cadr event)
          mouse-y (cddr event))))

;; (run-test 'quadtree)

(deftest text-rendering (:width 500 :height 500)
  :tags text font file-format lfont
  :init
  november  := (lfont-from-file "./res/lfont/november.lfont")
  open-sans := (lfont-from-file "./res/lfont/OpenSans-Light.lfont")
  :update
  (lfont-draw-string november "Hello, world!" 50.0 50.0)
  (lfont-draw-string open-sans
                     "The quick brown fox jumped over the lazy dog."
                     10.0 150.0 :letter-spacing 0.0))

;; (run-test 'text-rendering)

(deftest anchor-point-test-0 (:width 500 :height 500)
  :tags node anchor-point
  :init
  root    := (make-instance 'node)
  started := nil
  (flet ((make-rect (&optional (ax (/ (random 150) 100.0))
                               (ay (/ (random 150) 100.0)))
           (let ((r (make-instance
                     'rect
                     :content-width 40.0
                     :content-height 40.0
                     :anchor-x ax
                     :anchor-y ay
                     :x 250.0
                     :y 250.0
                     :opacity 0.5)))
             (prog1 r
               (add-child root r)
               (run-action r (list (delay 1.0)
                                   (rotate-by 3.0 360))
                           :repeat :forever)))))
    (let ((a (make-rect)) (b (make-rect))
          (c (make-rect)) (d (make-rect))
          (e (make-rect 0.0 0.0))
          (f (make-rect 0.5 0.5))
          (top 450.0) (left 50.0)
          (bottom 50.0) (right 450.0))
      ;; a b
      ;; c d
      (setf
       (left a) left
       (left c) left
       (top a) top
       (top b) top
       (right b) right
       (right d) right
       (bottom d) bottom
       (bottom c) bottom
       (x e) 250.0
       (y e) 250.0
       (x f) 250.0
       (y f) 250.0
       (rotation f) 0.0)))
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (visit root))

;; (run-test 'anchor-point-test-0)

(deftest anchor-point-test-1 (:width 500 :height 500)
  :tags sprite node anchor-point content-size
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  sprite1 := (make-instance 'sprite :x 150.0 :y 250.0
                            :sprite-frame (get-frame "pickle.png"))
  sprite2 := (make-instance 'sprite :x 250.0 :y 250.0
                            :content-width 40.0
                            :content-height 60.0
                            :sprite-frame (get-frame "pickle.png"))
  sprite3 := (make-instance 'sprite :x 400.0 :y 250.0
                            :content-width 100.0
                            :content-height 150.0
                            :sprite-frame (get-frame "pickle.png"))
  :update
 (flet ((draw-it (s)
           (xmas.render-buffer::set-color 1.0 1.0 1.0 1.0)
           (xmas.render-buffer::draw-rect (left s) (bottom s) (width s) (height s))
           (visit s)))
    (draw-it sprite1)
    (draw-it sprite2)
    (draw-it sprite3)))

;; (run-test 'anchor-point-test-1)

(deftest texture-wrap-0 (:width 500 :height 500)
  :tags texture drawing texture-wrapping
  :init
  tex := (get-texture "./alien.png" :wrap :repeat)
  :update
  (draw-texture-at-tex-coords tex 0 0 500 500 0.0 0.0 4.0 4.0))

;; (run-test 'texture-wrap-0)

(deftest tint-to-test (:width 500 :height 500)
  :tags actions tint-to scale-x-to
  :init
  node := (make-instance 'debug-node :x 250.0 :y 250.0 :color (vector 1.0 0.0 0.0))
  started := nil
  (run-action node (list
                    (tint-to 1.0 0.0 1.0 0.0)
                    (tint-to 1.0 0.0 0.0 1.0)
                    (tint-to 1.0 1.0 0.0 0.0))
              :repeat :forever)
  (run-action node (list
                    (scale-x-to 1.0 2.0)
                    (scale-x-to 1.0 0.5))
              :repeat :forever)
  :update
  (unless started
    (setf started t)
    (on-enter node))
  (visit node))

;; (run-test 'tint-to-test)

(deftest empty-window (:width 500 :height 500)
  :tags memory window
  :init
  :update)

;; (run-test 'empty-window)

(deftest load-texture (:width 500 :height 500)
  :tags memory texture texture-loading
  :init
  (get-texture "./alien.png")
  :update)

;; (run-test 'load-texture)

(defun make-circle-image (width height)
  (xmas.vecto-texture:vecto-image (:width width :height height)
    (vecto:set-rgb-fill 1.0 0.0 0.0)
    (vecto:centered-circle-path (* 0.5 width) (* 0.5 height) (* 0.5 (min width height)))
    (vecto:fill-path)))

(deftest texture-from-vector (:width 500 :height 500)
  :tags static-vectors texture texture-loading
  :init
  width := 100
  height := 100
  circle :=  (make-circle-image width height)
  texture := (make-texture-from-rgba-vector circle width height)
  :update
  (draw-texture-at texture
                   (- 250.0 (* width 0.5))
                   (- 250.0 (* height 0.5))
                   width height))

;; (run-test 'texture-from-vector)

(deftest dynamic-text-rendering (:width 500 :height 500 :expandable t)
  :tags text font file-format lfont texture texture-loading static-vectors
  :init
  path := "./res/ttf/OpenSans-Light.ttf"
  font10 := (xmas.ttf-font:make-font :font path :size 10)
  font15 := (xmas.ttf-font:make-font :font path :size 15)
  font20 := (xmas.ttf-font:make-font :font path :size 20)
  font30 := (xmas.ttf-font:make-font :font path :size 30)
  font72 := (xmas.ttf-font:make-font :font path :size 72)
  str := "The quick brown fox jumped over the lazy dog."
  :update
  (lfont-draw-string font10 str 10.0 150.0 :letter-spacing 2.0)
  (lfont-draw-string font15 str 10.0 200.0 :letter-spacing 2.0)
  (lfont-draw-string font20 str 10.0 250.0 :letter-spacing 3.0)
  (lfont-draw-string font30 str 10.0 300.0 :letter-spacing 2.0)
  (lfont-draw-string font72 str 10.0 350.0 :letter-spacing 4.0))

;; (run-test 'dynamic-text-rendering)

(deftest move-to-test (:width 500 :height 500)
  :tags action move-to
  :init
  started := nil
  root := (make-instance 'debug-node :x 25.0 :y 25.0)
  (run-action root (list
                    (move-to 1.0 25.0 475.0)
                    (move-to 1.0 475.0 475.0)
                    (move-to 1.0 475.0 25.0)
                    (move-to 1.0 25.0 25.0))
              :repeat :forever)
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (visit root))

;; (run-test 'move-to-test)

(deftest move-by-test (:width 500 :height 500)
  :tags action move-by
  :init
  started := nil
  root := (make-instance 'debug-node :x 25.0 :y 25.0)
  (run-action root (list
                    (move-by 1.0 0.0 450.0)
                    (move-by 1.0 450.0 0.0)
                    (move-by 1.0 0.0 -450.0)
                    (move-by 1.0 -450.0 0.0))
              :repeat :forever)
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (visit root))

;; (run-test 'move-by-test)

(deftest batched-writes-0 (:width 500 :height 500)
  :tags batched pointers
  :init
  tex := (get-texture "./bayarea.png")
  :update
  (draw-texture-at tex 100.0 350.0 50.0 50.0)
  (xmas.render-buffer::disable-texture)
  (xmas.render-buffer::with-2d-triangles ()
    (xmas.render-buffer::vert 10.0 10.0)
    (xmas.render-buffer::vert 200.0 200.0)
    (xmas.render-buffer::vert 200.0 10.0)

    (xmas.render-buffer::vert 450.0 10.0)
    (xmas.render-buffer::vert 250.0 200.0)
    (xmas.render-buffer::vert 250.0 10.0))
  (draw-texture-at tex 250.0 350.0 50.0 50.0))

;; (run-test 'batched-writes-0)

(deftest batched-writes-1 (:width 800 :height 800)
  :tags texture batched pointers
  :init
  width := 800.0
  height := 800.0
  diagonal := (coerce (sqrt (+ (* width width)
                               (* height height)))
                      'single-float)
  outset-x := (* -0.5 (- diagonal width))
  outset-y := (* -0.5 (- diagonal height))
  tex := (get-texture "./bayarea.png")
  rotation := 0.0
  counter := 0
  :update
  (incf rotation (* 7.0 dt))
  (setf rotation (mod rotation 360.0))
  (incf counter (* 0.25 dt))
  (setf counter (mod counter 8.0))
  (xmas.render-buffer::set-color 1.0 1.0 1.0 1.0)
  (xmas.render-buffer::push-matrix)
  (xmas.render-buffer::translate-scale-rotate-translate (* width 0.5)
                                                        (* height 0.5)
                                                        1.0 1.0
                                                        rotation
                                                        (* width -0.5)
                                                        (* height -0.5))
  (when-let (id (texture-id tex))
    (xmas.render-buffer::with-textured-2d-triangles (id)
      (flet ((draw-texture (x y w h)
               (let ((x2 (+ x w))
                     (y2 (+ y h)))
                 (xmas.render-buffer::vert x y 0.0 1.0)
                 (xmas.render-buffer::vert x y2 0.0 0.0)
                 (xmas.render-buffer::vert x2 y2 1.0 0.0)
                 (xmas.render-buffer::vert x2 y2 1.0 0.0)
                 (xmas.render-buffer::vert x y 0.0 1.0)
                 (xmas.render-buffer::vert x2 y 1.0 1.0))))
        (declare (dynamic-extent #'draw-texture))
        (loop
           with limit = (1+ (floor counter))
           for i below limit do
             (loop
                with count = (1+ (* 5 i))
                with inset = (* i 100.0)
                with width = (/ (- diagonal inset) count)
                with height = (/ (- diagonal inset) count)
                with offs = (* inset 0.5)
                for x below count do
                  (loop for y below count do
                       (loop for div from 1 upto 3 do
                            (draw-texture (+ (* x width) offs outset-x)
                                          (+ (* y height) offs outset-y)
                                          (/ width div)
                                          (/ height div)))))))))
  (xmas.render-buffer::pop-matrix))

;; (run-test 'batched-writes-1)

(deftest batched-writes-2 (:width 500 :height 500)
  :init
  tex := (get-texture "./bayarea.png")
  :update
  (when-let (id (texture-id tex))
    (xmas.render-buffer::with-textured-2d-quads (id)
      (xmas.render-buffer::quad 0.0   0.0   0.0 1.0
                                250.0 250.0 1.0 0.0)
      (xmas.render-buffer::quad 250.0 250.0 0.0 1.0
                                500.0 500.0 1.0 0.0)
      (xmas.render-buffer::quad 0.0   250.0 0.0 1.0
                                250.0 500.0 1.0 0.0)
      (xmas.render-buffer::quad 250.0 0.0   0.0 1.0
                                500.0 250.0 1.0 0.0))))

;; (run-test 'batched-writes-2)

(deftest batched-writes-3 (:width 500 :height 500)
  :tags batched-drawing matrix quad
  :init
  tex := (get-texture "./bayarea.png")
  :update
  (when-let (id (texture-id tex))
    (xmas.render-buffer::with-textured-2d-quads (id)
      (xmas.render-buffer::%draw-quad 0.0 0.0
                                      10.0 400.0
                                      400.0 400.0
                                      400.0 10.0
                                      0.0 1.0
                                      1.0 0.0))))

;; (run-test 'batched-writes-3)

(defun draw-batched-node-quad (node)
  (multiple-value-bind (llx lly ulx uly urx ury lrx lry)
      (node-four-corners node (node-transform node))
    (xmas.render-buffer::%draw-quad llx lly ulx uly urx ury lrx lry
                                    0.0 1.0
                                    1.0 0.0)))
(defun force-matrix-multiply (node)
  (multiple-value-bind (llx lly ulx uly urx ury lrx lry)
      (node-four-corners node (node-transform node))
    (declare (ignorable llx lly ulx uly urx ury lrx lry))) ) 

(deftest batched-writes-4 (:width 500 :height 500)
  :tags batched-drawing matrix quad
  :init
  tex := (get-texture "./bayarea.png")
  started := nil
  ;; we can draw alot of nodes, but can't do that
  ;; many matrix recalculations... having trouble optimizing it
  count := 10000
  nodes := (loop repeat count collect
                (make-instance 'node :x (random 500.0) :y (random 500.0)
                               :content-width (+ (random 100.0) 50.0)
                               :content-height (+ (random 100.0) 50.0)
                               :anchor-x 0.5 :anchor-y 0.5
                               :rotation (random 360.0)))
  (setf nodes (coerce nodes 'vector))
  :update
  (unless started
    (setf started t)
    (map nil 'on-enter nodes))
  (loop
     for node across nodes do
       (setf (rotation node) (mod (+ (rotation node) (* dt 100.0)) 360.0)))
  (when-let (id (texture-id tex))
    (xmas.render-buffer::with-textured-2d-quads (id)
      (loop for i from (1- (length nodes)) downto 0 do
           (draw-batched-node-quad (aref nodes i))))))

;; (run-test 'batched-writes-4)

(defun draw-batched-node-quad-with-matrix (node matrix)
  (multiple-value-bind (llx lly ulx uly urx ury lrx lry)
      (four-corners 0.0 0.0
                    (content-width node)
                    (content-height node)
                    (apply-node-transform node matrix))
    (xmas.render-buffer::%draw-quad llx lly ulx uly urx ury lrx lry
                                    0.0 1.0
                                    1.0 0.0)))

(defvar *consing* 0)

(deftest batched-writes-5 (:width 500 :height 500)
  :tags batched-drawing matrix quad
  :init
  tex := (get-texture "./alien.png")
  started := nil
  count := 25000
  root := (make-instance 'node :x 250.0 :y 250.0
                         :rotation (random 360.0))
  diameter := (sqrt (+ (* 500.0 500.0) (* 500.0 500.0)))
  radius := (* diameter 0.5)
  nodes := (loop repeat count collect
                (make-instance 'node
                               :x (- (random diameter) radius)
                               :y (- (random diameter) radius)
                               :content-width (+ (random 100.0) 50.0)
                               :content-height (+ (random 100.0) 50.0)
                               :anchor-x 0.5 :anchor-y 0.5
                               :rotation (random 360.0)))
  matrix := (make-matrix)
  consed := 0
  (setf nodes (coerce nodes 'vector))
  (run-action root (rotate-by 12.0 360.0) :repeat :forever)
  :update
  (unless started
    (setf started t)
    (on-enter root)
    (map nil 'on-enter nodes))
  (setf consed (ccl::total-bytes-allocated))
  (when-let (id (texture-id tex))
    (xmas.render-buffer::with-textured-2d-quads (id)
      (loop
         with root-transform = (node-transform root)
         for i from (1- (length nodes)) downto 0
         for node = (aref nodes i)
         do
           (setf (rotation node) (mod (+ (rotation node) (* dt 100.0)) 360.0))
           (into-matrix (matrix)
             (load-matrix root-transform))
           (draw-batched-node-quad-with-matrix node matrix))))
  (setf *consing* (- (ccl::total-bytes-allocated) consed))
  ;; :on-event (format t "got event: ~S~%" event)
  )

;; (run-test 'batched-writes-5)

(deftest batched-writes-6 (:width 500 :height 500)
  :tags batched-drawing matrix quad color
  :init
  tex := (get-texture "./bayarea.png")
  :update
  (when-let (id (texture-id tex))
    (xmas.render-buffer::with-colored-textured-2d-quads (id)
      (xmas.render-buffer::%draw-quad 0.0 0.0
                                      10.0 400.0
                                      400.0 400.0
                                      400.0 10.0
                                      0.0 1.0
                                      1.0 0.0)
      (let ((buff (xmas.render-buffer::buffer-u8-values
                   xmas.render-buffer::*write-buffer*)))
        (xmas.render-buffer::write-u8s!
         buff
         255 0   0   255
         0   255 0   255
         0   0   255 255
         255 255 0   255)))))

;; (run-test 'batched-writes-6)

(deftest batched-writes-7 (:width 500 :height 500)
  :tags batched drawing quad color sprites
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  stack := (make-matrix-stack)
  sprite1 := (make-instance 'sprite :x 150.0 :y 250.0
                            :sprite-frame (get-frame "pickle.png")
                            :color (vector 1.0 1.0 0.0))
  sprite2 := (make-instance 'sprite :x 250.0 :y 250.0
                            :sprite-frame (get-frame "pickle.png")
                            :color (vector 0.0 1.0 0.0))
  sprite3 := (make-instance 'sprite :x 350.0 :y 250.0
                            :sprite-frame (get-frame "pickle.png")
                            :color (vector 0.0 1.0 1.0))
  root := (make-instance 'node)
  (add-children root (list sprite1 sprite2 sprite3))
  :update
  (let ((*matrix-stack* stack))
    (visit-with-xform root)))

;; (run-test 'batched-writes-7)

(deftest draw-with-xform-0 (:width 500 :height 250)
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  frame := (get-frame "pickle blink.png")
  frame2 := (get-frame "pickle.png")
  sprite1 := (make-instance 'sprite :x 175.0 :y 175.0
                            :sprite-frame frame)
  sprite2 := (make-instance 'sprite :x 75.0 :y 175.0
                            :flip-x t
                            :sprite-frame frame)
  sprite3 := (make-instance 'sprite :x 175.0 :y 75.0
                            :flip-y t
                            :sprite-frame frame)
  sprite4 := (make-instance 'sprite :x 75.0 :y 75.0
                            :flip-y t :flip-x t
                            :sprite-frame frame)
  sprite5 := (make-instance 'sprite :x (+ 250.0 175.0) :y 175.0
                            :sprite-frame frame2)
  sprite6 := (make-instance 'sprite :x (+ 250.0 75.0) :y 175.0
                            :flip-x t
                            :sprite-frame frame2)
  sprite7 := (make-instance 'sprite :x (+ 250.0 175.0) :y 75.0
                            :flip-y t
                            :sprite-frame frame2)
  sprite8 := (make-instance 'sprite :x (+ 250.0 75.0) :y 75.0
                            :flip-y t :flip-x t
                            :sprite-frame frame2)
  stack := (make-matrix-stack)
  :update
  (let ((*matrix-stack* stack))
    (visit-with-xform sprite1)
    (visit-with-xform sprite2)
    (visit-with-xform sprite3)
    (visit-with-xform sprite4)
    (visit-with-xform sprite5)
    (visit-with-xform sprite6)
    (visit-with-xform sprite7)
    (visit-with-xform sprite8)))

;; (run-test 'draw-with-xform-0)

(deftest draw-with-xform-1 (:width 500 :height 250)
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  frame := (get-frame "throwcat.png")
  started := nil
  root := (make-instance 'node)
  sprite1 := (make-instance 'sprite :x 175.0 :y 175.0
                            :rotation 20.0
                            :sprite-frame frame)
  sprite2 := (make-instance 'sprite :x 75.0 :y 175.0
                            :flip-x t
                            :rotation 20.0
                            :sprite-frame frame)
  sprite3 := (make-instance 'sprite :x 175.0 :y 75.0
                            :rotation 20.0
                            :sprite-frame frame)
  sprite4 := (make-instance 'sprite :x 75.0 :y 75.0
                            :flip-x t
                            :rotation 20.0
                            :sprite-frame frame)
  sprite5 := (make-instance 'sprite :x (+ 250.0 175.0) :y 175.0
                            :rotation 20.0
                            :flip-y t
                            :sprite-frame frame)
  sprite6 := (make-instance 'sprite :x (+ 250.0 75.0) :y 175.0
                            :flip-x t
                            :flip-y t
                            :rotation 20.0
                            :sprite-frame frame)
  sprite7 := (make-instance 'sprite :x (+ 250.0 175.0) :y 75.0
                            :rotation 20.0
                            :flip-y t
                            :sprite-frame frame)
  sprite8 := (make-instance 'sprite :x (+ 250.0 75.0) :y 75.0
                            :flip-x t
                            :flip-y t
                            :rotation 20.0
                            :sprite-frame frame)
  (run-action sprite1 (rotate-by 3.0 360.0) :repeat :forever)
  (run-action sprite2 (rotate-by 3.0 360.0) :repeat :forever)
  (run-action sprite5 (rotate-by 3.0 360.0) :repeat :forever)
  (run-action sprite6 (rotate-by 3.0 360.0) :repeat :forever)
  (add-children root (list sprite1 sprite2 sprite3 sprite4 sprite5
                           sprite6 sprite7 sprite8))
  stack := (make-matrix-stack)
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (let ((*matrix-stack* stack))
    (visit-with-xform root)))

;; (run-test 'draw-with-xform-1)

(deftest translate-scale-rotate-translate ()
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  started := nil
  stack := (make-matrix-stack)
  frame := (get-frame "pickle blink.png")
  sprite1 := (make-instance 'sprite :x 125.0 :y 125.0
                            :scale-x 1.2 :scale-y 1.2
                            :sprite-frame frame)
  (run-action sprite1 (rotate-by 12.0 360.0) :repeat :forever)
  (run-action sprite1 (list (scale-x-to 1.0 0.8) (scale-x-to 1.0 1.2))
              :repeat :forever)
  (run-action sprite1 (list (scale-y-to 1.0 0.8) (scale-y-to 1.0 1.2))
              :repeat :forever)
  :update
  (unless started
    (setf started t)
    (on-enter sprite1))
  (let ((*matrix-stack* stack))
    (visit-with-xform sprite1)))

;; (run-test 'translate-scale-rotate-translate)


(deftest error-in-init ()
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  started := nil
  stack := (make-matrix-stack)
  frame := (get-frame "pickle blink.png")
  sprite1 := (make-instance 'sprite :x 125.0 :y 125.0
                            :scale-x 1.2 :scale-y 1.2
                            :sprite-frame frame)
  (run-action sprite1 (rotate-by 12.0 360.0) :repeat :forever)
  (error "Whoops.")
  :update
  (unless started
    (setf started t)
    (on-enter sprite1))
  (let ((*matrix-stack* stack))
    (visit-with-xform sprite1)))

;; (run-test 'error-in-init)

(deftest error-in-frame ()
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  started := nil
  stack := (make-matrix-stack)
  frame := (get-frame "pickle blink.png")
  sprite1 := (make-instance 'sprite :x 125.0 :y 125.0
                            :scale-x 1.2 :scale-y 1.2
                            :sprite-frame frame)
  (run-action sprite1 (rotate-by 12.0 360.0) :repeat :forever)
  counter := 0
  :update
  (unless started
    (setf started t)
    (on-enter sprite1))
  (incf counter)
  (when (= counter 20) (error "Whoops."))
  (let ((*matrix-stack* stack))
    (visit-with-xform sprite1)))

;; (run-test 'error-in-frame)

(deftest error-in-event-handler ()
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  started := nil
  stack := (make-matrix-stack)
  frame := (get-frame "pickle blink.png")
  sprite1 := (make-instance 'sprite :x 125.0 :y 125.0
                            :scale-x 1.2 :scale-y 1.2
                            :sprite-frame frame)
  (run-action sprite1 (rotate-by 12.0 360.0) :repeat :forever)
  :update
  (unless started
    (setf started t)
    (on-enter sprite1))
  (let ((*matrix-stack* stack))
    (visit-with-xform sprite1))
  :on-event
  (case (car event)
    (:click (error "Whoops."))))

;; (run-test 'error-in-event-handler)

(deftest batch-node-test-0 (:width 500 :height 500)
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  frame := (get-frame "pickle blink.png")
  texture := (texture-frame-texture frame)
  started := nil
  root := (make-instance 'sprite-batch-node :texture texture)
  stack := (make-matrix-stack)
  sprite1 := (make-instance 'sprite :x 125.0 :y 125.0
                            :scale-x 1.2 :scale-y 1.2
                            :sprite-frame frame)
  frame-names := (circular-list "pickle.png" "pickle blink.png"
                                 "throwcat.png" "jewel.png")
  count := 5000
  sprites := (loop repeat count collect
                  (let ((s (make-instance 'sprite
                                 :x (random 500.0)
                                 :y (random 500.0)
                                 :rotation (random 360.0)
                                 :scale-x (+ 0.8 (random 0.4))
                                 :scale-y (+ 0.8 (random 0.4))
                                 :sprite-frame (get-frame (pop frame-names))
                                 )))
                    (run-action s (rotate-by (+ 3.0 (random 6.0))
                                             360.0) :repeat :forever)
                    s))
  (add-children root sprites)
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (let ((*matrix-stack* stack))
    (visit-with-xform root)))

;; (run-test 'batch-node-test-0)

(read-tilemap "./res/platformer/infinite.tmx")
(read-tilemap "./res/platformer/dev.tmx")
