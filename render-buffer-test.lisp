(defpackage :xmas.render-buffer-tests (:use :cl :alexandria
                                            :xmas.action
                                            :xmas.animation-manager
                                            :xmas.display
                                            :xmas.draw
                                            :xmas.lfont-reader
                                            :xmas.matrix
                                            :xmas.node
                                            :xmas.qtree
                                            :xmas.sprite
                                            :xmas.texture
                                            :xmas.texture-packer
                                            :xmas.tmx-reader
                                            :xmas.tmx-renderer))
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

(xmas.deftest:deftest bouncy-balls (:width 500 :height 500 :expandable t)
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

(xmas.deftest:deftest draw-texture ()
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

(xmas.deftest:deftest draw-many-textures (:width 500 :height 500)
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

(xmas.deftest:deftest matrix-translation-rotation ()
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

(defclass rect (node)
  ()
  (:default-initargs :anchor-x 0.5 :anchor-y 0.5))

(defmethod draw ((self rect))
  (draw-node-color self)
  (xmas.render-buffer::draw-rect 0.0 0.0 (width self) (height self)))

(defmethod draw ((self node))
  (draw-node-color self)
  (xmas.render-buffer::draw-rect -20.0 -20.0 40.0 40.0))

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
    (draw-texture-frame-at frame offs-x offs-y frame-width frame-height)))

;; the fast method:
(defmethod visit ((self node))
  (xmas.render-buffer::push-matrix)
  (let ((ax (anchor-x self))
        (ay (anchor-y self)))
    (if (and (zerop ax) (zerop ay))
        (xmas.render-buffer::translate-scale-rotate
         (x self) (y self)
         (scale-x self) (scale-y self)
         (rotation self))
        (xmas.render-buffer::translate-scale-rotate-translate
         (x self) (y self)
         (scale-x self) (scale-y self)
         (rotation self)
         (* -1.0 ax (content-width self))
         (* -1.0 ay (content-height self)))))
  (draw self)
  (when (children self)
    (loop for child across (children self) do
         (visit child)))
  (xmas.render-buffer::pop-matrix))

;; the also-fast method:
;; (defmethod visit ((self node))
;;   (push-matrix)
;;   (translate-scale-rotate (x self) (y self)
;;                           (scale-x self) (scale-y self)
;;                           (rotation self))
;;   (node-transform self) ;; <- note this addition
;;   (draw self)
;;   (pop-matrix))

;; this method also performs fairly well (some minor stuttering),
;; seeming to indicate that the bottleneck is actually in
;; %gl:mult-matrix-f
;; (defmethod visit ((self node))
;;   (push-matrix)
;;   (translate-scale-rotate (x self) (y self)
;;                           (scale-x self) (scale-y self)
;;                           (rotation self))
;;   (mult-matrix-noop (matrix:unwrap-matrix (node-transform self)))
;;   (draw self)
;;   ;;visit children here.
;;   (pop-matrix))

;; the slow as sin method.
;; (defmethod visit ((self node))
;;   (push-matrix)
;;   (mult-matrix (matrix:unwrap-matrix (node-transform self)))
;;   (draw self)
;;   (when (children self)
;;     (loop for child across (children self) do
;;          (visit child)))
;;   (pop-matrix))


(xmas.deftest:deftest visit-and-draw-many-nodes (:width 500 :height 500)
  :tags node draw-heavy add-child rotation slow-start
  :init
  width     := (display-width display)
  height    := (display-height display)
  diagonal  := (sqrt (+ (* width width) (* height height)))
  d/2       := (/ diagonal 2)
  root-node := (make-instance 'node :x (/ width 2.0) :y (/ height 2.0))
  nodes     := (loop repeat 4000 collect
                    (make-instance 'node
                                   :x (coerce (- (random diagonal) d/2) 'float)
                                   :y (coerce (- (random diagonal) d/2)'float)
                                   :rotation (coerce (random 360) 'float)
                                   :color (vector
                                           (/ (random 100) 100.0)
                                           (/ (random 100) 100.0)
                                           (/ (random 100) 100.0))
                                   :opacity (/ (random 100) 100.0)))
  (dolist (node nodes)
    (add-child root-node node))
  :update
  (dolist (node (cons root-node nodes))
    (let ((r (rotation node)))
      (incf r (* dt 100))
      (setf r (mod r 360))
      (setf (rotation node) r)))
  (xmas.render-buffer::set-color 0.0 1.0 1.0 0.4)
  (visit root-node))

;; (xmas.deftest:run-test 'visit-and-draw-many-nodes)

(xmas.deftest:deftest node-actions ()
  :tags node actions run-action repeat easing callfunc remove-from-parent
  :init
  started := nil
  node := (make-instance 'node
                         :x (/ (display-width display) 2)
                         :y (/ (display-height display) 2))
  node2 := (make-instance 'node
                          :color (vector 0.0 1.0 1.0)
                          :opacity 0.4
                          :x 20.0
                          :y 20.0)
  node3 := (make-instance 'node
                          :color (vector 1.0 0.0 0.0)
                          :opacity 0.4
                          :x -20.0
                          :y -20.0)
  node4 := (make-instance 'node
                          :color (vector 0.0 1.0 0.0)
                          :opacity 0.4
                          :x  20.0
                          :y -20.0)
  node5 := (make-instance 'node
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

;; (xmas.deftest:run-test 'node-actions)

(xmas.deftest:deftest action-manager-test ()
  :tags node actions action-manager
  :init
  started := nil
  root := (make-instance 'node
                         :x (/ (display-width display) 2)
                         :y (/ (display-height display) 2))
  (labels ((add-node (&aux (node (make-instance 'node)))
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

;; (xmas.deftest:run-test 'action-manager-test)

(xmas.deftest:deftest texture-frame-test (:width 500 :height 500)
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

;; (xmas.deftest:run-test 'texture-frame-test)

(xmas.deftest:deftest texture-packer-test (:width 500 :height 500)
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

;; (xmas.deftest:run-test 'texture-packer-test)

(xmas.deftest:deftest tilemap-test0 (:width 500 :height 500)
  :tags tmx file-format
  :init
  map     := (read-tilemap "./res/test-tilemap.tmx")
  tileset := (first (map-tilesets map))
  frames  := (make-tileset-texture-frames tileset)
  :update
  (loop
     with x = 100 with y = 250
     for frame across frames
     for i upfrom 0
     when frame
     do (draw-texture-frame frame (+ x (* i 50)) y)))

;; (xmas.deftest:run-test 'tilemap-test0)

(xmas.deftest:deftest tilemap-test-1 (:width 500 :height 500)
  :tags tmx file-format draw-tmx-layer
  :init 
  map     := (read-tilemap "./res/test-tilemap.tmx")
  tileset := (first (map-tilesets map))
  frames  := (make-tileset-texture-frames tileset)
  layer   := (first (map-layers map))
  :update
  (draw-tmx-layer 250 250 
     (tileset-tile-width tileset)
     (tileset-tile-height tileset)
     layer
     frames))

;; (xmas.deftest:run-test 'tilemap-test-1)

(xmas.deftest:deftest tmx-renderer-test (:width 500 :height 500)
  :tags tmx file-format tmx-renderer
  :init
  r := (tmx-renderer-from-file "./res/test-tilemap.tmx")
  :update
  (let* ((x (/ (tmx-renderer-width r) 2.0))
         (y (/ (tmx-renderer-height r) 2.0)))
    (draw-tmx-renderer x y r)))

;; (xmas.deftest:run-test 'tmx-renderer-test)

(xmas.deftest:deftest action-tags (:width 500 :height 500)
  :tags node action-manager actions action-tags
  :init
  started := nil
  n := (make-instance 'node :x 250 :y 250)
  (run-action n (list (move-by 3.0 -100.0 0)
                      (move-by 3.0 100.0 0))
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

;; (xmas.deftest:run-test 'action-tags)

(xmas.deftest:deftest animation (:width 500 :height 500)
  :tags file-format texture-packer animation anchor-point
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  (add-animation 'cat (/ 1.0 7.5) '("catwalk0.png" "catwalk1.png"))
  started := nil
  sprite := (make-instance 'sprite
                           :x 250
                           :y 250
                           :scale-x 2.0
                           :scale-y 2.0
                           :sprite-frame (get-frame "pickle.png")
                           :anchor-x 0.0
                           :anchor-y 0.0)
  (run-animation sprite 'cat :repeat :forever)
  (setf (rotation sprite) -15)
  :update
  (unless started
    (setf started t)
    (on-enter sprite))
  (visit sprite))

;; (xmas.deftest:run-test 'animation)

(defclass box (rect)
  ((vx :accessor box-vx :initarg :vx)
   (vy :accessor box-vy :initarg :vy)))

(defun draw-marker (rect)
  (xmas.render-buffer::draw-rect (x rect)
                                 (y rect)
                                 5.0 5.0))

(xmas.deftest:deftest quadtree (:width 500 :height 500)
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
                          :content-width 20
                          :content-height 20
                          :opacity 0.5
                          :x (random 500)
                          :y (random 500)
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

;; (xmas.deftest:run-test 'quadtree)

(xmas.deftest:deftest text-rendering (:width 500 :height 500)
  :tags text font file-format lfont
  :init
  november  := (lfont-from-file "./res/lfont/november.lfont")
  open-sans := (lfont-from-file "./res/lfont/OpenSans-Light.lfont")
  :update
  (lfont-draw-string november "Hello, world!" 50.0 50.0)
  (lfont-draw-string open-sans
                     "The quick brown fox jumped over the lazy dog."
                     10.0 150.0 :letter-spacing 0.0))

;; (xmas.deftest:run-test 'text-rendering)

(xmas.deftest:deftest anchor-point-test-0 (:width 500 :height 500)
  :tags node anchor-point
  :init
  root    := (make-instance 'node)
  started := nil
  (flet ((make-rect (&optional (ax (/ (random 150) 100.0))
                               (ay (/ (random 150) 100.0)))
           (let ((r (make-instance
                     'rect
                     :content-width 40
                     :content-height 40
                     :anchor-x ax
                     :anchor-y ay
                     :x 250
                     :y 250
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
          (top 450) (left 50)
          (bottom 50) (right 450))
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
       (x e) 250
       (y e) 250
       (x f) 250
       (y f) 250
       (rotation f) 0.0)))
  :update
  (unless started
    (setf started t)
    (on-enter root))
  (visit root))

;; (xmas.deftest:run-test 'anchor-point-test-0)

(xmas.deftest:deftest anchor-point-test-1 (:width 500 :height 500)
  :tags sprite node anchor-point content-size
  :init
  (texture-packer-add-frames-from-file "./res/test.json")
  sprite1 := (make-instance 'sprite :x 150 :y 250
                            :sprite-frame (get-frame "pickle.png"))
  sprite2 := (make-instance 'sprite :x 250 :y 250
                            :content-width 40
                            :content-height 60
                            :sprite-frame (get-frame "pickle.png"))
  sprite3 := (make-instance 'sprite :x 400 :y 250
                            :content-width 100
                            :content-height 150
                            :sprite-frame (get-frame "pickle.png"))
  :update
 (flet ((draw-it (s)
           (xmas.render-buffer::set-color 1.0 1.0 1.0 1.0)
           (xmas.render-buffer::draw-rect (left s) (bottom s) (width s) (height s))
           (visit s)))
    (draw-it sprite1)
    (draw-it sprite2)
    (draw-it sprite3)))

;; (xmas.deftest:run-test 'anchor-point-test-1)

(xmas.deftest:deftest texture-wrap-0 (:width 500 :height 500)
  :tags texture drawing texture-wrapping
  :init
  tex := (get-texture "./alien.png" :wrap :repeat)
  :update
  (draw-texture-at-tex-coords tex 0 0 500 500 0.0 0.0 4.0 4.0))

;; (xmas.deftest:run-test 'texture-wrap-0)


(read-tilemap "./res/platformer/infinite.tmx")
(read-tilemap "./res/platformer/dev.tmx")
