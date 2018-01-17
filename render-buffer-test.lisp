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

(defvar *tests* (make-hash-table :test 'equal))

(defmacro deftest (name (&key) &body body)
  `(setf (gethash ',name *tests*) (lambda () ,@body)))

(defun run-test (name) (funcall (gethash name *tests*)))
(defun run-tests (list)
  (labels ((next ()
             (when-let (test (pop list))
               (let ((cl-user::*display-closed-hook* #'next))
                 (format t "RUNNING TEST: ~S~%" test)
                 (run-test test)))))
    (next)))
(defun run-all-tests ()
  (run-tests (hash-table-keys *tests*)))

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

(defstruct test4
  alien)

(defmethod cl-user::contents-will-mount ((self test4) display)
  (declare (ignorable display))
  (let ((texture (get-texture #P"./alien.png")))
    (assert (texture-width texture))
    (assert (texture-height texture))
    (setf (test4-alien self) texture))
  (unless (test4-alien self)
    (format t "missing texture!")))

(defmethod cl-user::step-contents ((self test4) dt)
  (declare (ignorable dt))
  (draw-texture (test4-alien self)))

(deftest draw-texture ()
  (cl-user::display-contents (make-test4)))

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

(defstruct test5
  width
  height
  texture
  sprites)

(defmethod cl-user::contents-will-mount ((self test5) display)
  (let ((width (display-width display))
        (height (display-height display))
        (texture (get-texture #P"./alien.png")))
    (unless texture
      (format t "missing texture!"))
    (setf (test5-width self) width)
    (setf (test5-height self) height)
    (setf (test5-texture self) texture)
    (setf (test5-sprites self)
          (loop repeat 4000 collect
               (make-simple-sprite :x (random width)
                                   :y (random height)
                                   :dx (- (random 100) 50)
                                   :dy (- (random 100) 50)
                                   :r (random 360)
                                   :dr (- (random 100) 50)
                                   :sx 0.5
                                   :sy 0.5
                                   :texture texture)))))


(defmethod cl-user::step-contents ((self test5) dt)
  (declare (ignorable dt))
  (let ((maxx (test5-width self)) (maxy (test5-height self)))
    (dolist (sprite (test5-sprites self))
      (update-sprite sprite maxx maxy dt)))
  (dolist (sprite (test5-sprites self))
    (draw-sprite sprite)))

(defmethod cl-user::handle-event ((self test5) event)
  (case (car event)
    (:resize
     (let ((w (cadr event))
           (h (cddr event)))
       (setf (test5-width self) w
             (test5-height self) h)))
    (t (format t "got unhandled event: ~S~%" event))))

(deftest draw-many-textures ()
  (cl-user::display-contents (make-test5) :width 500 :height 500 :expandable t))

(defstruct test6
  (matrix (make-matrix))
  (scratch-matrix (make-matrix)))

(defmethod cl-user::contents-will-mount ((self test6) display)
  (declare (ignorable display))
  (let ((m (test6-matrix self)))
    (into-matrix (m)
      (load-identity)
      (translate 125.0 125.0))))

(defmethod cl-user::step-contents ((self test6) dt)
  (declare (ignorable dt))
  (let ((*tmp-matrix* (test6-scratch-matrix self)))
    (into-matrix ((test6-matrix self))
      (rotate (* 40 dt))))
  (xmas.render-buffer::set-color 0.0 1.0 0.0 0.5)
  (xmas.render-buffer::draw-rect 10.0 10.0 20.0 20.0)
  (xmas.render-buffer::push-matrix)
  (xmas.render-buffer::mult-matrix (unwrap-matrix (test6-matrix self)))
  (xmas.render-buffer::draw-rect 10.0 10.0 20.0 20.0)
  (xmas.render-buffer::draw-rect 30.0 30.0 20.0 20.0)
  (xmas.render-buffer::pop-matrix))

(deftest matrix-translation ()
  (cl-user::display-contents (make-test6)))

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
         (width (texture-frame-width frame))
         (height (texture-frame-height frame))
         (offs-x (* 0.5 (- (content-width self) width)))
         (offs-y (* 0.5 (- (content-height self) height))))
    (draw-texture-frame-at
     frame offs-x offs-y width height)))

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

(defstruct test7
  nodes
  width
  height
  root-node)

(defmethod cl-user::contents-will-mount ((self test7) display)
  (with-slots (nodes width height root-node) self
    (let* ((w (display-width display))
           (h (display-height display))
           (w/2 (/ w 2))
           (h/2 (/ h 2))
           (diagonal (sqrt (+ (* w w) (* h h))))
           (d/2 (/ diagonal 2)))
      (setf width w
            height h
            root-node (make-instance 'node
                                     :x w/2
                                     :y h/2)
            nodes
            (loop
               repeat 4000 collect
                 (make-instance 'node
                                :x (coerce (- (random diagonal) d/2) 'float)
                                :y (coerce (- (random diagonal) d/2)'float)
                                :rotation (coerce (random 360) 'float)
                                :color (vector
                                        (/ (random 100) 100.0)
                                        (/ (random 100) 100.0)
                                        (/ (random 100) 100.0))
                                :opacity (/ (random 100) 100.0)))))
    (dolist (child nodes)
      (add-child root-node child))))

(defmethod cl-user::step-contents ((self test7) dt)
  (declare (ignorable dt))
  (dolist (node (cons (test7-root-node self) (test7-nodes self)))
    (let ((r (rotation node)))
      (incf r (* dt 100))
      (setf r (mod r 360))
      (setf (rotation node) r)))
  (xmas.render-buffer::set-color 0.0 1.0 1.0 0.4)
  (visit (test7-root-node self)))

(deftest visit-and-draw-many-nodes ()
  (cl-user::display-contents (make-test7) :width 500 :height 500))


(defstruct test8
  node
  started)

(defmethod cl-user::contents-will-mount ((self test8) display)
  (let ((node (make-instance 'node
                             :x (/ (display-width display) 2)
                             :y (/ (display-height display) 2)))
        (node2 (make-instance 'node
                              :color (vector 0.0 1.0 1.0)
                              :opacity 0.4
                              :x 20.0
                              :y 20.0))
        (node3 (make-instance 'node
                              :color (vector 1.0 0.0 0.0)
                              :opacity 0.4
                              :x -20.0
                              :y -20.0))
        (node4 (make-instance 'node
                              :color (vector 0.0 1.0 0.0)
                              :opacity 0.4
                              :x  20.0
                              :y -20.0))
        (node5 (make-instance 'node
                              :color (vector 1.0 1.0 0.0)
                              :opacity 0.4
                              :x -20.0
                              :y  20.0)))
    (run-action node
                (repeat-forever
                 (run-sequence
                  (delay 1.0)
                  (ease-in-out-quad
                   (rotate-by 1.5 180.0))
                  (delay 1.0)
                  (ease-in-out-sine
                   (rotate-by 1.25 -90.0)))))
    (run-action node2
                (repeat-forever
                 (rotate-by 2.5 -360.0)))
    (run-action node3
                (repeat-forever
                 (run-sequence
                  (rotate-by 0.25 -60.0)
                  (rotate-by 0.25 60.0))))
    (run-action node4
                (repeat-forever
                 (run-sequence
                  (rotate-by 0.5 -60.0)
                  (rotate-by 0.25 -60.0)
                  (rotate-by 0.25 60.0))))
    (run-action node5
                (repeat-forever
                 (run-sequence
                  (rotate-by 0.5 -60.0)
                  (rotate-by 0.5 60.0)
                  (rotate-by 0.5 -360.0)
                  (rotate-by 0.5 360.0))))
    (add-child node node2)
    (add-child node node3)
    (add-child node node4)
    (add-child node node5)
    (run-action
     node
     (repeat-forever
      (run-sequence
       (callfunc (lambda () (format t " tick! ")))
       (delay 2.0))))
    (run-action
     node
     (repeat-forever
      (run-sequence
       (delay 1.0)
       (callfunc (lambda () (format t " tock! ")))
       (delay 1.0))))
    (run-action
     node
     (run-sequence
      (delay 3.0)
      (callfunc (lambda () (remove-from-parent node2)))))
    (setf (test8-node self) node)))

(defmethod cl-user::step-contents ((self test8) dt)
  (declare (ignorable dt))
  (unless (test8-started self)
    (setf (test8-started self) t)
    (on-enter (test8-node self)))
  (visit (test8-node self)))

(deftest node-actions ()
  (cl-user::display-contents (make-test8)))


(defstruct test9
  node
  started)

(defun test9-add-node (self)
  (let ((node (make-instance 'node))
        (root (test9-node self)))
    (run-action
     node
     (list (move-by 3.0 -250.0 -250.0)
           (callfunc (lambda () (remove-from-parent node)))))
    (run-action
     root
     (list
      (delay 4.0)
      (callfunc
       (lambda ()
         (add-child root node)
         (format t "~S ~%" (mod (/ (get-internal-real-time) (coerce internal-time-units-per-second 'float)) 60))
         (test9-add-node self)))))))

(defmethod cl-user::contents-will-mount ((self test9) display)
  (let ((node (make-instance 'node
                             :x (/ (display-width display) 2)
                             :y (/ (display-height display) 2))))
    (setf (test9-node self) node)
    (test9-add-node self)))

(defmethod cl-user::step-contents ((self test9) dt)
  (declare (ignorable dt))
  (unless (test9-started self)
    (setf (test9-started self) t)
    (on-enter (test9-node self)))
  (visit (test9-node self)))

(deftest action-manager-test ()
  (cl-user::display-contents (make-test9)))


(defstruct test10
  a b c d)

(defmethod cl-user::contents-will-mount ((self test10) display)
  (declare (ignore display))
  (let ((tex (get-texture "./bayarea.png")))
    (setf (test10-a self) (texture-frame tex 0.0 0.0 250.0 250.0))
    (setf (test10-b self) (texture-frame tex 0.0 250.0 250.0 250.0))
    (setf (test10-c self) (texture-frame tex 250.0 0.0 250.0 250.0))
    (setf (test10-d self) (texture-frame tex 250.0 250.0 250.0 250.0)) ))


(defmethod cl-user::step-contents ((self test10) dt)
  (declare (ignore dt))
  (xmas.render-buffer::push-matrix)
  (draw-texture-frame (test10-a self) 125.0 125.0)
  (draw-texture-frame (test10-b self) 125.0 (+ 250.0 125.0))
  (draw-texture-frame (test10-c self) (+ 250.0 125.0) 125.0)
  (draw-texture-frame (test10-d self) (+ 250.0 125.0) (+ 250.0 125.0))
  (xmas.render-buffer::pop-matrix))

(deftest texture-frame-test ()
  (cl-user::display-contents (make-test10) :width 500 :height 500))

(defstruct test11
  packer normal-frame blink-frame jewel)

(defmethod cl-user::contents-will-mount ((self test11) display)
  (declare (ignore display))
  (let ((packed (texture-packer-from-file "./res/test.json")))
    (assert (xmas.texture-packer::texture-packer-file-texture packed))
    (setf (test11-packer self) packed
          (test11-normal-frame self) (texture-packer-get-frame
                                      packed "pickle.png")
          (test11-blink-frame self) (texture-packer-get-frame
                                     packed "pickle blink.png")
          (test11-jewel self) (texture-packer-get-frame
                               packed "jewel.png"))))

(defmethod cl-user::step-contents ((self test11) dt)
  (declare (ignore dt))
  (draw-texture-frame (test11-normal-frame self) 125.0 250.0)
  (draw-texture-frame (test11-blink-frame self) 375.0 250.0)
  (draw-texture-frame (test11-jewel self) 250.0 250.0))

(deftest texture-packer-test ()
  (cl-user::display-contents (make-test11) :width 500 :height 500))

(defstruct test12
  tmx-map
  frames)

(defmethod cl-user::contents-will-mount ((self test12) display)
  (declare (ignore display))
  (let* ((map (read-tilemap "./res/test-tilemap.tmx"))
         (tileset (first (map-tilesets map)))
         (frames (make-tileset-texture-frames tileset)))
    (setf (test12-tmx-map self) map
          (test12-frames self) frames)))

(defmethod cl-user::step-contents ((self test12) dt)
  (declare (ignore dt))
  (let ((x 100)
        (y 250)
        (frames (test12-frames self)))
    (loop for frame across frames
       for i upfrom 0
       when frame
       do (draw-texture-frame frame (+ x (* i 50)) y))))


(deftest tilemap-test-0 ()
  (cl-user::display-contents (make-test12) :width 500 :height 500))

(defstruct test13
  tmx-map
  tileset
  frames
  layer)

(defmethod cl-user::contents-will-mount ((self test13) display)
  (declare (ignore display))
  (let* ((map (read-tilemap "./res/test-tilemap.tmx"))
         (tileset (first (map-tilesets map)))
         (frames (make-tileset-texture-frames tileset))
         (layer (first (map-layers map))))
    (setf (test13-tmx-map self) map
          (test13-tileset self) tileset
          (test13-frames self) frames
          (test13-layer self) layer)))


(defmethod cl-user::step-contents ((self test13) dt)
  (declare (ignore dt))
  (let ((x 250)
        (y 250)
        (tileset (test13-tileset self))
        (frames (test13-frames self))
        (layer (test13-layer self)))
    (draw-tmx-layer
     x y
     (tileset-tile-width tileset)
     (tileset-tile-height tileset)
     layer
     frames)))

(deftest tilemap-test-1 ()
  (cl-user::display-contents (make-test13) :width 500 :height 500))

(defstruct test14
  renderer)

(defmethod cl-user::contents-will-mount ((self test14) display)
  (declare (ignore display))
  (setf (test14-renderer self) (tmx-renderer-from-file "./res/test-tilemap.tmx")))

(defmethod cl-user::step-contents ((self test14) dt)
  (declare (ignore dt))
  (let* ((r (test14-renderer self))
         (x (/ (tmx-renderer-width r) 2.0))
         (y (/ (tmx-renderer-height r) 2.0)))
    (draw-tmx-renderer x y r)))

(deftest tmx-renderer ()
  (cl-user::display-contents (make-test14) :width 500 :height 500))

(defstruct test15
  node started)

(defmethod cl-user::contents-will-mount ((self test15) display)
  (declare (ignore display))
  (let ((n (make-instance 'node :x 250 :y 250)))
    (setf (test15-node self) n)
    (run-action
     n
     (list
      (move-by 3.0 -100.0 0)
      (move-by 3.0 100.0 0))
     :repeat :forever :tag 'moving)
    (run-action
     n
     (rotate-by 5.0 360.0)
     :repeat :forever :tag 'rotating)
    (run-action
     n
     (list
      (delay 3.0)
      (callfunc (lambda ()
                  (stop-all-actions n :tag 'moving)))))))

(defmethod cl-user::step-contents ((self test15) dt)
  (declare (ignorable dt))
  (unless (test15-started self)
    (setf (test15-started self) t)
    (on-enter (test15-node self)))
  (visit (test15-node self)))

(deftest action-tags ()
  (cl-user::display-contents (make-test15) :width 500 :height 500))

(defstruct test16
  node started)

(defmethod cl-user::contents-will-mount ((self test16) display)
  (declare (ignore display))
  (texture-packer-add-frames-from-file "./res/test.json")
  (add-animation 'cat (/ 1.0 7.5) '("catwalk0.png" "catwalk1.png"))
  (let ((sprite (make-instance 'sprite
                               :x 250
                               :y 250
                               :scale-x 2.0
                               :scale-y 2.0
                               :sprite-frame (get-frame "pickle.png")
                               :anchor-x 0.0
                               :anchor-y 0.0)))
    (run-animation sprite 'cat :repeat :forever)
    (setf (rotation sprite) -15)
    (setf (test16-node self) sprite)))

(defmethod cl-user::step-contents ((self test16) dt)
  (declare (ignorable dt))
  (unless (test16-started self)
    (setf (test16-started self) t)
    (on-enter (test16-node self)))
  (visit (test16-node self)))

(deftest animation ()
  (cl-user::display-contents (make-test16) :width 500 :height 500))

(defclass box (rect)
  ((vx :accessor box-vx :initarg :vx)
   (vy :accessor box-vy :initarg :vy)))

(defstruct test17
  root started nodes
  (qtree (qtree))
  (mouse-x 0.0)
  (mouse-y 0.0))

(defmethod cl-user::contents-will-mount ((self test17) display)
  (declare (ignore display))
  (setf (test17-root self) (make-instance 'node))
  (let (nodes)
    (loop repeat 100 do
         (let ((n (make-instance 'box
                                 :content-width 20
                                 :content-height 20
                                 :opacity 0.5
                                 :x (random 500)
                                 :y (random 500)
                                 :vx (- (/ (random 100) 100.0) 0.5)
                                 :vy (- (/ (random 100) 100.0) 0.5))))
           (add-child (test17-root self) n)
           (push n nodes)))
    (setf (test17-nodes self) nodes)))

(defun draw-marker (rect)
  (xmas.render-buffer::draw-rect (x rect)
                                 (y rect)
                                 5.0 5.0))

(defmethod cl-user::step-contents ((self test17) dt)
  (unless (test17-started self)
    (setf (test17-started self) t)
    (on-enter (test17-root self)))
  (qtree-reset (test17-qtree self) :x 250 :y 250 :width 500 :height 500)
  (dolist (box (test17-nodes self))
    (incf (x box) (* 100 dt (box-vx box)))
    (incf (y box) (* 100 dt (box-vy box)))
    (when (or (< (x box) 0.0) (> (x box) 500.0))
      (setf (box-vx box) (* -1.0 (box-vx box)))
      (clampf (x box) 0.0 500.0))
    (when (or (< (y box) 0.0) (> (y box) 500.0))
      (setf (box-vy box) (* -1.0 (box-vy box)))
      (clampf (y box) 0.0 500.0))
    (qtree-add (test17-qtree self) box))
  (xmas.render-buffer::set-color 1.0 1.0 1.0 0.1)
  (qtree-map-nodes
   (test17-qtree self)
   (lambda (x y w h items)
     (declare (ignore items))
     (xmas.render-buffer::draw-rect
      (- x (/ w 2.0)) (- y (/ h 2.0)) (- w 4.0) (- h 4.0))))
  (visit (test17-root self))
  (xmas.render-buffer::set-color 1.0 0.0 0.0 1)
  (xmas.render-buffer::draw-rect (test17-mouse-x self)
                                 (test17-mouse-y self)
                                 5.0 5.0)
  (let ((x (test17-mouse-x self))
        (y (test17-mouse-y self)))
    (xmas.render-buffer::set-color 0.0 1.0 0.0 1)
    (qtree-query
     (test17-qtree self) x y (+ x 5) (+ y 5) 'draw-marker)
    (xmas.render-buffer::set-color 0.0 0.0 1.0 1)
    (qtree-query-collisions
     (test17-qtree self) x y (+ x 5) (+ y 5) 'draw-marker)))

(defmethod cl-user::handle-event ((self test17) event)
  (when (eq (car event) :mousemove)
    (setf (test17-mouse-x self) (cadr event)
          (test17-mouse-y self) (cddr event))))

(deftest quadtree-test ()
  (cl-user::display-contents (make-test17) :width 500 :height 500))


(defstruct test18
  font
  font2)

(defmethod cl-user::contents-will-mount ((self test18) display)
  (declare (ignore display))
  (setf (test18-font self) (lfont-from-file "./res/lfont/november.lfont")
        (test18-font2 self) (lfont-from-file "./res/lfont/OpenSans-Light.lfont")))

(defmethod cl-user::step-contents ((self test18) dt)
  (declare (ignore dt))
  (lfont-draw-string (test18-font self)
                     "Hello, world!"
                     50.0 50.0)
  (lfont-draw-string (test18-font2 self)
                     "The quick brown fox jumped over the lazy dog."
                     10.0 150.0 :letter-spacing 0.0))

(deftest text-rendering ()
  (cl-user::display-contents (make-test18) :width 500 :height 500))


(defstruct test19
  started root)

(defmethod cl-user::contents-will-mount ((self test19) display)
  (declare (ignore display))
  (with-struct (test19- root) self
    (flet ((make-rect (&optional (ax (/ (random 150) 100.0))
                                 (ay (/ (random 150) 100.0)))
             (let ((r (make-instance 'rect
                                     :content-width 40
                                     :content-height 40
                                     :anchor-x ax
                                     :anchor-y ay
                                     :x 250
                                     :y 250
                                     :opacity 0.5)))
               (prog1 r
                 (add-child root r)
                 (run-action r
                             (list
                              (delay 1.0)
                              (rotate-by 3.0 360))
                             :repeat :forever)))))
      (setf root (make-instance 'node))
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
         (rotation f) 0.0)))))

(defmethod cl-user::step-contents ((self test19) dt)
  (declare (ignore dt))
  (with-struct (test19- root started) self
    (unless started
      (setf started t)
      (on-enter root))
    (visit root)))

(deftest anchor-point-test-0 ()
  (cl-user::display-contents (make-test19) :width 500 :height 500))

(defstruct test20
  sprite1 sprite2 sprite3 started)

(defmethod cl-user::contents-will-mount ((self test20) display)
  (declare (ignore display))
  (texture-packer-add-frames-from-file "./res/test.json")
  (setf (test20-sprite1 self)
        (make-instance 'sprite :x 150 :y 250
                       :sprite-frame (get-frame "pickle.png"))
        (test20-sprite2 self)
        (make-instance 'sprite :x 250 :y 250
                       :content-width 40
                       :content-height 60
                       :sprite-frame (get-frame "pickle.png"))
        (test20-sprite3 self)
        (make-instance 'sprite :x 400 :y 250
                       :content-width 100
                       :content-height 150
                       :sprite-frame (get-frame "pickle.png"))))

(defmethod cl-user::step-contents ((self test20) dt)
  (declare (ignorable dt))
  (flet ((draw-it (s)
           (xmas.render-buffer::set-color 1.0 1.0 1.0 1.0)
           (xmas.render-buffer::draw-rect (left s) (bottom s) (width s) (height s))
           (visit s)))
    (draw-it (test20-sprite1 self))
    (draw-it (test20-sprite2 self))
    (draw-it (test20-sprite3 self))))

(deftest anchor-point-test-1 ()
  (cl-user::display-contents (make-test20) :width 500 :height 500))

