(defpackage :xmas.render-buffer-tests)
(in-package :xmas.render-buffer-tests)

(defvar *tests* (make-hash-table :test 'equal))

(defmacro deftest (name (&key) &body body)
  `(setf (gethash ',name *tests*) (lambda () ,@body)))

(defun run-test (name) (funcall (gethash name *tests*)))

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
    (set-color r g b a)
    (draw-rect x y 20 20)))

(defstruct test3
  (boxes (loop repeat 4000 collect (make-bouncy-box)))
  width
  height)

(defmethod cl-user::contents-will-mount ((self test3) display)
  (with-slots (boxes width height) self
    (setf width  (xmas.display:display-width display)
          height (xmas.display:display-height display))
    (dolist (box boxes)
      (setf (bouncy-box-x box) (random (- width 20))
            (bouncy-box-y box) (random (- height 20))))))

(defmethod cl-user::step-contents ((self test3) dt)
  (with-slots (boxes width height) self
    (let ((maxx (- width 20))
          (maxy (- height 20)))
      (dolist (box boxes)
        (update-bouncing-box box maxx maxy dt)))
    (map nil 'draw-bouncing-box boxes)))

(defmethod cl-user::contents-will-unmount ((self test3) display)
  (declare (ignorable self display)))

(defmethod cl-user::handle-event ((self test3) event)
  (case (car event)
    (:resize
     (let ((w (cadr event))
           (h (cddr event)))
       (setf (test3-width self) w
             (test3-height self) h)))
    (t (format t "got unhandled event: ~S~%" event))))

(defvar *my-random-state* (make-random-state t))

(deftest bouncy-balls ()
  (let ((*random-state* *my-random-state*))
    (cl-user::display-contents (make-test3)
                               :width 500
                               :height 500
                               :expandable t)))

(defstruct test4
  alien)

(defmethod cl-user::contents-will-mount ((self test4) display)
  (declare (ignorable display))
  (let ((texture (xmas.texture:get-texture #P"./alien.png")))
    (assert (xmas.texture:texture-width texture))
    (assert (xmas.texture:texture-height texture))
    (setf (test4-alien self) texture))
  (unless (test4-alien self)
    (format t "missing texture!")))

(defmethod cl-user::step-contents ((self test4) dt)
  (declare (ignorable dt))
  (xmas.draw:draw-texture (test4-alien self)))

(deftest draw-texture ()
  (cl-user::display-contents (make-test4)))

(defstruct sprite
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
  (push-matrix)
  (translate-scale-rotate (sprite-x sprite) (sprite-y sprite)
                          (sprite-sx sprite) (sprite-sy sprite)
                          (sprite-r sprite))
  (set-color 1.0 1.0 1.0 1.0)
  (xmas.draw:draw-texture (sprite-texture sprite))
  (pop-matrix))

(defstruct test5
  width
  height
  texture
  sprites)

(defmethod cl-user::contents-will-mount ((self test5) display)
  (let ((width (xmas.display:display-width display))
        (height (xmas.display:display-height display))
        (texture (xmas.texture:get-texture #P"./alien.png")))
    (unless texture
      (format t "missing texture!"))
    (setf (test5-width self) width)
    (setf (test5-height self) height)
    (setf (test5-texture self) texture)
    (setf (test5-sprites self)
          (loop repeat 4000 collect
               (make-sprite :x (random width)
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
  (matrix (xmas.matrix:make-matrix))
  (scratch-matrix (xmas.matrix:make-matrix)))

(defmethod cl-user::contents-will-mount ((self test6) display)
  (declare (ignorable display))
  (let ((m (test6-matrix self)))
    (xmas.matrix:into-matrix (m)
      (xmas.matrix:load-identity)
      (xmas.matrix:translate 125.0 125.0))))

(defmethod cl-user::step-contents ((self test6) dt)
  (declare (ignorable dt))
  (let ((xmas.matrix:*tmp-matrix* (test6-scratch-matrix self)))
    (xmas.matrix:into-matrix ((test6-matrix self))
      (xmas.matrix:rotate (* 40 dt))))
  (set-color 0.0 1.0 0.0 0.5)
  (draw-rect 10.0 10.0 20.0 20.0)
  (push-matrix)
  (mult-matrix (xmas.matrix:unwrap-matrix (test6-matrix self)))
  (draw-rect 10.0 10.0 20.0 20.0)
  (draw-rect 30.0 30.0 20.0 20.0)
  (pop-matrix))

(deftest matrix-translation ()
  (cl-user::display-contents (make-test6)))

(defun draw-node-color (node)
  (let ((c (xmas.node:color node)) (a (xmas.node:opacity node)))
    (xmas.render-buffer::set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defmethod draw ((self xmas.node:node))
  (draw-node-color self)
  (xmas.render-buffer::draw-rect -20.0 -20.0 40.0 40.0))

(defmethod draw ((self xmas.sprite:sprite))
  (draw-node-color self)
  (xmas.draw:draw-texture-frame (xmas.sprite:sprite-frame self) 0.0 0.0))

;; the fast method:
(defmethod visit ((self xmas.node:node))
  (xmas.render-buffer::push-matrix)
  (xmas.render-buffer::translate-scale-rotate
   (xmas.node:x self) (xmas.node:y self)
   (xmas.node:scale-x self) (xmas.node:scale-y self)
   (xmas.node:rotation self))
  (draw self)
  (when (xmas.node:children self)
    (loop for child across (xmas.node:children self) do
         (visit child)))
  (xmas.render-buffer::pop-matrix))

;; the also-fast method:
;; (defmethod visit ((self xmas.node:node))
;;   (push-matrix)
;;   (translate-scale-rotate (xmas.node:x self) (xmas.node:y self)
;;                           (xmas.node:scale-x self) (xmas.node:scale-y self)
;;                           (xmas.node:rotation self))
;;   (xmas.node:node-transform self) ;; <- note this addition
;;   (draw self)
;;   (pop-matrix))

;; this method also performs fairly well (some minor stuttering),
;; seeming to indicate that the bottleneck is actually in
;; %gl:mult-matrix-f
;; (defmethod visit ((self xmas.node:node))
;;   (push-matrix)
;;   (translate-scale-rotate (xmas.node:x self) (xmas.node:y self)
;;                           (xmas.node:scale-x self) (xmas.node:scale-y self)
;;                           (xmas.node:rotation self))
;;   (mult-matrix-noop (matrix:unwrap-matrix (xmas.node:node-transform self)))
;;   (draw self)
;;   ;;visit children here.
;;   (pop-matrix))

;; the slow as sin method.
;; (defmethod visit ((self xmas.node:node))
;;   (push-matrix)
;;   (mult-matrix (matrix:unwrap-matrix (xmas.node:node-transform self)))
;;   (draw self)
;;   (when (xmas.node:children self)
;;     (loop for child across (xmas.node:children self) do
;;          (visit child)))
;;   (pop-matrix))

(defstruct test7
  nodes
  width
  height
  root-node)

(defmethod cl-user::contents-will-mount ((self test7) display)
  (with-slots (nodes width height root-node) self
    (let* ((w (xmas.display:display-width display))
           (h (xmas.display:display-height display))
           (w/2 (/ w 2))
           (h/2 (/ h 2))
           (diagonal (sqrt (+ (* w w) (* h h))))
           (d/2 (/ diagonal 2)))
      (setf width w
            height h
            root-node (make-instance 'xmas.node:node
                                     :x w/2
                                     :y h/2)
            nodes
            (loop
               repeat 4000 collect
                 (make-instance 'xmas.node:node
                                :x (coerce (- (random diagonal) d/2) 'float)
                                :y (coerce (- (random diagonal) d/2)'float)
                                :rotation (coerce (random 360) 'float)
                                :color (vector
                                        (/ (random 100) 100.0)
                                        (/ (random 100) 100.0)
                                        (/ (random 100) 100.0))
                                :opacity (/ (random 100) 100.0)))))
    (dolist (child nodes)
      (xmas.node:add-child root-node child))))

(defmethod cl-user::step-contents ((self test7) dt)
  (declare (ignorable dt))
  (dolist (node (cons (test7-root-node self) (test7-nodes self)))
    (let ((r (xmas.node:rotation node)))
      (incf r (* dt 100))
      (setf r (mod r 360))
      (setf (xmas.node:rotation node) r)))
  (set-color 0.0 1.0 1.0 0.4)
  (visit (test7-root-node self)))

(deftest visit-and-draw-many-nodes ()
  (cl-user::display-contents (make-test7) :width 500 :height 500))


(defstruct test8
  node
  started)

(defmethod cl-user::contents-will-mount ((self test8) display)
  (let ((node (make-instance 'xmas.node:node
                             :x (/ (xmas.display:display-width display) 2)
                             :y (/ (xmas.display:display-height display) 2)))
        (node2 (make-instance 'xmas.node:node
                              :color (vector 0.0 1.0 1.0)
                              :opacity 0.4
                              :x 20.0
                              :y 20.0))
        (node3 (make-instance 'xmas.node:node
                              :color (vector 1.0 0.0 0.0)
                              :opacity 0.4
                              :x -20.0
                              :y -20.0))
        (node4 (make-instance 'xmas.node:node
                              :color (vector 0.0 1.0 0.0)
                              :opacity 0.4
                              :x  20.0
                              :y -20.0))
        (node5 (make-instance 'xmas.node:node
                              :color (vector 1.0 1.0 0.0)
                              :opacity 0.4
                              :x -20.0
                              :y  20.0)))
    (xmas.node:run-action node
                     (xmas.action:repeat-forever
                      (xmas.action:run-sequence
                       (xmas.action:delay 1.0)
                       (xmas.action:ease-in-out-quad
                        (xmas.action:rotate-by 1.5 180.0))
                       (xmas.action:delay 1.0)
                       (xmas.action:ease-in-out-sine
                        (xmas.action:rotate-by 1.25 -90.0)))))
    (xmas.node:run-action node2
                     (xmas.action:repeat-forever
                      (xmas.action:rotate-by 2.5 -360.0)))
    (xmas.node:run-action node3
                     (xmas.action:repeat-forever
                      (xmas.action:run-sequence
                       (xmas.action:rotate-by 0.25 -60.0)
                       (xmas.action:rotate-by 0.25 60.0))))
    (xmas.node:run-action node4
                     (xmas.action:repeat-forever
                      (xmas.action:run-sequence
                       (xmas.action:rotate-by 0.5 -60.0)
                       (xmas.action:rotate-by 0.25 -60.0)
                       (xmas.action:rotate-by 0.25 60.0))))
    (xmas.node:run-action node5
                     (xmas.action:repeat-forever
                      (xmas.action:run-sequence
                       (xmas.action:rotate-by 0.5 -60.0)
                       (xmas.action:rotate-by 0.5 60.0)
                       (xmas.action:rotate-by 0.5 -360.0)
                       (xmas.action:rotate-by 0.5 360.0))))
    (xmas.node:add-child node node2)
    (xmas.node:add-child node node3)
    (xmas.node:add-child node node4)
    (xmas.node:add-child node node5)
    (xmas.node:run-action
     node
     (xmas.action:repeat-forever
      (xmas.action:run-sequence
       (xmas.action:callfunc (lambda () (format t " tick! ")))
       (xmas.action:delay 2.0))))
    (xmas.node:run-action
     node
     (xmas.action:repeat-forever
      (xmas.action:run-sequence
       (xmas.action:delay 1.0)
       (xmas.action:callfunc (lambda () (format t " tock! ")))
       (xmas.action:delay 1.0))))
    (xmas.node:run-action
     node
     (xmas.action:run-sequence
      (xmas.action:delay 3.0)
      (xmas.action:callfunc (lambda () (xmas.node:remove-from-parent node2)))))
    (setf (test8-node self) node)))

(defmethod cl-user::step-contents ((self test8) dt)
  (declare (ignorable dt))
  (unless (test8-started self)
    (setf (test8-started self) t)
    (xmas.node:on-enter (test8-node self)))
  (visit (test8-node self)))

(deftest node-actions ()
  (cl-user::display-contents (make-test8)))


(defstruct test9
  node
  started)

(defun test9-add-node (self)
  (let ((node (make-instance 'xmas.node:node))
        (root (test9-node self)))
    (xmas.node:run-action
     node
     (list (xmas.action:move-by 3.0 -250.0 -250.0)
           (xmas.action:callfunc (lambda () (xmas.node:remove-from-parent node)))))
    (xmas.node:run-action
     root
     (list
      (xmas.action:delay 4.0)
      (xmas.action:callfunc
       (lambda ()
         (xmas.node:add-child root node)
         (format t "~S ~%" (mod (/ (get-internal-real-time) (coerce internal-time-units-per-second 'float)) 60))
         (test9-add-node self)))))))

(defmethod cl-user::contents-will-mount ((self test9) display)
  (let ((node (make-instance 'xmas.node:node
                             :x (/ (xmas.display:display-width display) 2)
                             :y (/ (xmas.display:display-height display) 2))))
    (setf (test9-node self) node)
    (test9-add-node self)))

(defmethod cl-user::step-contents ((self test9) dt)
  (declare (ignorable dt))
  (unless (test9-started self)
    (setf (test9-started self) t)
    (xmas.node:on-enter (test9-node self)))
  (visit (test9-node self)))

(deftest action-manager-test ()
  (cl-user::display-contents (make-test9)))


(defstruct test10
  a b c d)

(defmethod cl-user::contents-will-mount ((self test10) display)
  (declare (ignore display))
  (let ((tex (xmas.texture:get-texture "./bayarea.png")))
    (setf (test10-a self) (xmas.texture:texture-frame tex 0.0 0.0 250.0 250.0))
    (setf (test10-b self) (xmas.texture:texture-frame tex 0.0 250.0 250.0 250.0))
    (setf (test10-c self) (xmas.texture:texture-frame tex 250.0 0.0 250.0 250.0))
    (setf (test10-d self) (xmas.texture:texture-frame tex 250.0 250.0 250.0 250.0)) ))


(defmethod cl-user::step-contents ((self test10) dt)
  (declare (ignore dt))
  (push-matrix)
  (xmas.draw:draw-texture-frame (test10-a self) 125.0 125.0)
  (xmas.draw:draw-texture-frame (test10-b self) 125.0 (+ 250.0 125.0))
  (xmas.draw:draw-texture-frame (test10-c self) (+ 250.0 125.0) 125.0)
  (xmas.draw:draw-texture-frame (test10-d self) (+ 250.0 125.0) (+ 250.0 125.0))
  (pop-matrix))

(deftest texture-frame-test ()
  (cl-user::display-contents (make-test10) :width 500 :height 500))

(defstruct test11
  packer normal-frame blink-frame jewel)

(defmethod cl-user::contents-will-mount ((self test11) display)
  (declare (ignore display))
  (let ((packed (xmas.texture-packer:texture-packer-from-file "./res/test.json")))
    (assert (xmas.texture-packer::texture-packer-file-texture packed))
    (setf (test11-packer self) packed
          (test11-normal-frame self) (xmas.texture-packer:texture-packer-get-frame
                                      packed "pickle.png")
          (test11-blink-frame self) (xmas.texture-packer:texture-packer-get-frame
                                     packed "pickle blink.png")
          (test11-jewel self) (xmas.texture-packer:texture-packer-get-frame
                                     packed "jewel.png"))))

(defmethod cl-user::step-contents ((self test11) dt)
  (declare (ignore dt))
  (xmas.draw:draw-texture-frame (test11-normal-frame self) 125.0 250.0)
  (xmas.draw:draw-texture-frame (test11-blink-frame self) 375.0 250.0)
  (xmas.draw:draw-texture-frame (test11-jewel self) 250.0 250.0))

(deftest texture-packer-test ()
  (cl-user::display-contents (make-test11) :width 500 :height 500))

(defstruct test12
  tmx-map
  frames)

(defmethod cl-user::contents-will-mount ((self test12) display)
  (declare (ignore display))
  (let* ((map (xmas.tmx-reader:read-tilemap "./res/test-tilemap.tmx"))
         (tileset (first (xmas.tmx-reader:map-tilesets map)))
         (frames (xmas.tmx-renderer:make-tileset-texture-frames tileset)))
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
       do (xmas.draw:draw-texture-frame frame (+ x (* i 50)) y))))


(deftest tilemap-test-0 ()
  (cl-user::display-contents (make-test12) :width 500 :height 500))

(defstruct test13
  tmx-map
  tileset
  frames
  layer)

(defmethod cl-user::contents-will-mount ((self test13) display)
  (declare (ignore display))
  (let* ((map (xmas.tmx-reader:read-tilemap "./res/test-tilemap.tmx"))
         (tileset (first (xmas.tmx-reader:map-tilesets map)))
         (frames (xmas.tmx-renderer:make-tileset-texture-frames tileset))
         (layer (first (xmas.tmx-reader:map-layers map))))
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
    (xmas.tmx-renderer:draw-tmx-layer
     x y
     (xmas.tmx-reader:tileset-tile-width tileset)
     (xmas.tmx-reader:tileset-tile-height tileset)
     layer
     frames)))

(deftest tilemap-test-1 ()
  (cl-user::display-contents (make-test13) :width 500 :height 500))

(defstruct test14
  renderer)

(defmethod cl-user::contents-will-mount ((self test14) display)
  (declare (ignore display))
  (setf (test14-renderer self) (xmas.tmx-renderer:tmx-renderer-from-file "./res/test-tilemap.tmx")))

(defmethod cl-user::step-contents ((self test14) dt)
  (declare (ignore dt))
  (let* ((r (test14-renderer self))
         (x (/ (xmas.tmx-renderer:tmx-renderer-width r) 2.0))
         (y (/ (xmas.tmx-renderer:tmx-renderer-height r) 2.0)))
    (xmas.tmx-renderer:draw-tmx-renderer x y r)))

(deftest tmx-renderer ()
  (cl-user::display-contents (make-test14) :width 500 :height 500))

(defstruct test15
  node started)

(defmethod cl-user::contents-will-mount ((self test15) display)
  (declare (ignore display))
  (let ((n (make-instance 'xmas.node:node :x 250 :y 250)))
    (setf (test15-node self) n)
    (xmas.node:run-action
     n
     (list
      (xmas.action:move-by 3.0 -100.0 0)
      (xmas.action:move-by 3.0 100.0 0))
     :repeat :forever :tag 'moving)
    (xmas.node:run-action
     n
     (xmas.action:rotate-by 5.0 360.0)
     :repeat :forever :tag 'rotating)
    (xmas.node:run-action
     n
     (list
      (xmas.action:delay 3.0)
      (xmas.action:callfunc (lambda ()
                         (xmas.node:stop-all-actions n :tag 'moving)))))))

(defmethod cl-user::step-contents ((self test15) dt)
  (declare (ignorable dt))
  (unless (test15-started self)
    (setf (test15-started self) t)
    (xmas.node:on-enter (test15-node self)))
  (visit (test15-node self)))

(deftest action-tags ()
  (cl-user::display-contents (make-test15) :width 500 :height 500))

(defstruct test16
  node started)

(defmethod cl-user::contents-will-mount ((self test16) display)
  (declare (ignore display))
  (xmas.texture-packer:texture-packer-add-frames-from-file "./res/test.json")
  (xmas.animation-manager:add-animation 'cat (/ 1.0 7.5) '("catwalk0.png" "catwalk1.png"))
  (let ((sprite (make-instance 'xmas.sprite:sprite
                               :x 250
                               :y 250
                               :scale-x 2.0
                               :scale-y 2.0
                               :sprite-frame (xmas.texture:get-frame "pickle.png"))))
    (xmas.sprite:run-animation sprite 'cat :repeat :forever)
    (setf (xmas.node:rotation sprite) -15)
    (setf (test16-node self) sprite)))

(defmethod cl-user::step-contents ((self test16) dt)
  (declare (ignorable dt))
  (unless (test16-started self)
    (setf (test16-started self) t)
    (xmas.node:on-enter (test16-node self)))
  (visit (test16-node self)))

(deftest animation ()
  (cl-user::display-contents (make-test16) :width 500 :height 500))

(defstruct test17
  root started nodes
  (qtree (xmas.qtree:qtree)))

(defmethod cl-user::contents-will-mount ((self test17) display)
  (declare (ignore display))
  (setf (test17-root self) (make-instance 'xmas.node:node))
  (let (nodes)
    (loop repeat 10 do
         (let ((n (make-instance 'xmas.node:node
                                 :x (random 500)
                                 :y (random 500))))
           (xmas.node:add-child (test17-root self) n)
           (push n nodes)))
    (setf (test17-nodes self) nodes)))

(defmethod cl-user::step-contents ((self test17) dt)
  (declare (ignorable dt))
  (unless (test17-started self)
    (setf (test17-started self) t)
    (xmas.node:on-enter (test17-root self)))
  (xmas.qtree:qtree-reset (test17-qtree self) :x 250 :y 250 :width 500 :height 500)
  (dolist (node (test17-nodes self))
    (xmas.qtree:qtree-add (test17-qtree self) node))
  (visit (test17-root self)))

(cl-user::display-contents (make-test17) :width 500 :height 500)
