(in-package :render-buffer)

(definstr set-color (r g b a)
  (gl:color r g b a))

(definstr draw-rect (x y w h)
  (gl:rect x y (+ x w) (+ y h)))

(definstr simple-draw-gl-texture (id w h)
  (gl:bind-texture :texture-2d id)
  (gl:color 1 1 1)
  (let* ((x (- (/ w 2)))
         (y (- (/ h 2)))
         (x2 (+ x w))
         (y2 (+ y h)))
    (gl:with-primitive :quads
      (gl:tex-coord 0  1)
      (gl:vertex    x  y  0)
      (gl:tex-coord 1  1)
      (gl:vertex    x2 y  0)
      (gl:tex-coord 1  0)
      (gl:vertex    x2 y2 0)
      (gl:tex-coord 0  0)
      (gl:vertex    x  y2 0))))

(definstr simple-draw-gl-texture-no-color (id w h)
  (gl:bind-texture :texture-2d id)
  (let* ((x (- (/ w 2)))
         (y (- (/ h 2)))
         (x2 (+ x w))
         (y2 (+ y h)))
    (gl:with-primitive :quads
      (gl:tex-coord 0  1)
      (gl:vertex    x  y  0)
      (gl:tex-coord 1  1)
      (gl:vertex    x2 y  0)
      (gl:tex-coord 1  0)
      (gl:vertex    x2 y2 0)
      (gl:tex-coord 0  0)
      (gl:vertex    x  y2 0))))

(definstr simple-draw-gl-with-tex-coords (id x y w h tx1 ty1 tx2 ty2)
  (gl:bind-texture :texture-2d id)
  (let* ((x (+ x (- (/ w 2))))
         (y (+ y (- (/ h 2))))
         (x2 (+ x w))
         (y2 (+ y h)))
    (gl:with-primitive :quads
      (gl:tex-coord tx1  ty2)
      (gl:vertex    x  y  0)
      (gl:tex-coord tx2  ty2)
      (gl:vertex    x2 y  0)
      (gl:tex-coord tx2  ty1)
      (gl:vertex    x2 y2 0)
      (gl:tex-coord tx1  ty1)
      (gl:vertex    x  y2 0))))

(definstr simple-draw-gl-with-tex-coords-rotated (id x y w h tx1 ty1 tx2 ty2)
  (gl:bind-texture :texture-2d id)
  (rotatef w h)
  (let* ((x (+ x (- (/ w 2))))
         (y (+ y (- (/ h 2))))
         (x2 (+ x w))
         (y2 (+ y h)))
    (gl:with-primitive :quads
      (gl:tex-coord tx1  ty2)
      (gl:vertex    x2 y  0)
      (gl:tex-coord tx2  ty2)
      (gl:vertex    x2 y2 0)
      (gl:tex-coord tx2  ty1)
      (gl:vertex    x  y2 0)
      (gl:tex-coord tx1  ty1)
      (gl:vertex    x  y  0))))

(definstr push-matrix ()
  (gl:push-matrix))

(definstr pop-matrix ()
  (gl:pop-matrix))

(definstr-vec mult-matrix (vec)
  (cffi:with-foreign-object (matrix '%gl:float 16)
    (let ((i 0))
      (do-vec! (val)
        (setf (cffi:mem-aref matrix '%gl:float i) val)
        (incf i))
      (%gl:mult-matrix-f matrix))))

;; test method for node transforms
;; (definstr-vec mult-matrix-noop (vec)
;;   (cffi:with-foreign-object (matrix '%gl:float 16)
;;     (let ((i 0))
;;       (do-vec! (val)
;;         (setf (cffi:mem-aref matrix '%gl:float i) val)
;;         (incf i))
;;       ;;(%gl:mult-matrix-f matrix)
;;       )))

(definstr translate-scale-rotate (x y sx sy r)
  (gl:translate x y 0.0)
  (gl:scale sx sy 1.0)
  (gl:rotate r 0.0 0.0 1.0))

(defun draw-texture (texture)
  (when (texture:texture-id texture)
    (simple-draw-gl-texture-no-color
     (texture:texture-id texture)
     (texture:texture-width texture)
     (texture:texture-height texture))))


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
    (setf width  (display:display-width display)
          height (display:display-height display))
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

(let ((*random-state* *my-random-state*))
  (cl-user::display-contents (make-test3)
                             :width 500
                             :height 500
                             :expandable t))

(defstruct test4
  alien)

(defmethod cl-user::contents-will-mount ((self test4) display)
  (declare (ignorable display))
  (let ((texture (texture:get-texture #P"./alien.png")))
    (assert (texture:texture-width texture))
    (assert (texture:texture-height texture))
    (setf (test4-alien self) texture))
  (unless (test4-alien self)
    (format t "missing texture!")))

(defmethod cl-user::step-contents ((self test4) dt)
  (declare (ignorable dt))
  (draw-texture (test4-alien self)))

(cl-user::display-contents (make-test4))


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
  (draw-texture (sprite-texture sprite))
  (pop-matrix))

(defstruct test5
  width
  height
  texture
  sprites)

(defmethod cl-user::contents-will-mount ((self test5) display)
  (let ((width (display:display-width display))
        (height (display:display-height display))
        (texture (texture:get-texture #P"./alien.png")))
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

(cl-user::display-contents (make-test5) :width 500 :height 500 :expandable t)

(defstruct test6
  (matrix (matrix:make-matrix))
  (scratch-matrix (matrix:make-matrix)))

(defmethod cl-user::contents-will-mount ((self test6) display)
  (declare (ignorable display))
  (let ((m (test6-matrix self)))
    (matrix:into-matrix (m)
      (matrix:load-identity)
      (matrix:translate 125.0 125.0))))

(defmethod cl-user::step-contents ((self test6) dt)
  (declare (ignorable dt))
  (let ((matrix:*tmp-matrix* (test6-scratch-matrix self)))
    (matrix:into-matrix ((test6-matrix self))
      (matrix:rotate (* 40 dt))))
  (set-color 0.0 1.0 0.0 0.5)
  (draw-rect 10.0 10.0 20.0 20.0)
  (push-matrix)
  (mult-matrix (matrix:unwrap-matrix (test6-matrix self)))
  (draw-rect 10.0 10.0 20.0 20.0)
  (draw-rect 30.0 30.0 20.0 20.0)
  (pop-matrix))

(cl-user::display-contents (make-test6))

(defun draw-node-color (node)
  (let ((c (node:color node)) (a (node:opacity node)))
    (set-color (svref c 0) (svref c 1) (svref c 2) a)))

(defmethod draw ((self node:node))
  (draw-node-color self)
  (draw-rect -20.0 -20.0 40.0 40.0))

;; the fast method:
(defmethod visit ((self node:node))
  (push-matrix)
  (translate-scale-rotate (node:x self) (node:y self)
                          (node:scale-x self) (node:scale-y self)
                          (node:rotation self))
  (draw self)
  (when (node:children self)
    (loop for child across (node:children self) do
         (visit child)))
  (pop-matrix))

;; the also-fast method:
;; (defmethod visit ((self node:node))
;;   (push-matrix)
;;   (translate-scale-rotate (node:x self) (node:y self)
;;                           (node:scale-x self) (node:scale-y self)
;;                           (node:rotation self))
;;   (node:node-transform self) ;; <- note this addition
;;   (draw self)
;;   (pop-matrix))

;; this method also performs fairly well (some minor stuttering),
;; seeming to indicate that the bottleneck is actually in
;; %gl:mult-matrix-f
;; (defmethod visit ((self node:node))
;;   (push-matrix)
;;   (translate-scale-rotate (node:x self) (node:y self)
;;                           (node:scale-x self) (node:scale-y self)
;;                           (node:rotation self))
;;   (mult-matrix-noop (matrix:unwrap-matrix (node:node-transform self)))
;;   (draw self)
;;   ;;visit children here.
;;   (pop-matrix))

;; the slow as sin method.
;; (defmethod visit ((self node:node))
;;   (push-matrix)
;;   (mult-matrix (matrix:unwrap-matrix (node:node-transform self)))
;;   (draw self)
;;   (when (node:children self)
;;     (loop for child across (node:children self) do
;;          (visit child)))
;;   (pop-matrix))

(defstruct test7
  nodes
  width
  height
  root-node)

(defmethod cl-user::contents-will-mount ((self test7) display)
  (with-slots (nodes width height root-node) self
    (let* ((w (display:display-width display))
           (h (display:display-height display))
           (w/2 (/ w 2))
           (h/2 (/ h 2))
           (diagonal (sqrt (+ (* w w) (* h h))))
           (d/2 (/ diagonal 2)))
      (setf width w
            height h
            root-node (make-instance 'node:node
                                     :x w/2
                                     :y h/2)
            nodes
            (loop
               repeat 4000 collect
                 (make-instance 'node:node
                                :x (coerce (- (random diagonal) d/2) 'float)
                                :y (coerce (- (random diagonal) d/2)'float)
                                :rotation (coerce (random 360) 'float)
                                :color (vector
                                        (/ (random 100) 100.0)
                                        (/ (random 100) 100.0)
                                        (/ (random 100) 100.0))
                                :opacity (/ (random 100) 100.0)))))
    (dolist (child nodes)
      (node:add-child root-node child))))

(defmethod cl-user::step-contents ((self test7) dt)
  (declare (ignorable dt))
  (dolist (node (cons (test7-root-node self) (test7-nodes self)))
    (let ((r (node:rotation node)))
      (incf r (* dt 100))
      (setf r (mod r 360))
      (setf (node:rotation node) r)))
  (set-color 0.0 1.0 1.0 0.4)
  (visit (test7-root-node self)))

(cl-user::display-contents (make-test7) :width 500 :height 500)


(defstruct test8
  node
  started)

(defmethod cl-user::contents-will-mount ((self test8) display)
  (let ((node (make-instance 'node:node
                             :x (/ (display:display-width display) 2)
                             :y (/ (display:display-height display) 2)))
        (node2 (make-instance 'node:node
                              :color (vector 0.0 1.0 1.0)
                              :opacity 0.4
                              :x 20.0
                              :y 20.0))
        (node3 (make-instance 'node:node
                              :color (vector 1.0 0.0 0.0)
                              :opacity 0.4
                              :x -20.0
                              :y -20.0))
        (node4 (make-instance 'node:node
                              :color (vector 0.0 1.0 0.0)
                              :opacity 0.4
                              :x  20.0
                              :y -20.0))
        (node5 (make-instance 'node:node
                              :color (vector 1.0 1.0 0.0)
                              :opacity 0.4
                              :x -20.0
                              :y  20.0)))
    (node:run-action node
                     (action:repeat-forever
                      (action:run-sequence
                       (action:delay 1.0)
                       (action:ease-in-out-quad
                        (action:rotate-by 1.5 180.0))
                       (action:delay 1.0)
                       (action:ease-in-out-sine
                        (action:rotate-by 1.25 -90.0)))))
    (node:run-action node2
                     (action:repeat-forever
                      (action:rotate-by 2.5 -360.0)))
    (node:run-action node3
                     (action:repeat-forever
                      (action:run-sequence
                       (action:rotate-by 0.25 -60.0)
                       (action:rotate-by 0.25 60.0))))
    (node:run-action node4
                     (action:repeat-forever
                      (action:run-sequence
                       (action:rotate-by 0.5 -60.0)
                       (action:rotate-by 0.25 -60.0)
                       (action:rotate-by 0.25 60.0))))
    (node:run-action node5
                     (action:repeat-forever
                      (action:run-sequence
                       (action:rotate-by 0.5 -60.0)
                       (action:rotate-by 0.5 60.0)
                       (action:rotate-by 0.5 -360.0)
                       (action:rotate-by 0.5 360.0))))
    (node:add-child node node2)
    (node:add-child node node3)
    (node:add-child node node4)
    (node:add-child node node5)
    (node:run-action
     node
     (action:repeat-forever
      (action:run-sequence
       (action:callfunc (lambda () (format t " tick! ")))
       (action:delay 2.0))))
    (node:run-action
     node
     (action:repeat-forever
      (action:run-sequence
       (action:delay 1.0)
       (action:callfunc (lambda () (format t " tock! ")))
       (action:delay 1.0))))
    (node:run-action
     node
     (action:run-sequence
      (action:delay 3.0)
      (action:callfunc (lambda () (node:remove-from-parent node2)))))
    (setf (test8-node self) node)))

(defmethod cl-user::step-contents ((self test8) dt)
  (declare (ignorable dt))
  (unless (test8-started self)
    (setf (test8-started self) t)
    (node:on-enter (test8-node self)))
  (visit (test8-node self)))

(cl-user::display-contents (make-test8))


(defstruct test9
  node
  started)

(defun test9-add-node (self)
  (let ((node (make-instance 'node:node))
        (root (test9-node self)))
    (node:run-action
     node
     (list (action:move-by 3.0 -250.0 -250.0)
           (action:callfunc (lambda () (node:remove-from-parent node)))))
    (node:run-action
     root
     (list
      (action:delay 4.0)
      (action:callfunc
       (lambda ()
         (node:add-child root node)
         (format t "~S ~%" (mod (/ (get-internal-real-time) (coerce internal-time-units-per-second 'float)) 60))
         (test9-add-node self)))))))

(defmethod cl-user::contents-will-mount ((self test9) display)
  (let ((node (make-instance 'node:node
                             :x (/ (display:display-width display) 2)
                             :y (/ (display:display-height display) 2))))
    (setf (test9-node self) node)
    (test9-add-node self)))

(defmethod cl-user::step-contents ((self test9) dt)
  (declare (ignorable dt))
  (unless (test9-started self)
    (setf (test9-started self) t)
    (node:on-enter (test9-node self)))
  (visit (test9-node self)))

(cl-user::display-contents (make-test9))



(defun draw-texture-frame (frame x y)
  (when-let (id (texture:texture-id (texture:texture-frame-texture frame)))
    (let ((w (texture:texture-frame-width frame))
          (h (texture:texture-frame-height frame)))
      (if (texture:texture-frame-rotated frame)
          (simple-draw-gl-with-tex-coords-rotated
           id x y w h
           (texture:texture-frame-tx1 frame)
           (texture:texture-frame-ty1 frame)
           (texture:texture-frame-tx2 frame)
           (texture:texture-frame-ty2 frame))
          (simple-draw-gl-with-tex-coords
           id x y w h
           (texture:texture-frame-tx1 frame)
           (texture:texture-frame-ty1 frame)
           (texture:texture-frame-tx2 frame)
           (texture:texture-frame-ty2 frame))))))

(defstruct test10
  a b c d)

(defmethod cl-user::contents-will-mount ((self test10) display)
  (declare (ignore display))
  (let ((tex (texture:get-texture "./bayarea.png")))
    (setf (test10-a self) (texture:texture-frame tex 0.0 0.0 250.0 250.0))
    (setf (test10-b self) (texture:texture-frame tex 0.0 250.0 250.0 250.0))
    (setf (test10-c self) (texture:texture-frame tex 250.0 0.0 250.0 250.0))
    (setf (test10-d self) (texture:texture-frame tex 250.0 250.0 250.0 250.0)) ))


(defmethod cl-user::step-contents ((self test10) dt)
  (declare (ignore dt))
  (push-matrix)
  (draw-texture-frame (test10-a self) 125.0 125.0)
  (draw-texture-frame (test10-b self) 125.0 (+ 250.0 125.0))
  (draw-texture-frame (test10-c self) (+ 250.0 125.0) 125.0)
  (draw-texture-frame (test10-d self) (+ 250.0 125.0) (+ 250.0 125.0))
  (pop-matrix))

(cl-user::display-contents (make-test10) :width 500 :height 500)

(defstruct test11
  packer normal-frame blink-frame jewel)

(defmethod cl-user::contents-will-mount ((self test11) display)
  (declare (ignore display))
  (let ((packed (texture-packer:texture-packer-from-file "./res/test.json")))
    (assert (texture-packer::texture-packer-file-texture packed))
    (setf (test11-packer self) packed
          (test11-normal-frame self) (texture-packer:texture-packer-get-frame
                                      packed "pickle.png")
          (test11-blink-frame self) (texture-packer:texture-packer-get-frame
                                     packed "pickle blink.png")
          (test11-jewel self) (texture-packer:texture-packer-get-frame
                                     packed "jewel.png"))))

(defmethod cl-user::step-contents ((self test11) dt)
  (declare (ignore dt))
  (draw-texture-frame (test11-normal-frame self) 125.0 250.0)
  (draw-texture-frame (test11-blink-frame self) 375.0 250.0)
  (draw-texture-frame (test11-jewel self) 250.0 250.0))

(cl-user::display-contents (make-test11) :width 500 :height 500)

;; -----------------------------------------------------------------------------
;; tmx reader proto/test

(defun make-tileset-texture-frames (tileset)
  (let* ((texture (texture:get-texture (tmx-reader:tileset-source tileset)))
         (tile-width (coerce (tmx-reader:tileset-tile-width tileset) 'float))
         (tile-height (coerce (tmx-reader:tileset-tile-height tileset) 'float))
         (texture-width (texture:texture-width texture))
         (texture-height (texture:texture-height texture))
         (cols (floor texture-width tile-width))
         (rows (floor texture-height tile-height))
         ;;TODO: read this from input
         (first-gid 1)
         (vec (make-array (1+ (* rows cols)) :element-type t :initial-element nil))
         (idx first-gid))
    ;; (format t "got ~S elements ~%" (length vec))
    ;; (format t "texture width = ~S ~%" texture-width)
    ;; (format t "texture height = ~S ~%" texture-height)
    ;; (format t "tile-width = ~S ~%" tile-width)
    ;; (format t "tile-height = ~S ~%" tile-height)
    (prog1 vec
      (dotimes (row rows)
        (dotimes (col cols)
          (setf (aref vec idx)
                (texture:texture-frame texture
                                       (* col tile-width)
                                       (* (- rows row) tile-height)
                                       tile-width tile-height
                                       :flipped nil))
          (incf idx))))))

(defstruct test12
  tmx-map
  frames)

(defmethod cl-user::contents-will-mount ((self test12) display)
  (declare (ignore display))
  (let* ((map (tmx-reader:read-tilemap "./res/test-tilemap.tmx"))
         (tileset (first (tmx-reader:map-tilesets map)))
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


(cl-user::display-contents (make-test12) :width 500 :height 500)

(defstruct test13
  tmx-map
  tileset
  frames
  layer)

(defmethod cl-user::contents-will-mount ((self test13) display)
  (declare (ignore display))
  (let* ((map (tmx-reader:read-tilemap "./res/test-tilemap.tmx"))
         (tileset (first (tmx-reader:map-tilesets map)))
         (frames (make-tileset-texture-frames tileset))
         (layer (first (tmx-reader:map-layers map))))
    (setf (test13-tmx-map self) map
          (test13-tileset self) tileset
          (test13-frames self) frames
          (test13-layer self) layer)))

(defun draw-tmx-layer (at-x at-y tile-width tile-height layer frames)
  (let* ((cols (tmx-reader:layer-width layer))
         (rows (tmx-reader:layer-height layer))
         (start-x (+ (- at-x (/ (* tile-width cols) 2.0)) (/ tile-width 2.0)))
         (start-y (- (+ at-y (/ (* tile-height rows) 2.0)) (/ tile-width 2.0))))
    (dotimes (row rows)
      (dotimes (col cols)
        (when-let (frame (aref frames (tmx-reader:layer-gid-at layer col row)))
          (let ((x (+ start-x (* col tile-width)))
                (y (- start-y (* row tile-height))))
            (draw-texture-frame frame x y)))))))

(defmethod cl-user::step-contents ((self test13) dt)
  (declare (ignore dt))
  (let ((x 250)
        (y 250)
        (tileset (test13-tileset self))
        (frames (test13-frames self))
        (layer (test13-layer self)))
    (draw-tmx-layer x y
                    (tmx-reader:tileset-tile-width tileset)
                    (tmx-reader:tileset-tile-height tileset)
                    layer
                    frames)))

(cl-user::display-contents (make-test13) :width 500 :height 500)

(defstruct tmx-renderer
  width height tile-width tile-height layer frames)

(defun tmx-renderer-from-file (path)
  (let* ((map (tmx-reader:read-tilemap path))
         (tileset (first (tmx-reader:map-tilesets map)))
         (frames (make-tileset-texture-frames tileset))
         (layer (first (tmx-reader:map-layers map)))
         (tile-width (tmx-reader:tileset-tile-width tileset))
         (tile-height (tmx-reader:tileset-tile-height tileset))
         (width (* tile-width (tmx-reader:layer-width layer)))
         (height (* tile-height (tmx-reader:layer-height layer))))
    (make-tmx-renderer :width width :height height
                       :tile-width tile-width :tile-height tile-height
                       :layer layer :frames frames)))

(defun tmx-renderer-tile-at-point (tmx x y &optional (default 0))
  (let* ((w (tmx-renderer-width tmx))
         (h (tmx-renderer-height tmx))
         (tw (tmx-renderer-tile-width tmx))
         (th (tmx-renderer-tile-height tmx))
         (height-in-tiles (floor h th))
         (layer (tmx-renderer-layer tmx)))
    (cond ((or (<= x 0.0) (>= x w)) default)
          ((or (<= y 0.0) (>= y h)) default)
          (t (let ((tx (floor x tw))
                   (ty (- height-in-tiles (floor y th) 1)))
               (tmx-reader:layer-gid-at layer tx ty))))))

(defun draw-tmx-renderer (x y renderer)
  (draw-tmx-layer x y
                  (tmx-renderer-tile-width renderer)
                  (tmx-renderer-tile-height renderer)
                  (tmx-renderer-layer renderer)
                  (tmx-renderer-frames renderer)))

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

(cl-user::display-contents (make-test14) :width 500 :height 500)
