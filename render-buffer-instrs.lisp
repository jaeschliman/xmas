(in-package :xmas.render-buffer)

(definstr set-color (r g b a)
  (gl:color r g b a))

(definstr disable-texture ()
  (gl:bind-texture :texture-2d 0))

(definstr draw-rect (x y w h)
  (gl:bind-texture :texture-2d 0)
  (gl:rect x y (+ x w) (+ y h)))

(defun bind-texture (id)
  (gl:bind-texture :texture-2d id))

(definstr simple-draw-gl-texture ((id :u32) w h)
  (bind-texture id)
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

(definstr simple-draw-gl-texture-no-color ((id :u32) w h)
  (bind-texture id)
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

(definstr draw-gl-texture-at ((id :u32) x y w h)
  (bind-texture id)
  (let* ((x2 (+ x w))
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

(definstr draw-gl-texture-at-tex-coords ((id :u32) x y w h u0 v0 u1 v1)
  (bind-texture id)
  (let* ((x2 (+ x w))
         (y2 (+ y h)))
    (gl:with-primitive :quads
      (gl:tex-coord u0  v1)
      (gl:vertex    x  y  0)
      (gl:tex-coord u1  v1)
      (gl:vertex    x2 y  0)
      (gl:tex-coord u1  v0)
      (gl:vertex    x2 y2 0)
      (gl:tex-coord u0  v0)
      (gl:vertex    x  y2 0))))

(definstr simple-draw-gl-with-tex-coords ((id :u32) x y w h tx1 ty1 tx2 ty2)
  (bind-texture id)
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

(definstr simple-draw-gl-with-tex-coords-rotated ((id :u32) x y w h tx1 ty1 tx2 ty2)
  (bind-texture id)
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

(definstr draw-gl-with-tex-coords ((id :u32) x y w h tx1 ty1 tx2 ty2)
  (bind-texture id)
  (let* ((x2 (+ x w))
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

(definstr draw-gl-with-tex-coords-rotated ((id :u32) x y w h tx1 ty1 tx2 ty2)
  (bind-texture id)
  (rotatef w h)
  (let* ((x2 (+ x w))
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

(definstr translate-scale-rotate-translate (x y sx sy r x2 y2)
  (gl:translate x y 0.0)
  (gl:scale sx sy 1.0)
  (gl:rotate r 0.0 0.0 1.0)
  (gl:translate x2 y2 0.0))

(definstr-batched with-2d-triangles ()
  (:write
   `(flet ((vert (x y)
             (write-float! x)
             (write-float! y)))
      (declare (dynamic-extent (function vert)))
      ,@body))
  (:read
   (gl:enable-client-state :vertex-array)
   (%gl:vertex-pointer 2 :float (* 4 2) float-ptr)
   (gl:draw-arrays :triangles 0 (/ float-count 2))
   (gl:disable-client-state :vertex-array)))

(definstr-batched with-colored-2d-triangle-fan ()
  (:write
   `(flet ((vert (x y r g b a)
             (write-float! x)
             (write-float! y)
             (write-u8! r)
             (write-u8! g)
             (write-u8! b)
             (write-u8! a)))
      (declare (dynamic-extent (function vert)))
      ,@body))
  (:read
   (gl:enable-client-state :vertex-array)
   (gl:enable-client-state :color-array)
   (%gl:vertex-pointer 2 :float (* 4 2) float-ptr)
   (%gl:color-pointer 4 :unsigned-byte 0 u8-ptr)
   (gl:draw-arrays :triangle-fan 0 (/ float-count 2))
   (gl:disable-client-state :vertex-array)
   (gl:disable-client-state :color-array)))


(definstr-batched with-textured-2d-triangles ((texture-id :u32))
  (:write
   `(flet ((vert (x y tx ty)
             (write-float! x)
             (write-float! y)
             (write-float! tx)
             (write-float! ty)))
      (declare (dynamic-extent (function vert)))
      ,@body))
  (:read
   (bind-texture texture-id)
   (gl:enable-client-state :vertex-array)
   (gl:enable-client-state :texture-coord-array)
   (%gl:vertex-pointer 2 :float (* 4 (+ 2 2)) float-ptr)
   (%gl:tex-coord-pointer 2 :float (* 4 (+ 2 2))
                          (cffi:inc-pointer float-ptr (* 4 2)))
   (gl:draw-arrays :triangles 0 (/ float-count 4))
   (gl:disable-client-state :texture-coord-array)
   (gl:disable-client-state :vertex-array)))

(defmacro write-floats! (values &rest floats)
  (once-only (values)
    (let ((count (length floats)))
      `(locally (declare (type adjustable-static-vector ,values))
         (adjustable-static-vector-reserve-capacity ,values ,count)
         (let ((idx (adjustable-static-vector-fill-pointer ,values))
               (vec (adjustable-static-vector-vector ,values)))
           (declare (type array-index idx)
                    (type single-float ,@(remove-if #'numberp floats))
                    (type (simple-array single-float) vec)
                    (optimize (speed 3) (safety 0)))
           ,@(loop for f in floats
                for inc upfrom 0
                collect
                  `(setf (aref vec (the array-index (+ (the fixnum idx) ,inc)))
                         (the single-float ,f)))
           (incf (adjustable-static-vector-fill-pointer ,values) ,count))))))

(defmacro write-u8s! (values &rest bytes)
  (once-only (values)
    (let ((count (length bytes)))
      `(locally (declare (type adjustable-static-vector ,values))
         (adjustable-static-vector-reserve-capacity ,values ,count)
         (let ((idx (adjustable-static-vector-fill-pointer ,values))
               (vec (adjustable-static-vector-vector ,values)))
           (declare (type array-index idx)
                    (type (unsigned-byte 8) ,@(remove-if #'numberp bytes))
                    (type (simple-array (unsigned-byte 8)) vec)
                    (optimize (speed 3) (safety 1)))
           ,@(loop for b in bytes
                for inc upfrom 0
                collect
                  `(setf (aref vec (the array-index (+ (the fixnum idx) ,inc)))
                         (the (unsigned-byte 8) ,b)))
           (incf (adjustable-static-vector-fill-pointer ,values) ,count))))))

(defun %draw-quad (llx lly ulx uly urx ury lrx lry tx1 ty1 tx2 ty2)
  (declare (optimize (speed 3) (safety 1))
           (type single-float llx lly ulx uly urx ury lrx lry tx1 ty1 tx2 ty2))
  (let ((values (buffer-float-values *write-buffer*)))
    (declare (type adjustable-static-vector values))
    (write-floats! values
                   ;; b c
                   ;; a d
                   llx lly tx1 ty1
                   ulx uly tx1 ty2
                   urx ury tx2 ty2
                   lrx lry tx2 ty1)))

(defun %draw-quad-color (r g b a)
  (declare (optimize (speed 3) (safety 1))
           (type single-float r g b a))
  (flet ((c (c) (floor (* c 255.0))))
    (declare (dynamic-extent #'c))
    (let ((r (c r))
          (g (c g))
          (b (c b))
          (a (c a)))
      (declare (type (unsigned-byte 8) r g b a))
      (let ((buff (buffer-u8-values *write-buffer*)))
        (declare (type adjustable-static-vector buff))
        (write-u8s! buff
                    r g b a
                    r g b a
                    r g b a
                    r g b a)))))

(definstr-batched with-textured-2d-quads ((texture-id :u32))
  (:write
   `(flet ((quad (x1 y1 tx1 ty1 x2 y2 tx2 ty2)
             (let ((values (buffer-float-values *write-buffer*)))
               (declare (type adjustable-static-vector values))
               (write-floats! values
                              x1 y1 tx1 ty1
                              x1 y2 tx1 ty2
                              x2 y2 tx2 ty2
                              x2 y1 tx2 ty1))))
      (declare (dynamic-extent (function quad)))
      ,@body))
  (:read
   (bind-texture texture-id)
   (gl:enable-client-state :vertex-array)
   (gl:enable-client-state :texture-coord-array)
   (%gl:vertex-pointer 2 :float (* 4 (+ 2 2)) float-ptr)
   (%gl:tex-coord-pointer 2 :float (* 4 (+ 2 2))
                          (cffi:inc-pointer float-ptr (* 4 2)))
   (gl:draw-arrays :quads 0 (/ float-count 4))
   (gl:disable-client-state :texture-coord-array)
   (gl:disable-client-state :vertex-array)))

(definstr-batched with-colored-textured-2d-quads ((texture-id :u32))
  (:write
   `(progn ,@body))
  (:read
   (bind-texture texture-id)
   (gl:enable-client-state :vertex-array)
   (gl:enable-client-state :texture-coord-array)
   (gl:enable-client-state :color-array)
   (%gl:vertex-pointer 2 :float (* 4 (+ 2 2)) float-ptr)
   (%gl:tex-coord-pointer 2 :float (* 4 (+ 2 2))
                          (cffi:inc-pointer float-ptr (* 4 2)))
   (%gl:color-pointer 4 :unsigned-byte 0 u8-ptr)
   (gl:draw-arrays :quads 0 (/ float-count 4))
   (gl:disable-client-state :texture-coord-array)
   (gl:disable-client-state :vertex-array)
   (gl:disable-client-state :color-array)))
