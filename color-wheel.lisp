(defpackage :xmas.color-wheel (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.action :xmas.texture :xmas.texture-packer :xmas.display :xmas.animation-manager :xmas.qtree :xmas.game-object :xmas.matrix-stack :xmas.spotlight-node :xmas.deftest))
(in-package :xmas.color-wheel)

(defun hsv-to-rgb (h s v)
  (let* ((chroma (* v s))
         (c chroma)
         (hprime (mod (/ h 60.0) 6.0))
         (x (* chroma (- 1.0 (abs (- (mod hprime 2.0) 1.0)))))
         (m (- v chroma)))
    (macrolet ((cases (&rest cases) 
                 `(cond ,@(loop for (lower upper r g b) in cases collect
                               `((and (<= ,lower hprime) (< hprime ,upper))
                                 (values (+ m ,r) (+ m ,g) (+ m ,b))))
                        (t (values m m m)))))
      (cases
       (0.0 1.0 c x 0.0)
       (1.0 2.0 x c 0.0)
       (2.0 3.0 0.0 c x)
       (3.0 4.0 0.0 x c)
       (4.0 5.0 x 0.0 c)
       (5.0 6.0 c 0.0 x)))))

(defun draw-color-wheel (x y radius offset-1 offset-2)
  (let* ((count 360)
         (2pi (load-time-value (coerce (* 2.0 pi) 'single-float)))
         (step (/ 2pi count))
         (pct-step (/ 1.0 count)))
    (xmas.render-buffer::with-colored-2d-triangle-fan ()
      (macrolet ((v (x y r g b a)
                   `(xmas.render-buffer::vert ,x ,y ,r ,g ,b ,a)))
        (v x y 255 255 255 255)
        (loop repeat (1+ count)
           with angle = 2pi
           with pct = 1.0
           for x1 = (+ x (* (sin angle) radius))
           for y1 = (+ y (* (cos angle) radius))
           for pos = (mod (+ pct (* -1.0 offset-1) ) 1.0)
           for degrees = (* 360.0 (mod (+ offset-2 (ease :in-out-quad pos)) 1.0))
           do
             (multiple-value-bind (r g b) (hsv-to-rgb degrees 1.0 1.0)
               (setf
                 r (floor (* r 255.0))
                 g (floor (* g 255.0))
                 b (floor (* b 255.0)))
               (v x1 y1 r g b 255))
             (decf angle step)
             (decf pct pct-step))))))

(deftest color-wheel (:width 500 :height 500)
  :init
  step := 0.0
  step2 := 0.0
  :update
  (incf step (* dt 5.0))
  (setf step (mod step 100.0))
  (incf step2 (* dt 7.5))
  (setf step2 (mod step2 100.0))
  (draw-color-wheel (+ 125.0 250.0) (+ 125.0 250.0) 177.0 (* step 0.01) (* step2 0.01))
  (draw-color-wheel (+ 125.0   0.0) (+ 125.0   0.0) 177.0 (* step2 0.01) (* step2 0.01))
  (draw-color-wheel (+ 125.0 250.0) (+ 125.0   0.0) 177.0 (* step2 0.01) (* step 0.01))
  (draw-color-wheel (+ 125.0   0.0) (+ 125.0 250.0) 177.0 (* step 0.01) (* step 0.01)))

(run-test 'color-wheel)
