(ql:quickload :trivial-main-thread)
(ql:quickload :cl-opengl)
(ql:quickload :cl-glut)

;; doesn't really work in sbcl unfortunately. haven't tried ccl yet.

(defclass my-window (glut:window)
  ()
  (:default-initargs :title "My window"
    :width 500 :height 500 :mode '(:double :rgb :depth)))

(defmethod glut:display ((window my-window))
  (glut:swap-buffers))

(defmethod glut:reshape ((window my-window) w h))

(defmethod glut:idle ((window my-window))
  (glut:post-redisplay))

(defmethod glut:keyboard ((window my-window) key x y)
  (declare (ignore x y))
  (case key
    (#\q
     (trivial-main-thread:with-body-in-main-thread ()
       (glut::set-window (glut::id window))
       (glut:destroy-current-window)
       )
     (return-from glut:keyboard)))
  (glut:post-redisplay))

(defun show-my-window ()
  (trivial-main-thread:with-body-in-main-thread ()
    (glut:display-window (make-instance 'my-window))
    ))


