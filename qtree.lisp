(defpackage xmas.qtree (:use :cl :alexandria :xmas.node)
            (:export
             #:qtree
             #:qtree-reset
             #:qtree-add))
(in-package xmas.qtree)

(defstruct qtree
  root
  x y
  width
  height
  pool
  (items-pool (make-array 256 :element-type t :adjustable t :fill-pointer 0))
  (items-index 0))

(defstruct qtree-node
  x y width height
  ul ll ur lr
  items
  split)

(defun alloc-items (qtree)
  (let ((pool (qtree-items-pool qtree)))
    (cond
      ((< (qtree-items-index qtree) (length pool))
       (prog1 (aref pool (qtree-items-index qtree))
         (incf (qtree-items-index qtree))))
      (t
       (let ((items (make-array 5 :element-type t :adjustable t :fill-pointer 0)))
         (vector-push-extend items pool)
         (incf (qtree-items-index qtree))
         items)))))

(defun free-items (qtree items)
  (declare (ignore qtree))
  (setf (fill-pointer items) 0))

(defun alloc-node (qtree &key x y width height)
  (if-let (node (pop (qtree-pool qtree)))
    (prog1 node
      (setf
       (qtree-node-x node) x
       (qtree-node-y node) y
       (qtree-node-width node) width
       (qtree-node-height node) height
       (qtree-node-items node) (alloc-items qtree)))
    (make-qtree-node :x x :y y :width width :height height
                     :items (alloc-items qtree))))

(defun free-node (qtree node)
  (when node
    (free-node qtree (qtree-node-ul node))
    (free-node qtree (qtree-node-ll node))
    (free-node qtree (qtree-node-ur node))
    (free-node qtree (qtree-node-lr node))   
    (free-items qtree (qtree-node-items node))
    (setf
     (qtree-node-ul node) nil
     (qtree-node-ll node) nil
     (qtree-node-ur node) nil
     (qtree-node-lr node) nil
     (qtree-node-split node) nil
     (qtree-node-items node) nil)
    ;;TODO: should not cons
    (push node (qtree-pool qtree))))

(defun ensure-child-node (qtree node which)
  (let ((x (qtree-node-x node))
        (y (qtree-node-y node))
        (w (qtree-node-width node))
        (h (qtree-node-height node)))
    (case which
      (ll (unless (qtree-node-ll node)
            (setf (qtree-node-ll node)
                  (alloc-node qtree
                              :x (- x (/ w 4.0))
                              :y (- y (/ h 4.0))
                              :width (/ w 2.0)
                              :height (/ h 2.0)))))
      (ul (unless (qtree-node-ul node)
            (setf (qtree-node-ul node)
                  (alloc-node qtree
                              :x (- x (/ w 4.0))
                              :y (+ y (/ h 4.0))
                              :width (/ w 2.0)
                              :height (/ h 2.0)))))
      (lr (unless (qtree-node-lr node)
            (setf (qtree-node-lr node)
                  (alloc-node qtree
                              :x (+ x (/ w 4.0))
                              :y (- y (/ h 4.0))
                              :width (/ w 2.0)
                              :height (/ h 2.0)))))
      (ur (unless (qtree-node-ur node)
            (setf (qtree-node-ur node)
                  (alloc-node qtree
                              :x (+ x (/ w 4.0))
                              :y (+ y (/ h 4.0))
                              :width (/ w 2.0)
                              :height (/ h 2.0))))))))

(defun split-node (qtree node item)
  (let ((items (qtree-node-items node)))
    (setf (qtree-node-items node) (alloc-items qtree)
          (qtree-node-split node) t)
    (loop for item across items do (add-to-node qtree node item))
    (add-to-node qtree node item)
    (free-items qtree items)))

(defun add-to-node (qtree node item)
  (let ((items (qtree-node-items node))
        (x (qtree-node-x node))
        (y (qtree-node-y node))
        (w (qtree-node-width node))
        (h (qtree-node-height node)))
    (cond
      ((or (< w 32.0) (< h 32.0))
       (vector-push-extend item items))
      ((qtree-node-split node)
       (cond
         ((<= (right item) x)
          (cond
            ((<= (top item) y)
             (ensure-child-node qtree node 'll)
             (add-to-node qtree (qtree-node-ll node) item))
            ((> (bottom item) y)
             (ensure-child-node qtree node 'ul)
             (add-to-node qtree (qtree-node-ul node) item))
            (t (vector-push-extend item items))))
         ((> (left item) x)
          (cond
            ((<= (top item) y)
             (ensure-child-node qtree node 'lr)
             (add-to-node qtree (qtree-node-lr node) item))
            ((> (bottom item) y)
             (ensure-child-node qtree node 'ur)
             (add-to-node qtree (qtree-node-ur node) item))
            (t (vector-push-extend item items))))
         (t (vector-push-extend item items))))
      (t (if (< (length items) 5)
             (vector-push-extend item items)
             (split-node qtree node item))))))

(defun qtree ()
  (let ((result (make-qtree)))
    (prog1 result
      (setf (qtree-root result) (alloc-node result :x 0 :y 0 :width 0 :height 0)))))

(defun qtree-reset (qtree &key x y width height)
  (when-let ((root (qtree-root qtree)))
    (free-node qtree root))
  (setf (qtree-items-index qtree) 0)
  (setf (qtree-root qtree) (alloc-node qtree :x x :y y :width width :height height)))
 
(defun qtree-add (qtree item)
  (add-to-node qtree (qtree-root qtree) item))
