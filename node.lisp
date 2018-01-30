(defpackage :xmas.node
  (:use :cl :alexandria :xmas.matrix)
  (:export
   #:node
   #:x
   #:y
   #:scale-x
   #:scale-y
   #:flip-x
   #:flip-y
   #:skew-x
   #:skew-y
   #:rotation
   #:color
   #:parent
   #:children
   ;; #:xform
   ;; #:xform-dirty-p
   ;; #:parent-xform
   ;; #:parent-xform-dirty-p

   #:add-child
   #:remove-child
   #:remove-from-parent

   #:node-transform
   #:running
   #:run-action
   #:stop-all-actions
   #:on-enter
   #:on-exit
   #:visible
   #:opacity
   #:z-order


   #:width
   #:height
   #:right
   #:left
   #:bottom
   #:top
   #:content-height
   #:content-width
   #:anchor-y
   #:anchor-x
   #:right-for-x
   #:left-for-x
   #:bottom-for-y
   #:top-for-y
   #:node-contains-world-point-p
   #:draw
   #:visit
   #:add-children))
(in-package :xmas.node)

;;; honestly, this should probably be a struct...
(defclass node ()
  ((x          :reader   x          :initarg  :x        :type single-float)
   (y          :reader   y          :initarg  :y        :type single-float)
   (z-order    :reader   z-order    :initarg  :z-order  :type single-float)
   (scale-x    :reader   scale-x    :initarg  :scale-x  :type single-float)
   (scale-y    :reader   scale-y    :initarg  :scale-y  :type single-float)
   (flip-x     :reader   flip-x     :initarg  :flip-x   :type boolean)
   (flip-y     :reader   flip-y     :initarg  :flip-y   :type boolean)
   (skew-x     :reader   skew-x     :initarg  :skew-x   :type single-float)
   (skew-y     :reader   skew-y     :initarg  :skew-y   :type single-float)
   (rotation   :reader   rotation   :initarg  :rotation :type single-float)
   (color                :accessor color                :initarg  :color)
   (opacity              :accessor opacity              :initarg  :opacity)
   (visible              :accessor visible              :initarg  :visible)

   (anchor-x       :reader anchor-x       :initarg  :anchor-x :type single-float)
   (anchor-y       :reader anchor-y       :initarg  :anchor-y :type single-float)
   (content-width  :reader content-width  :initarg  :content-width :type single-float)
   (content-height :reader content-height :initarg  :content-height :type single-float)

   (parent               :accessor parent               :initform nil)
   (children             :accessor children             :initform nil)
   (xform                :accessor xform                :initform (xmas.matrix:make-matrix))
   (xform-dirty-p        :accessor xform-dirty-p        :initform t)
   (parent-xform         :accessor parent-xform         :initform (xmas.matrix:make-matrix))
   (parent-xform-dirty-p :accessor parent-xform-dirty-p :initform t)
   (running              :accessor running              :initform nil)
   (pending-actions      :accessor pending-actions      :initform nil))
  (:default-initargs
   :x        0.0 :y       0.0
   :z-order  0.0
   :scale-x  1.0 :scale-y 1.0
   :flip-x   nil :flip-y  nil
   :skew-x   0.0 :skew-y  0.0
   :rotation 0.0
   :color    (vector 1.0 1.0 1.0) :opacity 1.0
   :visible  t
   :anchor-x 0.0 :anchor-y 0.0
   :content-width 0.0 :content-height 0.0))

(defun mark-as-dirty (node)
  (setf (xform-dirty-p node) t
        (parent-xform-dirty-p node) t))

(macrolet ((declare-setf-marks-as-dirty ((&rest options) &rest slot-names)
             (declare (ignore options))
             `(progn ,@(loop for s in slot-names collect
                            `(defmethod (setf ,s) (v (object node))
                               (mark-as-dirty object)
                               (setf (slot-value object ',s) v))))))
  (declare-setf-marks-as-dirty
   ()
   x y scale-x scale-y flip-x flip-y skew-x skew-y rotation
   anchor-x anchor-y content-width content-height))

;;TODO: matrix tests taking anchor-point into acct

(defmethod node-transform ((self node))
  (when (xform-dirty-p self)
    (setf (xform-dirty-p self) nil)
    (into-matrix ((xform self))
      (load-identity)
      (translate (x self) (y self))
      (rotate (rotation self))
      (scale (if (flip-x self)
                 (- (scale-x self))
                 (scale-x self))
             (if (flip-y self)
                 (- (scale-y self))
                 (scale-y self)))
      (translate (* -1.0 (anchor-x self) (content-width self))
                 (* -1.0 (anchor-y self) (content-height self)))))
  (xform self))

(defmethod node-to-parent-transform ((self node))
  (when (parent-xform-dirty-p self)
    (setf (parent-xform-dirty-p self) nil)
    (let ((-r (- (rotation self)))
          (sx (scale-x self))
          (sy (scale-y self))
          (-x (- (x self)))
          (-y (- (y self)))  
          (fx (flip-x self))
          (fy (flip-y self))
          (ax (anchor-x self))
          (ay (anchor-y self)))
      (let ((isx (if (zerop sx) 100.0 (/ 1.0 sx)))
            (isy (if (zerop sy) 100.0 (/ 1.0 sy))))
        (into-matrix ((parent-xform self))
          (load-identity)
          (translate (* ax (content-width self))
                     (* ay (content-height self)))
          (scale (if fx (- isx) isx)
                 (if fy (- isy) isy))
          (rotate -r)
          (translate -x -y)))))
  (parent-xform self))

;;TODO: make these optionally non-consing
(defmethod world-to-node-transform ((self node))
  (if-let ((p (parent self)))
    (into-matrix ((world-to-node-transform p))
      (cat-matrix (node-transform self)))
    (copy-matrix (node-transform self))))

(defmethod node-to-world-transform ((self node))
  (if-let ((p (parent self)))
    (into-matrix ((node-to-world-transform p))
      (left-cat-matrix (node-to-parent-transform self)))
    (copy-matrix (node-to-parent-transform self))))

(defmethod node-contains-world-point-p ((self node) x y)
  (multiple-value-bind (x y)
      (matrix-multiply-point-2d (node-to-world-transform self) x y)
    (and (<= x (content-width self))
         (>= x 0.0)
         (<= y (content-height self))
         (>= y 0.0))))

(defgeneric width (node))
(defgeneric height (node))

(defmethod width  ((self node)) (* (scale-x self) (content-width self)))
(defmethod height ((self node)) (* (scale-y self) (content-height self)))

(defmethod draw ((self node))
  (declare (ignore self)))

(defmethod visit ((self node))
  (when (not (visible self))
    (return-from visit))
  (xmas.render-buffer::push-matrix)
  (let ((ax (anchor-x self))
        (ay (anchor-y self)))
    (if (and (zerop ax) (zerop ay))
        (xmas.render-buffer::translate-scale-rotate
         (x self) (y self)
         (if (flip-x self) (* -1.0 (scale-x self)) (scale-x self))
         (if (flip-y self) (* -1.0 (scale-y self)) (scale-y self))
         (rotation self))
        (xmas.render-buffer::translate-scale-rotate-translate
         (x self) (y self)
         (if (flip-x self) (* -1.0 (scale-x self)) (scale-x self))
         (if (flip-y self) (* -1.0 (scale-y self)) (scale-y self))
         (rotation self)
         (* -1.0 ax (content-width self))
         (* -1.0 ay (content-height self)))))
  (draw self)
  (when (children self)
    (loop for child across (children self) do
         (visit child)))
  (xmas.render-buffer::pop-matrix))

;;; previous previous versions of visit here, for reference:
;;; TL;DR: using the matrix directly appears to slow things down considerably
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



;;TODO: yuck I don't like this. shouldn't need an anchor point to
;;      get/set right/left etc.
(defmethod anchor-x ((self t)) 0.5)
(defmethod anchor-y ((self t)) 0.5)

;;TODO: cache these?

(defun right-for-x (node x)
  (+ x (* (width node) (- 1.0 (anchor-x node)))))

(defun right (node)
  (right-for-x node (x node)))

(defun (setf right) (x node)
  (setf (x node) (- x (* (width node) (- 1.0 (anchor-x node))))))

(defun left-for-x (node x)
  (- x (* (width node) (anchor-x node))))

(defun left (node)
  (left-for-x node (x node)))

(defun (setf left) (x node)
  (setf (x node) (+ x (* (width node) (anchor-x node)))))

(defun bottom-for-y (node y)
  (- y (* (height node) (anchor-y node))))

(defun bottom (node)
  (bottom-for-y node (y node)))

(defun (setf bottom) (y node)
  (setf (y node) (+ y (* (height node) (anchor-y node)))))

(defun top-for-y (node y)
  (+ y (* (height node) (- 1.0 (anchor-y node)))))

(defun top (node)
  (top-for-y node (y node)))

(defun (setf top) (y node)
  (setf (y node) (- y (* (height node) (- 1.0 (anchor-y node))))))


