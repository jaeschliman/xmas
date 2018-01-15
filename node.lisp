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
   #:anchor-x))
(in-package :xmas.node)

;;; honestly, this should probably be a struct...
(defclass node ()
  ((x                    :reader   x                    :initarg  :x)
   (y                    :reader   y                    :initarg  :y)
   (z-order              :reader   z-order              :initarg  :z-order)
   (scale-x              :reader   scale-x              :initarg  :scale-x)
   (scale-y              :reader   scale-y              :initarg  :scale-y)
   (flip-x               :reader   flip-x               :initarg  :flip-x)
   (flip-y               :reader   flip-y               :initarg  :flip-y)
   (skew-x               :reader   skew-x               :initarg  :skew-x)
   (skew-y               :reader   skew-y               :initarg  :skew-y)
   (rotation             :reader   rotation             :initarg  :rotation)
   (color                :accessor color                :initarg  :color)
   (opacity              :accessor opacity              :initarg  :opacity)
   (visible              :accessor visible              :initarg  :visible)

   (anchor-x             :reader anchor-x             :initarg  :anchor-x)
   (anchor-y             :reader anchor-y             :initarg  :anchor-y)
   (content-width        :reader content-width        :initarg  :content-width)
   (content-height       :reader content-height       :initarg  :content-height)

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
   :z-order  0
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

(defmethod node-transform ((self node))
  (when (xform-dirty-p self)
    (setf (xform-dirty-p self) nil)
    (into-matrix ((xform self))
      (load-identity)
      (translate (x self) (y self))
      (rotate (rotation self))
      (scale (if (flip-y self)
                 (- (scale-x self))
                 (scale-x self))
             (if (flip-x self)
                 (- (scale-y self))
                 (scale-y self)))))
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
          (fy (flip-y self)))
      (let ((isx (if (zerop sx) 100.0 (/ 1.0 sx)))
            (isy (if (zerop sy) 100.0 (/ 1.0 sy))))
        (into-matrix ((parent-xform self))
          (load-identity)
          (scale (if fy (- isx) isx)
                 (if fx (- isy) isy))
          (rotate -r)
          (translate -x -y)))))
  (parent-xform self))

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

(defgeneric width (node))
(defgeneric height (node))

(defmethod width  ((self node)) (* (scale-x self) (content-width self)))
(defmethod height ((self node)) (* (scale-y self) (content-height self)))

(defmethod anchor-x ((self t)) 0.5)
(defmethod anchor-y ((self t)) 0.5)

;;TODO: cache these?
(defun right (node)
  (+ (x node) (* (width node) (- 1.0 (anchor-x node)))))

(defun (setf right) (x node)
  (setf (x node) (- x (* (width node) (- 1.0 (anchor-x node))))))

(defun left (node)
  (- (x node) (* (width node) (anchor-x node))))

(defun (setf left) (x node)
  (setf (x node) (+ x (* (width node) (anchor-x node)))))

(defun bottom (node)
  (- (y node) (* (height node) (anchor-y node))))

(defun (setf bottom) (y node)
  (setf (y node) (+ y (* (height node) (anchor-y node)))))

(defun top (node)
  (+ (y node) (* (height node) (- 1.0 (anchor-y node)))))

(defun (setf top) (y node)
  (setf (y node) (- y (* (height node) (- 1.0 (anchor-y node))))))

