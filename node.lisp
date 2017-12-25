(defpackage :node
  (:use :cl :alexandria :matrix)
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

   #:node-transform
   ))
(in-package :node)

(defclass node ()
  ((x                    :reader   x                    :initarg  :x)
   (y                    :reader   y                    :initarg  :y)
   (scale-x              :reader   scale-x              :initarg  :scale-x)
   (scale-y              :reader   scale-y              :initarg  :scale-y)
   (flip-x               :reader   flip-x               :initarg  :flip-x)
   (flip-y               :reader   flip-y               :initarg  :flip-y)
   (skew-x               :reader   skew-x               :initarg  :skew-x)
   (skew-y               :reader   skew-y               :initarg  :skew-y)
   (rotation             :reader   rotation             :initarg  :rotation)
   (color                :accessor color                :initarg  :color)

   (parent               :accessor parent               :initform nil)
   (children             :accessor children             :initform nil)
   (xform                :accessor xform                :initform (matrix:make-matrix))
   (xform-dirty-p        :accessor xform-dirty-p        :initform t)
   (parent-xform         :accessor parent-xform         :initform (matrix:make-matrix))
   (parent-xform-dirty-p :accessor parent-xform-dirty-p :initform t))
  (:default-initargs
   :x        0.0 :y       0.0
   :scale-x  1.0 :scale-y 1.0
   :flip-x   nil :flip-y  nil
   :skew-x   0.0 :skew-y  0.0
   :rotation 0.0
   :color    '(1.0 1.0 1.0 1.0)))

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
   x y scale-x scale-y flip-x flip-y skew-x skew-y rotation))


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
