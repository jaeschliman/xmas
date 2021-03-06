(defpackage :xmas.node
  (:use :cl :alexandria :xmas.matrix :xmas.matrix-stack)
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
   #:add-children
   #:node-four-corners
   #:apply-node-transform
   #:draw-with-xform
   #:visit-with-xform
   #:four-corners))
(in-package :xmas.node)

(defstruct ivars
 (x              0.0 :type single-float)
 (y              0.0 :type single-float)
 (scale-x        1.0 :type single-float)
 (scale-y        1.0 :type single-float)
 (flip-x         nil :type boolean)
 (flip-y         nil :type boolean)
 (skew-x         0.0 :type single-float)
 (skew-y         0.0 :type single-float)
 (rotation       0.0 :type single-float)
 (anchor-x       0.0 :type single-float)
 (anchor-y       0.0 :type single-float)
 (content-width  0.0 :type single-float)
 (content-height 0.0 :type single-float)
 (xform-dirty-p  t :type boolean)
 (parent-xform-dirty-p t :type boolean))

(defclass node ()
  ((ivars      :initform (make-ivars))
   (z-order    :reader   z-order    :initarg  :z-order  :type single-float)
   (color      :accessor color      :initarg  :color)
   (opacity    :accessor opacity    :initarg  :opacity :type single-float)
   (visible    :accessor visible    :initarg  :visible :type boolean)
   (parent               :accessor parent               :initform nil)
   (children             :accessor children             :initform nil)
   (xform                :accessor xform                :initform (xmas.matrix:make-matrix))
   (parent-xform         :accessor parent-xform         :initform (xmas.matrix:make-matrix))
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
  (declare (type node node)
           (optimize (speed 3) (safety 1)))
  (let ((ivars (slot-value node 'ivars)))
    (setf (ivars-xform-dirty-p ivars) t
          (ivars-parent-xform-dirty-p ivars) t)))

(declaim (inline mark-as-dirty))

(macrolet ((declare-ivar-accessors ((&rest options) &rest slot-names)
             (declare (ignore options))
             `(progn ,@(loop for s in slot-names
                          for name = (symbolicate 'ivars- s)
                          collect
                            `(progn
                               (defmethod ,s ((self node))
                                 (declare (type node self)
                                          (optimize (speed 3) (safety 1)))
                                 (,name (slot-value self 'ivars)))
                               (defmethod (setf ,s) (v (object node))
                                 (declare (type node object)
                                          (optimize (speed 3) (safety 0)))

                                 ,@(unless (or (eq s 'xform-dirty-p)
                                               (eq s 'parent-xform-dirty-p))
                                     (list '(mark-as-dirty object)))
                                 (setf (,name (slot-value object 'ivars)) v)))))))
  (declare-ivar-accessors
   ()
   x y scale-x scale-y flip-x flip-y
   skew-x skew-y rotation
   anchor-x anchor-y content-width content-height
   xform-dirty-p
   parent-xform-dirty-p))

(defmethod initialize-instance ((self node) &rest initargs
                                &key
                                  x y scale-x scale-y flip-x flip-y
                                  skew-x skew-y rotation anchor-x anchor-y
                                  content-width content-height
                                &allow-other-keys )
  (declare (ignore initargs)
           (dynamic-extent initargs))
  (call-next-method)
  (macrolet ((set-all (&rest syms)
               `(progn
                  ,@(loop for s in syms
                       for name = (symbolicate 'ivars- s)
                       collect `(setf (,name ivars) ,s)))))
    (let ((ivars (slot-value self 'ivars)))
      (set-all
       x y scale-x scale-y flip-x flip-y
       skew-x skew-y rotation
       anchor-x anchor-y content-width content-height))))

;;TODO: matrix tests taking anchor-point into acct

(defun %node-transform (self)
  (declare (type node self)
           (optimize (speed 3) (safety 1)))
  (when (xform-dirty-p self)
    (setf (xform-dirty-p self) nil)
    (let ((sx (scale-x self))
          (sy (scale-y self))
          (ax (anchor-x self))
          (ay (anchor-y self))
          (cw (content-width self))
          (ch (content-height self)))
      (declare (type single-float sx sy ax ay cw ch))
      (into-matrix ((xform self))
        (load-translation-rotation (x self) (y self) (rotation self))
        (unless (= 1.0 sx sy)
          ;;TODO: flip-x and flip-y should be handled
          ;;at the texture level, this doesn't belong on node
          (scale (if (flip-x self) (- sx) sx)
                 (if (flip-y self) (- sy) sy)))
        (unless (or (= 0.0 ax ay) (= 0.0 cw ch))
          (translate (* -1.0 ax cw) (* -1.0 ay ch))))))
  (xform self))

(defmethod node-transform ((self node))
  (%node-transform self))

(defun apply-node-transform (self matrix)
  (declare (type node self)
           (type xmas.matrix::m4 matrix)
           (optimize (speed 3) (safety 1)))
  (let ((iv (slot-value self 'ivars)))
    (declare (type ivars iv))
    (macrolet ((f (prop) `(the single-float (,(symbolicate 'ivars- prop) iv))))
      (let ((m (xmas.matrix::m4-vector matrix)))
        (declare (type xmas.matrix::matrix m)
                 (inline xmas.matrix::translate/unwrapped
                         xmas.matrix::rotate/unwrapped
                         xmas.matrix::scale/unwrapped)
                  (optimize (speed 3) (safety 0)))
        (xmas.matrix::translate/unwrapped (f x) (f y) m)
        (xmas.matrix::rotate/unwrapped (f rotation) m)
        (xmas.matrix::scale/unwrapped (f scale-x) (f scale-y) m)
        (xmas.matrix::translate/unwrapped
         (* -1.0 (f anchor-x) (f content-width))
         (* -1.0 (f anchor-y) (f content-height))
         m))))
  matrix)

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

(defmethod node-four-corners ((self node) matrix)
  (four-corners 0.0 0.0 (content-width self) (content-height self) matrix))

(defun four-corners (x1 y1 x2 y2 matrix)
  (declare (optimize (speed 3) (safety 1))
           (type single-float x1 y1 x2 y2))
  (let ((m (xmas.matrix::m4-vector matrix)))
    (macrolet ((multiply-point-2d (x y)
                 `(locally (declare (type xmas.matrix::matrix m)
                                    (single-float ,x ,y))
                    (macrolet ((f (x) `(the single-float ,x))
                               (+f (&rest args) `(f (+ ,@(mapcar (lambda (x) `(f ,x)) args))))
                               (*f (a b) `(f (* (f ,a) (f ,b)))))
                      (locally (declare (optimize (speed 3) (safety 0)))
                        (values (+f (*f ,x (aref m 0))
                                    (*f ,y (aref m 4))
                                    (aref m 12))
                                (+f (*f ,x (aref m 1))
                                    (*f ,y (aref m 5))
                                    (aref m 13))))))))
      (multiple-value-bind (llx lly) (multiply-point-2d x1 y1)
        (multiple-value-bind (ulx uly) (multiply-point-2d x1 y2)
          (multiple-value-bind (urx ury) (multiply-point-2d x2 y2)
            (multiple-value-bind (lrx lry) (multiply-point-2d x2 y1)
              (values llx lly
                      ulx uly
                      urx ury
                      lrx lry))))))))

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


