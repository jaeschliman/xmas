(defpackage :xmas.matrix-stack (:use :cl :alexandria :xmas.matrix)
            (:export
             #:*matrix-stack*
             #:make-matrix-stack
             #:current-matrix
             #:push-matrix
             #:pop-matrix
             #:with-pushed-matrix))
(in-package :xmas.matrix-stack)

(defvar *matrix-stack* nil)

(defstruct stack
  array
  (index 0 :type fixnum))

(defun make-matrix-stack (&optional (initial-capacity 16))
  (let* ((array (make-array initial-capacity :adjustable t))
         (stack (make-stack :array array)))
    (loop for i below (length array) do (setf (aref array i) (make-matrix)))
    (into-matrix ((aref array 0))
      (load-identity))
    stack))

(defun current-matrix ()
  (when-let (stack *matrix-stack*)
    (aref (stack-array stack) (stack-index stack))))

(defun %maybe-grow-stack (stack)
  (when (= (stack-index stack) (1- (length (stack-array stack))))
    (loop repeat 16 do
         (vector-push-extend (make-matrix) (stack-array stack)))))

(defun push-matrix ()
  (when-let (stack *matrix-stack*)
    (%maybe-grow-stack stack)
    (let* ((arr (stack-array stack))
           (idx (stack-index stack))
           (prev (aref arr idx))
           (curr (aref arr (1+ idx))))
      (into-matrix (curr) (load-matrix prev))
      (incf (stack-index stack)))))

(defun pop-matrix ()
  (when-let (stack *matrix-stack*)
    (decf (stack-index stack))))

(defmacro with-pushed-matrix ((var) &body body)
  `(progn
     (push-matrix)
     (unwind-protect
          (let ((,var (current-matrix))) ,@body)
       (pop-matrix))))
