#|
'render' the scene into a vector of bytecode which can be double/triple buffered,
 and have the drawing thread just interpret that.
|#
(defpackage :render-buffer (:use :cl :alexandria))
(in-package :render-buffer)

(defun make-buffer (&key (size 1024))
  (make-array size :element-type t :adjustable t :fill-pointer 0))

(defvar *read-buffer* (make-buffer))
(defvar *write-buffer* (make-buffer))

(defvar *pc* 0)

(defvar *instr-table* (make-buffer))
(defvar *instr-counter* 0)

(defun write! (val)
  (vector-push-extend val *write-buffer*))
(defun reset-write-buffer! ()
  (setf (fill-pointer *write-buffer*) 0))

(defun read! ()
  (prog1 (aref *read-buffer* *pc*)
    (incf *pc*)))

;; TODO: how to make this thread-safe?
(defun swap-buffers! ()
  (rotatef *read-buffer* *write-buffer*)
  (reset-write-buffer!))

(defmacro definstr (name (&rest args) &body body)
  (let ((instr-name (symbolicate '%instr- name))
        (instr *instr-counter*))
    (incf *instr-counter*)
    `(progn
       (defun ,name ,args
         (write! ,instr)
         ,@(loop for arg in args collect
                `(write! ,arg)))
       (defun ,instr-name ()
         (let ,(loop for arg in args collect `(,arg (read!)))
           ,@body))
       (vector-push-extend #',instr-name *instr-table*))))


(defun run! ()
  (unwind-protect
       (loop
          with end = (length *read-buffer*)
          while (< *pc* end)
          for instr = (read!)
          for fn = (aref *instr-table* instr)
          do (funcall fn))
    (setf *pc* 0)))
