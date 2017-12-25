#|
'render' the scene into a vector of bytecode which can be double/triple buffered,
 and have the drawing thread just interpret that.
|#
(defpackage :render-buffer (:use :cl :alexandria))
(in-package :render-buffer)

(defun make-buffer (&key (size 1024))
  (make-array size :element-type t :adjustable t :fill-pointer 0))

(defvar *read-buffer* nil)
(defvar *write-buffer* nil)

(defvar *pc* 0)

(defstruct render-buffer
  (read-buffer  (make-buffer))
  (write-buffer (make-buffer))
  (back-buffer  (make-buffer))
  (back-buffer-ready? nil)
  (swap-lock    (bt:make-lock)))

(defun swap-write-buffer! (render-buffer)
  (bt:with-lock-held ((render-buffer-swap-lock render-buffer))
    (rotatef (render-buffer-write-buffer render-buffer)
             (render-buffer-back-buffer render-buffer))
    (setf (render-buffer-back-buffer-ready? render-buffer) t)))

(defun maybe-swap-read-buffer! (render-buffer)
  (when (render-buffer-back-buffer-ready? render-buffer)
    (bt:with-lock-held ((render-buffer-swap-lock render-buffer))
      (rotatef (render-buffer-read-buffer render-buffer)
               (render-buffer-back-buffer render-buffer))
      (setf (render-buffer-back-buffer-ready? render-buffer) nil))))

(defun write! (val)
  (vector-push-extend val *write-buffer*))
(defun reset-write-buffer! ()
  (setf (fill-pointer *write-buffer*) 0))

(defmacro with-writes-to-render-buffer ((buffer) &body body)
  (once-only (buffer)
    `(let ((*write-buffer* (render-buffer-write-buffer ,buffer)))
       (reset-write-buffer!)
       (unwind-protect (progn ,@body)
         (swap-write-buffer! ,buffer)))))

(defmacro with-reads-from-render-buffer ((buffer) &body body)
  (once-only (buffer)
    `(progn
       (maybe-swap-read-buffer! ,buffer)
       (let ((*read-buffer* (render-buffer-read-buffer ,buffer))
             (*pc* 0))
         ,@body))))

(defun read! ()
  (prog1 (aref *read-buffer* *pc*)
    (incf *pc*)))

(defvar *instr-table* (make-buffer))
(defvar *instr-counter* 0)

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
