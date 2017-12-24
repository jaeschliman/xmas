(defpackage :state (:use :cl :alexandria))
(in-package :state)

#|
 wacky idea to have double-buffered global state...
 probably more trouble than it's worth...
 but could (potentially) simplify running
 an update loop in one thread, and the drawing loop on the main thread
 most obvious drawback is that you'd have to update every field on every
 tick or data will be corrupted... definitely tricky to use...


 what could make a lot more sense than this would be to 'render' the
 scene into a vector of bytecode which can be double/triple buffered,
 and have the drawing thread just interpret that.

|#


(defstruct state vector)

;; this should really be one of those defglobals not a defvar (save an indirection)
(defvar *swap* 0)

(defun swap! ()
  (setf *swap* (if (= *swap* 0) 1 0)))

(defmacro defstate (name &rest slots)
  (with-gensyms (slots-vector)
    (let ((constructor (symbolicate '%make- name))
          (swap '*swap*)
          (slot-count (length slots)))
      `(let* ((,slots-vector (coerce ',slots 'vector)))
         (setf (get ',name 'state-slots) ,slots-vector)
         (defstruct (,name (:include state) (:constructor ,constructor)))
         ,@(loop
              for slot in slots
              for offset upfrom 0
              for fname = (symbolicate name '- slot)
              appending
                (list
                 `(defun ,fname (self)
                    (declare (fixnum ,swap))
                    (svref (state-vector self)
                           (+ (* ,swap ,slot-count) ,offset)))
                 `(defun (setf ,fname) (value self)
                    (declare (fixnum ,swap))
                    (setf (svref (state-vector self)
                           (+ (* (if (= ,swap 0) 1 0) ,slot-count) ,offset)) value))))
         (defun ,(symbolicate 'make- name) (&key ,@slots)
           (let* ((vec (make-array (* 2 ,slot-count) :element-type t :adjustable nil :initial-element nil))
                  
                  (it (,constructor :vector vec)))
             ,@(loop for slot in slots for i upfrom 0 collect
                    `(setf (svref vec ,i) ,slot))
             ,@(loop for slot in slots for i upfrom (length slots) collect
                    `(setf (svref vec ,i) ,slot))
             it))))))

(defstate point x y)

;;; interesting to note that the below works..
#|
(flet (((setf p) (v arg) (format t "~A <- ~A!~%" arg v)))
  (setf (p 'foo) 'bar))
|#
