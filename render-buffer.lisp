#|
'render' the scene into a vector of bytecode which can be double/triple buffered,
 and have the drawing thread just interpret that.
|#
(defpackage :xmas.render-buffer (:use :cl :alexandria)
            (:export
             #:cleanup-render-buffer))
(in-package :xmas.render-buffer)

(defmacro with-struct ((prefix &rest slots) var &body body)
  (once-only (var)
    `(symbol-macrolet
         ,(loop for slot in slots collect
               (list slot (list (symbolicate prefix slot) var)))
       ,@body)))

(defun make-adjustable-vector (&key (size 1024) (element-type t))
  (make-array size :element-type element-type :adjustable t :fill-pointer 0))

(defstruct (adjustable-static-vector (:constructor %make-adjustable-static-vector))
  vector
  (fill-pointer 0 :type fixnum)
  element-type)

(defmethod print-object ((self adjustable-static-vector) stream)
  (print-unreadable-object (self stream :identity t)
    (format stream "~S" 'adjustable-static-vector)))

(defun make-adjustable-static-vector (&key (size 1024) (element-type 'single-float))
  (%make-adjustable-static-vector
   :element-type element-type
   :vector (static-vectors:make-static-vector size :element-type element-type)))

(defun free-adjustable-static-vector (v)
  (static-vectors:free-static-vector (adjustable-static-vector-vector v))
  (setf (adjustable-static-vector-vector v) nil))

(defun static-vector-push-extend (item v)
  (with-struct (adjustable-static-vector- vector fill-pointer element-type) v
    (when (= fill-pointer (1- (length vector)))
      (let ((new-vector (static-vectors:make-static-vector
                         (* 2 (length vector))
                         :element-type element-type))
            (old-vector vector))
        (map-into new-vector 'identity vector)
        (setf vector new-vector)
        (static-vectors:free-static-vector old-vector)))
    (prog1 (setf (aref vector fill-pointer) item)
      (incf fill-pointer))))

(defstruct buffer
  (instrs (make-adjustable-vector :element-type 'fixnum))
  (values (make-adjustable-static-vector :element-type 'single-float)))

(defmethod print-object ((self buffer) stream)
  (print-unreadable-object (self stream :identity t)
    (format stream "~S" 'buffer)))

(defun release-buffer (buffer)
  (format t "releasing buffer: ~S~%" buffer)
  (free-adjustable-static-vector (buffer-values buffer)))


;; (defun make-buffer (&key (size 1024))
;;   (make-array size :element-type t :adjustable t :fill-pointer 0))

(defvar *read-buffer* nil)
(defvar *write-buffer* nil)

(defvar *pc* 0)

(defstruct render-buffer
  (read-buffer  (make-buffer))
  (write-buffer (make-buffer))
  (back-buffer  (make-buffer))
  (back-buffer-ready? nil)
  (swap-lock    (bt:make-lock)))

(defmethod print-object ((self render-buffer) stream)
  (print-unreadable-object (self stream :identity t)
    (format stream "~S" 'render-buffer)))

(defun cleanup-render-buffer (render-buffer)
  (format t "cleaning up render-buffer: ~S ~%" render-buffer)
  (release-buffer (render-buffer-read-buffer render-buffer))
  (release-buffer (render-buffer-write-buffer render-buffer))
  (release-buffer (render-buffer-back-buffer render-buffer)))

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

(defun write-float! (val)
  (static-vector-push-extend (coerce val 'single-float) (buffer-values *write-buffer*)))
(defun write-instr! (val)
  (vector-push-extend val (buffer-instrs *write-buffer*)))

(defun reset-write-buffer! ()
  (setf (fill-pointer (buffer-instrs *write-buffer*)) 0
        (adjustable-static-vector-fill-pointer (buffer-values *write-buffer*)) 0))

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
  (prog1 (aref
          (adjustable-static-vector-vector (buffer-values *read-buffer*)) *pc*)
    (incf *pc*)))

(defvar *instr-table*  (make-adjustable-vector :element-type t))
(defvar *instr-counter* (length *instr-table*))

(defmacro definstr (name (&rest args) &body body)
  (let ((instr-name (symbolicate '%instr- name))
        (instr *instr-counter*))
    (incf *instr-counter*)
    `(progn
       (defun ,name ,args
         (write-instr! ,instr)
         ,@(loop for arg in args collect
                `(write-float! ,arg)))
       (defun ,instr-name ()
         (let ,(loop for arg in args collect `(,arg (read!)))
           ,@body))
       (vector-push-extend #',instr-name *instr-table*))))

(defmacro definstr-vec (name (arg) &body body)
  (let ((instr-name (symbolicate '%instr- name))
        (instr *instr-counter*))
    (incf *instr-counter*)
    `(progn
       (defun ,name (,arg)
         (write-instr! ,instr)
         (write-float! (coerce (length ,arg) 'float))
         (loop for elt across ,arg do (write-float! elt)))
       (defun ,instr-name ()
         (macrolet ((do-vec! ((sym) &body body)
                      `(loop repeat (ceiling (read!))
                          for ,sym = (read!) do
                            ,@body)))
           ,@body))
       (vector-push-extend #',instr-name *instr-table*))))


(defun run! ()
  (unwind-protect
       (loop
          for instr across (buffer-instrs *read-buffer*)
          for fn = (aref *instr-table* instr)
          do (funcall fn))
    (setf *pc* 0)))
