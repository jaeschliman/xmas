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

(defun %grow-adjustable-static-vector (v)
  (with-struct (adjustable-static-vector- vector element-type) v
    (let ((new-vector (static-vectors:make-static-vector
                       (* 2 (length vector))
                       :element-type element-type))
          (old-vector vector))
      (replace new-vector old-vector)
      (setf vector new-vector)
      (static-vectors:free-static-vector old-vector))))

(defun static-vector-push (item v)
  (with-struct (adjustable-static-vector- vector fill-pointer) v
    (prog1 (setf (aref vector fill-pointer) item)
      (incf fill-pointer))))

(defun static-vector-push-extend (item v)
  (with-struct (adjustable-static-vector- vector fill-pointer) v
    (when (= fill-pointer (1- (length vector)))
      (%grow-adjustable-static-vector v))
    (static-vector-push item v)))

(defun adjustable-static-vector-reserve-capacity (v count)
  (declare (optimize (speed 3) (safety 1))
           (type adjustable-static-vector v)
           (type fixnum count))
  (with-struct (adjustable-static-vector- vector fill-pointer) v
    (when (< (- (1- (length vector)) fill-pointer) count)
      (%grow-adjustable-static-vector v)
      ;;FIXME: this is lazy programming
      ;;grow again if needed (unlikely)
      (adjustable-static-vector-reserve-capacity v count))))

(defstruct buffer
  (instrs (make-adjustable-vector :element-type 'fixnum))
  (batch-counts (make-adjustable-static-vector :element-type '(unsigned-byte 32)))
  (batch-counts-idx 0)
  (float-values (make-adjustable-static-vector :element-type 'single-float))
  (float-idx 0 :type array-index)
  (u32-values (make-adjustable-static-vector :element-type '(unsigned-byte 32)))
  (u32-idx 0 :type array-index)
  (u8-values (make-adjustable-static-vector :element-type '(unsigned-byte 8)))
  (u8-idx 0 :type array-index))

(defmethod print-object ((self buffer) stream)
  (print-unreadable-object (self stream :identity t)
    (format stream "~S" 'buffer)))

(defun release-buffer (buffer)
  (format t "releasing buffer: ~S~%" buffer)
  (free-adjustable-static-vector (buffer-float-values buffer))
  (free-adjustable-static-vector (buffer-batch-counts buffer))
  (free-adjustable-static-vector (buffer-u32-values buffer))
  (free-adjustable-static-vector (buffer-u8-values buffer)))

;; (defun make-buffer (&key (size 1024))
;;   (make-array size :element-type t :adjustable t :fill-pointer 0))

(defvar *read-buffer* nil)
(defvar *write-buffer* nil)

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

(defun %write-float-on (val values)
  (static-vector-push-extend val values))
(declaim (inline %write-float-on))

(defun write-float! (val)
  (%write-float-on val (buffer-float-values *write-buffer*)))

(defun write-instr! (val)
  (vector-push-extend val (buffer-instrs *write-buffer*)))

(defun write-count! (val)
  (static-vector-push-extend val (buffer-batch-counts *write-buffer*)))

(defun write-u32! (val)
  (static-vector-push-extend val (buffer-u32-values *write-buffer*)))

(defun write-u8! (val)
  (static-vector-push-extend val (buffer-u8-values *write-buffer*)))

(defun current-write-position ()
  (adjustable-static-vector-fill-pointer (buffer-float-values *write-buffer*)))

(defmacro write-position (type)
  (let ((accessor (ecase type
                    (:float 'buffer-float-values)
                    (:u32   'buffer-u32-values)
                    (:u8    'buffer-u8-values))))
    `(adjustable-static-vector-fill-pointer (,accessor *write-buffer*))))

(defun current-read-pointer () ;;TODO: rename to current-float-read-pointer
  (let ((vec (adjustable-static-vector-vector (buffer-float-values *read-buffer*)))
        (pos (buffer-float-idx *read-buffer*)))
    (static-vectors:static-vector-pointer vec :offset (* pos 4))))

(defmacro read-pointer (type)
  (multiple-value-bind (accessor index width-in-bytes)
      (ecase type
        (:float (values 'buffer-float-values 'buffer-float-idx 4))
        (:u32   (values 'buffer-u32-values 'buffer-u32-idx 4))
        (:u8    (values 'buffer-u8-values 'buffer-u8-idx 1)))
    `(let ((vec (adjustable-static-vector-vector (,accessor *read-buffer*)))
           (pos (,index *read-buffer*)))
       (static-vectors:static-vector-pointer vec :offset (* pos ,width-in-bytes)))))

(defun reset-read-buffer! ()
  (let ((b *read-buffer*))
    (setf (buffer-float-idx b) 0
          (buffer-u32-idx b) 0
          (buffer-u8-idx b) 0
          (buffer-batch-counts-idx b) 0)))

(defun reset-write-buffer! ()
  (let ((b *write-buffer*))
    (setf (fill-pointer (buffer-instrs b)) 0
          (adjustable-static-vector-fill-pointer (buffer-float-values b)) 0
          (adjustable-static-vector-fill-pointer (buffer-u32-values b)) 0
          (adjustable-static-vector-fill-pointer (buffer-u8-values b)) 0
          (adjustable-static-vector-fill-pointer (buffer-batch-counts b)) 0)))

(defmacro with-writes-to-render-buffer ((buffer) &body body)
  (once-only (buffer)
    (with-gensyms (unwound)
      `(let ((*write-buffer* (render-buffer-write-buffer ,buffer))
             (,unwound t))
         (reset-write-buffer!)
         (unwind-protect (progn ,@body (setf ,unwound nil))
           (unless ,unwound
             (swap-write-buffer! ,buffer)))))))

(defmacro with-reads-from-render-buffer ((buffer) &body body)
  (once-only (buffer)
    `(progn
       (maybe-swap-read-buffer! ,buffer)
       (let ((*read-buffer* (render-buffer-read-buffer ,buffer)))
         (reset-read-buffer!)
         ,@body))))

(defun read! () ;;TODO: rename to read-float!
  (prog1 (aref
          (adjustable-static-vector-vector (buffer-float-values *read-buffer*))
          (buffer-float-idx *read-buffer*))
    (incf (buffer-float-idx *read-buffer*))))

(defun read-u32! ()
  (prog1 (aref
          (adjustable-static-vector-vector (buffer-u32-values *read-buffer*))
          (buffer-u32-idx *read-buffer*))
    (incf (buffer-u32-idx *read-buffer*))))

(defun read-u8! ()
  (prog1 (aref
          (adjustable-static-vector-vector (buffer-u8-values *read-buffer*))
          (buffer-u8-idx *read-buffer*))
    (incf (buffer-u8-idx *read-buffer*))))

(defun read-count! ()
  (prog1 (aref
          (adjustable-static-vector-vector (buffer-batch-counts *read-buffer*))
          (buffer-batch-counts-idx *read-buffer*))
    (incf (buffer-batch-counts-idx *read-buffer*))))

(defun call-with-batched-writes (instruction fn)
  (write-instr! instruction)
  (let ((float-start (write-position :float))
        (u32-start   (write-position :u32))
        (u8-start    (write-position :u8)))
    (unwind-protect (funcall fn)
      (write-count! (- (write-position :float) float-start))
      (write-count! (- (write-position :u32) u32-start))
      (write-count! (- (write-position :u8) u8-start)))))

(defun call-with-batched-read-pointer (fn)
  (let* ((float-count (read-count!))
         (u32-count (read-count!))
         (u8-count (read-count!))
         (float-ptr (read-pointer :float))
         (u32-ptr (read-pointer :u32))
         (u8-ptr (read-pointer :u8)))
    (unwind-protect (funcall
                     fn
                     float-count float-ptr
                     u32-count u32-ptr
                     u8-count u8-ptr)
      (incf (buffer-float-idx *read-buffer*) float-count)
      (incf (buffer-u32-idx *read-buffer*) u32-count)
      (incf (buffer-u8-idx *read-buffer*) u8-count))))

(defmacro with-batched-writes ((instr) &body body)
  (with-gensyms (fn)
    `(let ((,fn (lambda () ,@body)))
       (declare (dynamic-extent ,fn))
       (call-with-batched-writes ,instr ,fn))))

(defmacro with-batched-read-pointer ((float-count
                                      float-ptr
                                      u32-count
                                      u32-ptr
                                      u8-count
                                      u8-ptr) &body body)
  (with-gensyms (fn)
    `(let ((,fn (lambda (,float-count ,float-ptr ,u32-count ,u32-ptr ,u8-count ,u8-ptr)
                  (declare (ignorable ,float-count ,float-ptr
                                      ,u32-count ,u32-ptr
                                      ,u8-count ,u8-ptr))
                  ,@body)))
       (declare (dynamic-extent ,fn))
       (call-with-batched-read-pointer ,fn))))

(defvar *instr-table*  (make-adjustable-vector :element-type t))
(defvar *instr-counter* (length *instr-table*))

(defun add-instruction (fn)
  (prog1 *instr-counter*
    (vector-push-extend fn *instr-table*)
    (incf *instr-counter*)))

(defun arg-to-write-form (arg)
  (let* ((arg (ensure-list arg))
         (type (or (second arg) :float))
         (var (first arg)))
    (ecase type
      (:float `(write-float! (coerce ,var 'single-float)))
      (:u32   `(write-u32! ,var))
      (:u8    `(write-u8! ,var)))))

(defun arg-to-macro-write-form (arg)
 (let* ((arg (ensure-list arg))
         (type (or (second arg) :float))
         (var (first arg)))
    (ecase type
      (:float ``(write-float! (coerce ,,var 'single-float)))
      (:u32   ``(write-u32! ,,var))
      (:u8    ``(write-u8! ,,var)))))

(defun arg-to-let-binding (arg)
  (let* ((arg (ensure-list arg))
         (type (or (second arg) :float))
         (var (first arg)))
    (ecase type
      (:float `(,var (read!)))
      (:u32   `(,var (read-u32!)))
      (:u8    `(,var (read-u8!))))))

(defmacro definstr (name (&rest args) &body body)
  (let ((instr-name (symbolicate '%instr- name))
        (instr *instr-counter*))
    (incf *instr-counter*)
    `(progn
       (defun ,name ,(mapcar #'ensure-car args)
         (write-instr! ,instr)
         ,@(loop for arg in args collect (arg-to-write-form arg)))
       (defun ,instr-name ()
         (let ,(loop for arg in args collect (arg-to-let-binding arg))
           ,@body))
       (vector-push-extend #',instr-name *instr-table*))))

(defmacro definstr-vec (name (arg) &body body)
  (let ((instr-name (symbolicate '%instr- name))
        (instr *instr-counter*))
    (incf *instr-counter*)
    `(progn
       (defun ,name (,arg)
         (write-instr! ,instr)
         (write-float! (coerce (length ,arg) 'single-float))
         (loop for elt across ,arg do (write-float! elt)))
       (defun ,instr-name ()
         (macrolet ((do-vec! ((sym) &body body)
                      `(loop repeat (ceiling (read!))
                          for ,sym = (read!) do
                            ,@body)))
           ,@body))
       (vector-push-extend #',instr-name *instr-table*))))

(defmacro definstr-batched (name write-args &body body)
  (let ((instr-name (symbolicate '%instr- name))
        (instr *instr-counter*)
        (var-names (mapcar #'ensure-car write-args)))
    (incf *instr-counter*)
    `(progn
       (defmacro ,name (,var-names &body body)
         `(progn
            ,,@(mapcar #'arg-to-macro-write-form write-args)
            (with-batched-writes (,,instr) ,,@(cdr (assoc :write body)))))
       (defun ,instr-name ()
         (let ,(mapcar #'arg-to-let-binding write-args)
           (with-batched-read-pointer (float-count float-ptr
                                       u32-count u32-ptr
                                       u8-count u8-ptr)
             ,@(cdr (assoc :read body)))))
       (vector-push-extend #',instr-name *instr-table*))))

(defun run! ()
  (loop
     for instr across (buffer-instrs *read-buffer*)
     for fn = (aref *instr-table* instr)
     do (funcall fn)))
