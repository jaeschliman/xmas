(defpackage :action
  (:use :cl :alexandria)
  (:export
   #:start-with-target
   #:step-action
   #:stop
   #:stopped-p
   #:rotate-by
   #:repeat-forever
   #:run-sequence
   #:call-next-method
   #:delay
   #:ease-in-sine
   #:ease-out-sine
   #:ease-in-out-sine
   #:ease-in-quad
   #:ease-out-quad
   #:ease-in-out-quad
   #:ease-in-cubic
   #:ease-in-quart
   #:ease-in-quint
   #:ease-in-quadratic))
(in-package :action)

(defmacro with-struct ((prefix &rest slots) var &body body)
  (once-only (var)
    `(symbol-macrolet
         ,(loop for slot in slots collect
               (list slot (list (symbolicate prefix slot) var)))
       ,@body)))

(defstruct action
  target
  running)

(defmethod start-with-target ((self action) target)
  (setf (action-target self) target
        (action-running self) t))

(defmethod reset ((self action))
  (setf (action-running self) t))

(defmethod step-action ((self action) dt)
  (declare (ignorable self dt))
  (error "subclasses must override action:step-action"))

(defmethod stop ((self action))
  (setf (action-running self) nil))

(defmethod stopped-p ((self action))
  (not (action-running self)))

(defstruct (finite-time-action (:include action))
  (duration 0.0)
  (elapsed 0.0))

(defmethod update ((self finite-time-action) time)
  (declare (ignorable time))
  (error "finite-time-action subclasses must override action:update"))

(defmethod step-action ((self finite-time-action) dt)
  (with-struct (finite-time-action- elapsed duration) self
    (incf elapsed dt)
    (update self (clamp (/ elapsed duration) 0.0 1.0))
    (when (>= elapsed duration)
      (stop self))))

(defmethod reset ((self finite-time-action))
  (call-next-method)
  (setf (finite-time-action-elapsed self) 0.0))

(defstruct (rotate-by (:include finite-time-action))
  delta
  initial-rotation)

(defmethod start-with-target ((self rotate-by) target)
  (call-next-method)
  (setf (rotate-by-initial-rotation self)
        (node:rotation target)))

(defmethod reset ((self rotate-by))
  (call-next-method)
  (setf (rotate-by-initial-rotation self)
        (node:rotation (rotate-by-target self))))

(defmethod update ((self rotate-by) time)
  (with-struct (rotate-by- delta initial-rotation target) self
    (let* ((rotation (mod (+ initial-rotation (* time delta)) 360.0)))
      (setf (node:rotation target) rotation))))

(defun rotate-by (duration delta)
  (make-rotate-by :duration duration :delta delta))


(defstruct (repeat-forever (:include action))
  action)

(defmethod start-with-target ((self repeat-forever) node)
  (call-next-method)
  (start-with-target (repeat-forever-action self) node))

(defmethod step-action ((self repeat-forever) dt)
  (with-struct (repeat-forever- action) self
    (when (stopped-p action)
      (reset action))
    (step-action action dt)))

(defmethod reset ((self repeat-forever))
  (call-next-method)
  (reset (repeat-forever-action self)))

(defun repeat-forever (action)
  (make-repeat-forever :action action))

(defstruct (run-sequence (:include finite-time-action))
  item0 item1 (prev -1)  split)

(defmethod reset ((self run-sequence))
  (call-next-method)
  (with-struct (run-sequence- item0 item1 prev) self
    (setf prev -1)
    (reset item0)
    (reset item1)))

(defmethod stop ((self run-sequence))
  (call-next-method)
  (with-struct (run-sequence- item0 item1 prev) self
    (when (not (= prev -1))
      (if (= prev 0)
          (stop item0)
          (stop item1)))))

(defmethod start-with-target ((self run-sequence) node)
  (declare (ignorable node))
  (call-next-method)
  (with-struct (run-sequence- item0 duration split prev) self
    (setf split (/ (finite-time-action-duration item0) duration)
          prev -1))) 

(defmethod update ((self run-sequence) time)
  (with-struct (run-sequence- item0 item1 target split prev) self
    (let (found new-time action)
      (if (< time split)
          (setf found 0
                new-time (if (= split 0.0) time (/ time split)))
          (setf found 1
                new-time (/ (- time split) (- 1.0 split))))
      (cond
        ((= found 1)
         (cond
           ((= prev -1)
            (start-with-target item0 target)
            (update item0 1.0)
            (stop item0))
           ((= prev 0)
            (update item0 1.0)
            (stop item0))))
        ((and (= found 0) (= prev 1))
         ;;"reverse" noted to be buggy.
         (update item1 0.0)
         (stop item1)))
      (setf action (if (= found 0) item0 item1))
      (when (and (= found prev) (stopped-p action))
        (return-from update))
      (when (not (= found prev))
        (start-with-target action target))
      (update action new-time)
      (setf prev found))))

(defun sequence-2 (item0 item1)
  (assert item0)
  (assert item1)
  (make-run-sequence :item0 item0
                    :item1 item1
                    :duration (+ (finite-time-action-duration item0)
                                 (finite-time-action-duration item1))))

(defun run-sequence (&rest items)
  (declare (dynamic-extent items))
  (assert items)
  (when (null (cdr items))
    (return-from run-sequence (first items)))
  (let ((prev (first items)))
    (dolist (item (rest items))
      (setf prev (sequence-2 prev item)))
    prev))


(defstruct (delay (:include finite-time-action)))

(defmethod update ((self delay) time)
  (declare (ignore time)))

(defun delay (seconds)
  (make-delay :duration seconds))


(defstruct (ease (:include finite-time-action))
  inner
  function)


(defmethod start-with-target ((self ease) node)
  (call-next-method)
  (start-with-target (ease-inner self) node))

(defmethod reset ((self ease))
  (call-next-method self)
  (reset (ease-inner self)))

(defmethod stop ((self ease))
  (call-next-method)
  (stop (ease-inner self)))

(defmethod update ((self ease) time)
  (update (ease-inner self)
          (funcall (ease-function self) time)))

(macrolet ((defease (name (var) &body body)
             (let ((easefn (symbolicate '%ease- name)))
               `(progn
                 (defun ,easefn (,var) ,@body)
                 (defun ,name (action)
                   (make-ease :duration (finite-time-action-duration action)
                              :function (function ,easefn)
                              :inner action))))))
  (defease linear (time) time)
  (defease ease-in-sine (time)
    (1+ (* -1 (cos (* time (/ pi 2.0))))))
  (defease ease-out-sine (time)
    (sin (* time (/ pi 2.0))))
  (defease ease-in-out-sine (time)
    (* -0.5 (1- (cos (* pi time)))))
  (defease ease-in-quad (time)
    (* time time))
  (defease ease-out-quad (time)
    (* -1 time (- time 2.0)))
  (defease ease-in-out-quad (time)
    (setf time (* 2.0 time))
    (if (< time 1.0)
        (* 0.5 time time)
        (progn
          (decf time)
          (* -0.5 (1- (* time (- time 2.0)))))))
  (defease ease-in-cubic (time)
    (* time time time))
  (defease ease-in-quart (time)
    (* time time time time))
  (defease ease-in-quint (time)
    (* time time time time time))
  (defease ease-in-quadratic (time)
    (expt time 2.0)))
