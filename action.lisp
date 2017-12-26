(defpackage :action
  (:use :cl :alexandria)
  (:export
   #:make-manager
   #:start-with-target
   #:update
   #:stop
   #:stopped-p
   #:start-action
   #:update-actions
   #:rotate-by
   #:repeat-forever))
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

(defstruct manager
  (actions (make-array 128 :element-type t :adjustable t :fill-pointer 0)))


(defmethod start-with-target ((self action) target)
  (setf (action-target self) target
        (action-running self) t))

(defmethod reset ((self action))
  (setf (action-running self) t))

(defmethod update ((self action) dt)
  (declare (ignorable self dt))
  (error "subclasses must override action:update"))

(defmethod stop ((self action))
  (setf (action-running self) nil))

(defmethod stopped-p ((self action))
  (not (action-running self)))

(defun start-action (manager action target)
  (vector-push-extend action (manager-actions manager))
  (start-with-target action target))

(defun update-actions (manager dt)
  (let (stopped)
    (loop for action across (manager-actions manager) do
         (if (stopped-p action)
             (setf stopped t)
             (update action dt)))
    (when stopped
      (setf (manager-actions manager)
            (delete-if #'stopped-p (manager-actions manager))))))

(defstruct (finite-time-action (:include action))
  (duration 0.0)
  (elapsed 0.0))

(defmethod update ((self finite-time-action) dt)
  (incf (finite-time-action-elapsed self) dt)
  (when (>= (finite-time-action-elapsed self)
            (finite-time-action-duration self))
    (stop self)))

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

(defmethod update ((self rotate-by) dt)
  (declare (ignorable dt))
  (call-next-method)
  (with-struct (rotate-by- elapsed duration delta initial-rotation target) self
    (let* ((pct (/ elapsed duration))
           (rotation (mod (+ initial-rotation (* pct delta)) 360.0)))
      (setf (node:rotation target) rotation))))

(defun rotate-by (duration delta)
  (make-rotate-by :duration duration :delta delta))


(defstruct (repeat-forever (:include action))
  action)

(defmethod start-with-target ((self repeat-forever) node)
  (call-next-method)
  (start-with-target (repeat-forever-action self) node))

(defmethod update ((self repeat-forever) dt)
  (with-struct (repeat-forever- action) self
    (when (stopped-p action)
      (reset action))
    (update action dt)))

(defmethod reset ((self repeat-forever))
  (call-next-method)
  (reset (repeat-forever-action self)))

(defun repeat-forever (action)
  (make-repeat-forever :action action))
