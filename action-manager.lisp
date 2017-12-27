(defpackage :action-manager
  (:use :cl :alexandria)
  (:export
   #:make-manager
   #:add-action
   #:remove-all-actions-for-target
   #:update-actions
   #:pause-all-actions-for-target
   #:resume-all-actions-for-target
   #:*action-manager*))
(in-package :action-manager)

(defstruct manager
  (actions (make-array 128 :element-type t :adjustable t :fill-pointer 0)))

(defstruct act
  action
  target
  paused
  started)

(defvar *action-manager* nil)

(defun maybe-start-action (act)
  (unless (act-started act)
    (setf (act-started act) t)
    (action:start-with-target (act-action act) (act-target act))))

(defun add-action (manager action target &optional (paused nil))
  (let ((act (make-act :action action
                       :target target
                       :paused paused)))
    (vector-push-extend act (manager-actions manager))
    (maybe-start-action act)))

(defun remove-all-actions-for-target (manager target)
  (setf (manager-actions manager)
        (delete target (manager-actions manager)
                :key #'act-target)))

(defun pause-all-actions-for-target (manager target)
  (loop for act across (manager-actions manager)
     when (eq (act-target act) target) do
       (setf (act-paused act) t)))

(defun resume-all-actions-for-target (manager target)
  (loop for act across (manager-actions manager)
     when (eq (act-target act) target) do
       (setf (act-paused act) nil)
       (maybe-start-action act)))

(defun update-actions (manager dt)
  (let (stopped)
    (loop for act across (manager-actions manager)
       for paused = (act-paused act)
       for action = (act-action act)
       do
         (unless paused
           (if (action:stopped-p action)
               (setf stopped t)
               (action:step-action action dt))))
    (when stopped
      (setf (manager-actions manager)
            (delete-if #'action:stopped-p (manager-actions manager) :key #'act-action)))))


