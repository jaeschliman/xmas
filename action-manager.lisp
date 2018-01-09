(defpackage :xmas.action-manager
  (:use :cl :alexandria)
  (:export
   #:make-manager
   #:add-action
   #:remove-all-actions-for-target
   #:update-actions
   #:pause-all-actions-for-target
   #:resume-all-actions-for-target
   #:*action-manager*))
(in-package :xmas.action-manager)

(defstruct manager
  (actions (make-array 128 :element-type t :adjustable t :fill-pointer 0))
  running
  pending-deletions
  pending-additions)

(defstruct act
  action
  target
  paused
  started
  tag)

(defvar *action-manager* nil)

(defun maybe-start-action (act)
  (unless (act-started act)
    (setf (act-started act) t)
    (xmas.action:start-with-target (act-action act) (act-target act))))

(defun add-act (manager act)
  (vector-push-extend act (manager-actions manager))
  (maybe-start-action act))

(defun add-action (manager action target &key paused tag)
  (let ((act (make-act :action action
                       :target target
                       :paused paused
                       :tag tag)))
    (if (manager-running manager)
        (push act (manager-pending-additions manager))
        (add-act manager act))))

(defun remove-all-actions-for-target (manager target &key tag)
  (if (manager-running manager)
      (push (cons target tag) (manager-pending-deletions manager))
      (if tag
          (setf (manager-actions manager)
                (delete-if (lambda (act)
                             (and (eq target (act-target act))
                                  (eq tag (act-tag act))))
                           (manager-actions manager)))
          (setf (manager-actions manager)
                (delete target (manager-actions manager)
                        :key #'act-target)))))

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
    (setf (manager-running manager) t)
    (unwind-protect
         (loop for act across (manager-actions manager)
            for paused = (act-paused act)
            for action = (act-action act)
            do
              (unless paused
                (if (xmas.action:stopped-p action)
                    (setf stopped t)
                    (xmas.action:step-action action dt))))
      (setf (manager-running manager) nil))
    (when-let (deletions (manager-pending-deletions manager))
      (loop for (target . tag) in deletions do
        (remove-all-actions-for-target manager target :tag tag))
      (setf (manager-pending-deletions manager) nil))
    (when stopped
      (setf (manager-actions manager)
            (delete-if #'xmas.action:stopped-p (manager-actions manager) :key #'act-action)))
    (when-let (additions (manager-pending-additions manager))
      (dolist (act additions)
        (add-act manager act))
      (setf (manager-pending-additions manager) nil)) ))


