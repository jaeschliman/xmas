(in-package :xmas.node)

(defmethod run-action ((self node) action &key repeat ease tag)
  (when (listp action)
    (setf action (apply 'xmas.action:run-sequence action)))
  (when ease
    (if-let (fn (xmas.action:find-easing-function ease))
      (setf action (funcall fn action))
      (error "unknown easing function: ~S" ease)))
  (when repeat
    (if (eq repeat :forever)
        (setf action (xmas.action:repeat-forever action))
        (error "unknown repeat method: ~S" repeat)))
  (if (running self)
      (xmas.action-manager:add-action xmas.action-manager:*action-manager* action self :tag tag)
      (push (cons action tag) (pending-actions self))))

(defmethod stop-all-actions ((self node) &key tag)
  (xmas.action-manager:remove-all-actions-for-target
   xmas.action-manager:*action-manager* self :tag tag)
  (setf (pending-actions self)
        (if tag (remove tag (pending-actions self) :key #'cdr) nil)))

(defmethod on-enter ((self node))
  (setf (running self) t)
  (when-let (actions (pending-actions self))
    (loop for (action . tag) in actions do
      (run-action self action :tag tag))
    (setf (pending-actions self) nil))
  (when-let (children (children self))
    (loop for child across children do
         (on-enter child))))

(defmethod on-exit ((self node))
  (setf (running self) nil)
  (when-let (children (children self))
    (loop for child across children do
         (on-exit child))))

(defmethod cleanup ((self node))
  (stop-all-actions self)
  (when-let (children (children self))
    (loop for child across children do
         (cleanup child))))

(defmethod remove-child ((parent node) (child node) &optional (cleanup t))
  (when-let (children (children parent))
    (setf (children parent) (delete child children)))
  (setf (parent child) nil)
  (when cleanup
    (when (running child)
      (on-exit child))
    (cleanup child)))

(defvar *adding-children* nil)

(defmethod add-child ((parent node) (child node))
  (unless (children parent)
    (setf (children parent) (make-array 0 :element-type t :fill-pointer 0 :adjustable t)))
  (when-let (child-parent (parent child))
    ;;remove from parent but don't clean up if we are reparenting
    (remove-child child-parent child) nil)
  (vector-push-extend child (children parent))
  (unless *adding-children*
    (setf (children parent) (stable-sort (children parent) #'< :key #'z-order)))
  (setf (parent child) parent)
  (mark-as-dirty child)
  (when (and (running parent) (not (running child)))
    (on-enter child)))

(defmethod add-children ((parent node) child-nodes)
  (let ((*adding-children* t))
    (dolist (child child-nodes)
      (add-child parent child))
    (setf (children parent) (stable-sort (children parent) #'< :key #'z-order))))

(defmethod remove-from-parent ((child node))
  (when (parent child)
    (remove-child (parent child) child)))
