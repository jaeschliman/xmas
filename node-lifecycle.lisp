(in-package :node)

(defmethod run-action ((self node) action)
  (if (running self)
      (action-manager:add-action action-manager:*action-manager* action self nil)
      (push action (pending-actions self))))

(defmethod stop-all-actions ((self node))
  (action-manager:remove-all-actions-for-target
   action-manager:*action-manager* self)
  (setf (pending-actions self) nil))

(defmethod on-enter ((self node))
  (setf (running self) t)
  (when-let (actions (pending-actions self))
    (dolist (action actions)
      (run-action self action))
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

(defmethod add-child ((parent node) (child node))
  (unless (children parent)
    (setf (children parent) (make-array 0 :element-type t :fill-pointer 0 :adjustable t)))
  (when-let (child-parent (parent child))
    ;;remove from parent but don't clean up if we are reparenting
    (remove-child child-parent child) nil)
  (vector-push-extend child (children parent))
  (setf (parent child) parent)
  (mark-as-dirty child)
  (when (and (running parent) (not (running child)))
    (on-enter child)))


(defmethod remove-from-parent ((child node))
  (when (parent child)
    (remove-child (parent child) child)))
