(defpackage :xmas.animation-manager (:use :cl :alexandria :texture :sprite :action)
            (:export
             #:*animation-manager*
             #:make-manager
             #:add-animation
             #:get-animation))
(in-package :xmas.animation-manager)

(defvar *animation-manager* nil)

(defstruct manager
  (table (make-hash-table :test 'eq)))

(defun add-animation (name delay framelist)
  (when-let (mgr *animation-manager*)
    (setf (gethash name (manager-table mgr)) (cons delay (map 'vector #'get-frame framelist)))))

(defun get-animation (name)
  (when-let* ((mgr *animation-manager*)
              (data (gethash name (manager-table mgr))))
    (destructuring-bind (delay . frames) data
      (sprite-animation-action delay frames))))
