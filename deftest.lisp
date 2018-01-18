(defpackage :xmas.deftest (:use :cl :alexandria)
            (:export
             #:deftest
             #:run-test))
(in-package :xmas.deftest)

(defvar *tests* (make-hash-table :test 'eq))

(defun sections->plist (list)
  (unless (keywordp (first list))
    (error "missing section name"))
  (let (result (current-key (first list)) current-list)
    (dolist (item (rest list))
      (if (and (keywordp item) (not (eq item :=)))
          (setf result (nconc result (list current-key (nreverse current-list)))
                current-key item
                current-list nil)
          (push item current-list)))
    (nconc result (list current-key (nreverse current-list)))))

(defun parse-init-section (list)
  (let (forms slots)
    (loop while list
       for item = (pop list) do
         (if (symbolp item)
             (progn
               (assert (eq := (pop list)))
               (push item slots)
               (push `(setf ,item ,(pop list)) forms))
             (push item forms)))
    (values (nreverse forms) (nreverse slots))))

(defun parse-deftest (test-name plist display-options)
  (let ((name (symbolicate '%deftest%test- test-name)))
    (multiple-value-bind (init-body slots) (parse-init-section (getf plist :init))
      (flet ((sym (symbol)
               (intern (symbol-name symbol) *package*))
             (method-body (forms)
               `(with-slots ,slots self ,@forms)))
        (let ((update-body (getf plist :update))
              (event-handler-body (getf plist :on-event)))
          `(progn
             (defclass ,name ()
               ,(mapcar 'list slots))
             (defmethod cl-user::contents-will-mount ((self ,name) ,(sym 'display))
               (declare (ignorable ,(sym 'display)))
               ,(method-body init-body))
             (defmethod cl-user::step-contents ((self ,name) ,(sym 'dt))
               (declare (ignorable ,(sym 'dt)))
               ,(method-body update-body))
             (defmethod cl-user::handle-event ((self ,name) ,(sym 'event))
               (declare (ignorable ,(sym 'event)))
               ,(method-body event-handler-body))
             (setf (gethash ',test-name *tests*)
                   (lambda () (cl-user::display-contents (make-instance ',name) ,@display-options)))))))))

(defmacro deftest (name (&rest display-options) &body sections)
  (parse-deftest name (sections->plist sections) display-options))

(defun run-test (name)
  (funcall (gethash name *tests*)))
