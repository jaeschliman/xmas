(defpackage :xmas.game-object (:use :cl :alexandria :xmas.node :xmas.sprite :xmas.qtree)
            (:export
             #:game-object
             #:game-sprite
             #:game-object-manager
             #:make-game-object-manager
             #:game-object-manager-sprite-node
             #:game-object-manager-objects
             #:game-object-manager-awake-objects
             #:game-object-manager-object-qtree
             #:game-object-manager-sprite-qtree
             #:game-object-manager-points
             #:wake-sprite
             #:wake
             #:to-sleep
             #:game-object-manager-set-active-area
             #:game-object-manager-add-object
             #:game-object-manager-sleep-all))
(in-package :xmas.game-object)

(defmacro with-struct ((prefix &rest slots) var &body body)
  (once-only (var)
    `(symbol-macrolet
         ,(loop for slot in slots collect
               (list slot (list (symbolicate prefix slot) var)))
       ,@body)))

(defclass game-object ()
  ((x :reader x :initarg :x) ;; immutable
   (y :reader y :initarg :y) ;; immutable
   (sprite :accessor sprite :initarg :sprite)
   (sleeping :accessor sleeping :initform t)))

(defmethod width  ((self game-object)) 0.0)
(defmethod height ((self game-object)) 0.0)

(defclass belongs-to-game-object ()
  ((game-object :accessor game-object)))

(defclass game-sprite (sprite belongs-to-game-object)
  ())

(defstruct game-object-manager
  sprite-node
  (objects (make-array 256 :element-type t :adjustable t :fill-pointer 0))
  (awake-objects (make-array 256 :element-type t :adjustable t :fill-pointer 0))
  (object-qtree (qtree))
  (sprite-qtree (qtree))
  (points (make-hash-table :test 'eq)))

(defgeneric wake-sprite (sprite game-object)
  (:method (sprite game-object)
    (declare (ignore sprite game-object))))

(defmethod wake ((object game-object) manager)
  (setf (sleeping object) nil)
  (with-struct (game-object-manager- awake-objects sprite-node) manager
    (vector-push-extend object awake-objects)
    (when-let (sprite (sprite object))
      (wake-sprite sprite object)
      (add-child sprite-node sprite))))

(defmethod wake :around ((object game-object) manager)
  (declare (ignore manager))
  (when (sleeping object)
    (call-next-method)))

(defmethod to-sleep ((object game-object) manager)
  (setf (sleeping object) t)
  (with-struct (game-object-manager- awake-objects) manager
    (setf awake-objects (delete object awake-objects))
    (when-let (sprite (sprite object))
      (remove-from-parent sprite))))

(defmethod to-sleep :around ((object game-object) manager)
  (declare (ignore manager))
  (unless (sleeping object)
    (call-next-method)))

(defun game-object-manager-set-active-area (manager x y w h)
  (with-struct (game-object-manager- object-qtree) manager
      (qtree-reset object-qtree :x x :y y :width w :height h)))
  
(defun game-object-manager-add-object (manager object)
  (with-struct (game-object-manager- objects object-qtree) manager
    (vector-push-extend object objects)
    (qtree-add object-qtree object)))

(defun game-object-manager-sleep-all (manager)
  (map 'nil (lambda (object) (to-sleep object manager))
       (copy-seq (game-object-manager-awake-objects manager))))
