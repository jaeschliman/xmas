(in-package :xmas.node)

(defmethod draw-with-xform ((self node) matrix)
  (declare (ignore matrix)))

(defmethod visit-with-xform ((self node))
  (when (visible self)
    (with-pushed-matrix (m)
      (apply-node-transform self m)
      (draw-with-xform self m)
      (when-let (children (children self))
        (loop for child across children do
               (visit-with-xform child))))))
