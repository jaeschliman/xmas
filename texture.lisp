(defpackage :xmas.texture (:use :cl :alexandria)
            (:export
             #:texture-id
             #:texture-width
             #:texture-height
             #:texture-path
             #:*texture-manager*
             #:make-texture-manager
             #:get-texture
             #:texture-frame
             #:make-texture-frame
             #:texture-frame-texture
             #:texture-frame-tx1
             #:texture-frame-ty1
             #:texture-frame-tx2
             #:texture-frame-ty2
             #:texture-frame-width
             #:texture-frame-height
             #:texture-frame-rotated
             #:texture-manager
             #:texture-manager-table
             #:texture-manager-display
             #:texture-manager-frames
             #:texture-manager-add-frame
             #:get-frame
             #:set-default-texture-directory
             #:texture-manager-release-all-textures
             #:make-texture-from-rgba-vector))

(in-package :xmas.texture)

(defstruct texture
  (id nil :type (or null (unsigned-byte 32)))
  (width  0.0 :type single-float)
  (height 0.0 :type single-float)
  path)

(defstruct texture-frame
  texture
  (tx1 0.0 :type single-float)
  (ty1 0.0 :type single-float)
  (tx2 0.0 :type single-float)
  (ty2 0.0 :type single-float)
  (width 0.0 :type single-float)
  (height 0.0 :type single-float)
  (rotated nil :type boolean))

(defun texture-frame (texture x y w h &key (flipped t) (rotated nil))
  (setf x (float x)
        y (float y)
        w (float w)
        h (float h))
  (when rotated (rotatef w h))
  (let* ((width (float (texture-width texture)))
         (height (float (texture-height texture)))
         (y (if flipped (- h y) y)) ;; flip y coord
         (x2 (float (+ x w)))
         (y2 (float (+ y h)))
         (tx1 (/ x  width))
         (ty1 (/ y  height))
         (tx2 (/ x2 width))
         (ty2 (/ y2 height)))
    (make-texture-frame :texture texture
                        :rotated rotated
                        :tx1 tx1 :ty1 ty1
                        :tx2 tx2 :ty2 ty2
                        :width w :height h)))

(defstruct texture-manager
  (table (make-hash-table :test 'equal))
  display
  (frames (make-hash-table :test 'equal))
  default-resource-dir)

(defvar *texture-manager* nil)

(defun set-default-texture-directory (dir)
  (when-let (mgr *texture-manager*)
    (setf (texture-manager-default-resource-dir mgr) dir)))

(defun load-image-rep (path)
  (let* ((p (truename path))
         (rep (ccl::with-autoreleased-nsstring (s (princ-to-string p))
                (#/imageRepWithContentsOfFile: ns:ns-bitmap-image-rep s)))
         (has-alpha (if (#/hasAlpha rep) #$YES #$NO))
         (width (#/pixelsWide rep))
         (height (#/pixelsHigh rep))
         (format (if has-alpha :rgba :rgb)))
    (values rep width height format)))

(defun gl-load-texture (texture)
  (let* ((p (truename (texture-path texture)))
         (rep (ccl::with-autoreleased-nsstring (s (princ-to-string p))
                (#/imageRepWithContentsOfFile: ns:ns-bitmap-image-rep s)))
         (has-alpha (if (#/hasAlpha rep) #$YES #$NO))
         (size (#/size rep))
         (w (ns:ns-size-width size))
         (h (ns:ns-size-height size))
         (dat (#/bitmapData rep))
         (fmt (if has-alpha :rgba :rgb)))
    (let ((id (car (gl:gen-textures 1))))
      (gl:bind-texture :texture-2d id)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-image-2d :texture-2d 0 :rgba
                       w h 0 fmt
                       :unsigned-byte dat)
      (setf (texture-width texture) (float w)
            (texture-height texture) (float h)
            (texture-id texture) id))))

(defun gl-load-texture-image-rep (texture rep format &key wrap)
  (let ((w (ceiling (texture-width texture)))
        (h (ceiling (texture-height texture)))
        (dat (#/bitmapData rep)))
    (let ((id (car (gl:gen-textures 1))))
      (gl:enable :texture-2d)
      (gl:bind-texture :texture-2d id)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (when wrap
        (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
        (gl:tex-parameter :texture-2d :texture-wrap-t wrap))
      (gl:tex-image-2d :texture-2d 0 :rgba
                       w h 0 format
                       :unsigned-byte dat)
      (gl:disable :texture-2d)
      (gl:bind-texture :texture-2d 0)
      (setf (texture-id texture) id))))

(defun gl-load-texture-from-rgba-vector (texture vector &key wrap)
  (let ((id (car (gl:gen-textures 1)))
        (width (ceiling (texture-width texture)))
        (height (ceiling (texture-height texture))))
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d id)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (when wrap
      (gl:tex-parameter :texture-2d :texture-wrap-s wrap)
      (gl:tex-parameter :texture-2d :texture-wrap-t wrap))
    (static-vectors:with-static-vector (static-vector (length vector) :element-type '(unsigned-byte 8) :initial-element 0)
      (map-into static-vector 'identity vector)
      (gl:tex-image-2d :texture-2d 0 :rgba
                       width height 0 :rgba
                       :unsigned-byte (static-vectors:static-vector-pointer static-vector)))
    (gl:disable :texture-2d)
    (gl:bind-texture :texture-2d 0)
    (setf (texture-id texture) id)))

(defun load-texture-on-display (display pathname &key wrap)
  (when (probe-file (truename pathname))
    (ccl::with-autorelease-pool
      (multiple-value-bind (rep width height format)
          (load-image-rep pathname)
        (let ((texture (make-texture :path pathname
                                     :width (float width)
                                     :height (float height))))
          (#/retain rep)
          (xmas.display:display-enqueue-for-gl
           display
           (lambda ()
             (gl-load-texture-image-rep texture rep format :wrap wrap)
             (#/release rep)))
          texture)))))

(defun make-texture-from-rgba-vector (vector width height &key wrap)
  (when-let (mgr *texture-manager*)
    (let ((texture (make-texture :path nil
                                 :width (float width)
                                 :height (float height))))
      (xmas.display:display-enqueue-for-gl
       (texture-manager-display mgr)
       (lambda () (gl-load-texture-from-rgba-vector texture vector :wrap wrap)))
      texture)))

(defun get-or-load-texture (texture-manager path &key wrap)
  (let ((key (if wrap (list path :wrap wrap) path)))
    (ensure-gethash
     key (texture-manager-table texture-manager)
     (load-texture-on-display (texture-manager-display texture-manager) path :wrap wrap))))

(defun get-texture (path &key wrap)
  (when-let (mgr *texture-manager*)
    (let ((default (texture-manager-default-resource-dir mgr)))
      (when default
        (setf path (merge-pathnames path default)))
      (get-or-load-texture mgr path :wrap wrap))))

(defun get-frame (path)
  (when-let (mgr *texture-manager*)
    (gethash path (texture-manager-frames mgr))))

(defun texture-manager-add-frame (path frame)
  (when-let (mgr *texture-manager*)
    (setf (gethash path (texture-manager-frames mgr)) frame)))

;;NOTE: must be called with correct opengl context current
(defun texture-manager-release-all-textures (mgr)
  (let* ((textures (hash-table-values (texture-manager-table mgr)))
         (ids (mapcar #'texture-id textures)))
    (loop for texture in textures do
         (setf (texture-id texture) nil))
    (gl:delete-textures ids)))
