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
             #:get-frame))

(in-package :xmas.texture)

(defstruct texture
  id width height path)

(defstruct texture-frame
  texture tx1 ty1 tx2 ty2 width height rotated)

(defun texture-frame (texture x y w h &key (flipped t) (rotated nil))
  (when rotated (rotatef w h))
  (let* ((width (texture-width texture))
         (height (texture-height texture))
         (y (if flipped (- h y) y)) ;; flip y coord
         (x2 (+ x w))
         (y2 (+ y h))
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
  (frames (make-hash-table :test 'equal)))

(defvar *texture-manager* nil)

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
      (setf (texture-width texture) w
            (texture-height texture) h
            (texture-id texture) id))))

(defun gl-load-texture-image-rep (texture rep format)
  (let ((w (texture-width texture))
        (h (texture-height texture))
        (dat (#/bitmapData rep)))
    (let ((id (car (gl:gen-textures 1))))
      (gl:bind-texture :texture-2d id)
      (gl:tex-parameter :texture-2d :texture-min-filter :linear)
      (gl:tex-image-2d :texture-2d 0 :rgba
                       w h 0 format
                       :unsigned-byte dat)
      (setf (texture-id texture) id))))

(defun load-texture-on-display (display pathname)
  (when (probe-file (truename pathname))
    (let ((load-across-threads t))
      (if load-across-threads
          (multiple-value-bind (rep width height format)
              (load-image-rep pathname)
            (let ((texture (make-texture :path pathname
                                         :width width
                                         :height height)))
              (#/retain rep)
              (xmas.display:display-enqueue-for-gl
               display
               (lambda ()
                 (gl-load-texture-image-rep texture rep format)
                 (#/release rep)))
              texture))
          (let ((texture (make-texture :path pathname)))
            (xmas.display:display-enqueue-for-gl
             display
             (lambda () (gl-load-texture texture)))
            texture)))))

(defun get-or-load-texture (texture-manager path)
  (ensure-gethash
   path (texture-manager-table texture-manager)
   (load-texture-on-display (texture-manager-display texture-manager) path)))

(defun get-texture (path)
  (when-let (mgr *texture-manager*)
    (get-or-load-texture mgr path)))

(defun get-frame (path)
  (when-let (mgr *texture-manager*)
    (gethash path (texture-manager-frames mgr))))

(defun texture-manager-add-frame (path frame)
  (when-let (mgr *texture-manager*)
    (setf (gethash path (texture-manager-frames mgr)) frame)))
