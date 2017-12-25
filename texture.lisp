(in-package :cl-user)

(defstruct texture
  id width height path)

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

(defun load-texture (display pathname)
  (when (probe-file (truename pathname))
    (let ((texture (make-texture :path pathname)))
      (display-enqueue-for-gl
       display
       (lambda () (gl-load-texture texture)))
      texture)))