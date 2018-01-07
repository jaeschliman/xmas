(defpackage :texture-packer (:use :cl :alexandria)
            (:export
             #:texture-packer-get-frame
             #:texture-packer-from-file
             #:texture-packer-add-frames-from-file))
(in-package :texture-packer)

#| requires file exported as 'JSON Array' |#

(defstruct texture-packer-file
  path image-path width height texture
  (table (make-hash-table :test 'equal)))

(defun read-packer-file (path)
  (with-input-from-file (s path)
    (cl-json:decode-json s)))

(defun file-pathname-relative-to-file (path file)
  (let ((dir (make-pathname :directory (pathname-directory (truename path)))))
    (merge-pathnames file dir)))

(defun texture-packer-from-file (path)
  (flet ((r (data k) (cdr (assoc k data))))
    (let* ((data (read-packer-file path))
           (frames (r data :frames))
           (meta   (r data :meta))
           (image  (r meta :image))
           (image-path (file-pathname-relative-to-file path image))
           (texture (texture:get-texture (namestring image-path)))
           (size   (r meta :size))
           (result (make-texture-packer-file
                    :path path
                    :texture texture
                    :image-path image
                    :width (r size :w)
                    :height (r size :h)))
           (table (texture-packer-file-table result)))
      (prog1 result
        (dolist (frame frames)
          (let ((f (r frame :frame)))
            (setf (gethash (r frame :filename) table)
                  (texture:texture-frame
                   texture
                   (r f :x) (r f :y)
                   (r f :w) (r f :h)
                   :flipped nil
                   :rotated (r frame :rotated)))))))))

(defun texture-packer-get-frame (texture-packer-file path)
  (gethash path (texture-packer-file-table texture-packer-file)))

(defun texture-packer-add-frames-from-file (path)
  (let ((p (texture-packer-from-file path)))
    (maphash (lambda (k v)
               (texture:texture-manager-add-frame k v))
             (texture-packer-file-table p))
    t))
