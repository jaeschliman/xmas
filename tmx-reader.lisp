(defpackage :tmx-reader (:use :cl :alexandria)
            (:export
             #:read-tilemap
             #:map
             #:make-map
             #:map-tile-width
             #:map-tile-height
             #:map-width
             #:map-height
             #:map-tilesets
             #:map-layers
             #:layer
             #:make-layer
             #:layer-name
             #:layer-width
             #:layer-height
             #:layer-data
             #:layer-gid-at
             #:tileset
             #:make-tileset
             #:tileset-name
             #:tileset-source
             #:tileset-tile-width
             #:tileset-tile-height
             #:tileset-first-gid
             #:tileset-tile-properties))
(in-package :tmx-reader)

(defun file-pathname-relative-to-file (path file)
  (let ((dir (make-pathname :directory (pathname-directory (truename path)))))
    (merge-pathnames file dir)))

;;binary utilities

(defun 8->32 (a b c d)
  (let ((int 0))
    (setf (ldb (byte 8 0) int) a)
    (setf (ldb (byte 8 8) int) b)
    (setf (ldb (byte 8 16) int) c)
    (setf (ldb (byte 8 24) int) d)
    int))

(defun vec8->32 (in)
  (let* ((len (/ (length in) 4))
         (out (make-array len :element-type '(unsigned-byte 32))))
    (loop
       for i below len
       for j = (* i 4)
       for k = (1+ j)
       for l = (1+ k)
       for m = (1+ l)
       do
	 (setf (aref out i) (8->32 (aref in j)
                                   (aref in k)
                                   (aref in l)
                                   (aref in m)))
       finally (return out))))


(defun unzip-byte-vector (zipped-vector)
  (chipz:decompress nil 'chipz:zlib zipped-vector))

(defun base64-string->usb8-vector (string)
  (cl-base64:base64-string-to-usb8-array string))

(defun expand-tile-data (string)
  (vec8->32 (unzip-byte-vector (base64-string->usb8-vector string))))


(defstruct tileset
  name
  source
  tile-width
  tile-height
  first-gid
  tile-properties)

(defstruct layer
  name
  width
  height
  data)

(defstruct map
  tile-width
  tile-height
  width
  height
  tilesets
  layers)

(defun tag-name (it) (first it))
(defun get-attr (it attr)
  (when-let (found (assoc attr (second it) :test 'string=))
    (second found)))
(defun children (it)
  (nthcdr 2 it))

(defun shared-parse-tileset (it path first-gid)
 (let* ((name (get-attr it "name"))
           (source (get-attr (first (children it)) "source"))
           (tile-width (parse-integer (get-attr it "tilewidth")))
           (tile-height (parse-integer (get-attr it "tileheight")))
           (first-gid (or first-gid (parse-integer (get-attr it "firstgid"))))
           (tilecount (parse-integer (get-attr it "tilecount")))
           (tile-properties (make-array tilecount :initial-element nil)))
      (make-tileset :name name
                    :source (file-pathname-relative-to-file path source)
                    :tile-width tile-width
                    :tile-height tile-height
                    :first-gid first-gid
                    :tile-properties tile-properties)) )

(defun parse-tsx-file (first-gid path)
  (let* ((it (with-input-from-file (s path)
               (xmls:parse s :compress-whitespace t)))
         (tileset (shared-parse-tileset it path first-gid)))
    tileset))

(defun parse-tileset (it path)
  (if-let (external-source (get-attr it "source"))
    (let ((first-gid (parse-integer (get-attr it "firstgid")))
          (file (file-pathname-relative-to-file path external-source)))
      (parse-tsx-file first-gid file))
    (shared-parse-tileset it path nil)))

(defun parse-data (it)
  (let* ((compression (get-attr it "compression"))
         (encoding (get-attr it "encoding"))
         (string (first (children it))))
    (assert (string= compression "zlib"))
    (assert (string= encoding "base64"))
    (expand-tile-data string)))

(defun parse-layer (it path)
  (let* ((name (get-attr it "name"))
         (width (parse-integer (get-attr it "width")))
         (height (parse-integer (get-attr it "height")))
         (data (parse-data (first (children it)))))
    (make-layer :name name :width width :height height :data data)))

(defun layer-gid-at (layer x y)
  (let ((gid (aref (layer-data layer) (+ x (* (layer-width layer) y)))))
    ;; TODO: handle flip masks
    (values gid nil nil)))

(defun parse-map (it path)
  (assert (string= "orthogonal" (get-attr it "orientation")))
  (let* ((tile-width (parse-integer (get-attr it "tilewidth")))
         (tile-height (parse-integer (get-attr it "tileheight")))
         (width (parse-integer (get-attr it "width")))
         (height (parse-integer (get-attr it "height")))
         (tilesets nil)
         (layers nil))
    (dolist (ch (children it))
      (cond ((string= "tileset" (tag-name ch))
             (push (parse-tileset ch path) tilesets))
            ((string= "layer" (tag-name ch))
             (push (parse-layer ch path) layers))
            (t (error "~S tag is unimplmented" (tag-name ch)))))
    (make-map :tile-width tile-width
              :tile-height tile-height
              :width width
              :height height
              :tilesets tilesets
              :layers layers)))

(defun read-tilemap (path)
  (with-input-from-file (s path)
    (parse-map (xmls:parse s :compress-whitespace t) path)))
