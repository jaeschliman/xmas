(defpackage :xmas.matrix (:use :cl :alexandria)
            (:export
             #:make-matrix
             #:copy-matrix
             #:unwrap-matrix
             #:into-matrix
             #:cat-matrix
             #:left-cat-matrix
             #:*current-matrix*
             #:*tmp-matrix*
             #:load-identity
             #:load-translation
             #:load-rotation
             #:load-scale
             #:load-matrix
             #:translate
             #:rotate
             #:scale
             #:matrix-multiply-point-2d
             #:load-translation-rotation))

(in-package :xmas.matrix)

(deftype subscript () '(integer 0 3))
(deftype matrix-index () '(integer 0 15))
(deftype matrix () '(simple-array single-float (16)))

;;column-major 4x4 matrix
(defun make-m4/unwrapped ()
  (make-array '(16) :initial-element 0.0 :element-type 'single-float))

(declaim (ftype (function (matrix subscript subscript) single-float)
                mref/unwrapped))
(defun mref/unwrapped (m col row)
  (declare (type matrix m)
           (type subscript col row)
           (optimize (speed 3) (safety 1)))
  (the single-float (aref m (the matrix-index (+ row (* col 4))))))

(define-compiler-macro mref/unwrapped (&whole w m col row)
  (cond
    ((and (numberp col)
          (numberp row))
     `(aref (the matrix ,m) (the matrix-index ,(+ row (* col 4)))))
    ((numberp col)
     `(aref (the matrix ,m) (the matrix-index (+ (the subscript ,row)
                                                 (the fixnum ,(* col 4))))))
    ((numberp row)
     `(aref (the matrix ,m) (the matrix-index (+ (the subscript ,row)
                                                 (the fixnum
                                                      (* (the subscript ,col)
                                                         4))))))
    (t w)))

;; TODO: setf-expander for this?
;; (defun (setf mref/unwrapped) (v m col row)
;;   (setf (aref m (+ row (* col 4))) v))

(define-setf-expander mref/unwrapped (m col row &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion m env)
    (declare (ignore newval setter))
    (with-gensyms (store colsym rowsym)
      (values (list* colsym rowsym dummies)
              (list* col row vals)
              `(,store)
              `(locally
                   (declare (optimize (speed 3) (safety 1)))
                 (setf
                  (aref (the matrix ,getter)
                        (the matrix-index
                             (+ (the subscript ,rowsym)
                                (the fixnum (* (the subscript ,colsym) 4)))))
                  ,store))
              `(mref/unwrapped ,getter ,colsym ,rowsym)))))

(defun load-identity/unwrapped (m)
  (declare (type matrix m)
           (optimize (speed 3) (safety 0)))
  (loop
     for i from 0 to 15
     do (setf (aref m i) 0.0))
  (setf (mref/unwrapped m 0 0) 1.0
        (mref/unwrapped m 1 1) 1.0
        (mref/unwrapped m 2 2) 1.0
        (mref/unwrapped m 3 3) 1.0))

(defun load-translation/unwrapped (x y m)
  (declare (type matrix m)
           (type single-float x y)
           (optimize (speed 3) (safety 1)))
  (setf (mref/unwrapped m 3 0) x
        (mref/unwrapped m 3 1) y))

(defun deg->rad (n)
  (declare (type single-float n))
  (* (load-time-value (coerce pi 'single-float)) (/ n 180.0)))

(defun load-rotation/unwrapped (deg m &aux (theta (deg->rad deg)))
  (declare (type matrix m)
           (type single-float deg)
           (optimize (speed 3) (safety 1)))
  (let ((s (the single-float
                #+ccl
                (#_sinf (the single-float theta))
                #-ccl
                (sin (the single-float theta))))
        (c (the single-float
                #+ccl
                (#_cosf (the single-float theta))
                #-ccl
                (cos (the single-float theta)))))
    (let ((-s (- s)))
      (setf
       (mref/unwrapped m 0 0)  c
       (mref/unwrapped m 1 0) -s
       (mref/unwrapped m 0 1)  s
       (mref/unwrapped m 1 1)  c))))

(defun load-scale/unwrapped (sx sy m)
  (declare (type matrix m)
           (type single-float sx sy)
           (optimize (speed 3) (safety 1)))
  (setf
   (mref/unwrapped m 0 0) sx
   (mref/unwrapped m 1 1) sy))


;;this method gets called ALOT -- could use some optimizing.
;;cat o into m
(defun cat-matrix/unwrapped (o m)
  (let ((tmp (make-array 16 :initial-element 0.0 :element-type 'single-float)))
    (declare ;;arggggh
     (optimize (speed 3) (safety 0) (compilation-speed 0))
     (dynamic-extent tmp)
     (type matrix m o tmp))
    (macrolet
        ((@ (m col row)
             `(the single-float
                   (aref (the matrix ,m)
                         (the matrix-index
                              (+ (the subscript ,row)
                                 (the fixnum (* (the subscript ,col) 4)))))))
         (*f (a b)
           `(the single-float (* (the single-float ,a)
                                 (the single-float ,b)))))
      (loop for col from 0 to 3
         for e = (@ o col 0)
         for f = (@ o col 1)
         for g = (@ o col 2)
         for h = (@ o col 3) do
           ;; attempt a cache-friendly inner loop...
           (loop for row from 0 to 3
              for a = (@ m 0 row)
              for b = (@ m 1 row)
              for c = (@ m 2 row)
              for d = (@ m 3 row) do
                (setf (@ tmp col row)
                      (the single-float 
                           (+ (*f a e)
                              (*f b f)
                              (*f c g)
                              (*f d h))))))
      ;;but that means we need to copy out
      (loop for i below 16 do
           (setf (aref m i) (aref tmp i))))))

(defstruct m4 (vector (make-m4/unwrapped) :type matrix))

(defvar *current-matrix* nil)
(defvar *tmp-matrix* nil)

(defun make-matrix () (make-m4))
(defun copy-matrix (m4) (make-m4 :vector (copy-seq (m4-vector m4))))
(defun unwrap-matrix (m4) (m4-vector m4))

(defmacro into-matrix ((&optional name) &body b)
  (if name
      `(let ((*current-matrix* ,name)
             (*tmp-matrix* (or *tmp-matrix* (make-m4))))
         (prog1 *current-matrix*
           ,@b))
      `(let ((*current-matrix* (make-m4))
             (*tmp-matrix* (or *tmp-matrix* (make-m4))))
         (prog1 *current-matrix*
           ,@b))))

;; args are in reverse order, as the left hand side is
;; the implied argument
(defun cat-matrix (other &optional (m4 *current-matrix*))
  (cat-matrix/unwrapped (m4-vector other) (m4-vector m4)))

(defun left-cat-matrix (other &optional (m *current-matrix*))
  ;;hmmmm.
  ;; c o m = MxO into M
  ;; l-c-m o m = OxM into M.

  ;; could probably rewrite this without
  ;; the extra storage with enough effort.
  
  ;; M <- OxM
  ;; copy O into T
  (load-matrix other *tmp-matrix*)
  ;; T <- TxM 
  (cat-matrix m *tmp-matrix*)
  ;; copy T into M
  (load-matrix *tmp-matrix* m))

(defun load-identity (&optional (m4 *current-matrix*))
  (load-identity/unwrapped (m4-vector m4))) 

(defun load-translation (x y &optional (m4 *current-matrix*))
  (load-identity/unwrapped (m4-vector m4))
  (load-translation/unwrapped x y (m4-vector m4)))

(defun load-translation-rotation (x y r &optional (m4 *current-matrix*))
  (let ((vec (m4-vector m4)))
    (load-identity/unwrapped vec)
    (load-translation/unwrapped x y vec)
    (load-rotation/unwrapped r vec)))

(defun load-rotation (deg &optional (m4 *current-matrix*))
  (load-identity/unwrapped (m4-vector m4))
  (load-rotation/unwrapped deg (m4-vector m4)))

(defun load-scale (sx sy &optional (m4 *current-matrix*))
  (load-identity/unwrapped (m4-vector m4))
  (load-scale/unwrapped sx sy (m4-vector m4)))

(defun load-matrix (other &optional (m *current-matrix*))
  (loop for i from 0 to 15
     with o = (m4-vector other)
     with v = (m4-vector m)
     do (setf (aref v i) (aref o i))))

(defun translate (x y)
  (unless (= x y 0.0)
    (load-translation x y *tmp-matrix*)
    (cat-matrix *tmp-matrix*)))

(defun rotate (deg)
  (unless (or (= deg 0.0)
              (= deg 360.0))
    (load-rotation deg *tmp-matrix*)
    (cat-matrix *tmp-matrix*)))

(defun scale (sx sy)
  (unless (= sx sy 1.0)
    (load-scale sx sy *tmp-matrix*)
    (cat-matrix *tmp-matrix*)))

(defun matrix-multiply-point-2d (matrix x y)
  (declare (optimize (speed 3) (safety 1)))
  (let ((m (m4-vector matrix))
        (input (vector x y 0.0 1.0))
        (output (vector 0.0 0.0 0.0 0.0)))
    (declare (dynamic-extent input output))
    (loop for row from 0 to 3 do
         (setf (svref output row)
               (the single-float
                    (loop for col from 0 to 3 sum
                         (the single-float
                              (* (the single-float (svref input col))
                                 (the single-float
                                      (aref m (+ (* col 4) row)))))))))
    (values (svref output 0) (svref output 1))))
