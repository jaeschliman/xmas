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
             #:matrix-multiply-point-2d))

(in-package :xmas.matrix)

;;column-major 4x4 matrix
(defun make-m4/unwrapped ()
  (make-array '(16) :initial-element 0.0 :element-type 'single-float))

(defun mref/unwrapped (m col row)
  (aref m (+ row (* col 4))))

(define-compiler-macro mref/unwrapped (&whole w m col row)
  (cond
    ((and (numberp col)
          (numberp row))
     `(aref ,m ,(+ row (* col 4))))
    ((numberp col)
     `(aref ,m (+ ,row ,(* col 4))))
    ((numberp row)
     `(aref ,m (+ ,row (* ,col 4))))
    (t w)))

;; TODO: setf-expander for this?
(defun (setf mref/unwrapped) (v m col row)
  (setf (aref m (+ row (* col 4))) v))

(defun load-identity/unwrapped (m)
  (loop
     for i from 0 to 15
     do (setf (aref m i) 0.0))
  (setf (mref/unwrapped m 0 0) 1.0
        (mref/unwrapped m 1 1) 1.0
        (mref/unwrapped m 2 2) 1.0
        (mref/unwrapped m 3 3) 1.0))

(defun load-translation/unwrapped (x y m)
  (load-identity/unwrapped m)
  (setf (mref/unwrapped m 3 0) x
        (mref/unwrapped m 3 1) y))

(defun deg->rad (n)
  (* pi (/ n 180.0)))

(defun load-rotation/unwrapped (deg m &aux (theta (deg->rad deg)))
  (let ((s (coerce (sin theta) 'single-float))
        (c (coerce (cos theta) 'single-float)))
    (let ((-s (- s)))
      (load-identity/unwrapped m)
      (setf
       (mref/unwrapped m 0 0)  c
       (mref/unwrapped m 1 0) -s
       (mref/unwrapped m 0 1)  s
       (mref/unwrapped m 1 1)  c))))

(defun load-scale/unwrapped (sx sy m)
  (load-identity/unwrapped m)
  (setf
   (mref/unwrapped m 0 0) sx
   (mref/unwrapped m 1 1) sy))

;;this method gets called ALOT -- could use some optimizing.
(defun cat-matrix/unwrapped (o m)
  (macrolet ((@ (&rest args) `(mref/unwrapped ,@args)))
    (loop for row from 0 to 3
     for a = (@ m 0 row)
     for b = (@ m 1 row)
     for c = (@ m 2 row)
     for d = (@ m 3 row) do
       (loop for col from 0 to 3
          for e = (@ o col 0)
          for f = (@ o col 1)
          for g = (@ o col 2)
          for h = (@ o col 3) do
            (setf (@ m col row)
                  (+ (* a e)
                     (* b f)
                     (* c g)
                     (* d h)))))))

(defstruct m4 (vector (make-m4/unwrapped)))

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
  (load-translation/unwrapped x y (m4-vector m4)))

(defun load-rotation (deg &optional (m4 *current-matrix*))
  (load-rotation/unwrapped deg (m4-vector m4)))

(defun load-scale (sx sy &optional (m4 *current-matrix*))
  (load-scale/unwrapped sx sy (m4-vector m4)))

;; args are in reverse order, as the left hand side is
;; the implied argument
(defun cat-matrix (other &optional (m4 *current-matrix*))
  (cat-matrix/unwrapped (m4-vector other) (m4-vector m4)))

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
