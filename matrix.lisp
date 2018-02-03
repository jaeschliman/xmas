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

;;----------------------------------------
;; translation
;;
;; [a b c d] [1.0 0.0 0.0 x  ]
;; [e f g h] [0.0 1.0 0.0 y  ]
;; [i j k l] [0.0 0.0 1.0 0.0]
;; [m n o p] [0.0 0 0 0.0 1.0]
;;
;; d = a * x + b * y + d
;; h = e * x + f * y + h
;; l = i * x + j * y + l
;;
(defun translate/unwrapped (x y m)
  (declare (type single-float x y)
           (type matrix m)
           (optimize (speed 3) (safety 1)))
  (macrolet ((@ (x y) `(aref m ,(+ y (* x 4)))))
    (incf (@ 3 0) (+ (* (@ 0 0) x) (* (@ 1 0) y)))
    (incf (@ 3 1) (+ (* (@ 0 1) x) (* (@ 1 1) y)))
    (incf (@ 3 2) (+ (* (@ 0 2) x) (* (@ 1 2) y)))))

(defun translate (x y)
  (unless (= x y 0.0)
    (translate/unwrapped x y (m4-vector *current-matrix*))))
;;----------------------------------------
;; rotation
;;
;; [a b c d] [cs  -s  0.0 0.0]
;; [e f g h] [s   cs  0.0 0.0]
;; [i j k l] [0.0 0.0 1.0 0.0]
;; [m n o p] [0.0 0 0 0.0 1.0]
;;
;; a = a * cs + b * s
;; b = a * -s + b * cs
;; e = e * cs + f * s
;; f = e * -s + f * cs
;; i = i * cs + j * s  ;; may possibly be ignored for 2d (not sure)
;; j = i * -s + j * cs ;; may possibly be ignored for 2d (not sure)
;; m and n can be ignored

(defun rotate/unwrapped (deg m)
  (declare (type single-float deg)
           (type matrix m)
           (optimize (speed 3) (safety 1)))
  (let* ((theta (deg->rad deg))
         (s (the single-float
                 #+ccl
                 (#_sinf (the single-float theta))
                 #-ccl
                 (sin (the single-float theta))))
         (cs (the single-float
                  #+ccl
                  (#_cosf (the single-float theta))
                  #-ccl
                  (cos (the single-float theta))))
         (-s (- s)))
    (declare (type single-float s cs -s))
    (macrolet ((@ (x y) `(aref m ,(+ y (* x 4)))))
      (psetf
       (@ 0 0) (+ (* (@ 0 0) cs) (* (@ 1 0) s))
       (@ 1 0) (+ (* (@ 0 0) -s) (* (@ 1 0) cs))
       (@ 0 1) (+ (* (@ 0 1) cs) (* (@ 1 1) s))
       (@ 1 1) (+ (* (@ 0 1) -s) (* (@ 1 1) cs))
       ;;TODO: can these next two be ignored?
       (@ 0 2) (+ (* (@ 0 2) cs) (* (@ 1 2) s))
       (@ 1 2) (+ (* (@ 0 2) -s) (* (@ 1 2) cs))))))

(defun rotate (deg)
  (unless (or (= deg 0.0)
              (= deg 360.0))
    (rotate/unwrapped deg (m4-vector *current-matrix*))))

;;----------------------------------------
;; scale
;;
;; [a b c d] [sx  0.0 0.0 0.0]
;; [e f g h] [0.0 sy  0.0 0.0]
;; [i j k l] [0.0 0.0 1.0 0.0]
;; [m n o p] [0.0 0 0 0.0 1.0]
;;
;; a = sx * a
;; b = sy * b
;; e = sx * e
;; f = sy * f
;; i = sx * i
;; j = sy * j
(defun scale/unwrapped (sx sy m)
  (declare (type single-float sx sy)
           (type matrix m)
           (optimize (speed 3) (safety 1)))
  (macrolet ((@ (x y) `(aref m ,(+ y (* x 4))))
             (*= (where what)
               `(setf ,where (the single-float (* ,what ,where)))))
    (*= (@ 0 0) sx)
    (*= (@ 0 1) sx)
    (*= (@ 0 2) sx)
    (*= (@ 1 0) sy)
    (*= (@ 1 1) sy)
    (*= (@ 1 2) sy)))

(defun scale (sx sy)
  (unless (= sx sy 1.0)
    (scale/unwrapped sx sy (m4-vector *current-matrix*))))

(declaim (ftype (function (m4 single-float single-float) (values single-float single-float)) matrix-multiply-point-2d))

(defun matrix-multiply-point-2d (matrix x y)
  (declare (optimize (speed 3) (safety 1)))
  (let ((m (m4-vector matrix)))
    (declare (type matrix m)
             (single-float x y))
    (values (the single-float
                 (+ (the single-float
                         (* (the single-float x)
                            (the single-float (aref m 0))))
                    (the single-float
                         (* (the single-float y)
                            (the single-float (aref m 4))))
                    (the single-float (aref m 12))))
            (the single-float
                 (+ (the single-float
                         (* (the single-float x)
                            (the single-float (aref m 1))))
                    (the single-float
                         (* (the single-float y)
                            (the single-float (aref m 5))))
                    (the single-float (aref m 13)))))))
