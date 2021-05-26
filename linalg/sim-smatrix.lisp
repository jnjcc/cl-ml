;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: smatrix class
;;;;
;;;;            smatrix-dense
;;;;          /
;;;; smatrix (- smatrix-sparse[*]         smatrix-view
;;;;          \                         /                   \
;;;;            smatrix-view-window[*] (- smatrix-transpose -) view of the underlying smatrix
;;;;                                    \                   /
;;;;                                      smatrix-diagview

(in-package #:cl-ml/linalg)

(defclass smatrix ()
  ())

;;; 1) rows and columns
(defgeneric nrow (ma)
  (:documentation "rows of `ma'"))
(defgeneric ncol (ma)
  (:documentation "cols of `ma'"))
(defgeneric mshape (ma))

(defmethod mshape ((ma smatrix))
  (list (nrow ma) (ncol ma)))

;;; 2) vector
(defun svector-type (vx)
  (cond
    ((= (ncol vx) 1) :column)
    ((= (nrow vx) 1) :row)
    (t nil)))

;;; 3) element access
(defgeneric mref (ma i j)
  (:documentation "access (i, j)-th element"))
(defgeneric (setf mref) (val ma i j)
  (:documentation "set (i, j)-th element"))
(defgeneric vref (vx i)
  (:documentation "access i-th element of vector"))
(defgeneric (setf vref) (val vx i)
  (:documentation "set i-th element of vector"))

;;; 4) make-* functions
;;;   - make-matrix
;;;   - make-square-matrix
;;;   - make-identity-matrix
;;;   - make-rand-matrix
;;;   - make-vector
;;;   - make-row-vector
;;;   - make-rand-vector
;;;   - make-rand-row-vector
(declaim (ftype (function (integer integer &key (:initial-element t) (:initial-contents t))
                          smatrix)
                make-matrix))
(declaim (ftype (function (integer &key (:initial-element t) (:initial-contents t))
                          smatrix)
                make-square-matrix))
(declaim (ftype (function (integer) smatrix)
                make-identity-matrix))
(declaim (ftype (function (integer integer &optional t t) smatrix)
                make-rand-matrix))
(deftype make-vector-ftype ()
  '(function (integer &key (:initial-element t) (:initial-contents t)) smatrix))
(declaim (ftype make-vector-ftype make-vector))
(declaim (ftype make-vector-ftype make-row-vector))
(deftype make-rand-vector-ftype ()
  '(function (integer &optional t t) smatrix))
(declaim (ftype make-rand-vector-ftype make-rand-vector))
(declaim (ftype make-rand-vector-ftype make-rand-row-vector))

;;; 5) matrix properties and operations
;;;   NOTICE: M-operations access element through (MREF), which handles polymorphism
;;;     - (M+) (M-) (M*) (M/) (MINV) (MPINV) return a fresh object of smatrix
;;;     - (MT) shares memory with the original smatrix
;;;     - (M+=) (M-=) (M*=) (M/=) (MMAP) (MREDUCE) do operations in-place
(defgeneric mdet (ma)
  (:documentation "determinant of squared matrix"))
(defgeneric m+ (ma mb &rest mrest)
  (:documentation "matrix addition"))
(defgeneric m+= (ma mb))
(defgeneric m- (ma mb &rest mrest)
  (:documentation "matrix substraction"))
(defgeneric m-= (ma mb))
(defgeneric m* (ma mb &rest mrest)
  (:documentation "matrix multiplication"))
(defgeneric m*= (ma mb)
  (:documentation "NOTICE: m*= does its best to do operation in-place:
`mb' is number, or (ncol `mb') = (nrow `mb'), or even (ncol `mb') < (nrow `mb')"))
(defgeneric m/ (ma mb)
  (:documentation "m/ only works on smatrix and number"))
(defgeneric m/= (ma mb)
  (:documentation "m/= only works on smatrix and number"))
(defgeneric mt (ma)
  (:documentation "matrix transpose"))
(defgeneric minv (ma)
  (:documentation "matrix inverse"))
(defgeneric mpinv (ma)
  (:documentation "Moore-Penrose pseudoinverse"))
(defgeneric mmap (ma op)
  (:documentation "do `op' on each matrix element"))
(defgeneric mreduce (ma op &key axis key initial-value)
  (:documentation "`axis' = nil: do `op' on all elements of `ma';
`axis' = 0: on elements of same column; `axis' = 1: on elements of same row"))
(defgeneric v+ (vx vy)
  (:documentation "vector element-wise addition"))
(defgeneric v+= (vx vy))
(defgeneric v- (vx vy)
  (:documentation "vector element-wise substraction"))
(defgeneric v-= (vx vy))
(defgeneric v* (vx vy)
  (:documentation "vector element-wise product"))
(defgeneric v*= (vx vy))
(defgeneric v/ (vx vy)
  (:documentation "v/ only works on smatrix and number"))
(defgeneric v/= (vx vy))
(defgeneric vdot (vx vy)
  (:documentation "vector dot product. NOTICE: accept different shapes"))
(defgeneric vouter (vx vy)
  (:documentation "outer product"))
(defgeneric vsum (vx)
  (:documentation "vector sum"))
(defgeneric vmap (vx op)
  (:documentation "do `op' on each vector element"))
(defgeneric vreduce (vx op &key initial-value)
  (:documentation "reduce `op' on vector"))

;;; 6) matrix statistics
(defgeneric mhead (ma &optional n))
(defgeneric msum (ma &key axis key initial-value)
  (:documentation "on same column by default"))
(defgeneric mmean (ma &key axis key))
(defgeneric mquantile (ma quad &key axis))
(defgeneric mmax (ma &key axis key))
(defgeneric mmin (ma &key axis key))
(defgeneric margmax (ma &key axis key))
(defgeneric margmin (ma &key axis key)
  (:documentation "on same column by default"))
(defgeneric mbincount (ma)
  (:documentation "bin classes and count"))
