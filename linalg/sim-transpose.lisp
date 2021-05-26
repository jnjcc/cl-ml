;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: transposed matrix

(in-package #:cl-ml/linalg)

(defclass smatrix-transpose (smatrix)
  ((displaced-to :initform nil :initarg :displaced-to :reader displaced-to)))

;;; rows and columns
(defmethod nrow ((ma smatrix-transpose))
  (if (displaced-to ma)
      (ncol (displaced-to ma))
      0))

(defmethod ncol ((ma smatrix-transpose))
  (if (displaced-to ma)
      (nrow (displaced-to ma))
      0))

;;; element access
(defmethod %translated-indices ((ma smatrix-transpose) i j)
  (declare (ignore ma))
  (values j i))

(defmethod mref ((ma smatrix-transpose) i j)
  (with-slots (displaced-to) ma
    (multiple-value-bind (ridx cidx) (%translated-indices ma i j)
      (if displaced-to
          (mref displaced-to ridx cidx)
          (error "out of bounds: (~A, ~A)" i j)))))

(defmethod (setf mref) (val (ma smatrix-transpose) i j)
  (with-slots (displaced-to) ma
    (multiple-value-bind (ridx cidx) (%translated-indices ma i j)
      (setf (mref displaced-to ridx cidx) val))))

;;; make-* functions
(defun %make-transpose-matrix (ma)
  (declare (type smatrix ma))
  (if (typep ma 'smatrix-transpose)
      (displaced-to ma)
      (make-instance 'smatrix-transpose :displaced-to ma)))
