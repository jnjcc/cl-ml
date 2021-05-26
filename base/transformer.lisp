;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Transformer

(in-package #:cl-ml)

(defclass transformer (estimator)
  ())

(defgeneric transform (trans X)
  (:documentation "transform dataset X"))
(defgeneric fit-transform (trans X)
  (:documentation "fit and transform dataset X"))
(defgeneric inv-transform (trans X)
  (:documentation "inverse transform dataset X"))

(defmethod transform ((trans transformer) X)
  X)
(defmethod fit-transform ((trans transformer) X)
  X)
(defmethod inv-transform ((trans transformer) X)
  X)

;;; we expand bias as the first term
(defun %expand-bias (X)
  (declare (type smatrix X))
  (let ((nrow (nrow X))
        (ncol (ncol X)))
    (let ((x-bias (make-matrix nrow (+ ncol 1) :initial-element 1)))
      (do-matrix-row (i X)
        (do-matrix-col (j X)
          (setf (mref x-bias i (+ j 1)) (mref X i j))))
      x-bias)))
