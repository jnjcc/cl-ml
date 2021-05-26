;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Min-max scaler and standard-scaler

(in-package #:cl-ml)

(defclass min-max-scaler (transformer)
  ())

(defclass standard-scaler (transformer)
  ((mu-vector :initarg nil :reader mu-vector :type smatrix)
   (sigma-vector :initarg nil :reader sigma-vector :type smatrix)))

(defmethod print-object ((stand standard-scaler) stream)
  (with-slots (mu-vector sigma-vector) stand
    (format stream "mu-vector: ~A~%" mu-vector)
    (format stream "sigma-vector: ~A~%" sigma-vector)))

(defmethod fit ((stand standard-scaler) X &optional y)
  (declare (type smatrix X) (ignore y))
  (with-slots (mu-vector sigma-vector) stand
    (setf mu-vector (make-row-vector (ncol X)))
    (setf sigma-vector (make-row-vector (ncol X)))
    (do-matrix-row (i X)
      (v+= mu-vector (mrv X i)))
    (v/= mu-vector (nrow X))
    (do-matrix-row (i X)
      (labels ((square (e) (* e e)))
        (v+= sigma-vector (vmap (v- (mrv X i) mu-vector) #'square))))
    (v/= sigma-vector (nrow X))))

(defmethod transform ((stand standard-scaler) X)
  (declare (type smatrix X))
  (with-slots (mu-vector sigma-vector) stand
    (do-matrix-row (i X)
      (v-= (mrv X i) mu-vector)
      (v/= (mrv X i) sigma-vector)))
  X)

(defmethod fit-transform ((stand standard-scaler) X)
  (fit stand X)
  (transform stand X))

(defmethod inv-transform ((stand standard-scaler) X)
  (with-slots (mu-vector sigma-vector) stand
    (do-matrix-row (i X)
      (v*= (mrv X i) sigma-vector)
      (v+= (mrv X i) mu-vector)))
  X)
