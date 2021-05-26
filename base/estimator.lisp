;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Estimator

(in-package #:cl-ml)

(defclass estimator ()
  ())

(defgeneric fit (est X &optional y)
  (:documentation "train estimator from training set X"))
(defgeneric predict (est X)
  (:documentation "predict for dataset X"))

(defmethod fit ((est estimator) X &optional y)
  (declare (ignore y)))
(defmethod predict ((est estimator) X))
