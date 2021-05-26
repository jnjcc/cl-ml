;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Linear Regression / Ridge Regression / Lasso Regression
;;;;   - X * w = y, where row of X is an example, w and y are column vectors
(in-package #:cl-ml)

(deftype linear-solver ()
  '(member :normal :pseudo))

(defclass linear-regressor (estimator)
  ((theta :initform nil :reader theta :type smatrix)
   (expand-bias :initform t :initarg :expand-bias :reader expand-bias)
   (solver :initform :normal :initarg :solver :reader solver :type linear-solver)))

(deftype ridge-solver ()
  '(member :normal :cholesky))

(defclass ridge-regressor (linear-regressor)
  ((solver :initform :normal :initarg :solver :reader solver :type ridge-solver)
   (alpha :initform 1.0 :initarg :alpha :reader alpha
          :documentation "weight for the L2 regularization term")))

(defmethod %normal-equation ((linear linear-regressor) X y)
  "fit using The Normal Equation"
  (with-slots (theta) linear
    ;; (X^{T} * X)^{-1} * X^{T} * y
    (setf theta (m* (minv (m* (mt X) X)) (mt X) y))
    nil))

;; TODO: pseudo-inverse
(defmethod %pseudo-inverse ((linear linear-regressor) X y)
  (with-slots (theta) linear
    (setf theta (m* (mpinv X) y))
    nil))

(defmethod %normal-equation ((ridge ridge-regressor) X y)
  (with-slots (theta expand-bias alpha) ridge
    (let ((idm (make-identity-matrix (ncol X))))
      (when expand-bias
        (setf (mref idm 0 0) 0))
      ;; (X^{T} * X + alpha * A)^{-1} * X^{T} * y
      (setf theta (m* (minv (m+ (m* (mt X) X) (m* alpha idm))) (mt X) y))
      nil)))

(defmethod fit ((linear linear-regressor) X &optional y)
  (declare (type smatrix X))
  (let ((solver-fn (ecase (solver linear)
                     (:normal #'%normal-equation)
                     (:pseudo #'%pseudo-inverse))))
    (when (expand-bias linear)
      (setf X (%expand-bias X)))
    (funcall solver-fn linear X y)))

(defmethod fit ((ridge ridge-regressor) X &optional y)
  (declare (type smatrix X))
  (let ((solver-fn (ecase (solver ridge)
                     (:normal #'%normal-equation)
                     (:cholesky #'%cholesky-solver))))
    (when (expand-bias ridge)
      (setf X (%expand-bias X)))
    (funcall solver-fn ridge X y)))

(defmethod predict ((linear linear-regressor) X)
  (with-slots (theta expand-bias) linear
    (if expand-bias
        (m* (%expand-bias X) theta)
        (m* X theta))))
