;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Gradient Descent

(in-package #:cl-ml)

(deftype sgd-loss ()
  ":mse - Mean Squared Error"
  '(member :mse))
(deftype sgd-regularizer ()
  '(member :none :l1 :l2))
(deftype sgd-btype ()
  "batch-type: batch sgd mini-batch"
  '(member :batch :sgd :mini))

(defclass sgd-regressor (estimator)
  ((theta :initform nil :initarg :theta :reader theta :type smatrix)
   (loss :initform :mse :initarg :loss :reader loss :type sgd-loss)
   (regularizer :initform :none :initarg :regularizer :reader regularizer :type sgd-regularizer)
   (alpha :initform 1.0 :initarg :alpha :reader alpha)
   (epochs :initform 10 :initarg :epochs :reader epochs)
   (eta0 :initform 0.1 :initarg :eta0 :reader eta0)
   (btype :initform :batch :initarg :btype :reader btype :type sgd-btype)
   (expand-bias :initform t :initarg :expand-bias :reader expand-bias)))

(defun %sign-vector (theta expand-bias)
  (declare (type smatrix theta))
  (let ((vsign (make-vector (nrow theta) :initial-element 1)))
    (dotimes (i (nrow vsign) vsign)
      (cond
        ((and expand-bias (= i 0)) (setf (vref sign 0) 0))
        ((< (vref theta i) 0) (setf (vref vsign i) -1))
        ((= (vref theta i) 0) (setf (vref vsign i) 0))))))

(defmethod gradient ((sgd sgd-regressor) X y)
  (declare (type smatrix X y))
  (with-slots (loss regularizer alpha theta expand-bias) sgd
    (let ((grad (ecase loss
                  (:mse (mse-gradient theta X y)))))
      (case loss
        (:l1 (incf grad (m* alpha (%sign-vector theta expand-bias))))
        (:l2 (incf grad (m* alpha theta))))
      grad)))

;; TODO: theta-initialization & mini-batch gradient descent
;; - random initialize theta
(defmethod %init-theta ((sgd sgd-regressor) nrow)
  (with-slots (theta) sgd
    (setf theta (make-rand-vector nrow 1.0))))
(defmethod %get-batch ((sgd sgd-regressor) X y)
  (declare (type smatrix X y))
  (ecase (btype sgd)
    (:batch (values X y))
    (:sgd (values X y))
    (:mini (values X y))))

;; TODO:
;; - learning rate schedule
;; - stopping criterion: loss tolerance
(defmethod fit ((sgd sgd-regressor) X &optional y)
  (declare (type smatrix X))
  (when (expand-bias sgd)
    (setf X (%expand-bias X)))
  (%init-theta sgd (ncol X))
  (with-slots (theta epochs eta0) sgd
    (dotimes (i epochs)
      (multiple-value-bind (xbatch ybatch) (%get-batch sgd X y)
        (setf theta (m- theta (m* eta0 (gradient sgd xbatch ybatch))))))))

(defmethod predict ((sgd sgd-regressor) X)
  (declare (type smatrix X))
  (with-slots (theta expand-bias) sgd
    (if expand-bias
        (m* (%expand-bias X) theta)
        (m* X theta))))
