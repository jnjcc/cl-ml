;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Logistic Regression

(in-package #:cl-ml)

(deftype logistic-loss ()
  '(member :logistic))

(defclass logistic-regression (sgd-regressor)
  ((loss :initform :logistic :initarg :loss :reader loss :type logistic-loss)))

(defmethod gradient ((lreg logistic-regression) X y)
  (declare (type smatrix X y))
  (with-slots (loss theta) lreg
    (ecase loss
      (:logistic (log-gradient theta X y)))))

(defmethod predict ((lreg logistic-regression) X)
  (declare (type smatrix X))
  (with-slots (theta expand-bias) lreg
    (if expand-bias
        (vmap (m* (%expand-bias X) theta) #'sigmoid)
        (vmap (m* X theta) #'sigmoid))))
