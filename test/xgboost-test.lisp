;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; XGBoost test

(in-package #:cl-ml/test)

(define-test xgboost-regressor-test
  (let* ((sz 1000)
         (X (copy-matrix (mhead *cal-train* sz)))
         (y (copy-matrix (mhead *cal-label* sz)))
         (xgreg (make-instance 'xgboost-regressor :max-depth 5)))
    (fit xgreg X y)
    (assert-true (< (mean-squared-error y (predict xgreg X)) 0.16))))
