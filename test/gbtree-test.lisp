;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Gradient Boosting Tree test

(in-package #:cl-ml/test)

(define-test gradient-boost-regressor
  (let* ((sz 1000)
         (X (copy-matrix (mhead *cal-train* sz)))
         (y (copy-matrix (mhead *cal-label* sz)))
         (gbleast (make-instance 'gboost-regressor :loss :least :max-depth 3))
         (gbleast5 (make-instance 'gboost-regressor :loss :least :max-depth 5))
         (gblad (make-instance 'gboost-regressor :loss :lad :max-depth 3)))
    (fit gbleast X y)
    (assert-true (< (mean-squared-error y (predict gbleast X)) 0.27))
    (fit gbleast5 X y)
    (assert-true (< (mean-squared-error y (predict gbleast5 X)) 0.16))
    (fit gblad X y)
    (assert-true (< (mean-squared-error y (predict gblad X)) 0.3))
    ))

(define-test gradient-boost-classifier
  (let ((y (load-iris-label-binary 0))
        ;; petal length, petal width
        (X (copy-matrix (make-matrix-view *iris-train* :col-view '(2 3))))
        (gbclf (make-instance 'gboost-classifier :loss :deviance :max-depth 5)))
    (fit gbclf X y)
    (assert-true (> (accuracy-score y (predict gbclf X)) 0.98))))
