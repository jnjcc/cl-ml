;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Unsupervised Learning > Probability Model Estimation > Mixture Model

(in-package #:cl-ml/test)

(define-test gaussian-mixture-test
  (let ((X (copy-matrix *gmm-train*))
        ;; (y (copy-matrix *gmm-label*))
        (gmm (make-instance 'gaussian-mixture :tolerance 0.001 :epochs 30
                            :ninit 1 :ncomponents 3))
        (pred nil))
    (with-random-seed (5)
      (fit gmm X)
      (setf pred (predict gmm (mhead X)))
      (assert-true (= (vref pred 0) (vref pred 1) (vref pred 3)))
      (assert-true (= (vref pred 2) (vref pred 4) (vref pred 5))))))
