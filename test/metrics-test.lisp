;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

(define-test clf-metrics-test
  (let ((ylabl (make-vector 10 :initial-contents '(1 0 0 0 1 0 1 0 0 0)))
        (ypred (make-vector 10 :initial-contents '(1 1 0 0 1 1 1 0 0 0))))
    (assert-eq 0.6 (precision-score ylabl ypred))
    (assert-eq 1.0 (recall-score ylabl ypred))
    (assert-eq 0.8 (accuracy-score ylabl ypred))
    (let ((float-eq (make-float-eq 0.01)))
      (assert-true (funcall float-eq 0.75 (f1-score ylabl ypred))))
    (let ((expect (make-square-matrix 2 :initial-contents '((3 0) (2 5))))
          (cmatrix (confusion-matrix ylabl ypred)))
      (assert-true (matrix-eq expect cmatrix))
      (multiple-value-bind (tp fn fp tn) (cl-ml::binary-confusion ylabl ypred 1)
        (assert-eq tp (mref expect 0 0))
        (assert-eq fn (mref expect 0 1))
        (assert-eq fp (mref expect 1 0))
        (assert-eq tn (mref expect 1 1))))))

(define-test clf-auc-test
  (let ((ylabl (make-vector 4 :initial-contents '(1   1   2    2)))
        (ypred (make-vector 4 :initial-contents '(0.1 0.4 0.35 0.8))))
    (multiple-value-bind (fpr tpr thresh) (roc-curve ylabl ypred 2)
      (assert-true (equal '(0.0 0.0 0.5 0.5 1.0) fpr))
      (assert-true (equal '(0.0 0.5 0.5 1.0 1.0) tpr))
      (assert-true (equal '(1.0 0.8 0.4 0.35 0.1) thresh)))
    (assert-eq 0.75 (roc-auc-score ylabl ypred 2))))

(define-test reg-metrics-test
  (let ((ylabl (make-vector 4 :initial-contents '(3 -0.5 2 7)))
        (ypred (make-vector 4 :initial-contents '(2.5 0.0 2 8))))
    (assert-eq 0.375 (mean-squared-error ylabl ypred))))
