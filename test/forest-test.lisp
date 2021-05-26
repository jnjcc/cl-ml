;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Random Forest test

(in-package #:cl-ml/test)

(define-test random-forest-classifier
  (let ((y (copy-matrix *iris-label*))
        ;; 0: petal length, 1: petal width
        (X (copy-matrix (make-matrix-view *iris-train* :col-view '(2 3))))
        (labenc (make-instance 'label-encoder))
        (rfclf (make-instance 'rforest-classifier :max-depth 3 :nestimators 10))
        (rfoob (make-instance 'rforest-classifier :max-depth 3 :nestimators 10 :oob-score t))
        (pred nil))
    (with-random-seed (3)
      (fit-transform labenc y)
      (fit rfclf X y)
      (setf pred (predict rfclf X))
      (format *test-stream* "~A~%" (confusion-matrix y pred))
      (format *test-stream* "~A~%" (accuracy-score y pred))
      (assert-true (> (accuracy-score y pred) 0.95))

      (format *test-stream* "~A~%" (mt (feature-importance rfclf)))

      (fit rfoob X y)
      (format *test-stream* "~A~%" (cl-ml::oob-score rfoob))
      (assert-true (> (cl-ml::oob-score rfoob) 0.95))
      )))
