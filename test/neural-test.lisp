;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

(defun mlp-classifier-test (epochs lrate)
  (let ((mlpclf (make-instance 'mlp-classifier :hidden-units '(2) :epochs epochs :learn-rate lrate))
        (X (copy-matrix *iris-train*))
        (y (load-iris-label-binary 0))
        (pred nil))
    (fit mlpclf X y)
    (setf pred (predict mlpclf X))
    (format *test-stream* "~A~%" (accuracy-score y pred))))
