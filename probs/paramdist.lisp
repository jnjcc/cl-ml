;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Parametric Probability Distributions

(in-package #:cl-ml/probs)

;;; Normal distribution

(defun cumsum (p)
  (let ((cdf (make-list (length p)))
        (sum 0.0))
    (dotimes (i (length p))
      (incf sum (nth i p))
      (setf (nth i cdf) sum))
    cdf))
