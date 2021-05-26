;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml)

(defun mean-squared-error (y ypred)
  (let ((sqerr 0.0)
        (sub 0.0))
    (do-vector (i y)
      (setf sub (- (vref y i) (vref ypred i)))
      (incf sqerr (* sub sub)))
    (/ sqerr (nrow y))))
