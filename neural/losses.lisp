;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Neural Network > Loss functions

(in-package #:cl-ml)

;;; Cross Entropy for binary classifier
(defun cross-entropy-gradient (A y)
  "broadcasting y accros columns of A: A - y"
  (do-matrix (i j A)
    (decf (mref A i j) (vref y i)))
  A)

;;; Square Error for Regression
