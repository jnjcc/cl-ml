;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Linear Support Vector Machine for regression

(in-package #:cl-ml)

(defclass linear-svr (estimator)
  ((theta :initform nil :reader theta :type smatrix)
   (expand-bias :initform t :initarg :expand-bias :reader expand-bias)))
