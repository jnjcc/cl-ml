;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Non-Linear Support Vector Machine for regression

(in-package #:cl-ml)

(defclass kernel-svr (estimator)
  ((yalpha :initform nil :reader yalpha :type smatrix)))
