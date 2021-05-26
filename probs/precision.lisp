;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Floating Point Precision

(in-package #:cl-ml/probs)

;;; float equal
(defvar *float-zero-epsilon* 1.0e-10)

(defun float= (a b &optional (epsilon *float-zero-epsilon*))
  (<= (abs (- a b)) epsilon))

(defun float/= (a b &optional (epsilon *float-zero-epsilon*))
  (> (abs (- a b)) epsilon))

(defun float< (a b &optional (epsilon *float-zero-epsilon*))
  (and (float/= a b epsilon)
       (< (- a b) 0)))

(defun float> (a b &optional (epsilon *float-zero-epsilon*))
  (and (float/= a b epsilon)
       (> (- a b) 0)))
