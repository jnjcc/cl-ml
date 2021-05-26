;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; "Data Frame" for XGBoost

(in-package #:cl-ml)

;;; `ylabels' is a m x 2 matrix, with first column being first-order derivative,
;;;   second column being second-order derivative
(defclass xgb-frame (data-frame)
  ())

(defmethod get-first-order ((xframe xgb-frame) i)
  "i-th first-order derivative"
  (with-slots (ylabels sindice) xframe
    (let ((idx (nth i sindice)))
      (mref ylabels idx 0))))

(defmethod get-second-order ((xframe xgb-frame) i)
  (with-slots (ylabels sindice) xframe
    (let ((idx (nth i sindice)))
      (mref ylabels idx 1))))

(defun make-xgframe (X y &optional feaname)
  (let ((xframe (make-instance 'xgb-frame :xmatrix X :ylabels y :feaname feaname)))
    (with-slots (sindice findice) xframe
      (setf sindice (make-indices (nrow X)))
      (setf findice (make-indices (ncol X))))
    xframe))
