;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; XGBoost Losses

(in-package #:cl-ml)

(defclass xgloss-least (gbloss-least)
  ())

(defmethod negative-gradient ((xgleast xgloss-least) y f)
  ;; second order derivative being 1
  (let ((grad (make-matrix (nrow y) 2 :initial-element 1.0)))
    (do-vector (i y)
      (setf (mref grad i 0) (- (vref f i) (vref y i))))
    grad))

(defmethod update-leaf-and-predict ((xgleast xgloss-least) xgreg X y residual fx eta)
  (let ((curpred (predict xgreg X)))
    (m*= curpred eta)
    (m+= fx curpred))
  fx)

;;; make-* functions
(defun make-xgloss (type)
  (ecase type
    (:xgleast (make-instance 'xgloss-least))))
