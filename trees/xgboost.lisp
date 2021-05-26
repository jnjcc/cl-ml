;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Extreme Gradient Boosting Trees

(in-package #:cl-ml)

(defclass xgboost-tree (gboost-estimator)
  ())

(defmethod initialize-instance :after ((xgtree xgboost-tree) &rest args)
  (declare (ignore args))
  (with-slots (estimators nestimators loss) xgtree
    (with-slots (max-depth min-samples-split min-samples-leaf max-leaf-nodes) xgtree
      (dotimes (i nestimators)
        (push (make-instance 'xgb-regressor :max-depth max-depth
                             :min-samples-split min-samples-split
                             :min-samples-leaf min-samples-leaf
                             :max-leaf-nodes max-leaf-nodes)
              estimators)))))

;;; XGBoost classifier
(defclass xgboost-classifier (xgboost-tree)
  ())

(defmethod fit ((xgclf xgboost-classifier) X &optional y)
  )

;;; XGBoost regressor
(defclass xgboost-regressor (xgboost-tree)
  ((loss :initform :xgleast)))

(defmethod initialize-instance :after ((xgreg xgboost-regressor) &rest args)
  (declare (ignore args))
  (with-slots (loss) xgreg
    (setf loss (make-xgloss loss))))

(defmethod fit ((xgreg xgboost-regressor) X &optional y)
  (with-slots (estimators loss eta) xgreg
    (let ((fx (init-fit-and-predict loss y)))
      (dolist (xgreg estimators)
        (let ((ghgrad (negative-gradient loss y fx)))
          (fit xgreg X ghgrad)
          (setf fx (update-leaf-and-predict loss xgreg X y ghgrad fx eta)))))))
