;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; XGBoost Regression Tree Estimator

(in-package #:cl-ml)

(defclass xgb-regressor (dtree-regressor)
  ((criterion :initform :xgb)))

(defmethod fit ((xgreg xgb-regressor) X &optional y)
  "NOTICE: y is a mx2 matrix"
  (unless (= (ncol y) 2)
    (error "xgb-regressor expect a mx2 matrix as y"))
  (with-slots (droot depth nfeat) xgreg
    (setf nfeat (ncol X))
    (let ((xframe (make-xgframe X y))
          (splitter (make-root-splitter xgreg X)))
      (setf depth (split-node splitter xframe xgreg)))))
