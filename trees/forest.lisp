;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Random Forest Estimator

(in-package #:cl-ml)

(defclass rforest-estimator (estimator)
  ((estimators :initform nil :documentation "list of decision tree estimators")
   (nestimators :initform 10 :initarg :nestimators)
   (bootstrap :initform t :initarg :bootstrap
              :documentation "if NIL, the whole dataset is used to build each tree")
   (oob-score :initform nil :documentation "oob score only works when BOOTSTRAP")
   (max-samples :initform nil :initarg :max-samples :documentation
                "number of samples to draw from training set if BOOTSTRAP")
   (max-features :initform :log2 :initarg :max-features :type rand-feature-type
                 :documentation
                 "number of features to consider when looking for the best split")
   (max-depth :initform nil :initarg :max-depth :reader max-depth)
   (min-samples-split :initform 2 :initarg :min-samples-split :reader min-samples-split)
   (min-samples-leaf :initform 1 :initarg :min-samples-leaf :reader min-samples-leaf)
   (max-leaf-nodes :initform nil :initarg :max-leaf-nodes :reader max-leaf-nodes)
   (criterion :initform :gini :type criterion-type)))

(defmethod %get-samples ((rfest rforest-estimator) X y)
  (with-slots (bootstrap max-samples) rfest
    (if bootstrap
        (let* ((n (nrow X))
               (nsamples (cond
                           ((null max-samples) n)
                           ((< 0 max-samples 1) (* max-samples n))
                           (t n))))
          (multiple-value-bind (indices unindices) (bootstrap-indices nsamples)
            (values (make-matrix-view X :row-view indices)
                    (make-matrix-view y :row-view indices)
                    (make-matrix-view X :row-view unindices)
                    (make-matrix-view y :row-view unindices)
                    unindices indices)))
        (values X y nil nil nil nil))))

(defmethod predict-one ((rfest rforest-estimator) X i)
  (with-slots (estimators) rfest
    (let ((preds nil))
      (dolist (dtest estimators)
        (push (predict-one dtest X i) preds))
      (argmax (bincount preds)))))

(defmethod predict ((rfest rforest-estimator) X)
  (let ((pred (make-vector (nrow X) :initial-element 0)))
    (do-matrix-row (i X)
      (setf (vref pred i) (predict-one rfest X i)))
    pred))

(defmethod feature-importance ((rfest rforest-estimator))
  (with-slots (estimators) rfest
    (let ((vimps nil))
      (dolist (dest estimators)
        (if vimps
            (m+= vimps (feature-importance dest))
            (setf vimps (feature-importance dest))))
      (m/= vimps (length estimators))
      vimps)))

;;; Random Forest Classifier
(defclass rforest-classifier (rforest-estimator)
  ((criterion :initform :gini :initarg :criterion :type impurity-type)
   (nclasses :initform 0)
   (oob-score :initform nil :initarg :oob-score :reader oob-score)))

(defmethod initialize-instance :after ((rfclf rforest-classifier) &rest args)
  (declare (ignore args))
  (with-slots (estimators nestimators max-features) rfclf
    (with-slots (max-depth min-samples-split min-samples-leaf max-leaf-nodes) rfclf
      (dotimes (i nestimators)
        (push (make-instance 'dtree-classifier :max-features max-features
                             :max-depth max-depth :min-samples-split min-samples-split
                             :min-samples-leaf min-samples-leaf
                             :max-leaf-nodes max-leaf-nodes)
              estimators)))))

(defmethod fit ((rfclf rforest-classifier) X &optional y)
  (with-slots (estimators bootstrap oob-score max-samples nclasses) rfclf
    (setf nclasses (length (mbincount y)))
    (let ((oob-preds nil))
      (when oob-score
        (setf oob-preds (make-matrix (nrow X) nclasses :initial-element 0.0)))
      (dolist (dtclf estimators)
        (multiple-value-bind (Xv yv Xob yob oob-indices) (%get-samples rfclf X y)
          (declare (ignore yob))
          (fit dtclf Xv yv)
          (when oob-score
            ;; TODO: what if `oob-view' & `predict-proba' has different shape?
            (with-matrix-view (oob-view :row-view oob-indices) oob-preds
              (m+= oob-view (predict-proba dtclf Xob))))))
      ;; TODO: it is possible some examples have no `oob-preds' values
      (when oob-score
        (setf oob-preds (margmax oob-preds :axis 1))
        (setf oob-score (accuracy-score y oob-preds))))))

;;; Random Forest Regressor
(defclass rforest-regressor (rforest-estimator)
  ((criterion :initform :mse :type dtree-reg-type)))

(defmethod initialize-instance :after ((rfreg rforest-regressor) &rest args)
  (declare (ignore args))
  (with-slots (estimators nestimators max-features) rfreg
    (with-slots (max-depth min-samples-split min-samples-leaf max-leaf-nodes) rfreg
      (dotimes (i nestimators)
        (push (make-instance 'dtree-regressor :max-features max-features
                             :max-depth max-depth :min-samples-split min-samples-split
                             :min-samples-leaf min-samples-leaf
                             :max-leaf-nodes max-leaf-nodes)
              estimators)))))

(defmethod fit ((rfreg rforest-regressor) X &optional y)
  (with-slots (estimators bootstrap max-samples) rfreg
    (dolist (dtreg estimators)
      (multiple-value-bind (Xv yv) (%get-samples rfreg X y)
        (fit dtreg Xv yv)))))

(defmethod predict-one ((rfreg rforest-regressor) X i)
  (with-slots (estimators) rfreg
    (let ((preds nil))
      (dolist (dtest estimators)
        (push (predict-one dtest X i) preds))
      (/ (reduce #'+ preds :initial-value 0.0) (length preds)))))
