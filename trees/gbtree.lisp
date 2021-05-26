;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Gradient Boosting Trees

(in-package #:cl-ml)

(defclass gboost-estimator (estimator)
  ((estimators :initform nil :documentation "list of decision tree estimators")
   (nestimators :initform 20 :initarg :nestimators)
   (loss :initform :least :documentation "gboost loss type")
   (eta :initform 0.1 :initarg :eta :documentation "shrink the contribution of each tree")
   (max-depth :initform nil :initarg :max-depth :reader max-depth)
   (min-samples-split :initform 2 :initarg :min-samples-split :reader min-samples-split)
   (min-samples-leaf :initform 1 :initarg :min-samples-leaf :reader min-samples-leaf)
   (max-leaf-nodes :initform nil :initarg :max-leaf-nodes :reader max-leaf-nodes)))

(defmethod predict ((gbest gboost-estimator) X)
  (with-slots (estimators loss eta) gbest
    (let ((pred (init-predict loss X)))
      (dolist (dtest estimators)
        (m+= pred (m* eta (predict dtest X))))
      pred)))

;;; Gradient Boosting Tree with regression tree as base estimator
(defclass gboost-tree (gboost-estimator)
  ())

;;; NOTICE: all trees are regression tree
(defmethod initialize-instance :after ((gbtree gboost-tree) &rest args)
  (declare (ignore args))
  (with-slots (estimators nestimators loss) gbtree
    (with-slots (max-depth min-samples-split min-samples-leaf max-leaf-nodes) gbtree
      (dotimes (i nestimators)
        (push (make-instance 'dtree-regressor :max-depth max-depth
                             :min-samples-split min-samples-split
                             :min-samples-leaf min-samples-leaf
                             :max-leaf-nodes max-leaf-nodes)
              estimators)))))

;;; Gradient Boosting Classifier
(deftype gbclf-loss-type ()
  '(member :deviance :exponential))

(defclass gboost-classifier (gboost-tree)
  ((loss :initform :deviance :initarg :loss :type gbclf-loss-type)
   (nclasses :initform 0)))

(defmethod initialize-instance :after ((gbclf gboost-classifier) &rest args)
  (declare (ignore args))
  (with-slots (loss) gbclf
    (setf loss (make-gbloss loss))))

(defmethod fit ((gbclf gboost-classifier) X &optional y)
  (with-slots (estimators loss eta nclasses) gbclf
    (setf nclasses (length (mbincount y)))
    (let ((fx (init-fit-and-predict loss y)))
      (dolist (dtreg estimators)
        (let ((residual (negative-gradient loss y fx)))
          (fit dtreg X residual)
          (setf fx (update-leaf-and-predict loss dtreg X y residual fx eta)))))))

(defmethod predict-proba ((gbclf gboost-classifier) X)
  (with-slots (estimators loss eta) gbclf
    (let ((pred (init-predict loss X)))
      (dolist (dtest estimators)
        (m+= pred (m* eta (predict dtest X))))
      (mmap pred #'sigmoid)
      pred)))

(defmethod predict ((gbclf gboost-classifier) X)
  (let ((pred (predict-proba gbclf X)))
    (labels ((posfun (val)
               (if (> val 0.5) +1 0)))
      (mmap pred #'posfun))
    pred))

;;; Gradient Boosting Regressor
(deftype gbreg-loss-type ()
  '(member :least :lad :huber :quantile))

(defclass gboost-regressor (gboost-tree)
  ((loss :initform :least :initarg :loss :type 'gbreg-loss-type)))

(defmethod initialize-instance :after ((gbreg gboost-regressor) &rest args)
  (declare (ignore args))
  (with-slots (loss) gbreg
    (setf loss (make-gbloss loss))))

(defmethod fit ((gbreg gboost-regressor) X &optional y)
  (with-slots (estimators loss eta) gbreg
    (let ((fx (init-fit-and-predict loss y)))
      (dolist (dtreg estimators)
        (let ((residual (negative-gradient loss y fx)))
          (fit dtreg X residual)
          (setf fx (update-leaf-and-predict loss dtreg X y residual fx eta)))))))
