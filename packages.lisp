;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Machine Learning in Common Lisp

(in-package #:cl-user)

(defpackage #:cl-ml
  (:nicknames #:ml)
  (:use #:cl #:cl-ml/probs #:cl-ml/linalg #:cl-ml/algo)
  (:export #:precision-score #:recall-score #:f1-score #:accuracy-score
           #:confusion-matrix #:roc-curve #:roc-auc-score
           #:mean-squared-error
           #:euclidean-distance #:cosine-distance

           ;;; common APIs of Estimators and Transformers
           #:fit #:transform #:fit-transform #:inv-transform
           #:predict #:predict-proba

           ;;; preprocessing
           #:standard-scaler #:poly-transformer #:label-encoder

           ;;; Linear Estimators
           #:linear-regressor #:ridge-regressor #:sgd-regressor
           #:logistic-regression
           #:linear-svc #:kernel-svc

           ;;; Tree Estimators
           #:dtree-classifier #:dtree-regressor
           #:rforest-classifier #:rforest-regressor
           #:gboost-classifier #:gboost-regressor
           #:xgboost-classifier #:xgboost-regressor
           #:feature-importance

           ;;; Neural Network
           #:mlp-classifier #:mlp-regressor

           ;;; Unsupervised > Clustering
           #:kmeans-cluster
           ;;; Unsupervised > Mixture Model
           #:gaussian-mixture
           ))
