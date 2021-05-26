;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(asdf:defsystem #:cl-ml
  :description "Machine Learning in Common Lisp"
  :version "0.1.0"
  :author "jnjcc at live.com"
  :licence "GPL"
  :serial t
  :components ((:module "probs"
                        :components ((:file "packages")
                                     (:file "precision")
                                     (:file "bins")
                                     (:file "stats")
                                     ;; parametric probability distributions
                                     (:file "paramdist")
                                     (:file "randoms")
                                     (:file "indices")
                                     (:file "combination")
                                     (:file "shuffle")
                                     (:file "sampling")))
               (:module "linalg"
                        :components ((:file "packages")
                                     (:file "sim-smatrix")
                                     (:file "sim-dense")
                                     (:file "sim-transpose")
                                     (:file "sim-view")
                                     (:file "sim-diagview")
                                     (:file "sim-access")
                                     (:file "sim-transform")
                                     (:file "sim-operation")
                                     (:file "sim-indirect")
                                     (:file "sim-stats")
                                     (:file "sim-bidiagonal")
                                     (:file "sim-hessenberg")
                                     (:file "sim-qr")
                                     (:file "sim-svd")))
               (:module "algo"
                        :components ((:file "packages")
                                     (:file "string")
                                     (:file "kmp")
                                     (:file "binary-tree")
                                     (:file "stack")
                                     (:file "queue")
                                     (:file "huffman")
                                     ))
               (:module "graph"
                        :components ((:file "packages")
                                     (:file "sim-base")
                                     (:file "sim-graph")))
               (:module "io"
                        :components ((:file "packages")
                                     (:file "sim-char")
                                     (:file "sim-lex")
                                     (:file "sim-synt")
                                     (:file "sim-file")
                                     (:file "sim-example")
                                     (:file "sim-matrix")
                                     (:file "sim-graph")))
               (:file "packages")
               (:module "base"
                        :components ((:file "estimator")
                                     (:file "transformer")
                                     (:file "dframe")
                                     (:file "numeric")))
               (:module "preprocess"
                        :components ((:file "scaler")
                                     (:file "polynomial")
                                     (:file "onehot")
                                     (:file "label")))
               (:module "metrics"
                        :components ((:file "cost")
                                     (:file "classifier")
                                     (:file "regressor")
                                     (:file "cluster")))
               (:module "linear"
                        :components ((:file "regression")
                                     (:file "sgd")
                                     (:file "logistic")
                                     (:file "svm")
                                     (:file "linsvc")
                                     (:file "linsvr")
                                     (:file "ksvc")
                                     (:file "ksvr")))
               (:module "trees"
                        :components ((:file "dtcrite")
                                     (:file "xgframe")
                                     (:file "xgcrite")
                                     (:file "dtree")
                                     (:file "cart")
                                     (:file "xgcart")
                                     (:file "forest")
                                     (:file "gbloss")
                                     (:file "xgloss")
                                     (:file "gbtree")
                                     (:file "xgboost")))
               (:module "neural"
                        :components ((:file "activations")
                                     (:file "losses")
                                     (:file "mlp")))
               ;;; Unsupervised Learning
               (:module "cluster"
                        :components ((:file "kmeans")
                                     (:file "dbscan")
                                     ))
               (:module "pgm"
                        :components ((:file "gaussian")
                                     (:file "gmm")))))

(asdf:defsystem #:cl-ml-test
  :description "cl-ml test suite"
  :version "0.1.0"
  :author "jnjcc at live.com"
  :licence "GPL"
  :depends-on (#:cl-ml #:lisp-unit)
  :serial t
  :components ((:module "test"
                        :components ((:file "packages")
                                     (:file "common")
                                     (:file "dataset")
                                     (:file "linalg-test")
                                     (:file "algo-test")
                                     (:file "graph-test")
                                     (:file "prepro-test")
                                     (:file "metrics-test")
                                     (:file "linear-test")
                                     (:file "dtree-test")
                                     (:file "forest-test")
                                     (:file "gbtree-test")
                                     (:file "xgboost-test")
                                     (:file "kmeans-test")
                                     (:file "mixture-test")
                                     (:file "neural-test")
                                     (:file "tests")))))
