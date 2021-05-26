;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Unsupervised Learning > clustering > k-Means clustering

(in-package #:cl-ml/test)

(define-test unsupervised-kmeans-test
  (let ((X (copy-matrix *iris-train*))
        (kmeans (make-instance 'kmeans-cluster :epochs 300 :ninit 1
                               :init :kmeans++ :nclusters 3))
        (pred nil))
    (with-random-seed (5)
      (fit kmeans X)
      (setf pred (predict kmeans X))
      ;; [0, 50): Iris-setosa; [50, 100): Iris-versicolor; [100, 150): Iris-virginica
      (let ((bins1 (mbincount (mhead pred 50)))
            (bins2 (mbincount (make-matrix-view pred :row-view `(50 . 100))))
            (bins3 (mbincount (mhead pred -50))))
        (assert-true 1 (length bins1)) ;; all examples predicted to the same class
        (setf bins2 (sort (cl-ml/probs::probability bins2) #'>))
        (assert-true (equal '(48/50 2/50) bins2))
        (setf bins3 (sort (cl-ml/probs::probability bins3) #'>))
        (assert-true (equal '(36/50 14/50) bins3))
        ))))
