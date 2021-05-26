;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Decision Tree test

(in-package #:cl-ml/test)

(define-test decision-tree-classifier
  "Binary tree of the form:
        [length <= 2.45]
         /            \
Iris-setosa          [width <= 1.75]
                      /           \
         Iris-versicolor         Iris-virginica
"
  (let ((y (copy-matrix *iris-label*))
        ;; 0: petal length, 1: petal width
        (X (copy-matrix (make-matrix-view *iris-train* :col-view '(2 3))))
        (labenc (make-instance 'label-encoder))
        (dtclf (make-instance 'dtree-classifier :max-depth 3))
        (pred nil))
    (fit-transform labenc y)
    (fit dtclf X y)
    (setf pred (predict dtclf X))
    (format *test-stream* "~A~%" (confusion-matrix y pred))
    (format *test-stream* "~A~%" (accuracy-score y pred))
    (let* ((droot (slot-value dtclf 'cl-ml::droot))
           (pdata (cl-ml::get-tree-data droot))
           (ldata (cl-ml::get-tree-data (cl-ml::get-left-tree droot)))
           (rchild (cl-ml::get-right-tree droot))
           (rdata (cl-ml::get-tree-data rchild))
           (rldata (cl-ml::get-tree-data (cl-ml::get-left-tree rchild)))
           (rrdata (cl-ml::get-tree-data (cl-ml::get-right-tree rchild))))
      ;; parent: petal length
      (assert-eq 0 (slot-value pdata 'cl-ml::feature))
      (assert-true (cl-ml::float= 2.45 (slot-value pdata 'cl-ml::threshold) 0.01))
      (assert-true (cl-ml::float= 0.667 (slot-value pdata 'cl-ml::impurity) 0.001))
      ;; left child: Iris-setosa
      (assert-true (string= "Iris-setosa" (cl-ml::get-encoder-label
                                           labenc
                                           (slot-value ldata 'cl-ml::value))))
      ;; right child: petal width
      (assert-eq 1 (slot-value rdata 'cl-ml::feature))
      (assert-true (cl-ml::float= 1.75 (slot-value rdata 'cl-ml::threshold) 0.01))
      (assert-eq 100 (slot-value rdata 'cl-ml::nsamples))
      ;; left child of right child: Iris-versicolor
      (assert-true (string= "Iris-versicolor" (cl-ml::get-encoder-label
                                               labenc
                                               (slot-value rldata 'cl-ml::value))))
      ;; right child of right child: Iris-virginica
      (assert-true (string= "Iris-virginica" (cl-ml::get-encoder-label
                                              labenc
                                              (slot-value rrdata 'cl-ml::value)))))))
