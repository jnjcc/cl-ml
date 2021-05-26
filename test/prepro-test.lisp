;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

;; Polynomial Features
(defvar *poly-feature* (make-matrix 2 2 :initial-contents '((1 2) (3 4))))

;; Standard scaler
(define-test stand-scaler-iris
  (let ((train (make-matrix 3 3 :initial-contents '((1 2 3) (2 3 1) (3 1 2))))
        (mean (make-row-vector 3 :initial-contents '(2 2 2)))
        (stand (make-instance 'standard-scaler)))
    (fit stand train)
    (assert-true (matrix-eq mean (cl-ml::mu-vector stand)))
    (transform stand train)
    (assert-eq 0 (mref train 0 1))
    (assert-eq 0 (mref train 1 0))))

;; Polynomial feature transformer
(define-test poly-transformer-test
  (let ((poly (make-instance 'poly-transformer :degree 2 :expand-bias nil))
        (x-poly (make-matrix 2 5 :initial-contents '((1 2 1 2 4)
                                                     (3 4 9 12 16))))
        (poly-bias (make-instance 'poly-transformer :degree 2))
        (x-bias (make-matrix 2 6 :initial-contents '((1 1 2 1 2 4)
                                                     (1 3 4 9 12 16)))))
    (assert-true (matrix-eq x-poly (fit-transform poly *poly-feature*)))
    (fit poly-bias *poly-feature*)
    (assert-true (matrix-eq x-bias (transform poly-bias *poly-feature*)))))

;; Label encoder from iris dataset
(define-test label-encoder-iris
  (let ((lencoder (make-instance 'label-encoder))
        ;; Iris-setosa, Iris-setosa, Iris-versicolor, Iris-setosa, Iris-virginica
        (y-label (copy-matrix (make-matrix-view *iris-label* :row-view '(0 10 50 20 100))))
        (y-trans (make-matrix 5 1 :initial-contents '((0) (0) (1) (0) (2)))))
    (let ((y-copy (copy-matrix y-label)))
      (assert-true (matrix-eq y-trans (fit-transform lencoder y-label)))
      (assert-true (matrix-eq y-copy (inv-transform lencoder y-label) #'string=)))))
