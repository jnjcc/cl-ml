;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

(defvar *train* (make-square-matrix 3 :initial-contents
                                    '((2 3 1)
                                      (0 1 3)
                                      (1 2 5))))
(defvar *weight* (make-vector 3 :initial-contents '(1 1 1)))
(defvar *label* (m* *train* *weight*))

;; Linear Regression using Normal Equation
(define-test linear-regression-normal
  (let ((lreg (make-instance 'linear-regressor :expand-bias nil)))
    (fit lreg *train* *label*)
    (assert-true (matrix-eq *label* (predict lreg *train*)))))

;; Ridge Regression using Normal Equation
(define-test ridge-regression-normal
  (let ((ridge (make-instance 'ridge-regressor :alpha 1.0 :solver :normal))
        (float-eq (make-float-eq 0.6)))
    (fit ridge *train* *label*)
    (format *test-stream* "Ridge regression: ~A vs. ~A~%" (mt (predict ridge *train*))(mt *label*))
    (assert-true (matrix-eq *label* (predict ridge *train*) float-eq))))

;; Batch gradient descent, with L2 regularizer
(define-test batch-gradient-descent
  (let ((sgd (make-instance 'sgd-regressor :epochs 50 :eta0 0.1 :expand-bias nil))
        (sgd-l2 (make-instance 'sgd-regressor :epochs 50 :eta0 0.1 :regularizer :l2))
        (float-eq (make-float-eq 0.5)))
    (fit sgd *train* *label*)
    (format *test-stream* "Batch gd: ~A vs. ~A~%" (mt (predict sgd *train*)) (mt *label*))
    (assert-true (matrix-eq *label* (predict sgd *train*) float-eq))
    (fit sgd-l2 *train* *label*)
    (format *test-stream* "Batch gd L2: ~A vs. ~A~%" (mt (predict sgd-l2 *train*)) (mt *label*))
    (assert-true (matrix-eq *label* (predict sgd-l2 *train*) float-eq))))

;; Linear SVC
(define-test linear-kernel-svc
  (let ((y (load-iris-label-binary))
        ;; petal length, petal width
        (X (copy-matrix (make-matrix-view *iris-train* :col-view '(2 3))))
        (stand (make-instance 'standard-scaler))
        (linsvc (make-instance 'linear-svc))
        (rbfsvc (make-instance 'kernel-svc :kernel :rbf :epochs 100))
        (sigsvc (make-instance 'kernel-svc :kernel :sigmoid :coef0 1))
        (test (make-matrix 1 2 :initial-contents '((5.5 1.7)))))
    (fit-transform stand X)
    (transform stand test)

    (fit linsvc X y)
    (fit rbfsvc X y)
    (fit sigsvc X y)
    (let ((ylin (predict linsvc X))
          (yrbf (predict rbfsvc X))
          (ysig (predict sigsvc X)))
      (format *test-stream* "accuracy: ~A; ~A; ~A~%" (accuracy-score y ylin) (accuracy-score y yrbf)
              (accuracy-score y ysig))
      (assert-true (> (accuracy-score y ylin) 0.95))
      (assert-true (> (accuracy-score y yrbf) 0.95)))

    (let ((lpred (predict linsvc test))
          (rpred (predict rbfsvc test))
          (spred (predict sigsvc test)))
      (assert-eq +1 (vref lpred 0))
      (assert-eq +1 (vref rpred 0))
      (assert-eq +1 (vref spred 0)))))
