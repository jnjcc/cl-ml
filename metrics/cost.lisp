;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml)

;;; MSE (Mean squared error):
;;;   - 1/2m \sum_{i=1}^{m}(x^{(i)}w - y(i))^{2}
;;;   - 1/2(Xw - y)(Xw - y)
(defun mse-cost (y ypred)
  (let ((sv (v- y ypred)))
    (* (/ 1 2) (vdot sv sv))))

(defun mse-gradient (theta X y)
  ;; 1/m * X^{T} * (Xw - y)
  (m* (/ 1 (nrow X)) (mt X) (m- (m* X theta) y)))

(defun sigmoid (tval)
  (/ 1 (+ 1 (exp (- tval)))))

(defun log-loss (y ypred)
  "Logistic Regression cost function, cross entropy, or log loss"
  ;; -1/m * \sum_{i=1}^{m}[y * log(p) + (1 - y) * log(1 - p)]
  (- (* (/ 1 (nrow y))
        (+ (vdot y (vmap ypred #'log))
           (vdot (v- 1 y) (vmap (v- 1 ypred) #'log))))))

(defun log-gradient (theta X y)
  ;; 1/m * X^{T} * (a - y) =>
  ;; 1/m * X^{T} * [sigmoid(X * w) - y]
  (m* (/ 1 (nrow X))
      (mt X)
      (v- (vmap (m* X theta) #'sigmoid) y)))

;;; entropy
(defun log2 (x)
  (if (= x 0)
      0.0
      (log x 2)))

(defun entropy (p)
  (let ((ent 0.0)
        (vpi 0.0))
    (do-vector (i p)
      (setf vpi (vref p i))
      (incf ent (- (* vpi (log2 vpi)))))
    ent))

(defun entropy-list (p)
  (let ((ent 0.0))
    (dolist (lpi p)
      (incf ent (- (* lpi (log2 lpi)))))
    ent))

;;; gini
(defun gini-impurity (p)
  (let ((gni 1.0)
        (vpi 0.0))
    (do-vector (i p)
      (setf vpi (vref p i))
      (decf gni (* vpi vpi)))
    gni))

(defun gini-list (p)
  (let ((gni 1.0))
    (dolist (lpi p)
      (decf gni (* lpi lpi)))
    gni))
