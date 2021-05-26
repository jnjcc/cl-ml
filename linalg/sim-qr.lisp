;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: The QR algorithm

(in-package #:cl-ml/linalg)

(defun %make-tridiagonal (alphas betas)
  (if (/= (length alphas) (+ (length betas) 1))
      (error "main diagonal and sub diagonal length not match")
      (let* ((n (length alphas))
             (ma (make-matrix n n)))
        (dotimes (i (- n 1))
          (setf (mref ma i i) (nth i alphas))
          (setf (mref ma i (+ i 1)) (nth i betas))
          (setf (mref ma (+ i 1) i) (nth i betas)))
        (setf (mref ma (- n 1) (- n 1)) (nth (- n 1) alphas))
        ma)))

(defun %sign (delta)
  (if (>= delta 0) 1 -1))

(defun %sqdref (diag i)
  "squared dref"
  (* (dref diag i) (dref diag i)))

(defun wilkinson-tridiagonal (mT)
  "Symmetric tridiagonal QR algorithm with implicit Wilkinson shift,
with main diagonal being (alpha[1], alpha[2], ..., alpha[n]),
first diagonal above main diagonal being (beta[1], ..., beta[n-1])
NOTICE: will modify mT in-place, copy-matrix if necessary"
  (let* ((n (nrow mT))
         (alpha (make-diagonal-view mT :diag-type :main :pseudo-start 1))
         (beta (make-diagonal-view mT :diag-type :both :pseudo-start 1))
         (delta (/ (- (dref alpha (- n 1)) (dref alpha n)) 2))
         (mu (- (dref alpha n)
                (/ (%sqdref beta (- n 1))
                   (+ delta (* (%sign delta)
                               (sqrt (+ (* delta delta)
                                        (%sqdref beta (- n 1)))))))))
         (x (- (dref alpha 1) mu))
         (y (dref beta 1)))
    (do-diagonal (k beta)
      ;; solving [[c s] [-s c]]
      (multiple-value-bind (c s sigma) (givens-from-value x y)
        (multiple-value-bind (atmp btmp etmp ftmp)
            (givens-on-square (dref alpha k) (dref beta k) (dref beta k) (dref alpha (+ k 1)) c s)
          (multiple-value-bind (atmp btmp etmp ftmp)
              (givens-on-row-square atmp btmp etmp ftmp c (- s))
            (assert (= btmp etmp))
            (setf (dref alpha k) atmp)
            (setf (dref beta k) btmp)
            (setf (dref alpha (+ k 1)) ftmp)))
        (setf (dref beta (+ k 1)) (* c (dref beta (+ k 1))))
        (when (> k 1)
          (setf (dref beta (- k 1)) sigma))
        (setf x (dref beta k))
        (setf y (* s (dref beta (+ k 1))))))))

(defun %make-bidiagonal (deltas gammas)
  (if (/= (length deltas) (+ (length gammas) 1))
      (error "main diagonal and sub diagonal length not match")
      (let* ((n (length deltas))
             (ma (make-matrix n n)))
        (dotimes (i (- n 1))
          (setf (mref ma i i) (nth i deltas))
          (setf (mref ma i (+ i 1)) (nth i gammas)))
        (setf (mref ma (- n 1) (- n 1)) (nth (- n 1) deltas))
        ma)))

(defun wilkinson-bidiagonal (mB)
  "Returns: (P, Q, B) such that B := P^{T}BQ
NOTICE: will modify B in-place, copy-matrix if necessary
main diagonal being (delta[0], delta[1], ..., delta[n-1]),
first diagonal above main diagonal being (gamma[1], ..., gamma[n-1])"
  (let* ((n (nrow mB))
         (delta (make-diagonal-view mB :diag-type :main))
         (gamma (make-diagonal-view mB :diag-type :upper :pseudo-start 1))
         ;; if mB is a 2x2 matrix, let gamma[0] be 0
         (sqgamma (if (= n 2) 0 (%sqdref gamma (- n 2))))
         (d (/ (- (+ (%sqdref delta (- n 2)) sqgamma)
                  (+ (%sqdref gamma (- n 1)) (%sqdref delta (- n 1))))
               2))
         (mu (/ (- (+ (%sqdref gamma (- n 1)) (%sqdref delta (- n 1)))
                   (* (%sqdref delta (- n 2)) (%sqdref gamma (- n 1))))
                (+ d
                   (* (%sign d)
                      (sqrt (+ (* d d) (* (%sqdref delta (- n 2))
                                          (%sqdref gamma (- n 1)))))))))
         (x (- (%sqdref delta 0) mu))
         (y (* (dref delta 0) (dref gamma 1)))
         (mP (make-identity-matrix n))
         (mQ (make-identity-matrix n)))
    (dotimes (k (- n 1))
      (multiple-value-bind (c s sigma) (givens-from-row-value x y)
        (let (btmp ftmp)
          (multiple-value-setq (x btmp y ftmp)
            (givens-on-row-square (dref delta k) (dref gamma (+ k 1)) 0
                                  (dref delta (+ k 1)) c s))
          (setf (dref gamma (+ k 1)) btmp)
          (setf (dref delta (+ k 1)) ftmp))
        (givens-on-row-matrix mQ k (+ k 1) c s)
        (when (> k 0)
          (setf (dref gamma k) sigma)))
      (multiple-value-bind (c s sigma) (givens-from-value x y)
        (setf (dref delta k) sigma)
        ;; by using -s, we are doing transpose of G
        (givens-on-row-matrix mP k (+ k 1) c (- s))
        (if (< k (- n 2))
            (let (etmp ftmp)
              (multiple-value-setq (x y etmp ftmp)
                (givens-on-square (dref gamma (+ k 1)) 0 (dref delta (+ k 1))
                                  (dref delta (+ k 2)) c s))
              (setf (dref delta (+ k 1)) etmp)
              (setf (dref gamma (+ k 2)) ftmp))
            (multiple-value-bind (atmp btmp)
                (givens-on-value (dref gamma (- n 1)) (dref delta (- n 1)) c s)
              (setf (dref gamma (- n 1)) atmp)
              (setf (dref delta (- n 1)) btmp)))))
    (values mP mQ mB)))
