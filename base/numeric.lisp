;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; numeric stuff such as infinity, overflow / underflow

(in-package #:cl-ml)

;;; infinity comparison, we will not do infinity arithmetic
(deftype infty-type ()
  "positive infinity, negative infinity"
  '(member :infty :ninfty))

(defun inf< (a b)
  (cond
    ((eq a :infty) nil)
    ((eq a :ninfty) (not (eq b :ninfty)))
    (t (cond
         ((eq b :infty) t)
         ((eq b :ninfty) nil)
         (t (< a b))))))

(defun inf= (a b)
  (cond
    ((eq a :infty) (eq b :infty))
    ((eq a :ninfty) (eq b :ninfty))
    (t (= a b))))

(defun inf<= (a b)
  (cond
    ((eq a :infty) (eq b :infty))
    ((eq a :ninfty) t)
    (t (cond
         ((eq b :infty) t)
         ((eq b :ninfty) nil)
         (t (<= a b))))))

(defun inf> (a b)
  (inf< b a))

(defun inf>= (a b)
  (inf<= b a))

(defun infmax (a b)
  (if (inf< a b)
      b
      a))

(defun infmin (a b)
  (if (inf< a b)
      a
      b))

;;; overflow, underflow
(defun logsumexp (y)
  "\log(\sum(\exp(x)))"
  (let ((b (mmax y :axis nil))
        (sumexp 0.0))
    (do-vector (i y)
      (incf sumexp (exp (- (vref y i) b))))
    (+ b (log sumexp))))

(defun softmax-on-vector (y)
  "exp(yi) / \sum{exp(yi)} = exp(yi - b) / \sum{exp(yi - b)}
NOTICE: will modify y in-place, copy-matrix if necessary"
  (let ((b (mmax y :axis nil))
        (sumexp 0.0))
    (do-vector (i y)
      ;; exp(y[i] - b)
      (setf (vref y i) (exp (- (vref y i) b)))
      (incf sumexp (vref y i)))
    (do-vector (i y)
      (setf (vref y i) (/ (vref y i) sumexp)))
    y))

(defun logsoftmax (y)
  "(log (softmax y))"
  (let ((b (mmax y :axis nil))
        (sumexp 0.0))
    (do-vector (i y)
      (setf (vref y i) (- (vref y i) b))
      (incf sumexp (exp (vref y i))))
    (setf sumexp (log sumexp))
    (do-vector (i y)
      (setf (vref y i) (- (vref y i) sumexp)))
    y))
