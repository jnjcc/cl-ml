;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: bidiagonalization using Householder transformation

(in-package #:cl-ml/linalg)

(defun %make-full-view (mA)
  (make-matrix-view mA :row-view '(0 . :end) :col-view '(0 . :end)))

;;; narrow from top-left corner to bottom-right corner
(defun %narrow-matrix-column (mAv)
  "A[k] = (v[k] A[k+1])
extract A[k+1] to calculate householder-of-row-vector"
  (let ((kplus (+ (car (col-view mAv)) 1)))
    (update-matrix-view mAv :col-view `(,kplus . :end))))

(defun %narrow-matrix-row (mAv)
  "A[k] = (v^{T} A[k+1])^{T}, extract A[k+1]"
  (let ((kplus (+ (car (row-view mAv)) 1)))
    (update-matrix-view mAv :row-view `(,kplus . :end))))

(defun %make-square-view (mH s)
  "bottom right corner of square matrix mH, with size S x S"
  (let* ((n (nrow mH))
         (start (- n s)))
    (make-matrix-view mH :row-view `(,start . :end)
                      :col-view `(,start . :end))))

;;; widen from bottom-right corner to top-left corner
(defun %widen-square-view (mHv)
  (let ((kminus (- (car (row-view mHv)) 1)))
    (update-matrix-view mHv :row-view `(,kminus . :end)
                        :col-view `(,kminus . :end))))

;;; Assume m > n
(defun %householder-on-matrix (mAv beta vvec delta &optional (force nil))
  (householder-on-matrix mAv beta vvec)
  (when force
    ;; force first column zero to avoid precision problem
    (householder-on-vector (mcv mAv 0) beta vvec delta)))

(defun %householder-on-row-matrix (mAv beta vvec gamma &optional (force nil))
  (householder-on-row-matrix mAv beta vvec)
  (when force
    (householder-on-row-vector (mrv mAv 0) beta vvec gamma)))

(defun bidiagonalize (mA)
  "Requires: nrow > ncol; Returns: (U, V, B) such that U^{T}AV = B;
NOTICE: will modify A in-place, copy-matrix if necessary
The backward version, which avoids (M*=) & (%narrow-householder-view) using
householder-on-matrix: U = P1 * (P2 * (P3 * ... * (Pn * I)))
Outside the view of P[k] and P[k+1], all other elements are 1's and 0's of identity
matrix, we can safely do (P[k] * P[k+1]) before P[k-1] with elements outside of P[k]'s
view untouched (imagine partitioned matrix!)"
  (let* ((m (nrow mA))
         (n (ncol mA))
         (mU (make-identity-matrix m))
         (mV (make-identity-matrix n))
         ;; \tilde{P}[n] starts from (m-n+1)x(m-n+1)
         (mUv (%make-square-view mU (- m n -1)))
         ;; \tilde{H}[n-2] starts from 2x2
         (mVv (%make-square-view mV 2))
         (mPlist nil) ;; list of (beta v) for Householder matrix P's
         (mHlist nil)
         (mAv (%make-full-view mA)))
    (do ((k 1 (+ k 1)))
        ((>= k (- n 1)))
      ;; A_{k-1} = (v_{k} A_{k})
      (multiple-value-bind (beta vvec delta) (householder (mcv mAv 0))
        ;; A_{k-1} = P * A_{k-1}
        (%householder-on-matrix mAv beta vvec delta)
        (push (list beta vvec) mPlist))
      ;; extract A_{k}
      (%narrow-matrix-column mAv)
      ;; A_{k} = (u^{T} \tilde{A}_{k})^{T}
      (multiple-value-bind (beta vvec gamma) (householder-of-row-vector (mrv mAv 0))
        ;; A * H = (H * A^{T})^{T}
        (%householder-on-row-matrix mAv beta vvec gamma)
        (push (list beta vvec) mHlist))
      ;; extract as A_{k} for the next loop
      (%narrow-matrix-row mAv))
    ;; k = n-1: collect P[n-1, n]
    (multiple-value-bind (beta vvec delta) (householder (mcv mAv 0))
      (%householder-on-matrix mAv beta vvec delta)
      (push (list beta vvec) mPlist))
    ;; extract A_{n-1} = (\gamma_{n} v_{n})^{T}
    (%narrow-matrix-column mAv)
    ;; extract v_{n}
    (%narrow-matrix-row mAv)
    (multiple-value-bind (beta vvec delta) (householder (mcv mAv 0))
      (%householder-on-matrix mAv beta vvec delta)
      (push (list beta vvec) mPlist))
    (dolist (Pvals mPlist)
      ;; U = P * U
      (householder-on-matrix mUv (car Pvals) (cadr Pvals))
      (%widen-square-view mUv))
    (dolist (Hvals mHlist)
      ;; V = H * V
      (householder-on-matrix mVv (car Hvals) (cadr Hvals))
      (%widen-square-view mVv))
    (values mU mV mA)))

(defun %narrow-householder-view (mHv)
  "make `mHv' identity matrix, and narrow view, so as to hold the next Householder matrix
\((k . :end) (k . :end)) => ((k+1 . :end) (k+1 . :end))"
  (do-matrix (i j mHv)
    (if (= i j)
        (setf (mref mHv i j) 1)
        (setf (mref mHv i j) 0)))
  (let ((kplus (+ (car (row-view mHv)) 1)))
    (update-matrix-view mHv :row-view `(,kplus . :end)
                        :col-view `(,kplus . :end))))

(defun bidiagonalize-forward (mA)
  "Requires: nrow > ncol; Returns: (U, V, D) such that U^{T}AV = D
NOTICE: will modify A in-place, copy-matrix if necessary
This is the forward version: U = ((P1 * P2) * P3) * ... Pn"
  (let* ((m (nrow mA))
         (n (ncol mA))
         (mU (make-identity-matrix m))
         (mV (make-identity-matrix n))
         (mP (make-identity-matrix m))
         (mH (make-identity-matrix n))
         (mPv (%make-square-view mP m))
         (mHv (%make-square-view mH (- n 1)))
         (mAv (%make-full-view mA)))
    (do ((k 1 (+ k 1)))
        ((>= k (- n 1)))
      ;; A_{k-1} = (v_{k} A_{k})
      (multiple-value-bind (beta vvec delta) (householder (mcv mAv 0))
        ;; A_{k-1} = P * A_{k-1}
        (%householder-on-matrix mAv beta vvec delta)
        ;; k-th Householder matrix: do Householder tranformation on identity matrix
        (householder-on-matrix mPv beta vvec)
        (m*= mU mP)
        ;; make identity matrix
        (%narrow-householder-view mPv))
      ;; extract A_{k}
      (%narrow-matrix-column mAv)
      ;; A_{k} = (u^{T} \tilde{A}_{k})^{T}
      (multiple-value-bind (beta vvec gamma) (householder-of-row-vector (mrv mAv 0))
        ;; A * H = (H * A^{T})^{T}
        (%householder-on-row-matrix mAv beta vvec gamma)
        (householder-on-row-matrix mHv beta vvec)
        (m*= mV mH)
        (%narrow-householder-view mHv))
      ;; extract as A_{k} for the next loop
      (%narrow-matrix-row mAv))
    ;; k = n-1: collect P[n-1, n]
    (multiple-value-bind (beta vvec delta) (householder (mcv mAv 0))
      (%householder-on-matrix mAv beta vvec delta)
      (householder-on-matrix mPv beta vvec)
      (m*= mU mP)
      (%narrow-householder-view mPv))
    ;; extract A_{n-1} = (\gamma_{n} v_{n})^{T}
    (%narrow-matrix-column mAv)
    ;; extract v_{n}
    (%narrow-matrix-row mAv)
    (multiple-value-bind (beta vvec delta) (householder (mcv mAv 0))
      (%householder-on-matrix mAv beta vvec delta)
      (householder-on-matrix mPv beta vvec)
      (m*= mU mP)
      (%narrow-householder-view mPv))
    (values mU mV mA)))
