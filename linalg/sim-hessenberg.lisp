;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: Hessenberg using Householder transformation

(in-package #:cl-ml/linalg)

(defun %bottom-left-vector (mAv)
  "mAv = ((A11, \pmb{a}_{2}) (\pmb{a}_{1}, A_{2})),
we want to extract \pmb{a}_{1}: first column vector without A11"
  (make-matrix-view mAv :row-view '(1 . :end) :col-view '(0)))

(defun %widen-to-right-block (mAv)
  "mAv = ((A1, A3) (A4, A2)), we want to extract ((A3) (A2))
right block of the whole matrix A"
  (update-matrix-view mAv :row-view '(0 . :end)))

(defun %householder-and-narrow-view (mAv beta vvec)
  "HAH, while `beta' and `vvec' are parameters of \tilde{H}:
H = ((I, 0) (0, \tilde{H})); A = ((A1, A3) (A4, A2)); mAv is A2 plus last row of A3 & last column of A4
H * mAv * H = ((a[1][1], a_{2}^{T} * \tilde{H}) (\tilde{H} * a_{1}, \tilde{H} * mAv * \tilde{H}))
HAH = ((A1, A3 * \tilde{H}) (\tilde{H} * A4, \tilde{H} * A2 * \tilde{H}))
The whole (A3 * \tilde{H}) part must be re-calculated each time"
  ;; bottom left vector a_{1}
  (householder-on-vector (%bottom-left-vector mAv) beta vvec)
  ;; right block B of the original matrix A, multiply \tilde{H} on the right:
  ;; B = B * \tilde{H}
  (%narrow-matrix-column mAv)
  (with-restored-view (mAv)
    (%widen-to-right-block mAv)
    (householder-on-row-matrix mAv beta vvec))
  ;; A2 = \tilde{H} * A2
  (%narrow-matrix-row mAv)
  (householder-on-matrix mAv beta vvec))

(defun hessenberg (mA)
  "Requires: nrow = ncol; Returns: (Q, H) such that Q^{T}AQ = H;
NOTICE: will modify A in-place, copy-matrix if necessary
The backward version: Q = H1 * (H2 * (H3 * ... * (H[n-2] * I))),
where \tilde{H}[n-2] is 2x2"
  (let* ((n (nrow mA))
         (mQ (make-identity-matrix n))
         ;; \tilde{H}[n-2] starts from 2x2
         (mQv (%make-square-view mQ 2))
         (mHlist nil)
         (mAv (%make-full-view mA)))
    (do ((k 1 (+ k 1)))
        ((>= k (- n 1)))
      (multiple-value-bind (beta vvec aval) (householder (%bottom-left-vector mAv))
        (declare (ignore aval))
        ;; A = H * A * H
        (%householder-and-narrow-view mAv beta vvec)
        (push (list beta vvec) mHlist)))
    (dolist (Hvals mHlist)
      ;; Q = H * Q
      (householder-on-matrix mQv (car Hvals) (cadr Hvals))
      (%widen-square-view mQv))
    (values mQ mA)))

(defun hessenberg-forward (mA)
  "Requires: nrow = ncol; Returns: (Q, H) such that Q^{T}AQ = H;
NOTICE: will modify A in-place, copy-matrix if necessary
The forward version: Q = ((H1 * H2) * H3) * ... * H[n-2]"
  (let* ((n (nrow mA))
         (mQ (make-identity-matrix n))
         (mH (make-identity-matrix n))
         ;; \tilde{H}[n-2] starts from (n-1)x(n-1)
         (mHv (%make-square-view mH (- n 1)))
         (mAv (%make-full-view mA)))
    (do ((k 1 (+ k 1)))
        ((>= k (- n 1)))
      (multiple-value-bind (beta vvec aval) (householder (%bottom-left-vector mAv))
        (declare (ignore aval))
        ;; A = H * A * H
        (%householder-and-narrow-view mAv beta vvec)
        (householder-on-matrix mHv beta vvec)
        (m*= mQ mH)
        (%narrow-householder-view mHv)))
    (values mQ mA)))
