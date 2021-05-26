;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: Singular Value Decomposition and Moore-Penrose Pseudoinverse

(in-package #:cl-ml/linalg)

;;; Singular Value Decomposition
(defun %make-square-view-bound (mB p q)
  (let ((end (+ q 1)))
    (make-matrix-view mB :row-view `(,p . ,end) :col-view `(,p . ,end))))

(defun %update-square-view-bound (mBv p q)
  (let ((end (+ q 1)))
    (update-matrix-view mBv :row-view `(,p . ,end) :col-view `(,p . ,end))))

(defun %update-colview-bound (mUv p q)
  (let ((end (+ q 1)))
    (update-matrix-view mUv :col-view `(,p . ,end))))

(defun msvd (mA &optional (eps 1e-5))
  "Requires: nrow > ncol; Returns: (U, B, V) such that A = UBV^{T}
NOTICE: will modify A in-place, copy-matrix if necessary"
  (multiple-value-bind (mU mV mB) (bidiagonalize mA)
    (let ((delta (make-diagonal-view mB :diag-type :main))
          (gamma (make-diagonal-view mB :diag-type :upper :pseudo-start 1))
          (mUv (%make-full-view mU))
          (mVv (%make-full-view mV)))
      ;; forever-loop
      (do ((mBsqv (%make-full-view mB))) (nil)
        (multiple-value-bind (converge-p p q) (%converge-test delta gamma mU eps)
          (when converge-p
            (return-from msvd (values mU mB mV)))
          (%update-square-view-bound mBsqv p q)
          (multiple-value-bind (mP mQ) (wilkinson-bidiagonal mBsqv)
            (%update-colview-bound mUv p q)
            (m*= mUv mP)
            (%update-colview-bound mVv p q)
            (m*= mVv mQ)))))))

(defun %converge-test (delta gamma mU eps)
  "(delta[0], delta[1], ..., delta[n-1]) vs (gamma[1], gamma[2], ..., gamma[n-1])"
  (let* ((n (ndiag delta))
         (converge-p t)
         (p 1)
         (q n))
    ;; forever-loop
    (do () (nil)
      ;;; (i) clear gamma's
      (do-diagonal (j gamma)
        (when (<= (abs (dref gamma j))
                  (* eps (+ (abs (dref delta j)) (abs (dref delta (- j 1))))))
          (setf (dref gamma j) 0)))
      ;;; (ii) find interval p, q
      (setf converge-p t)
      (do-diagonal (j gamma)
        (when (/= (dref gamma j) 0)
          (setf converge-p nil)
          (return)))
      (when converge-p
        (return-from %converge-test (values t nil nil)))
      (let ((found-q nil)
            (found-p nil))
        (do-diagonal-reverse (j gamma)
          (if (not found-q)
              (when (/= (dref gamma j) 0)
                (setf found-q t)
                (setf q j))
              (when (= (dref gamma j) 0)
                (setf found-p t)
                (setf p j)
                (return))))
        ;; assume a pseudo gamma[0] := 0, or {gamma[pseudo_start - 1] := 0}
        (unless found-p
          (setf p 0)))
      ;;; (iii) initialize x, y
      (let ((infnorm (%infinity-norm delta gamma))
            i x y)
        (do ((idx p (+ idx 1)))
            ((>= idx q) (return-from %converge-test (values nil p q)))
          (when (<= (abs (dref delta idx)) (* eps infnorm))
            (setf i idx)
            (setf (dref delta i) 0)
            (setf x (dref gamma (+ i 1)))
            (setf y (dref delta (+ i 1)))
            (setf (dref gamma (+ i 1)) 0)
            (return)))
        ;;; (iv) [[c s] [-s c]] * [[x] [y]] = [[0] [sigma]]
        (do ((l 1 (+ l 1)))
            ((>= l (- q i)))
          ;; what we want: c * x + s * y = 0
          (multiple-value-bind (s c) (givens-from-value x y)
            ;; what we get: -c * x + s * y = 0
            (setf c (- c))
            ;; -s * x + c * y
            (setf (dref delta (+ i l)) (+ (* (- s) x) (* c y)))
            (givens-on-row-matrix mU i (+ i l) c (- s))
            (setf x (* s (dref gamma (+ i l 1))))
            (setf (dref gamma (+ i l 1)) (* c (dref gamma (+ i l 1))))
            (setf y (dref delta (+ i l 1)))))))))

(defun %infinity-norm (delta gamma)
  "(delta[0], delta[1], ..., delta[n-1]) vs (gamma[1], gamma[2], ..., gamma[n-1])"
  (let ((norm (abs (dref delta (- (ndiag delta) 1))))
        (crow 0))
    (do-diagonal (i gamma)
      (setf crow (+ (abs (dref delta (- i 1)))
                    (abs (dref gamma i))))
      (when (> crow norm)
        (setf norm crow)))
    norm))

;;; Moore-Penrose Pseudoinverse
(defmethod mpinv ((ma smatrix))
  (let ((mD (copy-matrix ma))
        (dim (min (nrow ma) (ncol ma))))
    (multiple-value-bind (mU mD mV) (msvd mD)
      (dotimes (i dim)
        (when (/= (mref mD i i) 0)
          (setf (mref mD i i) (/ 1.0 (mref mD i i)))))
      (m* mV mD (mt mU)))))
