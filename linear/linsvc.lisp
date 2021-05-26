;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Linear Support Vector Machine for classification

(in-package #:cl-ml)

(deftype svc-loss ()
  "L2-regularized L1-loss SVC; L2-regularized L2-loss SVC"
  '(member :l2reg-l1 :l2reg-l2))

(defclass linear-svc (estimator)
  ((theta :initform nil :reader theta :type smatrix)
   (loss :initform :l2reg-l1 :initarg :loss :reader loss :type svc-loss)
   (penalty :initform 1 :initarg :penalty :reader penalty
            :documentation "penalty in soft margin SVM, a.k.a, the C parameter")
   (tolerance :initform 0.1 :initarg :tolerance :documentation "tolerance \epsilon")
   (epochs :initform 100 :initarg :epochs)
   (expand-bias :initform t :initarg :expand-bias :reader expand-bias)))

(defmethod %coord-descent-dual ((linsvc linear-svc) X y)
  "A Dual Coordinate Descent Method for Large-scale Linear SVM"
  (with-slots (theta loss penalty tolerance epochs) linsvc
    (let* ((m (nrow X))
           (n (ncol X))
           (alpha (make-vector m :initial-element 0))
           upper diag
           (Qdiag (make-vector m :initial-element 0))
           grad pgrad
           ;; permutation indices of alpha's
           (indices (make-indices m))
           ;; the number of active `indices' after shrinking
           (nactive m)
           ;; shrinking
           (pgmax :infty)
           (pgmin :ninfty)
           pgmax-inner pgmin-inner)
      (setf theta (make-vector n :initial-element 0))
      (ecase loss
        ;; L1-SVM: U = C; D[i][i] = 0
        (:l2reg-l1 (setf upper penalty
                         diag 0))
        ;; L2-SVM: U = infty; D[i][i] = 1/2C
        (:l2reg-l2 (setf upper :infty
                         diag (* 0.5 penalty))))
      ;;; Q[i][i] diagonal elements
      (do-vector (i Qdiag)
        (setf (vref Qdiag i) (+ (vdot (mrv X i) (mrv X i)) diag)))

      (dotimes (epoch epochs)
        (setf pgmax-inner :ninfty)
        (setf pgmin-inner :infty)
        (shuffle indices 0 nactive)
        ;; `nactive' will change during the loop!
        (do-active-indices (i indx indices nactive)
          ;; G = y[i] * w * x[i] - 1 + D[i][i] * alpha[i]
          (setf grad (- (* (vref y indx) (vdot (mt theta) (mrv X indx))) 1))
          (incf grad (* diag (vref alpha indx)))
          (setf pgrad 0)
          (cond
            ((= (vref alpha indx) 0)
             (cond
               ((inf> grad pgmax) (progn
                                    (shrink-indices indices i nactive)
                                    (go end-of-inner-loop)))
               ((< grad 0) (setf pgrad grad))))
            ((inf= (vref alpha indx) upper)
             (cond
               ((inf< grad pgmin) (progn
                                    (shrink-indices indices i nactive)
                                    (go end-of-inner-loop)))
               ((> grad 0) (setf pgrad grad))))
            (t (setf pgrad grad)))
          (setf pgmax-inner (infmax pgmax-inner pgrad))
          (setf pgmin-inner (infmin pgmin-inner pgrad))
          (when (float/= grad 0)
            (let ((alpold (vref alpha indx)))
              (setf (vref alpha indx) (infmin (max (- (vref alpha indx) (/ grad (vref Qdiag indx))) 0.0)
                                              upper))
              (m+= theta (mt (m* (- (vref alpha indx) alpold) (vref y indx) (mrv X indx))))))
         end-of-inner-loop)
        ;; it is possible that `pgmax-inner' be :ninfty and `pgmin-inner' be :infty
        (when (and (numberp pgmax-inner) (numberp pgmin-inner)
                   (< (- pgmax-inner pgmin-inner) tolerance))
          (if (= nactive m)
              ;; return-from outer-loop
              (return)
              (setf nactive m
                    pgmax :infty
                    pgmin :ninfty)))
        (setf pgmax pgmax-inner)
        (setf pgmin pgmin-inner)
        (when (inf<= pgmax-inner 0)
          (setf pgmax :infty))
        (when (inf>= pgmin-inner 0)
          (setf pgmin :ninfty))))))

(defmethod fit ((linsvc linear-svc) X &optional y)
  (declare (type smatrix X))
  (when (expand-bias linsvc)
    (setf X (%expand-bias X)))
  (%coord-descent-dual linsvc X y))

(defun signfn (elem)
  (if (>= elem 0) +1 -1))

(defmethod predict ((linsvc linear-svc) X)
  (with-slots (theta expand-bias) linsvc
    (if expand-bias
        (vmap (m* (%expand-bias X) theta) #'signfn)
        (vmap (m* X theta) #'signfn))))
