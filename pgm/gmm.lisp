;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Unsupervised Learning > Probability Model Estimation > Mixture Model

(in-package #:cl-ml)

(defclass gaussian-mixture (estimator)
  ((ncomponents :initform 3 :initarg :ncomponents
                :documentation "number of mixture components")
   (alphas :initform nil :documentation "weight for each of the `ncomponents'")
   (mus :initform nil :documentation "mus of the shape `ncomponents'x`nfeatures'")
   ;; TODO: for now this is a list of matrices
   (sigmas :initform nil :documentation "convariance of each mixture component")
   ;; TODO: multiple initializations
   (ninit :initform 1 :initarg :ninit :documentation "number of inits to perform")
   (epochs :initform 30 :initarg :epochs :documentation "number of EM iterations")
   (tolerance :initform 1.0e-3 :initarg :tolerance)))

(defmethod init-components ((gauss gaussian-mixture) X)
  (with-slots (ncomponents alphas mus sigmas) gauss
    (setf alphas (make-rand-vector ncomponents))
    (m/= alphas (msum alphas))
    (setf mus (make-rand-matrix ncomponents (ncol X)))
    (setf sigmas nil)
    (dotimes (i ncomponents)
      (push (make-identity-matrix (ncol X)) sigmas))))

(defmethod expectation-step ((gauss gaussian-mixture) X qdist)
  "for each example I, estimate probability for component K"
  (with-slots (ncomponents alphas mus sigmas) gauss
    (let ((logvec (make-vector ncomponents :initial-element 0.0))
          ;; logsumexp(logvec)
          (logsum nil)
          ;; log(\alpha[k])
          (logalpha nil)
          ;; log(\phi(x[i])), given \theta[k]
          (logdensity nil))
      (do-matrix-row (i X)
        (dotimes (k ncomponents)
          (setf logalpha (log (vref alphas k)))
          (setf logdensity (mglogpdf (mrv X i) (mrv mus k) (nth k sigmas)))
          (setf (vref logvec k) (+ logalpha logdensity)))
        (setf logsum (logsumexp logvec))
        (dotimes (k ncomponents)
          (setf (mref qdist i k) (exp (- (vref logvec k) logsum))))))
    qdist))

(defmethod maximization-step ((gauss gaussian-mixture) X qdist)
  (with-slots (ncomponents alphas mus sigmas) gauss
    (let ((denoms (msum qdist :axis 0)))
      (dotimes (k ncomponents)
        (setf (vref alphas k) (/ (vref denoms k) (nrow X)))

        (setf (mrv mus k) 0.0)
        (do-matrix-row (i X)
          (m+= (mrv mus k) (m* (mref qdist i k) (mrv X i))))
        (if (float= (vref denoms k) 0 1.0e-5)
            (setf (mrv mus k) 0.0)
            (m/= (mrv mus k) (vref denoms k)))

        (let ((sigmak (nth k sigmas)))
          (do-matrix (i j sigmak)
            (setf (mref sigmak i j) 0.0))
          (do-matrix-row (i X)
            (let ((dif (m- (mrv X i) (mrv mus k))))
              (m+= sigmak (m* (mref qdist i k) (mt dif) dif))))
          (unless (float= (vref denoms k) 0 1.0e-5)
            (m/= sigmak (vref denoms k)))))
      )))

(defmethod fit ((gauss gaussian-mixture) X &optional y)
  (declare (ignore y))
  (with-slots (ncomponents alphas mus sigmas ninit epochs tolerance) gauss
    (dotimes (ini ninit)
      (init-components gauss X)
      (let ((qdist (make-matrix (nrow X) ncomponents)))
        (dotimes (ep epochs)
          ;; E-step: qdist[i][k] = P(z[i] = k | X, \Theta)
          (expectation-step gauss X qdist)
          ;; M-step: alphas, mus, sigmas
          (maximization-step gauss X qdist))))))

(defmethod predict-proba ((gauss gaussian-mixture) X)
  (with-slots (ncomponents) gauss
    (let ((pred (make-matrix (nrow X) ncomponents)))
      (expectation-step gauss X pred)
      pred)))

(defmethod predict ((gauss gaussian-mixture) X)
  (let ((prob (predict-proba gauss X)))
    (margmax prob :axis 1)))
