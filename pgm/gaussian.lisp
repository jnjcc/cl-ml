;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Multivariate Gaussian Distribution

(in-package #:cl-ml)

(defun mgpdf (x mus sigmas)
  "Probability Density Function for Multivariate Gaussian Distribution
`mus': row or column vector; `sigmas': covariance matrix"
  (declare (type smatrix mus sigmas))
  (let ((n (nrow sigmas))
        (det (abs (mdet sigmas)))
        (maha (mahalanobis-distance x mus sigmas)))
    (/ (exp (* -1/2 maha maha))
       (* (expt (* 2 pi) (/ n 2))
          (expt det 1/2)))))

(defun mglogpdf (x mus sigmas)
  (declare (type smatrix mus sigmas))
  (let ((n (nrow sigmas))
        (det (abs (mdet sigmas)))
        (maha (mahalanobis-distance x mus sigmas)))
    (- (* -1/2 maha maha)
       (* n 1/2 (log (* 2 pi)))
       (* 1/2 (log det)))))
