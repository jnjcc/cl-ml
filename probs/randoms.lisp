;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Probability stuff

(in-package #:cl-ml/probs)

(defun %random-generator (shape randfn &rest args)
  (let ((randret nil))
    (cond
      ((null shape) (setf randret (apply randfn args)))
      ((integerp shape) (dotimes (i shape)
                          (push (apply randfn args) randret)))
      ((consp shape)
       (dotimes (i (car shape))
         (let ((randcur nil))
           (dotimes (j (cadr shape))
             (push (apply randfn args) randcur))
           (push randcur randret)))))
    randret))

;;; Random integers
(defun %randint (low high &optional (state *random-state*))
  "random int [low, high)"
  (let* ((len (- high low))
         (idx (random len state)))
    (+ low idx)))

(defun randint (low &optional high shape (state *random-state*))
  (if (null high)
      (%random-generator shape #'%randint 0 low state)
      (%random-generator shape #'%randint low high state)))

;;; Uniform distribution
(defun randuni (limit &optional shape (state *random-state*))
  "Uniform distribution"
  (%random-generator shape #'random limit state))

;;; Normal Distribution
(defun randnorm (mu sigma)
  "random number from Gaussian Distribution"
  (+ mu sigma))
