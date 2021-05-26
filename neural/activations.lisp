;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Neural Network > Activation Functions

(in-package #:cl-ml)

(deftype activation-type ()
  '(member :sigmoid :relu))

(defclass activation-base ()
  ())

(defgeneric activate-matrix (act X)
  (:documentation "call activation on X"))

(defgeneric activate-gradient-matrix (act A)
  (:documentation "first-order derivative on A"))

;;; Sigmoid Activation
(defclass activation-sigmoid (activation-base)
  ())

(defmethod activate-matrix ((sigmoid activation-sigmoid) X)
  (mmap X #'sigmoid)
  X)

(defmethod activate-gradient-matrix ((sigmoid activation-sigmoid) A)
  (labels ((gradsig (z)
             (let ((sig (sigmoid z)))
               (* sig (- 1 sig)))))
    (mmap A #'gradsig))
  A)

;;; ReLu
(defclass activation-relu (activation-base)
  ())

(defmethod activate-matrix ((relu activation-relu) X)
  (labels ((relufn (z)
             (if (> z 0) z 0)))
    (mmap X #'relufn))
  X)

(defmethod activate-gradient-matrix ((relu activation-relu) A)
  (labels ((gradrelu (z)
             (if (> z 0) 1 0)))
    (mmap A #'gradrelu))
  A)

;;; Softmax
(defclass activation-softmax (activation-base)
  ())

(defun softmax-on-matrix (mA)
  (do-matrix-row (i mA)
    (softmax-on-vector (mrv mA i)))
  mA)

;;; make-* functions
(defun make-activation (type &optional args)
  (ecase type
    (:sigmoid (make-instance 'activation-sigmoid))
    (:relu (make-instance 'activation-relu))))
