;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Common Data Structure: Stack

(in-package #:cl-ml/algo)

;;; Stack with list
(defun make-stack ()
  nil)

(defmacro stack-push (stack data)
  `(setq ,stack (cons ,data ,stack)))

(defmacro stack-pop (stack)
  `(prog1
       (car ,stack)
     (setq ,stack (cdr ,stack))))

(defun stack-top (stack)
  (car stack))

(defun stack-empty (stack)
  (= (length stack) 0))
