;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

(defvar *test-stream* nil)

;;; utility functions
(defun matrix-eq (ma mb &optional (eq #'=))
  (when (and (= (nrow ma) (nrow mb)) (= (ncol ma) (ncol mb)))
    (do-matrix (i j ma)
      (unless (funcall eq (mref ma i j) (mref mb i j))
        (return-from matrix-eq nil)))
    (return-from matrix-eq t)))

(defun make-float-eq (epsilon)
  (lambda (a b)
    (cl-ml::float= a b epsilon)))

;;; TODO: unfortunately, this is not portable
#+sbcl
(defmacro with-random-seed ((seed) &body body)
  `(let ((*random-state* (sb-ext:seed-random-state ,seed)))
     ,@body))

#-sbcl
(defmacro with-random-seed ((seed) &body body)
  (declare (ignore seed))
  `(progn
     ,@body))

