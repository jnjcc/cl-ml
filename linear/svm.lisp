;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Support Vector Machine, the common algorithm

(in-package #:cl-ml)

;;; TODO: Shrinking, how about a class of shrinking?
(defmacro shrink-indices (indices i nactive)
  "struck out i-th index"
  ;; NOTICE: not necessary to do gensym, there are no &body
  `(progn
     (swap-indices ,indices ,i (- ,nactive 1))
     (setf ,nactive (- ,nactive 1))))

(defmacro do-active-alphas ((indx indices nactive) &body body)
  "`indx' is the index of alpha's
NOTICE: the `nactive' might change during loop"
  `(do-mutable-iacess (,indx ,indices ,nactive)
     ,@body))

(defmacro do-active-indices ((i indx indices nactive) &body body)
  "`indx' is the index of alpha's, I is the index of `indices'
NOTICE: the `nactive' might change during loop"
  `(do-mutable-indices (,i ,indx ,indices ,nactive)
     ,@body))

(defmacro do-shrunk-alphas ((indx indices nactive) &body body)
  "`indx' is the index of alpha's
NOTICE: change of `nactive' won't take effect"
  `(do-ia-access (,indx ,indices :start ,nactive)
     ,@body))

(defmacro do-shrunk-indices ((i indx indices nactive) &body body)
  "`indx' is the index of alpha's, I is the index of `indices'
NOTICE: change of `nactive' won't take effect"
  `(do-indices (,i ,indx ,indices :start ,nactive)
     ,@body))
