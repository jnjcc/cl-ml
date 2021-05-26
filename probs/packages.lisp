;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Probability and Statistics

(in-package #:cl-user)

(defpackage #:cl-ml/probs
  (:nicknames #:ml/probs)
  (:use #:cl)
  (:export #:float= #:float/= #:float< #:float>
           #:cumsum
           #:randint #:randuni #:randnorm

           ;; range and indices
           #:make-step-range
           #:make-indices

           ;; combinations and permutations
           #:combinations #:combinations-indices
           #:visit-combinations-indices
           #:permutations #:permutations-indices
           #:visit-permutations-indices

           ;; shuffle
           #:shuffle #:shuffle-indices

           ;; sampling
           #:simple-sampling #:simple-indices
           #:weighted-sampling #:weighted-indices
           #:alias-initialize #:alias-draw-index #:alias-draw-element
           #:alias-sampling #:alias-indices
           #:bootstrap-sampling #:bootstrap-indices
           #:pasting-sampling #:pasting-indices
           ;; bin operations

           #:binkey #:binvalue #:bincount
           #:binincf #:bindecf #:binpush #:do-bins
           #:density #:probability #:bindensity
           #:argmax #:argmin

           ;; statistics
           #:quantile-positions
           #:quantile
           ))
