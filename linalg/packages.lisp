;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Linear Algebra

(in-package #:cl-user)

(defpackage #:cl-ml/linalg
  (:nicknames #:ml/linalg)
  (:use #:cl #:cl-ml/probs)
  (:export #:smatrix #:nrow #:ncol #:mshape

           ;; smatrix creation
           #:make-matrix #:copy-matrix #:fill-matrix
           #:make-square-matrix #:make-identity-matrix #:make-rand-matrix
           #:make-vector #:make-rand-vector #:fill-vector
           #:make-row-vector #:make-rand-row-vector

           ;; smatrix-view creation
           #:make-matrix-view
           #:update-matrix-view #:with-matrix-view #:with-restored-view

           ;; smatrix element access
           #:do-matrix #:do-vector
           #:do-matrix-row #:do-matrix-col
           #:mref #:vref #:mrv #:mcv

           ;; smatrix properties
           #:mdet
           #:svector-type

           ;; smatrix operations
           #:m+ #:m- #:m* #:m/ #:m+= #:m-= #:m*= #:m/=
           #:mt #:minv #:msvd #:mpinv #:mmap #:mreduce
           #:v+ #:v- #:v* #:v/ #:v+= #:v-= #:v*= #:v/=
           #:vdot #:vouter #:vmap #:vreduce

           ;; smatrix indirect addressing
           #:swap-indices #:sort-indices #:do-indices #:do-ia-access
           #:do-mutable-indices #:do-mutable-iacess
           #:iavref

           ;; smatrix statistics
           #:mhead #:msum #:mmean #:mquantile #:mmax #:mmin #:margmax #:margmin
           #:mbincount
           ))
