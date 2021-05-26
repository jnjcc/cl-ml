;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-user)

(defpackage #:cl-ml/test
  (:nicknames #:ml/test)
  (:use #:cl #:lisp-unit #:cl-ml/probs #:cl-ml/linalg #:cl-ml/algo
        #:cl-ml/graph #:cl-ml/io #:cl-ml)
  (:export #:run-all-tests))
