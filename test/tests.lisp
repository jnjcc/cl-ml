;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

(defun run-all-tests ()
  (let ((result (run-tests :all :cl-ml/test)))
    (test-names result)
    result))
