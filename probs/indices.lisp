;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/probs)

(defun make-step-range (start end &optional (step 1))
  (let ((lst nil)
        (endfn #'>=))
    (when (< step 0)
      (setf endfn #'<=))
    (do ((val start (+ val step)))
        ((funcall endfn val end))
      (push val lst))
    (nreverse lst)))

(defun make-indices (n)
  (make-step-range 0 n))
