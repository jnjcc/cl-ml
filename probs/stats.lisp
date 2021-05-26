;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Statistics stuff on list

(in-package #:cl-ml/probs)

(defun sum (lst)
  (reduce #'+ lst :initial-value 0))

(defun mean (lst)
  (/ (sum lst) (length lst)))

(defun quantile-positions (len &optional (quad 0.5))
  "(values left-idx left-ratio right-idx right-ratio)"
  (cond
    ((= len 1) (values 0 1.0 0 0.0))
    ((<= quad 0) (values 0 1.0 0 0))
    ((>= quad 1) (values (- len 1) 1.0 0 0))
    (t (let* ((diff (- len 1))
              (loc (* quad diff)))
         (multiple-value-bind (idx ratio) (floor loc)
           (values idx (- 1.0 ratio) (+ idx 1) ratio))))))

(defun quantile (lst &optional (quant 0.5))
  "NOTICE: will modify `lst', copy-list if necessary"
  (setf lst (sort lst #'<))
  (unless (consp quant)
    (setf quant (list quant)))
  (let ((retlst nil))
    (dolist (qua quant)
      (multiple-value-bind (i ir j jr) (quantile-positions (length lst) qua)
        (if (>= ir 1)
            (push (nth i lst) retlst)
            (push (+ (* ir (nth i lst)) (* jr (nth j lst))) retlst))))
    (if (= (length retlst) 1)
        (car retlst)
        (nreverse retlst))))
