;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Polynomial Feature Transformer

(in-package #:cl-ml)

(defclass poly-transformer (transformer)
  ((degree :initform 1 :initarg :degree :reader degree)
   (expand-bias :initform t :initarg :expand-bias :reader expand-bias
                :documentation "first column as bias")
   (indices-list :initform nil :reader indices-list :type list)))

;; Problem: given `ncol' variables, list all possibles combination of variables up to `degree'
;;   by adding an extra bias of 1, and "assign" some degree to the bias, we can make
;;   the sum of the total degree "equal to" `degree', and thus we can make a call to
;;   (combination-with-replacement)
(defun %visit-comb-indices-poly (ncol degree &optional visit)
  (visit-combinations-indices (+ ncol 1) degree :replace t :visit visit))

(defun %comb-indices-poly (ncol degree)
  (combinations-indices (+ ncol 1) degree :replace t))

(defmethod fit ((poly poly-transformer) X &optional y)
  (declare (type smatrix X) (ignore y))
  (with-slots (degree indices-list) poly
    (setf indices-list
          (%comb-indices-poly (ncol X) degree))))

(defun %factorial (n)
  (let ((fact 1))
    (dotimes (i n fact)
      (setf fact (* fact (+ i 1))))))

(defun %make-poly-matrix (X degree &optional (expand-bias t))
  "initialize polynomial feature matrix from X, up to `degree' degree"
  (let* ((n (ncol X))
         (d degree)
         ;; (n+d)! / (n! d!)
         (newcol (/ (%factorial (+ n d)) (* (%factorial n) (%factorial d)))))
    (unless expand-bias
      (decf newcol))
    (make-matrix (nrow X) newcol :initial-element 1)))

(defun %fill-poly-matrix (x-poly colidx X indices &optional (expand-bias t))
  "fill column `colidx' of polynomial feature matrix from columns `indices' of X
Return t if we updated the corresponding column, NIL otherwise"
  (let ((fill-bias-p t))
    (dolist (i indices)
      ;; column 0 as bias when expand-bias
      (unless (= i 0)
        ;; we are filling some column other than the bias column
        (setf fill-bias-p nil)
        (setf (mcv x-poly colidx) (v* (mcv x-poly colidx) (mcv X (- i 1))))))
    (or (not fill-bias-p) expand-bias)))

(defmethod transform ((poly poly-transformer) X)
  (declare (type smatrix X))
  (with-slots (degree expand-bias indices-list) poly
    (let ((x-poly (%make-poly-matrix X degree expand-bias))
          (colidx 0))
      (dolist (indices indices-list)
        (when (%fill-poly-matrix x-poly colidx X indices expand-bias)
          (incf colidx)))
      x-poly)))

(defmethod fit-transform ((poly poly-transformer) X)
  (declare (type smatrix X))
  (let ((x-poly (%make-poly-matrix X (degree poly) (expand-bias poly)))
        ;; column index of x-poly
        (colidx 0))
    (with-slots (expand-bias indices-list) poly
      (setf indices-list nil)
      (labels ((intersect (indices)
                 (push (copy-list indices) indices-list)
                 (when (%fill-poly-matrix x-poly colidx X indices expand-bias)
                   (incf colidx))))
        (%visit-comb-indices-poly (ncol X) (degree poly) #'intersect)
        (setf indices-list (nreverse indices-list))))
    x-poly))
