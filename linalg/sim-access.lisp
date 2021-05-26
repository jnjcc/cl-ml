;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: access opertaions for smatrix and smatrix-view through (MREF)
;;;; NOTICE: unless stated, all access opertaions share memory, too

(in-package #:cl-ml/linalg)

;; loop with matrix indices:
;; with indices of `mref' properly translated, we can now safely start loop from indices (0, 0)
(defmacro do-matrix-row ((i ma) &body body)
  `(dotimes (,i (nrow ,ma))
     ,@body))

(defmacro do-matrix-col ((j ma) &body body)
  `(dotimes (,j (ncol ,ma))
     ,@body))

;; NOTICE: with the form
;;   (do-matrix-row (,i ,ma)
;;     (do-matrix-col (,j ,ma)
;;       ,@body))
;;   we cannot do (RETURN) right in `body'
;; NOTICE: with the form
;;   (multiple-value-bind (,i ,j) (floor ,idx-sym (ncol ,ma))
;;     ,@body)
;;   we cannot do (GO end-of-loop) right in `body'
(defmacro do-matrix ((i j ma) &body body)
  (let ((idx-sym (gensym "IDX-")))
    `(do ((,idx-sym 0 (1+ ,idx-sym))
          (,i nil) (,j nil))
         ((>= ,idx-sym (* (nrow ,ma) (ncol ,ma))))
       ;; let's do row major!
       (multiple-value-setq (,i ,j) (floor ,idx-sym (ncol ,ma)))
       ,@body)))

(defmacro do-vector ((i va) &body body)
  (let ((len-sym (gensym "LEN-")))
    ;; one of nrow / ncol should be 1
    `(let ((,len-sym (* (nrow ,va) (ncol ,va))))
       (dotimes (,i ,len-sym)
         ,@body))))

(defun copy-matrix (ma)
  (let ((copy (make-matrix (nrow ma) (ncol ma))))
    (do-matrix (i j copy)
      (setf (mref copy i j) (mref ma i j)))
    copy))

(defun fill-matrix (ma mb)
  (do-matrix (i j ma)
    (setf (mref ma i j) (mref mb i j))))

(defun fill-vector (va vb)
  (do-vector (i va)
    (setf (vref va i) (vref vb i))))

(defmethod vref ((va smatrix) i)
  (case (svector-type va)
    (:column (mref va i 0))
    (:row (mref va 0 i))
    (otherwise (error "malformed vector dimension (~A, ~A)" (nrow va) (ncol va)))))

(defmethod (setf vref) (val (va smatrix) i)
  (case (svector-type va)
    (:column (setf (mref va i 0) val))
    (:row (setf (mref va 0 i) val))
    (otherwise (error "malformed vector dimension (~A, ~A)" (nrow va) (ncol va)))))

(defmethod mrv ((ma smatrix) i)
  "row vector of matrix"
  (make-matrix-view ma :row-view (list i)))

(defmethod (setf mrv) ((val smatrix) (ma smatrix) i)
  "set row vector of matrix"
  (unless (and (= (ncol ma) (ncol val))
               (= (nrow val) 1))
    (error "row vector dimension not match: (~A, ~A) (~A, ~A)" (nrow ma) (ncol ma)
           (nrow val) (ncol val)))
  (do-matrix-col (j ma)
    (setf (mref ma i j) (vref val j))))

(defmethod (setf mrv) ((val number) (ma smatrix) i)
  (do-matrix-col (j ma)
    (setf (mref ma i j) val)))

(defmethod mcv ((ma smatrix) j)
  "column vector of matrix"
  (make-matrix-view ma :col-view (list j)))

(defmethod (setf mcv) ((val smatrix) (ma smatrix) j)
  "set column vector of matrix"
  (unless (and (= (nrow ma) (nrow val))
               (= (ncol val) 1))
    (error "column vector dimension not match: (~A, ~A) (~A, ~A)" (nrow ma) (ncol ma)
           (nrow val) (ncol val)))
  (do-matrix-row (i ma)
    (setf (mref ma i j) (vref val i))))

(defmethod (setf mcv) ((val number) (ma smatrix) j)
  (do-matrix-row (i ma)
    (setf (mref ma i j) val)))

(defmethod print-object ((ma smatrix) stream)
  (fresh-line stream)
  (format stream "~A x ~A (~A):~%" (nrow ma) (ncol ma) (type-of (mref ma 0 0)))
  (do-matrix-row (i ma)
    (do-matrix-col (j ma)
      (format stream "~A~A" (mref ma i j) #\Tab))
    (format stream "~%")))
