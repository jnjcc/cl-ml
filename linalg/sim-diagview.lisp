;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: matrix diagonal view

(in-package #:cl-ml/linalg)

(deftype diagonal-type ()
  '(member :main :upper :lower :both))

(defclass smatrix-diagview (smatrix)
  ((displaced-to :initform nil :initarg :displaced-to :reader displaced-to)
   (diag-type :initform :main :initarg :diag-type :type diagonal-type)
   ;; assume index starts from `pseudo-start'
   (pseudo-start :initform 0 :initarg :pseudo-start :reader pseudo-start)))

;;; rows and colums
(defmethod nrow ((ma smatrix-diagview))
  (with-slots (displaced-to) ma
    (if displaced-to
        (nrow displaced-to)
        0)))

(defmethod ncol ((ma smatrix-diagview))
  (with-slots (displaced-to) ma
    (if displaced-to
        (ncol displaced-to)
        0)))

(defmethod ndiag ((ma smatrix-diagview))
  (let ((n (min (nrow ma) (ncol ma))))
    (with-slots (diag-type) ma
      (ecase diag-type
        (:main n)
        (:upper (if (>= (nrow ma) (ncol ma))
                    (- n 1)
                    n))
        (:lower (if (<= (nrow ma) (ncol ma))
                    (- n 1)
                    n))
        (:both (if (= (nrow ma) (ncol ma))
                   (- n 1)
                   (error "square matrix expected: (~A, ~A)" (nrow ma) (ncol ma))))))))

;;; element access
(defmethod %indices-valid-p ((ma smatrix-diagview) i j)
  (with-slots (diag-type) ma
    (ecase diag-type
      (:main (= i j))
      (:upper (= (+ i 1) j))
      (:lower (= i (+ j 1)))
      (:both (or (= (+ i 1) j) (= i (+ j 1)))))))

(defmethod mref ((ma smatrix-diagview) i j)
  (with-slots (displaced-to) ma
    (if (%indices-valid-p ma i j)
        (mref displaced-to i j)
        0)))

(defmethod (setf mref) (val (ma smatrix-diagview) i j)
  (with-slots (displaced-to) ma
    (when (%indices-valid-p ma i j)
      (setf (mref displaced-to i j) val))))

;; diagonal element access
(defmethod dref ((ma smatrix-diagview) i)
  (with-slots (diag-type pseudo-start) ma
    (decf i pseudo-start)
    (ecase diag-type
      (:main (mref ma i i))
      (:upper (mref ma i (+ i 1)))
      (:lower (mref ma (+ i 1) i))
      (:both (mref ma i (+ i 1))))))

(defmethod (setf dref) (val (ma smatrix-diagview) i)
  (with-slots (diag-type pseudo-start) ma
    (decf i pseudo-start)
    (ecase diag-type
      (:main (setf (mref ma i i) val))
      (:upper (setf (mref ma i (+ i 1)) val))
      (:lower (setf (mref ma (+ i 1) i) val))
      (:both (setf (mref ma i (+ i 1)) val
                   (mref ma (+ i 1) i) val)))))

(defmacro do-diagonal ((i ma) &body body)
  (let ((ndiag-sym (gensym "NDIAG-"))
        (start-sym (gensym "START-")))
    `(let ((,ndiag-sym (ndiag ,ma))
           (,start-sym (pseudo-start ,ma)))
       (do ((,i ,start-sym (+ ,i 1)))
           ((>= ,i (+ ,start-sym ,ndiag-sym)))
         ,@body))))

(defmacro do-diagonal-reverse ((i ma) &body body)
  (let ((ndiag-sym (gensym "NDIAG-"))
        (start-sym (gensym "START-")))
    `(let ((,ndiag-sym (ndiag ,ma))
           (,start-sym (pseudo-start ,ma)))
       (do ((,i (+ ,start-sym ,ndiag-sym -1) (- ,i 1)))
           ((< ,i ,start-sym))
         ,@body))))

;;; make-* functions
(defun make-diagonal-view (ma &key (diag-type :main) (pseudo-start 0))
  (make-instance 'smatrix-diagview :displaced-to ma :diag-type diag-type
                 :pseudo-start pseudo-start))
