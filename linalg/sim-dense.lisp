;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: smatrix-dense

(in-package #:cl-ml/linalg)

(defclass smatrix-dense (smatrix)
  ((data :initform nil :type 'array)))

;;; rows and columns
(defmethod nrow ((ma smatrix-dense))
  (with-slots (data) ma
    (if data
        (array-dimension data 0)
        0)))

(defmethod ncol ((ma smatrix-dense))
  (with-slots (data) ma
    (if data
        (array-dimension data 1)
        0)))

;;; element access
(defmethod mref ((ma smatrix-dense) i j)
  (with-slots (data) ma
    (aref data i j)))

(defmethod (setf mref) (val (ma smatrix-dense) i j)
  (with-slots (data) ma
    (setf (aref data i j) val)))

;;; make-* functions
(defun make-matrix (nrow ncol &key (initial-element 0) initial-contents)
  (let ((ma (make-instance 'smatrix-dense)))
    (with-slots (data) ma
      (if initial-contents
          (setf data (make-array `(,nrow ,ncol) :initial-contents initial-contents))
          (setf data (make-array `(,nrow ,ncol) :initial-element initial-element))))
    ma))

(defun make-square-matrix (n &key (initial-element 0) initial-contents)
  (make-matrix n n :initial-element initial-element
               :initial-contents initial-contents))

(defun make-identity-matrix (n)
  (let ((idma (make-square-matrix n :initial-element 0)))
    (with-slots (data) idma
      (dotimes (i n idma)
        (setf (aref data i i) 1)))))

(defun make-rand-matrix (nrow ncol &optional (limit 1.0) (state *random-state*))
  (let ((ma (make-instance 'smatrix-dense)))
    (with-slots (data) ma
      (setf data (make-array (list nrow ncol)
                             :initial-contents (randuni limit (list nrow ncol) state))))
    ma))

;; accept a list of values
(defun make-vector (nrow &key (initial-element 0) initial-contents)
  "column vector as default"
  (when initial-contents
    ;; list of list
    (setf initial-contents (mapcar #'list initial-contents)))
  (make-matrix nrow 1 :initial-element initial-element
               :initial-contents initial-contents))

(defun make-row-vector (ncol &key (initial-element 0) initial-contents)
  (when initial-contents
    (setf initial-contents (list initial-contents)))
  (make-matrix 1 ncol :initial-element initial-element
               :initial-contents initial-contents))

(defun make-rand-vector (nrow &optional (limit 1.0) (state *random-state*))
  "column vector as default"
  (make-vector nrow :initial-contents (randuni limit nrow state)))

(defun make-rand-row-vector (ncol &optional (limit 1.0) (state *random-state*))
  (make-row-vector ncol :initial-contents (randuni limit ncol state)))
