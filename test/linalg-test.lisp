;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

(defvar *matrix* (make-matrix 3 5 :initial-contents
                              '((1 2 3 1 5)
                                (6 0 1 3 0)
                                (7 1 2 5 1))))
(defvar *square* (make-square-matrix 3 :initial-contents
                                     '((2 3 1)
                                       (0 1 3)
                                       (1 2 5))))
;; *SQUARE* + I
(defvar *square+1* (make-square-matrix 3 :initial-contents
                                       '((3 3 1)
                                         (0 2 3)
                                         (1 2 6))))
(defvar *identity* (make-identity-matrix 3))
;; inverse of *SQUARE*
(defvar *square-inv* (make-square-matrix 3 :initial-contents
                                         '((-1/6  -13/6  4/3)
                                           (1/2    3/2    -1)
                                           (-1/6  -1/6   1/3))))

;; vector of *SMATRIX* first column
(defvar *vector* (make-vector 3 :initial-contents '(1 6 7)))
;; vector of one's
(defvar *vector-one* (make-vector 3 :initial-element 1))
(defvar *matrix-transp* (mt *matrix*))
;; transpose of transpose of *MATRIX*, which will be *MATRIX* itself
(defvar *matrix-self* (mt *matrix-transp*))
(defvar *matrix-view0* (make-matrix-view *matrix*))
;; element of *MATRIX-VIEW1* is same as *SQUARE*
(defvar *matrix-view1* (make-matrix-view *matrix* :col-view '(1 2 3)))
;; same as *MATRIX-VIEW1*
(defvar *matrix-view2* (make-matrix-view *matrix* :col-view '(1 . 4)))

(define-test smatrix-creation
  (assert-eq 3 (nrow *matrix*))
  (assert-eq 5 (ncol *matrix*))
  ;; square matrix
  (assert-eq 3 (nrow *square*))
  (assert-eq 3 (ncol *square*))
  ;; column vector
  (assert-eq 1 (ncol *vector*))
  ;; transpose matrix
  (assert-eq 5 (nrow *matrix-transp*))
  (assert-eq 3 (ncol *matrix-transp*))
  ;; tranpose of transposed matrix is the original matrix
  (assert-eq *matrix* *matrix-self*)
  ;; matrix view
  (assert-eq (nrow *matrix*) (nrow *matrix-view0*))
  (assert-eq 3 (nrow *matrix-view1*))
  (assert-eq 3 (ncol *matrix-view1*)))

(define-test smatrix-mref
  (assert-eq 6 (mref *matrix* 1 0))
  (assert-eq 1 (mref *identity* 1 1))
  (assert-eq 7 (vref *vector* 2))
  (assert-eq 6 (mref *matrix-transp* 0 1))
  (assert-eq 1 (mref *matrix-view0* 0 0))
  (assert-eq 2 (mref *matrix-view1* 0 0))
  (assert-true (matrix-eq *matrix-view1* *matrix-view2*)))

(define-test smatrix-setf
  (let ((orig (mref *matrix* 0 1))
        (tmp 100))
    (setf (mref *matrix* 0 1) tmp)
    (assert-eq tmp (mref *matrix* 0 1))
    ;; modify back through view
    (setf (mref *matrix-view1* 0 0) orig)
    (assert-eq orig (mref *matrix* 0 1))

    (setf orig (vref *vector* 2))
    (setf (vref *vector* 2) tmp)
    (assert-eq tmp (vref *vector* 2))
    (setf (vref *vector* 2) orig))
  ;; vector access from matrix
  (setf (mcv *matrix* 0) 1)
  (assert-true (matrix-eq *vector-one* (mcv *matrix* 0)))
  (setf (mcv *matrix* 0) *vector*))

(define-test smatrix-loop-return
  (let ((return-success nil)
        (return-counter 0)
        (go-success t))
    (do-matrix (i j *matrix*)
      (incf return-counter)
      (when (= i j 0)
        (setf return-success t)
        (return))
      (setf return-success nil))
    (assert-true return-success)
    (assert-eq 1 return-counter)
    (do-matrix (i j *matrix*)
      (when (>= (+ i j) 0)
        (go end-of-loop))
      (setf go-success nil)
      end-of-loop)
    (assert-true go-success)))

(define-test smatrix-properties
  (let ((square (make-square-matrix 3 :initial-contents '((1 2 3)
                                                          (4 5 6)
                                                          (7 8 0)))))
    (assert-eq 27 (mdet square))))

(define-test smatrix-operation
  (assert-true (matrix-eq *square+1* (m+ *square* *identity*)))
  (assert-true (matrix-eq *identity* (m* *square* *square-inv*)))
  (assert-true (matrix-eq *square-inv* (minv *square*)))
  ;; operation through view
  (assert-true (matrix-eq *square-inv* (minv *matrix-view1*)))
  (let ((A (make-matrix 3 3 :initial-contents '((1 -1 -1)
                                                (2 -1 -3)
                                                (-3 4  4))))
        (B (make-matrix 3 3 :initial-contents '((1 2 3)
                                                (2 2 1)
                                                (3 4 3))))
        (A+2B+3 (make-matrix 3 3 :initial-contents '((6 6 8)
                                                     (9 6 2)
                                                     (6 15 13))))
        (BtA (make-matrix 3 3 :initial-contents '((-4 9  5)
                                                  (-6 12 8)
                                                  (-4 8  6))))
        (Ainv (make-matrix 3 3 :initial-contents '((4   0    1)
                                                   (0.5 0.5  0.5)
                                                   (2.5 -0.5 0.5)))))
    (assert-true (matrix-eq A+2B+3 (m+ 3 A (m* 2 B))))
    (assert-true (matrix-eq BtA (m* (mt B) A)))
    (assert-true (matrix-eq Ainv (minv A)))))

(defparameter *svd-ma* (make-matrix 4 2 :initial-contents
                                    '((2 4) (1 3) (0 0) (0 0))))

;; bidiagonalize
;; - (m* (mt *U*) *U*) shoulde be I
(defparameter *ma-small* (make-matrix 3 2 :initial-contents
                                '((1 4) (9 16) (25 36))))
(defparameter *ma* (make-matrix 6 5 :initial-contents
                                '((1 2 3 4 5)
                                  (6 7 8 9 10)
                                  (11 12 13 14 15)
                                  (16 17 18 19 20)
                                  (21 22 23 24 25)
                                  (26 27 28 29 30))))
(defparameter *U* nil)
(defparameter *V* nil)
(defparameter *D* nil)
(defparameter *U2* nil)
(defparameter *V2* nil)
(defparameter *D2* nil)
(multiple-value-setq (*U* *V* *D*) (cl-ml/linalg::bidiagonalize (copy-matrix *ma*)))
(multiple-value-setq (*U2* *V2* *D2*) (cl-ml/linalg::bidiagonalize-forward (copy-matrix *ma*)))

;; hessenberg
;; - (m* (mt *Q*) *Q*) shoule be I
;; - (m* (mt *Q*) *A* *Q*) shoule be *H*
(defparameter *sq* (make-square-matrix 5 :initial-contents
                                           '((1 2 3 4 5)
                                             (6 7 8 9 10)
                                             (11 12 13 14 15)
                                             (16 17 18 19 20)
                                             (21 22 23 24 25))))
(defparameter *Q* nil)
(defparameter *H* nil)
(defparameter *Q2* nil)
(defparameter *H2* nil)
(multiple-value-setq (*Q* *H*) (cl-ml/linalg::hessenberg (copy-matrix *sq*)))
(multiple-value-setq (*Q2* *H2*) (cl-ml/linalg::hessenberg-forward (copy-matrix *sq*)))
