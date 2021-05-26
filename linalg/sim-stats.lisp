;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Statistics stuff for smatrix

(in-package #:cl-ml/linalg)

(defmethod mhead ((ma smatrix) &optional (n 6))
  (if (< n 0)
      (make-matrix-view ma :row-view (cons (+ (nrow ma) n) :end))
      (make-matrix-view ma :row-view (cons 0 n))))

(defmethod msum ((ma smatrix) &key (axis 0) (key #'identity) (initial-value 0))
  (mreduce ma #'+ :initial-value initial-value :key key :axis axis))

(defmethod mmean ((ma smatrix) &key (axis 0) (key #'identity))
  (let ((sum (msum ma :axis axis :key key :initial-value 0)))
    (cond
      ((null axis) (setf sum (/ sum (* (nrow ma) (ncol ma)))))
      ((= axis 1) (m/= sum (ncol ma)))
      ((= axis 0) (m/= sum (nrow ma))))
    sum))

(defun %get-quant-value (va indices quat)
  "get quantile of a single vector, and a single quantile"
  (setf indices (sort-indices indices va))
  (multiple-value-bind (i ir j jr) (quantile-positions (length indices) quat)
    (+ (* ir (iavref va indices i)) (* jr (iavref va indices j)))))

(defun %fill-quant-vector (va indices quatlst quatvec)
  "fill quantile for vectors of a matrix"
  (setf indices (sort-indices indices va))
  (let ((idx 0)
        (len (length indices)))
    (dolist (quat quatlst)
      (multiple-value-bind (i ir j jr) (quantile-positions len quat)
        (setf (vref quatvec idx) (+ (* ir (iavref va indices i))
                                    (* jr (iavref va indices j)))))
      (incf idx))))

(defmethod mquantile ((ma smatrix) quat &key (axis 0))
  (let ((qumat nil))
    (cond
      ((null axis)
       (progn
         (do-matrix (i j ma)
           (push (mref ma i j) qumat))
         (setf qumat (sort qumat #'<))
         (setf qumat (quantile qumat quat))
         (when (consp qumat)
           (setf qumat (make-vector (length qumat) :initial-contents qumat)))
         qumat))
      ;; on all elements of the same column
      ((= axis 0)
       (let ((indices (make-indices (nrow ma))))
         (cond
           ((consp quat) (setf qumat (make-matrix (length quat) (ncol ma))))
           ((> (ncol ma) 1) (setf qumat (make-matrix 1 (ncol ma))
                                  quat (list quat))))
         (do-matrix-col (j ma)
           (if qumat
               (%fill-quant-vector (mcv ma j) indices quat (mcv qumat j))
               (setf qumat (%get-quant-value (mcv ma j) indices quat))))
         qumat))
      ;; on all elements of the sam row
      ((= axis 1)
       (let ((indices (make-indices (ncol ma))))
         (cond
           ((consp quat) (setf qumat (make-matrix (nrow ma) (length quat))))
           ((> (nrow ma) 1) (setf qumat (make-matrix (nrow ma) 1)
                                  quat (list quat))))
         (do-matrix-row (i ma)
           (if qumat
               (%fill-quant-vector (mrv ma i) indices quat (mrv qumat i))
               (setf qumat (%get-quant-value (mrv ma i) indices quat)))))))
    qumat))


(defmethod mmax ((ma smatrix) &key (axis 0) (key #'identity))
  (labels ((nilmax (a b)
             (if (null a)
                 b
                 (max a b))))
    (mreduce ma #'nilmax :axis axis :key key :initial-value nil)))

(defmethod mmin ((ma smatrix) &key (axis 0) (key #'identity))
  (labels ((nilmin (a b)
             (if (null a)
                 b
                 (min a b))))
    (mreduce ma #'nilmin :axis axis :key key :initial-value nil)))

(defmethod margfun ((ma smatrix) cmp &optional (axis 0) (key #'identity))
  (let ((argvals nil))
    (cond
      ((null axis) (let ((mval nil))
                     (do-matrix (i j ma)
                       (when (or (null mval) (funcall cmp (funcall key (mref ma i j)) mval))
                         (setf mval (funcall (mref ma i j)))
                         (setf argvals (cons i j))))))
      ;; on all elements of the same column
      ((= axis 0) (let ((mvals (make-row-vector (ncol ma) :initial-element nil)))
                    (setf argvals (make-row-vector (ncol ma) :initial-element nil))
                    (do-matrix (i j ma)
                      (when (or (null (vref mvals j)) (funcall cmp (funcall key (mref ma i j))
                                                               (vref mvals j)))
                        (setf (vref mvals j) (funcall key (mref ma i j)))
                        (setf (vref argvals j) i)))))
      ;; on all elements of the same row
      ((= axis 1) (let ((mvals (make-vector (nrow ma) :initial-element nil)))
                    (setf argvals (make-vector (nrow ma) :initial-element nil))
                    (do-matrix (i j ma)
                      (when (or (null (vref mvals i)) (funcall cmp (funcall key (mref ma i j))
                                                               (vref mvals i)))
                        (setf (vref mvals i) (funcall key (mref ma i j)))
                        (setf (vref argvals i) j))))))
    argvals))

(defmethod margmax ((ma smatrix) &key (axis 0) (key #'identity))
  (margfun ma #'> axis key))

(defmethod margmin ((ma smatrix) &key (axis 0) (key #'identity))
  (margfun ma #'< axis key))

(defmethod mbincount ((ma smatrix))
  (let ((binlist nil))
    (do-matrix (i j ma)
      (binincf binlist (mref ma i j)))
    binlist))
