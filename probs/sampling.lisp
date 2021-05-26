;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Random sampling (through indices)

(in-package #:cl-ml/probs)

;;; Simple random sampling (using reservoir sampling)
(defun simple-sampling (lst &optional k)
  (unless k
    (setf k (length lst)))
  (let ((sampl nil))
    (dotimes (i (length lst))
      (if (< i k)
          (push (nth i lst) sampl)
          (let ((ridx (randint 0 i)))
            (when (< ridx k)
              (setf (nth ridx sampl) (nth i lst))))))
    sampl))

(defun simple-indices (n &optional k)
  (unless k
    (setf k n))
  (let ((sampl nil))
    (dotimes (i n)
      (if (< i k)
          (push i sampl)
          (let ((ridx (randint 0 i)))
            (when (< ridx k)
              (setf (nth ridx sampl) i)))))
    sampl))

;;; Stratified random sampling
(defun stratified-sampling (lst &optional k)
  )

(defun stratified-indices (n &optional k)
  )

;;; Cluster random sampling
(defun cluster-sampling (lst &optional k)
  )

(defun cluster-indices (n &optional k)
  )

;;; Weighted random sampling
(defun weighted-sampling (lst weights &optional k)
  (unless k
    (setf k (length lst)))
  (let* ((len (length weights))
         (cdf (cumsum weights))
         (tot (car (last cdf)))
         (rnd (randuni 1.0 k))
         ;; result sampling list
         (spl nil))
    (unless (= tot 1.0)
      ;; in case total weight not equal to 1.0
      (setf cdf (mapcar (lambda (val) (/ val tot)) cdf)))
    (dotimes (i k)
      (dotimes (j len)
        (when (>= (nth j cdf) (nth i rnd))
          (when (<= (nth i rnd) (nth j cdf))
            (push (nth j lst) spl)
            (return)))))
    spl))

(defun weighted-indices (weights &optional k)
  (unless k
    (setf k (length weights)))
  (let* ((len (length weights))
         (cdf (cumsum weights))
         (tot (car (last cdf)))
         (rnd (randuni 1.0 k))
         (spl nil))
    (unless (= tot 1.0)
      (setf cdf (mapcar (lambda (val) (/ val tot)) cdf)))
    (dotimes (i k)
      (dotimes (j len)
        (when (>= (nth j cdf) (nth i rnd))
          (push j spl)
          (return))))
    spl))

;;; Alias method sampling
(defun alias-initialize (weights)
  "NOTICE: Requires: all(weights != 0)"
  (let* ((len (length weights))
         (sweight (sum weights))
         (probs-table (make-list len))
         (alias-table (make-list len))
         underfull overfull)
    (unless (float= sweight 1.0)
      ;; in case sum(weights) != 0
      (setf weights (mapcar (lambda (x) (/ x sweight)) weights)))
    (dotimes (i len)
      (setf (nth i probs-table) (* len (nth i weights)))
      (setf (nth i alias-table) i)
      (cond
        ((float< (nth i probs-table) 1.0) (push i underfull))
        ((float> (nth i probs-table) 1.0) (push i overfull))))
    (do ((uidx nil) (oidx nil))
        ((and (null underfull) (null overfull)))
      (setf uidx (pop underfull))
      (setf oidx (pop overfull))
      (setf (nth uidx alias-table) oidx)
      (decf (nth oidx probs-table) (- 1.0 (nth uidx probs-table)))
      (cond
        ((float< (nth oidx probs-table) 1.0) (push oidx underfull))
        ((float> (nth oidx probs-table) 1.0) (push oidx overfull))))
    (values probs-table alias-table)))

(defun alias-draw-index (probs-table alias-table)
  (let ((len (length probs-table))
        ridx prob)
    (setf ridx (randint len))
    (setf prob (random 1.0))
    (when (>= prob (nth ridx probs-table))
      (setf ridx (nth ridx alias-table)))
    ridx))

(defun alias-draw-element (lst probs-table alias-table)
  (let ((ridx (alias-draw-index probs-table alias-table)))
    (nth ridx lst)))

(defun alias-sampling (lst weights &optional k)
  (unless k
    (setf k (length lst)))
  (multiple-value-bind (probs-table alias-table) (alias-initialize weights)
    (let ((ridx nil)
          (spl nil))
      (dotimes (i k)
        (setf ridx (alias-draw-index probs-table alias-table))
        (push (nth ridx lst) spl))
      spl)))

(defun alias-indices (weights &optional k)
  (unless k
    (setf k (length weights)))
  (multiple-value-bind (probs-table alias-table) (alias-initialize weights)
    (let ((len (length weights))
          (ridx nil)
          (prob nil)
          (spl nil))
      (dotimes (i k)
        (setf ridx (randint len))
        (setf prob (random 1.0))
        (when (>= prob (nth ridx probs-table))
          (setf ridx (nth ridx alias-table)))
        (push ridx spl))
      spl)))

;;; Bootstrap sampling
(defun bootstrap-sampling (lst &optional k)
  "sampling with replacement"
  (let ((n (length lst))
        (ridx nil)
        (samples nil))
    (unless k
      (setf k (length lst)))
    (dotimes (i k)
      (setf ridx (random n))
      (push (nth ridx lst) samples))
    (nreverse samples)))

(defun bootstrap-indices (n &optional k)
  "(values sampled-indices unsampled-indices)"
  (unless k
    (setf k n))
  (let ((samples nil)
        (unsamples nil)
        (indices (make-indices n))
        (idx nil))
    (dotimes (i k)
      (setf idx (random n))
      (setf (nth idx indices) nil)
      (push idx samples))
    (dolist (ind indices)
      (when ind
        (push ind unsamples)))
    (values (nreverse samples) (nreverse unsamples))))

(defun pasting-sampling (lst &optional k)
  "sampling without replacement
NOTICE: will modify `lst', copy-list if necessary"
  (unless k
    (setf k (length lst)))
  (shuffle lst)
  (subseq lst 0 k))

(defun pasting-indices (n &optional k)
  (let ((indices (make-indices n)))
    (pasting indices k)))
