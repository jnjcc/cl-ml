;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Label Encoder: encode label to index

(in-package #:cl-ml)

;;; TODO: association list might also do the trick?
(defclass label-encoder (transformer)
  ((label-hash :initform nil :reader label-hash)
   (inver-hash :initform nil :reader inver-hash)
   (label-nums :initform 0 :reader label-nums)))

(defmethod print-object ((lab label-encoder) stream)
  (with-slots (label-hash label-nums) lab
    (if (<= label-nums 0)
        (format stream "~A labels" label-nums)
        (labels ((print-entry (key value)
                   (format stream "~A: ~A, " key value)))
          (format stream "~A labels:~%  {" label-nums)
          (maphash #'print-entry label-hash)
          (format stream "}~%")))))

(defmethod %init-parameters ((lab label-encoder))
  (with-slots (label-hash inver-hash label-nums) lab
    (setf label-hash (make-hash-table :test 'equal)
          inver-hash (make-hash-table :test 'equal)
          label-nums 0)))

;; x is a vector
(defmethod fit ((lab label-encoder) x &optional y)
  (declare (type smatrix x) (ignore y))
  (%init-parameters lab)
  (with-slots (label-hash inver-hash label-nums) lab
    (do-vector (i x)
      (multiple-value-bind (val exists-p) (gethash (vref x i) label-hash)
        (declare (ignore val))
        (unless exists-p
          (setf (gethash (vref x i) label-hash) label-nums)
          (setf (gethash label-nums inver-hash) (vref x i))
          (incf label-nums))))))

(defmethod transform ((lab label-encoder) x)
  (do-vector (i x)
    (setf (vref x i) (gethash (vref x i) (label-hash lab))))
  x)

(defmethod fit-transform ((lab label-encoder) x)
  (%init-parameters lab)
  (with-slots (label-hash inver-hash label-nums) lab
    (do-vector (i x)
      (multiple-value-bind (val exists-p) (gethash (vref x i) label-hash)
        (declare (ignore val))
        (unless exists-p
          (setf (gethash (vref x i) label-hash) label-nums)
          (setf (gethash label-nums inver-hash) (vref x i))
          (incf label-nums)))
      (setf (vref x i) (gethash (vref x i) label-hash))))
  x)

(defmethod inv-transform ((lab label-encoder) x)
  (do-vector (i x)
    (setf (vref x i) (gethash (vref x i) (inver-hash lab))))
  x)

(defmethod get-encoder-index ((lab label-encoder) label)
  "label -> index"
  (with-slots (label-hash) lab
    (gethash label label-hash)))

(defmethod get-encoder-label ((lab label-encoder) index)
  "index -> label"
  (with-slots (inver-hash) lab
    (gethash index inver-hash)))
