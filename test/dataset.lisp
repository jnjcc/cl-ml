;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

(defvar *dataset-path* (merge-pathnames "test/dataset/"
                                        (asdf:system-source-directory :cl-ml-test)))

;;; iris dataset
(defvar *iris-file-path* (merge-pathnames "iris/iris.data" *dataset-path*))
(defvar *iris-train* nil)
(defvar *iris-label* nil)
(multiple-value-setq (*iris-train* *iris-label*)
  (read-matrix *iris-file-path* :labclass :string))
(defvar *iris-labenc* (make-instance 'label-encoder))
(defvar *iris-class* (copy-matrix *iris-label*))
(fit-transform *iris-labenc* *iris-class*)

;;; california housing dataset
(defvar *cal-file-path* (merge-pathnames "california/cal_housing.data" *dataset-path*))
(defvar *cal-train* nil)
(defvar *cal-label* nil)
(multiple-value-setq (*cal-train* *cal-label*) (read-matrix *cal-file-path*))
(m/= *cal-label* 100000)

;;; blobs from make_blobs()
(defvar *kmeans-blob* (merge-pathnames "sklearn/kmeans.blob" *dataset-path*))
(defvar *kmeans-train* nil)
(defvar *kmeans-label* nil)
(multiple-value-setq (*kmeans-train* *kmeans-label*) (read-matrix *kmeans-blob*))

(defvar *gmm-blob* (merge-pathnames "sklearn/gmm.blob" *dataset-path*))
(defvar *gmm-train* nil)
(defvar *gmm-label* nil)
(multiple-value-setq (*gmm-train* *gmm-label*) (read-matrix *gmm-blob*))

;;; dataset preprocessing
(defun load-iris-label-binary (&optional (neg -1) (pos +1))
  (let ((label (copy-matrix *iris-label*)))
    (do-vector (i label)
      (if (string= (vref label i) "Iris-virginica")
          (setf (vref label i) pos)
          (setf (vref label i) neg)))
    label))

;;; karate.edgelist from `node2vec'
(defvar *karate-path* (merge-pathnames "karate.edgelist" *dataset-path*))
(defvar *karate-graph* (read-graph *karate-path* :delimiter #\Space))
