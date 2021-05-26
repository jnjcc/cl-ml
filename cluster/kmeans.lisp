;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Unsupervised Learning > clustering > k-Means clustering

(in-package #:cl-ml)

(deftype kmeans-init-type ()
  '(member :kmeans++ :random))

(defclass kmeans-cluster (estimator)
  ((nclusters :initform 5 :initarg :nclusters)
   (clusters :initform nil :documentation "smatrix of centroids")
   (xlabels :initform nil :documentation "labels of each point")
   (init :initform :kmeans++ :initarg :init :type kmeans-init-type
         :documentation "method for cluster centers initialization")
   ;;; TODO: choose best
   (ninit :initform 1 :initarg :ninit :documentation "number of times the k-means
algorithm will be run with different centroid")
   (epochs :initform 300 :initarg :epochs :documentation "max number of iterations
of the k-means algorithm for a single run")
   ;;; TODO: tolerance
   (tolerance :initform 1.0e-3 :initarg :tolerance)))

(defun %%min-distance (X i chosens)
  "min distance between `i'-th sample and `chosens' centroids"
  (let ((cur (mrv X i))
        (tdis nil) ;; temp distance
        (mdis nil)
        (midx nil))
    (do-ia-access (indx chosens)
      (setf tdis (euclidean-distance cur (mrv X indx)))
      (when (or (null mdis) (> mdis tdis))
        (setf mdis tdis)
        (setf midx indx)))
    (values mdis midx)))

(defun %%choose-next-index (X chosens lefts)
  (if (null chosens)
      (randint 0 (nrow X))
      (let ((probs nil)
            (curdis nil))
        (do-ia-access (l lefts)
          (setf curdis (%%min-distance X l chosens))
          (setf curdis (* curdis curdis))
          (push curdis probs))
        (car (weighted-sampling lefts (nreverse probs) 1)))))

(defun %kmeans++-centroids-indices (X k)
  (let (;; chosen indices, left indices
        (chosens nil)
        (lefts (make-indices (nrow X)))
        ;; current chosen index
        (cidx nil))
    (dotimes (i k)
      (setf cidx (%%choose-next-index X chosens lefts))
      (push cidx chosens)
      (setf lefts (nset-difference lefts (list cidx))))
    chosens))

(defmethod init-centroids ((kmeans kmeans-cluster) X)
  (with-slots (init nclusters clusters) kmeans
    (let ((rindices nil))
      (ecase init
        (:kmeans++ (setf rindices (%kmeans++-centroids-indices X nclusters)))
        (:random (setf rindices (simple-indices (nrow X) nclusters))))
      (cond
        ((or (null clusters)
             (or (/= (nrow clusters) nclusters) (/= (ncol clusters) (ncol X))))
         (setf clusters (copy-matrix (make-matrix-view X :row-view rindices))))
        (t (do-indices (i indx rindices)
             (setf (mrv clusters i) (mrv X indx))))))))

(defmethod cluster-one-sample ((kmeans kmeans-cluster) X i)
  (with-slots (clusters) kmeans
    (let ((cur (mrv X i))
          (mdist nil) ;; minimum distance
          (mclst nil) ;; minimum cluster
          (tdist nil))
      (do-matrix-row (clst clusters)
        (setf tdist (euclidean-distance cur (mrv clusters clst)))
        (when (or (null mdist) (> mdist tdist))
          (setf mdist tdist)
          (setf mclst clst)))
      (values mclst mdist))))

(defmethod cluster-samples ((kmeans kmeans-cluster) X)
  (let ((clst-bins nil))
    (do-matrix-row (i X)
      (multiple-value-bind (mclst) (cluster-one-sample kmeans X i)
        (binpush clst-bins mclst i)))
    clst-bins))

(defmethod update-centroids ((kmeans kmeans-cluster) X bins)
  (with-slots (clusters nclusters) kmeans
    (do-bins (clst indices bins)
      (let ((avg (mmean (make-matrix-view X :row-view indices))))
        (setf (mrv clusters clst) avg)))))

(defmethod inertia-score ((kmeans kmeans-cluster) X bins)
  (let ((inertia 0.0))
    (do-matrix-row (i X)
      (multiple-value-bind (clst dist) (cluster-one-sample kmeans X i)
        (declare (ignore clst))
        (incf inertia (* dist dist))))
    inertia))

(defmethod fit ((kmeans kmeans-cluster) X &optional y)
  (declare (ignore y))
  (with-slots (nclusters clusters xlabels init ninit epochs tolerance) kmeans
    (when (< (nrow X) nclusters)
      (error "sample number less than clusters"))
    (setf xlabels (make-vector (nrow X) :initial-element nil))
    (dotimes (ini ninit)
      (init-centroids kmeans X)
      (dotimes (ep epochs)
        (let ((clst-bins (cluster-samples kmeans X)))
          (update-centroids kmeans X clst-bins))))))

(defmethod predict-one ((kmeans kmeans-cluster) X i)
  (cluster-one-sample kmeans X i))

(defmethod predict ((kmeans kmeans-cluster) X)
  (let ((pred (make-vector (nrow X) :initial-element nil)))
    (do-matrix-row (i X)
      (setf (vref pred i) (cluster-one-sample kmeans X i)))
    pred))

(defmethod %fill-one-reduction ((kmeans kmeans-cluster) X i R j)
  "fill i-th dimension reduction vector into j-th row of R"
  (with-slots (clusters) kmeans
    (let ((cur (mrv X i)))
      (do-matrix-row (clst clusters)
        (setf (mref R j clst) (euclidean-distance cur (mrv clusters clst)))))))

(defmethod transform-one ((kmeans kmeans-cluster) X i)
  (with-slots (nclusters) kmeans
    (let ((reduced (make-row-vector nclusters :initial-element nil)))
      (%fill-one-reduction kmeans X i reduced 0)
      reduced)))

(defmethod transform ((kmeans kmeans-cluster) X)
  "NOTICE: unlike `transformer' class, this method does not modify `X'"
  (with-slots (nclusters) kmeans
    (let ((reduced (make-matrix (nrow X) nclusters :initial-element nil)))
      (do-matrix-row (i reduced)
        (%fill-one-reduction kmeans X i reduced i))
      reduced)))

(defmethod fit-transform ((kmeans kmeans-cluster) X)
  )
