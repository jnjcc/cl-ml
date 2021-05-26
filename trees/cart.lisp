;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Classification and Regression Tree

(in-package #:cl-ml)

(defmethod print-verbose ((dtree-est dtree-estimator) &optional (stream t) feaname labenc)
  (let ((feafun #'identity)
        (labelfun #'identity))
    (when feaname
      (setf feafun (lambda (fidx) (nth fidx feaname))))
    (when labenc
      (setf labelfun (lambda (value) (get-encoder-label labenc value))))
    (with-slots (droot) dtree-est
      (print-dtree-depth droot stream 0 feafun labelfun))))

(defmethod predict-leaf ((dtree-est dtree-estimator) X i)
  (with-slots (droot) dtree-est
    (do ((tree droot)
         (data nil))
        (data tree)
      (multiple-value-setq (tree data) (get-dtree-child tree X i)))))

(defmethod leaf-regions ((dtree-est dtree-estimator) X)
  (let ((cur-leaf nil)
        (leaf-bins nil))
    (do-matrix-row (i X)
      (setf cur-leaf (predict-leaf dtree-est X i))
      (binpush leaf-bins cur-leaf i))
    leaf-bins))

(defmethod %predict-one ((dtree-est dtree-estimator) X i)
  (with-slots (droot) dtree-est
    (do ((tree droot)
         (data nil))
        (data data)
      (multiple-value-setq (tree data) (get-dtree-child tree X i)))))

(defmethod predict-one ((dtree-est dtree-estimator) X i)
  (let ((node-data (%predict-one dtree-est X i)))
    (with-slots (value binfreqs nsamples) node-data
      (values value (density binfreqs nsamples)))))

(defmethod predict ((dtree-est dtree-estimator) X)
  (let ((pred (make-vector (nrow X) :initial-element 0)))
    (do-matrix-row (i X)
      (setf (vref pred i) (predict-one dtree-est X i)))
    pred))

(defmethod feature-importance ((dtree-est dtree-estimator))
  (with-slots (droot nfeat vimps) dtree-est
    (unless vimps
      (setf vimps (make-vector nfeat :initial-element 0.0)))
    (labels ((update-imp (node)
               (let* ((data (get-tree-data node))
                      (fidx (feature data)))
                 (when fidx
                   (incf (vref vimps fidx)
                         (- (* (nsamples data) (impurity data))
                            (* (nsamples (get-left-data node))
                               (impurity (get-right-data node)))
                            (* (nsamples (get-right-data node))
                               (impurity (get-right-data node)))))
                   (update-imp (get-left-tree node))
                   (update-imp (get-right-tree node))))))
      (update-imp droot))
    vimps))

;;; classifier
(deftype impurity-type ()
  '(member :gini :entropy :gain-ratio))

(defclass dtree-classifier (dtree-estimator)
  ((criterion :initform :gini :initarg :criterion :type impurity-type)
   (max-features :initform :all)
   (nclasses :initform 0)))

(defmethod fit ((dtree-clf dtree-classifier) X &optional y)
  (with-slots (droot depth nfeat nclasses) dtree-clf
    (setf nfeat (ncol X))
    (setf nclasses (length (mbincount y)))
    (let ((dframe (make-data-frame X y))
          (splitter (make-root-splitter dtree-clf X)))
      (setf depth (split-node splitter dframe dtree-clf)))))

(defmethod predict-proba ((dtree-clf dtree-classifier) X)
  (with-slots (nclasses) dtree-clf
    (let ((pred (make-matrix (nrow X) nclasses :initial-element 0.0)))
      (do-matrix-row (i X)
        (multiple-value-bind (fval binprobs) (predict-one dtree-clf X i)
          (declare (ignore fval))
          (dolist (pair binprobs)
            (setf (mref pred i (car pair)) (cdr pair)))))
      pred)))

;;; regressor
(deftype dtree-reg-type ()
  '(member :mse))

(defclass dtree-regressor (dtree-estimator)
  ((criterion :initform :mse :type dtree-reg-type)
   (max-features :initform :all)))

(defmethod fit ((dtree-reg dtree-regressor) X &optional y)
  (with-slots (droot depth nfeat) dtree-reg
    (setf nfeat (ncol X))
    (let ((dframe (make-data-frame X y))
          (splitter (make-root-splitter dtree-reg X)))
      (setf depth (split-node splitter dframe dtree-reg)))))
