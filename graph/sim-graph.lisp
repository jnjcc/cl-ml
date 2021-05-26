;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Undirected Graph and Directed Graph

(in-package #:cl-ml/graph)

;;; Undirected Graph
(defclass ungraph (graph-base)
  ())

(defmethod graph-type ((graph ungraph))
  :undirected)

(defmethod add-node-safe ((graph ungraph) node value)
  (with-slots (node-eq edge-hash node-hash) graph
    (setf (gethash node node-hash) value)
    (setf (gethash node edge-hash) (make-hash-table :test node-eq))))

(defmethod add-edge-safe ((graph ungraph) unode vnode weight)
  (with-slots (edge-nums edge-hash) graph
    (incf edge-nums)
    (setf (gethash vnode (gethash unode edge-hash)) weight)
    (setf (gethash unode (gethash vnode edge-hash)) weight)))

(defmethod remove-edge-safe ((graph ungraph) unode vnode)
  (with-slots (edge-nums edge-hash) graph
    (decf edge-nums)
    (remhash vnode (gethash unode edge-hash))
    (remhash unode (gethash vnode edge-hash))))

;;; Directed Graph
(defclass digraph (graph-base)
  ;; previous node of directed edge
  ((prev-hash :initform nil :reader prev-hash)))

(defmethod graph-type ((graph digraph))
  :directed)

(defmethod initialize-instance :after ((graph digraph) &rest args)
  (declare (ignore args))
  (with-slots (node-eq prev-hash) graph
    (setf prev-hash (make-hash-table :test node-eq))))

(defmethod has-in-neighbors ((graph digraph) node)
  "(pnode -> node) edges"
  (with-slots (prev-hash) graph
    (multiple-value-bind (in-neighbors exists-p) (gethash node prev-hash)
      (when exists-p
        (> (hash-table-count in-neighbors) 0)))))

(defmethod get-in-neighbors ((graph digraph) node &key (weighted nil))
  "for (pnode -> node) edges, return list of pnode's"
  (with-slots (prev-hash) graph
    (let ((in-lst nil))
      (labels ((collect (prev weight)
                 (if weighted
                     (push (list prev weight) nbr-lst)
                     (push prev in-lst))))
        (multiple-value-bind (in-neighbors exists-p) (gethash node prev-hash)
          (when exists-p
            (maphash #'collect in-neighbors))))
      in-lst)))

(defmethod add-node-safe ((graph digraph) node value)
  (with-slots (node-eq edge-hash node-hash prev-hash) graph
    (setf (gethash node node-hash) value)
    (setf (gethash node edge-hash) (make-hash-table :test node-eq))
    (setf (gethash node prev-hash) (make-hash-table :test node-eq))))

(defmethod add-edge-safe ((graph digraph) unode vnode weight)
  (with-slots (edge-nums edge-hash prev-hash) graph
    (incf edge-nums)
    (setf (gethash vnode (gethash unode edge-hash)) weight)
    (setf (gethash unode (gethash vnode prev-hash)) weight)))

(defmethod remove-edge-safe ((graph digraph) unode vnode)
  (with-slots (edge-nums edge-hash prev-hash) graph
    (decf edge-nums)
    (remhash vnode (gethash unode edge-hash))
    (remhash unode (gethash vnode prev-hash))))

;;; make-* functions
(defun make-graph (&optional (type :undirected) (node-eq #'equal))
  (ecase type
    (:undirected (make-instance 'ungraph :node-eq node-eq))
    (:directed (make-instance 'digraph :node-eq node-eq))))

(defun make-ungraph (&optional (node-eq #'equal))
  (make-instance 'ungraph :node-eq node-eq))

(defun make-digraph (&optional (node-eq #'equal))
  (make-instance 'digraph :ndoe-eq node-eq))

(defun copy-graph (graph)
  (let ((copy (make-instance (type-of graph))))
    (do-graph-edges (unode vnode weight graph)
      (add-edge copy unode vnode weight))
    copy))
