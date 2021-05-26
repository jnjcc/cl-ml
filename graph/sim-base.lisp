;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Graph base class

(in-package #:cl-ml/graph)

(defclass graph-base ()
  ((node-eq :initform #'equal :initarg :node-eq :reader node-eq)
   (node-le :initform #'<= :initarg :node-le)
   ;; edges using hash-table of hash-table's
   (edge-hash :initform nil :reader edge-hash)
   (edge-nums :initform 0)
   ;; nodes using hash-table, used to hold attribute of node's
   (node-hash :initform nil :reader node-hash)))

(defmethod initialize-instance :after ((graph graph-base) &rest args)
  (declare (ignore args))
  (with-slots (node-eq edge-hash node-hash) graph
    (setf edge-hash (make-hash-table :test node-eq))
    (setf node-hash (make-hash-table :test node-eq))))

(defmethod graph-type ((graph graph-base))
  (error "not implemented in base class"))

(defmethod node-count ((graph graph-base))
  (with-slots (node-hash) graph
    (hash-table-count node-hash)))

(defmethod edge-count ((graph graph-base))
  (with-slots (edge-nums) graph
    edge-nums))

(defmethod same-node ((graph graph-base) unode vnode)
  (with-slots (node-eq) graph
    (funcall node-eq unode vnode)))

(defmethod print-object ((graph graph-base) stream)
  (format stream "~A with ~A nodes and ~A edges" (type-of graph)
          (node-count graph) (edge-count graph)))

;; TODO: this macro cannot do (RETURN) right
(defmacro do-graph-nodes ((node value graph) &body body)
  `(loop with ,value = nil
      for ,node being the hash-keys of (node-hash ,graph)
      do (progn
           (setf ,value (gethash ,node (node-hash ,graph)))
           ,@body)))

;; TODO: this macro cannot do (RETURN) right
(defmacro do-graph-edges ((unode vnode weight graph) &body body)
  (let ((hash-sym (gensym "HASH-"))
        (neigh-sym (gensym "NEIGHBOR-")))
    `(loop with ,hash-sym = (edge-hash ,graph) with ,weight = nil
        for ,unode being the hash-keys of ,hash-sym
        for ,neigh-sym being the hash-values of ,hash-sym
        do (loop for ,vnode being the hash-keys of ,neigh-sym
              do (progn
                   (setf ,weight (gethash ,vnode ,neigh-sym))
                   ,@body)))))

(defmethod get-nodes ((graph graph-base) &key (valued t))
  (let ((nodes nil))
    (do-graph-nodes (node value graph)
      (if valued
          (push (list node value) nodes)
          (push node nodes)))
    nodes))

(defmethod get-edges ((graph graph-base) &key (weighted t))
  (let ((edges nil))
    (do-graph-edges (unode vnode weight graph)
      (if weighted
          (push (list unode vnode weight) edges)
          (push (list unode vnode) edges)))
    edges))

(defun %hash-contains (htable key)
  (nth-value 1 (gethash key htable)))

(defmethod has-node ((graph graph-base) node)
  (with-slots (node-hash) graph
    (%hash-contains node-hash node)))

(defmethod get-node-value ((graph graph-base) node)
  (with-slots (node-hash) graph
    (gethash node node-hash)))

(defmethod has-edge ((graph graph-base) unode vnode)
  (with-slots (edge-hash) graph
    (multiple-value-bind (neighbors exists-p) (gethash unode edge-hash)
      (when exists-p
        (%hash-contains neighbors vnode)))))

(defmethod get-edge-weight ((graph graph-base) unode vnode)
  (with-slots (edge-hash) graph
    (gethash vnode (gethash unode edge-hash))))

(defmethod gref ((graph graph-base) unode vnode)
  "counterpart of `mref'"
  (with-slots (edge-hash) graph
    (gethash vnode (gethash unode edge-hash))))

(defmethod has-neighbors ((graph graph-base) node)
  (with-slots (edge-hash) graph
    (multiple-value-bind (neighbors exists-p) (gethash node edge-hash)
      (when exists-p
        (> (hash-table-count neighbors) 0)))))

(defmethod get-neighbors ((graph graph-base) node &key (weighted t))
  "list of list: ((neighbor weight) ...) if `weighted'"
  (with-slots (edge-hash) graph
    (let ((nbr-lst nil))
      (labels ((collect (nbr weight)
                 (if weighted
                     (push (list nbr weight) nbr-lst)
                     (push nbr nbr-lst))))
        (multiple-value-bind (neighbors exists-p) (gethash node edge-hash)
          (when exists-p
            (maphash #'collect neighbors))))
      nbr-lst)))

(defmethod sorted-neighbors ((graph graph-base) node &key (weighted t))
  "sorted list of list: ((neighbor weight) ...) if `weighted'"
  (with-slots (node-le) graph
    (let ((neighbors (get-neighbors graph node :weighted weighted)))
      (if weighted
          (sort neighbors node-le :key #'car)
          (sort neighbors node-le)))))

(defmethod add-node-safe ((graph graph-base) node value)
  "Requires: `node' not in `graph' yet"
  (error "not implemented in base class"))

;;; TODO: should we update value if node exists?
(defmethod add-node ((graph graph-base) node &optional (value t))
  "add `node' to `graph', if already exists, do nothing"
  (with-slots (node-eq edge-hash node-hash) graph
    (unless (has-node graph node)
      (add-node-safe graph node value))))

(defmethod add-edge-safe ((graph graph-base) unode vnode weight)
  "Requires: `edge' not in `graph' yet"
  (error "not implemented in base class"))

;;; TODO: should we udpate weight if edge exists?
(defmethod add-edge ((graph graph-base) unode vnode &optional (weight 1))
  "add `edge' to `graph', if already exists, do nothing"
  (unless (has-node graph unode)
    (add-node graph unode))
  (unless (has-node graph vnode)
    (add-node graph vnode))
  (unless (has-edge graph unode vnode)
    (add-edge-safe graph unode vnode weight)))

(defmethod remove-edge-safe ((graph graph-base) unode vnode)
  "Requires: `edge' already in `graph'"
  (error "not implemented in base class"))

(defmethod remove-edge ((graph graph-base) unode vnode)
  (when (has-edge graph unode vnode)
    (remove-edge-safe graph unode vnode)))

(defmethod remove-node ((graph graph-base) node)
  (with-slots (edge-hash node-hash) graph
    (when (has-node graph node)
      (remhash node node-hash)
      (let ((neighbors (gethash node edge-hash)))
        (maphash (lambda (vnode weight)
                   (declare (ignore weight))
                   (remove-edge-safe graph vnode node))
                 neighbors)))))
