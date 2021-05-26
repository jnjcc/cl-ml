;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple Graph

(in-package #:cl-user)

(defpackage #:cl-ml/graph
  (:nicknames #:ml/graph)
  (:use #:cl #:cl-ml/probs)
  (:export #:ungraph #:digraph #:graph-type
           #:same-node
           #:make-graph #:copy-graph
           #:node-count #:edge-count
           #:do-graph-nodes #:do-graph-edges
           #:get-nodes #:get-edges
           #:has-node #:get-node-value #:has-edge #:get-edge-weight #:gref
           #:has-neighbors #:get-neighbors #:sorted-neighbors
           #:add-node #:add-edge
           #:remove-node #:remove-edge
           ))
