;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

(define-test undirected-graph-test
  (let ((copy (copy-graph *karate-graph*)))
    (assert-eq 34 (node-count *karate-graph*))
    (assert-eq 76 (edge-count *karate-graph*))
    (assert-true (has-node *karate-graph* 1))
    (assert-true (has-edge *karate-graph* 1 2))
    (assert-true (has-edge *karate-graph* 2 1))
    (assert-eq 16 (length (get-neighbors *karate-graph* 1)))

    (remove-node copy 1)
    (assert-eq 33 (node-count copy))
    (assert-eq 60 (edge-count copy))
    (assert-true (not (has-node copy 1)))
    (add-node copy 100)
    (add-node copy 200)
    (assert-eq 35 (node-count copy))
    (assert-true (not (has-edge copy 100 200)))
    (add-edge copy 100 200)
    (assert-eq 61 (edge-count copy))

    (assert-eq 16 (length (get-neighbors *karate-graph* 1)))
    (assert-eq 76 (edge-count *karate-graph*))))
