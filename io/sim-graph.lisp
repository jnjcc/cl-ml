;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple io: read graph from csv file

(in-package #:cl-ml/io)

(defun read-graph (fpath &key (type :undirected) (start 0) end (delimiter #\,))
  (let ((graph (make-graph type)))
    (do-example-from-file (example indx fpath :start start :end end)
      (let* ((varlst (split-string example delimiter))
             (unode (read-from-string (car varlst)))
             (vnode (read-from-string (second varlst)))
             (weight (third varlst)))
        (if (>= (length varlst) 3)
            (add-edge graph unode vnode (read-from-string weight))
            (add-edge graph unode vnode))))
    graph))
