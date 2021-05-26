;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Common data structure and algorithms for Machine Learning and NLP

(in-package #:cl-user)

(defpackage #:cl-ml/algo
  (:nicknames #:ml/algo)
  (:use #:cl)
  (:export #:repeat-string #:join-strings
           #:split-string #:split-string-if
           #:replace-string #:replace-string-if
           ;; binary tree
           #:make-binary-tree #:is-tree-leaf
           #:get-tree-data #:get-left-tree #:get-left-data #:get-right-tree #:get-right-data
           #:set-tree-data #:set-left-tree #:set-left-data #:set-right-tree #:set-right-data
           #:inorder-visit #:preorder-visit #:postorder-visit
           ;; stack
           #:make-stack
           #:stack-empty #:stack-top
           #:stack-push #:stack-pop
           ;; FIFO queue
           #:make-queue
           #:queue-empty #:queue-front
           #:queue-enque #:queue-deque
           ;; Huffman Tree
           #:huffman-from-list
           #:preorder-huffman #:inorder-huffman
           ))
