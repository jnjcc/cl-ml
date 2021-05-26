;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Common Data Structure: Binary Tree

(in-package #:cl-ml/algo)

;;; Binary tree with the list form (data ltree rtree)
;;;   so that (copy-tree) (tree-equal) works fine
(defun make-binary-tree (data &optional (ltree nil) (rtree nil))
  (list data ltree rtree))

(defun get-tree-data (root)
  (when root
    (car root)))

(defun get-left-tree (root)
  (when root
    (cadr root)))

(defun get-left-data (root)
  (get-tree-data (get-left-tree root)))

(defun get-right-tree (root)
  (when root
    (caddr root)))

(defun get-right-data (root)
  (get-tree-data (get-right-tree root)))

(defun is-tree-leaf (root)
  (and (null (get-left-tree root)) (null (get-right-tree root))))

(defun set-tree-data (root data)
  (when root
    (setf (car root) data)))

(defun set-left-tree (root ltree)
  (when root
    (setf (cadr root) ltree)))

(defun set-right-tree (root rtree)
  (when root
    (setf (caddr root) rtree)))

(defun set-left-data (root data)
  (if (get-left-tree root)
      (set-tree-data (get-left-tree root) data)
      (set-left-tree root (make-binary-tree data))))

(defun set-right-data (root data)
  (if (get-right-tree root)
      (set-tree-data (get-right-tree root) data)
      (set-right-tree root (make-binary-tree data))))

(defun inorder-visit (root &optional (visit #'princ))
  (when root
    (inorder-visit (get-left-tree root) visit)
    (funcall visit (get-tree-data root))
    (inorder-visit (get-right-tree root) visit)))

(defun preorder-visit (root &optional (visit #'princ))
  (when root
    (funcall visit (get-tree-data root))
    (preorder-visit (get-left-tree root) visit)
    (preorder-visit (get-right-tree root) visit)))

(defun postorder-visit (root &optional (visit #'princ))
  (when root
    (postorder-visit (get-left-tree root) visit)
    (postorder-visit (get-right-tree root) visit)
    (funcall visit (get-tree-data root))))
