;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Common Data Structure: Huffman Tree

(in-package #:cl-ml/algo)

(defun choose-min-node (nodes nleaf lpos ipos &key (freqfn #'cdr))
  "[0, `nleaf'] of `nodes' being leafs with default form ((word . freq) ...),
while [`nleaf', ) being inner nodes of the form (freq freq ...)"
  (let (lfreq ifreq midx mfreq)
    (if (< lpos nleaf)
        (progn
          (setf lfreq (funcall freqfn (nth lpos nodes))
                ifreq (nth ipos nodes))
          (cond
            ((or (null ifreq) (< lfreq ifreq))
             (progn
               ;; use up one leaf node
               (setf midx lpos
                     mfreq lfreq)
               (incf lpos)))
            ((>= lfreq ifreq)
             (progn
               (setf midx ipos
                     mfreq ifreq)
               (incf ipos)))))
        (progn
          ;; no more leafs left!
          (setf midx ipos
                mfreq (nth ipos nodes))
          (incf ipos)))
    (values midx mfreq lpos ipos)))

(defun enque-merged-tree (tree-que nodes nleaf midx1 midx2 pfreq)
  (cond
    ((and (< midx1 nleaf) (< midx2 nleaf))
     ;; merge two leafs
     (queue-enque tree-que (make-binary-tree pfreq
                                             (make-binary-tree (nth midx1 nodes))
                                             (make-binary-tree (nth midx2 nodes)))))
    ((and (< midx1 nleaf) (>= midx2 nleaf))
     (let ((rtree (queue-deque tree-que)))
       (queue-enque tree-que (make-binary-tree pfreq
                                               (make-binary-tree (nth midx1 nodes))
                                               rtree))))
    ((and (>= midx1 nleaf) (< midx2 nleaf))
     (let ((ltree (queue-deque tree-que)))
       (queue-enque tree-que (make-binary-tree pfreq
                                               ltree
                                               (make-binary-tree (nth midx2 nodes))))))
    ((and (>= midx1 nleaf) (>= midx2 nleaf))
     (let ((ltree (queue-deque tree-que))
           (rtree (queue-deque tree-que)))
       (queue-enque tree-que (make-binary-tree pfreq ltree rtree)))))
  tree-que)

(defun get-huffman-coding (nodes nleaf parents binarys &key (keyfn nil))
  (let ((huff-coding nil)
        (cur-coding nil))
    (dotimes (i nleaf)
      (setf cur-coding nil)
      (do ((idx i))
          ((null (nth idx parents)))
        (push (nth idx binarys) cur-coding)
        (setf idx (nth idx parents)))
      (if keyfn
          (push (cons (funcall keyfn (nth i nodes)) cur-coding) huff-coding)
          (push (cons (nth i nodes) cur-coding) huff-coding)))
    huff-coding))

(defun huffman-from-list (lsts &key (coded nil) (keyfn #'car) (freqfn #'cdr) (sorted nil))
  "Huffman tree from list with default form ((word . freq) ...), thus `freqfn' being #'cdr
Requires: `sorted' in ascending order
NOTICE: might modify `lsts', copy-list if necessary"
  (let* ((nleaf (length lsts))
         (leafs lsts)
         ;; there will only be (n-1) inner nodes for huffman tree, in fact
         (inner (make-list nleaf :initial-element nil))
         (nodes nil))
    (unless sorted
      (setf leafs (sort leafs #'< :key freqfn)))
    (setf nodes (append leafs inner))
    (let (;;; Two-way merge sort: leaf index and inner node index
          (lpos 0) (ipos nleaf)
          ;; left/right child index and frequency; parent index and frequency
          midx1 mfreq1 midx2 mfreq2 pidx pfreq
          (tree-que nil)
          ;;; Huffman coding when `coded'
          (parents nil) (binarys nil)
          (huff-coding nil))
      (when coded
        (setf parents (make-list (length nodes) :initial-element nil))
        (setf binarys (make-list (length nodes) :initial-element 0)))
      (dotimes (i (- nleaf 1))
        (multiple-value-setq (midx1 mfreq1 lpos ipos)
          (choose-min-node nodes nleaf lpos ipos :freqfn freqfn))
        (multiple-value-setq (midx2 mfreq2 lpos ipos)
          (choose-min-node nodes nleaf lpos ipos :freqfn freqfn))
        ;; i-th inner node!
        (setf pidx (+ i nleaf))
        (setf pfreq (+ mfreq1 mfreq2))
        (setf (nth pidx nodes) pfreq)
        (when coded
          (setf (nth midx1 parents) pidx)
          (setf (nth midx2 parents) pidx)
          ;; midx2 as the right child
          (setf (nth midx2 binarys) 1))
        (setf tree-que (enque-merged-tree tree-que nodes nleaf midx1 midx2 pfreq)))
      (when coded
        (setf huff-coding (get-huffman-coding nodes nleaf parents binarys :keyfn keyfn)))
      (values (car tree-que) huff-coding parents binarys))))

(defun preorder-huffman (root &optional (depth 0) (stream t))
  (when root
    (format stream "~A" (repeat-string "| " depth))
    (format stream "~A" (get-tree-data root))
    (format stream "~%")
    (preorder-huffman (get-left-tree root) (+ depth 1) stream)
    (preorder-huffman (get-right-tree root) (+ depth 1) stream)))

(defun inorder-huffman (root &optional (depth 0) (stream t) (level #\Tab))
  "not quite inorder: right-tree -> parent -> left-tree"
  (when root
    (inorder-huffman (get-right-tree root) (+ depth 1) stream level)
    (let* ((data (format nil "~A" (get-tree-data root)))
           (dlen (length data)))
      (format stream "~A~A/~%" (repeat-string level depth) (repeat-string #\Space dlen))
      (format stream "~A~A~%" (repeat-string level depth) data)
      (format stream "~A~A\\~%" (repeat-string level depth) (repeat-string #\Space dlen)))
    (inorder-huffman (get-left-tree root) (+ depth 1) stream level)))
