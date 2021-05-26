;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml/test)

(define-test split-string-test
  (let* ((str "ab,cd,ab,ef,gh,abab")
         (bychr (split-string str #\,))
         (bystr (split-string str "ab"))
         (byspc (split-string str #\Space)))
    ;; '("ab" "cd" "ab" "ef" "gh" "abab")
    (assert-eq 6 (length bychr))
    (assert-true (string= "ef" (nth 3 bychr)))

    ;; '("" ",cd," ",ef,gh," "" "")
    (assert-eq 5 (length bystr))
    (assert-true (string= "" (nth 0 bystr)))
    (assert-true (string= ",cd," (nth 1 bystr)))
    (assert-true (string= "" (nth 3 bystr)))

    ;; '(str)
    (assert-eq 1 (length byspc))
    (assert-true (string= str (nth 0 byspc)))))

(defun %load-tree-preorder (elems)
  (let ((root (make-binary-tree (car elems)))
        (elems (cdr elems))
        (tree-queue (make-queue)))
    (queue-enque tree-queue root)
    (do ((child (queue-front tree-queue) (queue-front tree-queue)))
        ((queue-empty tree-queue))
      (when (null elems)
        (return))
      (unless (null (car elems))
        (set-left-data child (car elems))
        (queue-enque tree-queue (get-left-tree child)))
      (setf elems (cdr elems))

      (when (null elems)
        (return))
      (unless (null (car elems))
        (set-right-data child (car elems))
        (queue-enque tree-queue (get-right-tree child)))
      (setf elems (cdr elems))

      (queue-deque tree-queue))
    root))

(define-test binary-tree-test
  "A binary tree of the form:
         A
        / \
       B   E
        \   \
         C   F
        /   /
       D   G
          / \
         H   K
preorder-visit: ABCDEFGHK"
  (let ((vlist nil)
        (root (%load-tree-preorder '(a b e nil c nil f d nil g nil nil nil h k))))
    (labels ((collect (data)
               (push data vlist)))
      (cl-ml::inorder-visit root #'collect)
      (assert-true (equal '(b d c a e h g k f) (nreverse vlist)))
      (setf vlist nil)
      (cl-ml::preorder-visit root #'collect)
      (assert-true (equal '(a b c d e f g h k) (nreverse vlist)))
      (setf vlist nil)
      (cl-ml::postorder-visit root #'collect)
      (assert-true (equal '(d c b h k g f e a) (nreverse vlist))))))

(define-test huffman-tree-test
  "Huffman tree of the form (inner nodes being sum of frequency):
            15
           /  \
         6      9
        / \    / \
       3   B  D   A
      / \
     E   C
"
  (let ((text '(a a b c c b b a d e d d a a d)))
    (multiple-value-bind (huff coding) (huffman-from-list (bincount text) :coded t)
      (declare (ignore huff))
      (assert-true (equal '(1 1) (binvalue coding 'a)))
      (assert-true (equal '(0 1) (binvalue coding 'b)))
      (assert-true (equal '(1 0) (binvalue coding 'd)))
      (assert-true (equal '(0 0 0) (binvalue coding 'e))))))
