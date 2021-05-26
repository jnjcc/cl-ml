;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Combinations and Permutations (n, k)

(in-package #:cl-ml/probs)

;;; Combinations
;; combinations-with-replacement
(defun %visit-comb-indices-replace (n k &optional visit)
  "given a set of `n' elements, `visit' all the `k'-length combinations of element indices
with replacement"
  (let ((indices (make-list k :initial-element 0)))
    (when visit
      (funcall visit indices))
    (block forever-loop
      (do ((idx (- k 1))) (nil)
        (do ((i (- k 1) (1- i)))
            ((< i 0) (return-from forever-loop))
          (when (/= (nth i indices) (- n 1))
            (setf idx i)
            (return)))
        (do ((j idx (1+ j))
             (next (+ (nth idx indices) 1)))
            ((>= j k))
          (setf (nth j indices) next))
        (when visit
          (funcall visit indices))))))

(defun %comb-indices-with-replace (n k)
  (let ((indices-list nil))
    (labels ((collect (indices)
               ;; we will change the content of `indices' during
               ;; (%visit-comb-indices-replace), copy needed!
               (push (copy-list indices) indices-list)))
      (%visit-comb-indices-replace n k #'collect))
    (nreverse indices-list)))

(defun %comb-with-replacement (lst k)
  "Elements are treated as unique based on their position, not on their value"
  (let ((combinations nil))
    (labels ((collect (indices)
               (let ((comb nil))
                 (dolist (ind indices)
                   (push (nth ind lst) comb))
                 (push (nreverse comb) combinations))))
      (%visit-comb-indices-replace (length lst) k #'collect))
    (nreverse combinations)))

;; combinations-without-replacement
(defun %visit-comb-indices-no-replace (n k &optional visit)
  (let ((indices (make-indices k)))
    (when visit
      (funcall visit indices))
    (block forever-loop
      (do ((idx (- k 1))) (nil)
        (do ((i (- k 1) (1- i)))
            ((< i 0) (return-from forever-loop))
          ;; the max index of i-th position is (n-k+i)
          (when (/= (nth i indices) (+ (- n k) i))
            (setf idx i)
            (return)))
        (incf (nth idx indices) 1)
        (do ((j (+ idx 1) (1+ j)))
            ((>= j k))
          (setf (nth j indices) (+ (nth (- j 1) indices) 1)))
        (when visit
          (funcall visit indices))))))

(defun %comb-indices-no-replace (n k)
  (let ((indices-list nil))
    (labels ((collect (indices)
               (push (copy-list indices) indices-list)))
      (%visit-comb-indices-no-replace n k #'collect))
    (nreverse indices-list)))

(defun %comb-with-no-replace (lst k)
  (let ((comb-lst nil))
    (labels ((collect (indices)
               (let ((comb nil))
                 (dolist (ind indices)
                   (push (nth ind lst) comb))
                 (push (nreverse comb) comb-lst))))
      (%visit-comb-indices-no-replace (length lst) k #'collect))
    (nreverse comb-lst)))

(defun visit-combinations-indices (n k &key (visit #'princ) (replace t))
  (if replace
      (%visit-comb-indices-replace n k visit)
      (%visit-comb-indices-no-replace n k visit)))

(defun combinations (lst k &key (replace t))
  (if replace
      (%comb-with-replacement lst k)
      (%comb-with-no-replace lst k)))

(defun combinations-indices (n k &key (replace t))
  (if replace
      (%comb-indices-with-replace n k)
      (%comb-indices-with-no-replace n k)))

;;; Permutations
(defun visit-permutations-indices (n k &key (visit #'princ))
  (let ((indices (make-indices n))
        ;; (n-i-1) choices for position i: [i+1, i+2, ..., n-1]
        (choices (make-step-range (- n 1) (- n k 1) -1)))
    (when visit
      (funcall visit (subseq indices 0 k)))
    (block forever-loop
      (do () (nil)
        (do ((i (- k 1) (1- i)))
            ((< i 0) (return-from forever-loop))
          (if (= (nth i choices) 0)
              ;; now that there are no available choices for position i, we should restore
              ;; the original lexicographic order for position (i-1)'s sake; since we came
              ;; from position (i+1) in the loop, so we know for sure indices[i+1:] already
              ;; in lexicographic order
              (let ((tmp (nth i indices)))
                (when (< i (- n 1))
                  (setf (subseq indices i) (subseq indices (+ i 1))
                        (nth (- n 1) indices) tmp)
                  ;; also restore choices
                  (setf (nth i choices) (- n i 1))))
              ;; again, we know for sure indices[i+1:] already in lexicographic order
              ;; choice index: [i+1, i+2, ..., n-1] = [n-(n-i-1), ...]
              (let ((choidx (- n (nth i choices)))
                    (tmp (nth i indices)))
                (setf (nth i indices) (nth choidx indices)
                      (nth choidx indices) tmp)
                ;; we've used up one choice
                (decf (nth i choices))
                (when visit
                  (funcall visit (subseq indices 0 k)))
                (return))))))))

(defun permutations (lst k)
  (let ((permut-lst nil))
    (labels ((collect (indices)
               (let ((permut nil))
                 (dolist (ind indices)
                   (push (nth ind lst) permut))
                 (push (nreverse permut) permut-lst))))
      (visit-permutation-indices (length lst) k :visit #'collect))
    (nreverse permut-lst)))

(defun permutations-indices (n k)
  (let ((indices-list nil))
    (labels ((collect (indices)
               (push (copy-list indices) indices-list)))
      (visit-permutation-indices n k :visit #'collect))
    (nreverse indices-list)))
