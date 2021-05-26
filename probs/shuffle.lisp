;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Generate a random permutation of a finite (sub-)sequence:
;;;;   choose random $k$ elements from a sequence of size $n$

(in-package #:cl-ml/probs)

(defun %subseq-values (lst start end)
  "Returns (actual start, acutal length, acutal end, full length)"
  (let (subbeg subend)
    (if start
        (setf subbeg start)
        (setf subbeg 0))
    (if end
        (setf subend end)
        (setf subend (length lst)))
    (values subbeg (- subend subbeg) subend (length lst))))

(defun %fisher-yates (lst &optional start end)
  "NOTICE: will modify `lst', copy-list if necessary"
  (let ((shuflst nil)
        k last)
    (multiple-value-bind (sbeg slen send llen) (%subseq-values lst start end)
      (dotimes (idx sbeg)
        (push (nth idx lst) shuflst))

      (do ((size slen (- size 1)))
          ((<= size 0))
        (setf k (random size))
        (incf k sbeg)
        (push (nth k lst) shuflst)
        (setf last (+ sbeg (- size 1)))
        ;; "struck out" k-th element
        (setf (nth k lst) (nth last lst)))

      (do ((idx send (+ idx 1)))
          ((>= idx llen))
        (push (nth idx lst) shuflst))
      (nreverse shuflst))))

(defun %swap-list (lst i j &optional start)
  (when start
    (setf i (+ start i))
    (setf j (+ start j)))
  (let ((tmp (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tmp)))

(defun %knuth-shuffle (lst &optional start end)
  "NOTICE: will modify `lst', copy-list if necessary"
  (let (k)
    (multiple-value-bind (sbeg slen) (%subseq-values lst start end)
      (do ((size slen (- size 1)))
          ((<= size 0))
        ;; `size' is the sub-list length
        (setf k (random size))
        (%swap-list lst k (- size 1) sbeg))
      lst)))

(defun shuffle-indices (n)
  (let ((indices (make-indices n)))
    (%knuth-shuffle indices)))

(defun shuffle (lst &optional start end)
  "NOTICE: will modify `lst', copy-list if necessary"
  (%knuth-shuffle lst start end))
