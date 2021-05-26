;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Common Data Structure: FIFO queue

(in-package #:cl-ml/algo)

;;; FIFO queue with list
(defun make-queue ()
  nil)

(defmacro queue-enque (queue data)
  "(macroexpand-1 '(push 1 stack)"
  `(setq ,queue (nconc ,queue (list ,data))))

(defmacro queue-deque (queue)
  "(macroexpand-1 '(pop stack))"
  `(prog1
       (car ,queue)
     (setq ,queue (cdr ,queue))))

(defun queue-front (queue)
  (car queue))

(defun queue-empty (queue)
  (= (length queue) 0))
