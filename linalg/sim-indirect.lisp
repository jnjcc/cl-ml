;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: Indirect Addressing through INDICES

(in-package #:cl-ml/linalg)

(defun iaref (data indices i &optional (ref #'vref))
  "reference through Indirect Addressing"
  (let ((indx (nth i indices)))
    (funcall ref data indx)))

(defun iavref (va indices i)
  "vector reference through Indirect Addressing"
  (iaref va indices i #'vref))

(defun (setf iavref) (val va indices i)
  (let ((indx (nth i indices)))
    (setf (vref va indx) val)))

(defun swap-indices (indices i j)
  (let ((tmp (nth i indices)))
    (setf (nth i indices) (nth j indices))
    (setf (nth j indices) tmp)))

(defun sort-indices (indices datas &key (start 0) end (predicate #'<) (ref #'vref))
  "sort `indices' according to `datas'
NOTICE: destructive function, remember to do (setf indices (sort-indices indices))
as for destructive function, the original sequence is implementation-dependent,
see HyperSpec of `nreverse'"
  (labels ((compare (i j)
             ;; here I and J (elemts of INDICES) are indices of DATAS, direct addressing
             (funcall predicate (funcall ref datas i) (funcall ref datas j))))
    (setf (subseq indices start end) (sort (subseq indices start end) #'compare))
    indices))

(defmacro do-indices ((i indx indices &key (start 0) end) &body body)
  "I is the index of `indices'; `indx' is the index of indirect addressing"
  (let ((end-sym (gensym "END-")))
    `(let ((,end-sym ,end)
           (,indx nil))
       (unless ,end-sym
         (setf ,end-sym (length ,indices)))
       (do ((,i ,start (1+ ,i)))
           ((>= ,i ,end-sym))
         (setf ,indx (nth ,i ,indices))
         ,@body))))

(defmacro do-ia-access ((indx indices &key (start 0) end) &body body)
  (let ((idx-sym (gensym "IDX-"))
        (end-sym (gensym "END-")))
    `(let ((,end-sym ,end)
           (,indx nil))
       (unless ,end-sym
         (setf ,end-sym (length ,indices)))
       (do ((,idx-sym ,start (1+ ,idx-sym)))
           ((>= ,idx-sym ,end-sym))
         (setf ,indx (nth ,idx-sym ,indices))
         ,@body))))

(defmacro do-mutable-indices ((i indx indices nactive) &body body)
  "I is the index of `indices'; `indx' is the index of indirect addressing
NOTICE: the `nactive' might change during loop"
  `(do ((,i 0 (1+ ,i))
        (,indx nil))
       ((>= ,i ,nactive))
     (setf ,indx (nth ,i ,indices))
     ,@body))

(defmacro do-mutable-iacess ((indx indices nactive) &body body)
  "`indx' is the index of indirect addressing
NOTICE: the `nactive' might change during loop"
  (let ((idx-sym (gensym "IDX-")))
    `(do ((,idx-sym 0 (+ ,idx-sym 1))
          (,indx nil))
         ((>= ,idx-sym ,nactive))
       (setf ,indx (nth ,idx-sym ,indices))
       ,@body)))
