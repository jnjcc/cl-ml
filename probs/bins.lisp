;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; bin: lightweight "hash-table" using association list
;;;;   - `bincount': ((k1 . count1) ...)
;;;;   - `binlist': ((k1 (v1 v2 ...)) ...)

(in-package #:cl-ml/probs)

(defun make-bins ()
  nil)

(defun copy-bins (bins)
  (copy-list bins))

;;; on `bincount'
(defun binkey (bins key)
  (assoc key bins))

(defun binvalue (bins key)
  (cdr (assoc key bins)))

;; NOTICE: using macro to mimic `incf', `decf' and `push'
(defmacro binincf (bins key &optional (value 1))
  `(if (assoc ,key ,bins)
       (incf (cdr (assoc ,key ,bins)) ,value)
       (push (cons ,key ,value) ,bins)))

(defmacro bindecf (bins key &optional (value 1))
  `(when (assoc ,key ,bins)
     (decf (cdr (assoc ,key ,bins)) ,value)
     (remove-if (lambda (x) (<= x 0)) ,bins :key #'cdr)))

;;; on `binlist'
(defmacro binpush (bins key value)
  `(if (assoc ,key ,bins)
       (push ,value (cdr (assoc ,key ,bins)))
       (push (cons ,key (list ,value)) ,bins)))

(defmacro do-bins ((key value bins) &body body)
  (let ((item-sym (gensym "ITEM-")))
    `(let ((,key nil)
           (,value nil))
       (dolist (,item-sym ,bins)
         (setf ,key (car ,item-sym))
         (setf ,value (cdr ,item-sym))
         ,@body))))

(defun bincount (lst &optional (start 0) end)
  "bin and count: (values ((key . count) ...) sum(count)))"
  (unless end
    (setf end (length lst)))
  (let ((bins nil))
    (do ((i start (1+ i)))
        ((>= i end))
      (binincf bins (nth i lst)))
    (values (nreverse bins) (- end start))))

(defun density (bins &optional count)
  "density of frequency: ((key . count) ...)"
  (unless count
    (setf count (reduce #'+ (mapcar #'cdr bins) :initial-value 0)))
  (labels ((conv2prob (elem)
             (cons (car elem) (/ (cdr elem) count))))
    (mapcar #'conv2prob bins)))

(defun probability (bins &optional count)
  (mapcar #'cdr (density bins count)))

(defun bindensity (lst &optional (start 0) end)
  "bin and density: ((key . density) ...)"
  (multiple-value-bind (bins cnt) (bincount lst start end)
    (density bins cnt)))

(defun %argfun (bins cmp &optional (start 0) end (key #'car) (value #'cdr))
  "`bins' default input from (bincount)"
  (unless end
    (setf end (length bins)))
  (let ((maxkey nil)
        (maxval nil)
        (curpair nil))
    (do ((i start (1+ i)))
        ((>= i end))
      (setf curpair (nth i bins))
      (when (or (null maxval) (funcall cmp (funcall value curpair) maxval))
        (if (eq key value)
            ;; argmax the index!
            (setf maxkey i)
            (setf maxkey (funcall key curpair)))
        (setf maxval (funcall value curpair))))
    (values maxkey maxval)))

(defun argmax (bins &key (start 0) end (key #'car) (value #'cdr))
  (%argfun bins #'> start end key value))

(defun argmin (bins &key (start 0) end (key #'car) (value #'cdr))
  (%argfun bins #'< start end key value))
