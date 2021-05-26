;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple io for training examples

(in-package #:cl-ml/io)

(defun %canonize-bound (bound len &optional (default 0))
  (cond
    ((null bound) default)
    ((integerp bound) (cond
                        ((<= 0 bound len) bound)
                        ((<= (- len) bound -1) (+ bound len))
                        (t (error "malformed bound ~A: range [~A, ~A] expected"
                                  bound (- len) len))))
    (t (error "malformed bound ~A: not supported" bound))))

(defun %canonize-start (start len)
  (%canonize-bound start len 0))

(defun %canonize-end (end len)
  (%canonize-bound end len len))

;; collect training example from file
(defmacro do-example-from-file ((example index fpath &key (start 0) end rows) &body body)
  "[`start', `end') can be variable or value; `rows' must be variable;
`index' is the index of the collected examples by now"
  (let ((examples-sym (gensym "EXAMPLES-"))
        (len-sym (gensym "LEN-"))
        (start-sym (gensym "START-")) ;; start might be value
        (end-sym (gensym "END-")) ;; end might be value
        (idx-sym (gensym "IDX-")))
    `(let* ((,examples-sym (read-file-lines ,fpath))
            (,len-sym (length ,examples-sym))
            (,start-sym (%canonize-start ,start ,len-sym))
            (,end-sym (%canonize-end ,end ,len-sym))
            (,example nil)
            (,index nil))
       (do ((,idx-sym ,start-sym (1+ ,idx-sym)))
           ((>= ,idx-sym ,end-sym))
         ,@(when rows
             `((setf ,rows (+ ,idx-sym 1))))
         (setf ,example (nth ,idx-sym ,examples-sym))
         (setf ,index (- ,idx-sym ,start-sym))
         ,@body))))

(defmacro do-example-row ((example index examples &key (start 0) end rows) &body body)
  "loop across rows of [`start', `end'); `example' as current row;
`index' as current index; `rows' as number of rows collected"
  (let ((len-sym (gensym "LEN-"))
        (start-sym (gensym "START-"))
        (end-sym (gensym "END-"))
        (idx-sym (gensym "IDX-")))
    `(let* ((,len-sym (length ,examples))
            (,start-sym (%canonize-start ,start ,len-sym))
            (,end-sym (%canonize-end ,end ,len-sym))
            (,example nil)
            (,index nil))
       (do ((,idx-sym ,start-sym (1+ ,idx-sym)))
           ((>= ,idx-sym ,end-sym))
         ,@(when rows
             `((setf ,rows (+ ,idx-sym 1))))
         (setf ,example (nth ,idx-sym ,examples))
         (setf ,index (- ,idx-sym ,start-sym))
         ,@body))))

(defmacro do-variable-col ((var index vars &key (start 0) end (label -1)) &body body)
  "loop across columns [`start', `end'), excluding `label'; `var' as current variable;
`index' as current index"
  (let ((len-sym (gensym "LEN-"))
        (start-sym (gensym "START-"))
        (end-sym (gensym "END-"))
        (labidx-sym (gensym "LABIDX-"))
        (idx-sym (gensym "IDX-")))
    `(let* ((,len-sym (length ,vars))
            (,start-sym (%canonize-start ,start ,len-sym))
            (,end-sym (%canonize-end ,end ,len-sym))
            (,labidx-sym ,label)
            (,var nil)
            (,index nil))
       (when (and ,labidx-sym (< ,labidx-sym 0))
         (incf ,labidx-sym ,len-sym))
       (do ((,idx-sym ,start-sym (1+ ,idx-sym)))
           ((>= ,idx-sym ,end-sym))
         (when (or (not ,labidx-sym) (/= ,idx-sym ,labidx-sym))
           (setf ,var (nth ,idx-sym ,vars))
           (setf ,index (- ,idx-sym ,start-sym))
           ,@body)))))
