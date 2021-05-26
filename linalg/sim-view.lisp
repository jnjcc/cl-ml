;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: matrix view window

(in-package #:cl-ml/linalg)

;;; three types of view window:
;;;   - (start . end) association pair, meaning [start, end)
;;;     + `start' must be integer
;;;     + `end' can be integer or :end
;;;   - (i1 i2 i3 ...) proper list
;;;   - nil
(defclass smatrix-view (smatrix)
  ((displaced-to :initform nil :initarg :displaced-to :reader displaced-to)
   (row-view :initform nil :initarg :row-view :accessor row-view
             :documentation "row indices of matrix")
   (col-view :initform nil :initarg :col-view :accessor col-view
             :documentation "col indices of matrix")))

;;; rows and colums
(defun %assoc-pair-p (view)
  "(a . b): a must be integer; b can be integer or :end"
  (and (consp view) (integerp (car view))
       (or (integerp (cdr view))
           (eq (cdr view) :end))))

(defmethod nrow ((ma smatrix-view))
  (with-slots (row-view) ma
    (cond
      ((%assoc-pair-p row-view) (- (cdr row-view) (car row-view)))
      ((consp row-view) (length row-view))
      ((displaced-to ma) (nrow (displaced-to ma)))
      (t 0))))

(defmethod ncol ((ma smatrix-view))
  (with-slots (col-view) ma
    (cond
      ((%assoc-pair-p col-view) (- (cdr col-view) (car col-view)))
      ((consp col-view) (length col-view))
      ((displaced-to ma) (ncol (displaced-to ma)))
      (t 0))))

;;; element access
(defun %translate-to-matrix-index (idx view)
  "translate from view index to matrix index"
  (cond
    ((%assoc-pair-p view)
     (let ((midx (+ idx (car view))))
       (if (< midx (cdr view))
           midx
           (error "out of bounds: ~A" idx))))
    ((consp view)
     (if (< idx (length view))
         (nth idx view)
         (error "out of bounds: ~A" idx)))
    (t idx)))

(defmethod %translated-row-index ((ma smatrix-view) i)
  (%translate-to-matrix-index i (row-view ma)))

(defmethod %translated-col-index ((ma smatrix-view) j)
  (%translate-to-matrix-index j (col-view ma)))

(defmethod %translated-indices ((ma smatrix-view) i j)
  (values (%translate-to-matrix-index i (row-view ma))
          (%translate-to-matrix-index j (col-view ma))))

(defmethod mref ((ma smatrix-view) i j)
  (with-slots (displaced-to) ma
    (multiple-value-bind (ridx cidx) (%translated-indices ma i j)
      (if displaced-to
          (mref displaced-to ridx cidx)
          (error "out of bounds: (~A, ~A)" i j)))))

(defmethod (setf mref) (val (ma smatrix-view) i j)
  (with-slots (displaced-to) ma
    (multiple-value-bind (ridx cidx) (%translated-indices ma i j)
      (setf (mref displaced-to ridx cidx) val))))

;;; make-* functions
(defun %canonize-index (idx len)
  (or (integerp idx) (error "malformed index ~A: integer expected" idx))
  (cond
    ((< -1 idx len) idx)
    ((<= (- len) idx -1) (+ idx len))
    (t (error "malformed index ~A: range [~A, ~A) expected" idx (- len) len))))

(defun %canonize-pair-view (view len)
  (let ((car-idx (%canonize-index (car view) len))
        (cdr-idx (cdr view)))
    (setf cdr-idx (cond
                    ((eq cdr-idx :end) len)
                    ((>= cdr-idx len) len)
                    (t (%canonize-index cdr-idx len))))
    (if (<= car-idx cdr-idx)
        (cons car-idx cdr-idx)
        (error "malformed view ~A: lower bound bigger than upper bound" view))))

(defun %canonize-view (view len)
  (labels ((canonize-index-with-len (idx)
             (%canonize-index idx len)))
    (cond
      ((%assoc-pair-p view) (%canonize-pair-view view len))
      ((consp view) (mapcar #'canonize-index-with-len view))
      ((null view) view)
      ((integerp view) (list (canonize-index-with-len view)))
      (t (error "malformed view ~A: not supported" view)))))

(defun make-matrix-view (ma &key (row-view nil) (col-view nil))
  (declare (type smatrix ma))
  (let ((rview (%canonize-view row-view (nrow ma)))
        (cview (%canonize-view col-view (ncol ma))))
    (make-instance 'smatrix-view :displaced-to ma :row-view rview :col-view cview)))

;;; NOTICE: we are changing view of the (displaced-to) matrix
(defun update-matrix-view (maview &key (row-view nil row-supplied-p)
                                    (col-view nil col-supplied-p))
  (when row-supplied-p
    (setf (row-view maview) (%canonize-view row-view (nrow (displaced-to maview)))))
  (when col-supplied-p
    (setf (col-view maview) (%canonize-view col-view (ncol (displaced-to maview))))))

(defmacro with-matrix-view ((maview &key row-view col-view) ma &body body)
  "matrix view for smatrix MA"
  `(let ((,maview (make-matrix-view ,ma :row-view ,row-view :col-view ,col-view)))
     ,@body))

(defmacro with-restored-view ((maview &key (row-view nil row-supplied-p)
                                      (col-view nil col-supplied-p)) &body body)
  (let ((row-sym (gensym "ROW-"))
        (col-sym (gensym "COL-"))
        (rview-sym (gensym "RVIEW-"))
        (cview-sym (gensym "CVIEW-")))
    `(let ((,row-sym (row-view ,maview))
           (,col-sym (col-view ,maview))
           (,rview-sym (%canonize-view ,row-view (nrow (displaced-to ,maview))))
           (,cview-sym (%canonize-view ,col-view (ncol (displaced-to ,maview)))))
       (unwind-protect
            (progn
              (when ,row-supplied-p
                (setf (row-view ,maview) ,rview-sym))
              (when ,col-supplied-p
                (setf (col-view ,maview) ,cview-sym))
              ,@body)
         (setf (row-view ,maview) ,row-sym
               (col-view ,maview) ,col-sym)))))
