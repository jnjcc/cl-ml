;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; "Data Frame" for training

(in-package #:cl-ml)

;;; NOTICE: reorder training data through indirect addressing
;;;   it is necessary we reorder the y label at the same time
;;;   smatrix-view is not able to do that
(defclass data-frame ()
  ((xmatrix :initform nil :initarg :xmatrix)
   (ylabels :initform nil :initarg :ylabels)
   ;; feature name (optional)
   (feaname :initform nil :initarg :feaname)
   ;; indirect access of samples and features
   (sindice :initform nil :initarg :sindice)
   (findice :initform nil :initarg :findice)))

(defun make-data-frame (X y &optional feaname)
  (let ((frame (make-instance 'data-frame :xmatrix X :ylabels y :feaname feaname)))
    (with-slots (sindice findice) frame
      (setf sindice (make-indices (nrow X)))
      (setf findice (make-indices (ncol X))))
    frame))

(defmethod get-shape ((frame data-frame))
  (with-slots (sindice findice) frame
    (values (length sindice) (length findice))))

(defmethod get-label ((frame data-frame) i)
  (with-slots (ylabels sindice) frame
    (iavref ylabels sindice i)))

(defmethod get-sample ((frame data-frame) i)
  (with-slots (xmatrix ylabels sindice findice) frame
    (values (make-matrix-view xmatrix :row-view (nth i sindice) :col-view findice)
            (get-label frame i))))

(defmethod get-feature ((frame data-frame) j)
  (with-slots (xmatrix sindice findice) frame
    (make-matrix-view xmatrix :row-view sindice :col-view (nth j findice))))

(defmethod get-feature-value ((frame data-frame) i j)
  "J-th feature value of I-th sample"
  (with-slots (xmatrix sindice findice) frame
    (mref xmatrix (nth i sindice) (nth j findice))))

(defmethod get-feature-name ((frame data-frame) j)
  "get the original feature before reordering"
  (with-slots (feaname findice) frame
    (let ((jndx (nth j findice)))
      (if feaname
          (values jndx (nth jndx feaname))
          (values jndx nil)))))

;;; for classification tasks
;;; TODO: what if label is string? (assoc #'string=)
(defmethod count-label ((frame data-frame) &optional (start 0) end)
  (unless end
    (setf end (get-shape frame)))
  (let ((alist nil))
    (do ((i start (1+ i)))
        ((>= i end))
      (binincf alist (get-label frame i)))
    (values alist (- end start))))

(defmethod label-density ((frame data-frame) &optional (start 0) end)
  (multiple-value-bind (alist cnt) (count-label frame start end)
    (labels ((conv2prob (elem)
               (/ (cdr elem) cnt)))
      (mapcar #'conv2prob alist))))

;;; for regression tasks
(defmethod sum-label ((frame data-frame) &optional (start 0) end)
  (unless end
    (setf end (get-shape frame)))
  (let ((sum 0))
    (do ((i start (1+ i)))
        ((>= i end))
      (incf sum (get-label frame i)))
    (values sum (- end start))))

(defmethod mse-label ((frame data-frame) &optional (start 0) end)
  (unless end
    (setf end (get-shape frame)))
  (let ((avg 0) (mse 0) (diff 0)
        (cnt (- end start)))
    (do ((i start (1+ i)))
        ((>= i end))
      (incf avg (get-label frame i)))
    (setf avg (/ avg cnt))
    (do ((i start (1+ i)))
        ((>= i end))
      (setf diff (- (get-label frame i) avg))
      (incf mse (* diff diff)))
    (values mse cnt)))

(defmethod swap-frame ((frame data-frame) i j)
  (with-slots (sindice) frame
    (swap-indices sindice i j)))

(defmethod shuffle-frame ((frame data-frame) &optional (start 0) end)
  (with-slots (sindice) frame
    (shuffle sindice start end)))

(defmethod sort-frame ((frame data-frame) j &optional (start 0) end)
  "sort data frame [start, end) by feature J"
  (with-slots (xmatrix sindice findice) frame
    (let ((jndx (nth j findice)))
      (setf sindice (sort-indices sindice (mcv xmatrix jndx) :start start :end end)))))

(defmethod swap-feature ((frame data-frame) i j)
  (with-slots (findice) frame
    (swap-indices findice i j)))

(defmethod shuffle-feature ((frame data-frame) &optional (start 0) end)
  (with-slots (findice) frame
    (shuffle findice start end)))
