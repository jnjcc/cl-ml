;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-ml)

(defun print-confusion-matrix (cmatrix labenc &optional (stream t))
  (format stream "A\\P")
  (dotimes (j (label-nums labenc))
    (format stream "~A~A" #\Tab (get-encoder-label labenc j)))
  (format stream "~%")
  (do-matrix-row (i cmatrix)
    (format stream "~A" (get-encoder-label labenc i))
    (do-matrix-col (j cmatrix)
      (format stream "~A~A" #\Tab (mref cmatrix i j)))
    (format stream "~%")))

(defun confusion-matrix (ylabel ypred)
  "row: actual class; column: predicted class"
  (let ((labenc (make-instance 'label-encoder)))
    (fit labenc ylabel)
    (let ((cmatrix (make-square-matrix (label-nums labenc) :initial-element 0))
          i j)
      (do-vector (iy ylabel)
        (setf i (get-encoder-index labenc (vref ylabel iy)))
        (setf j (get-encoder-index labenc (vref ypred iy)))
        (incf (mref cmatrix i j)))
      (values cmatrix labenc))))

;;; TODO: multiclass precision
(defun binary-confusion (ylabel ypred &optional (positive +1))
  (let ((tp 0) (fn 0) (fp 0) (tn 0))
    (do-vector (i ylabel)
      (let ((y (vref ylabel i))
            (yhat (vref ypred i)))
        (cond
          ((and (= yhat positive) (= y positive)) (incf tp))
          ((and (= yhat positive) (/= y positive)) (incf fp))
          ((and (/= yhat positive) (= y positive)) (incf fn))
          ((and (/= yhat positive) (/= y positive)) (incf tn)))))
    (values tp fn fp tn)))

(defun precision-score (ylabel ypred &optional (positive +1))
  (multiple-value-bind (tp fn fp tn) (binary-confusion ylabel ypred positive)
    (declare (ignore fn tn))
    (float (/ tp (+ tp fp)))))

(defun recall-score (ylabel ypred &optional (positive +1))
  (multiple-value-bind (tp fn fp tn) (binary-confusion ylabel ypred positive)
    (declare (ignore fp tn))
    (float (/ tp (+ tp fn)))))

(defun f1-score (ylabel ypred &optional (positive +1))
  "F1 = 2 / (1/prec + 1/reca) = 2 * prec * reca / (prec + reca) = tp / (tp + (fn + fp)/2)"
  (multiple-value-bind (tp fn fp tn) (binary-confusion ylabel ypred positive)
    (declare (ignore tn))
    (float (/ tp (+ tp (/ (+ fn fp) 2))))))

(defun accuracy-score (ylabel ypred)
  (let ((acc 0) (num (nrow ylabel)))
    (do-vector (i ylabel)
      (when (= (vref ypred i) (vref ylabel i))
        (incf acc)))
    (float (/ acc num))))

;;; roc-curve roc-auc-score
(defun roc-curve (ylabel ypred &optional (positive +1))
  (let ((tpnums nil) ;; true positive nums for each threshold
        (fpnums nil) ;;   will be converted to TPR and FPR
        (thresh nil) ;; threshold list
        (indices (make-indices (nrow ypred))))
    (setf indices (sort-indices indices ypred :predicate #'>))
    ;; for each (unique) threshold, in descending order
    (let ((prev-thresh nil)
          (curr-thresh nil)
          (tpcumsum 0.0) ;; cumsum of true positive
          (fpcumsum 0.0))
      (do-ia-access (indx indices)
        (setf curr-thresh (vref ypred indx))
        (when (or (null prev-thresh) (/= curr-thresh prev-thresh))
          (push tpcumsum tpnums)
          (push fpcumsum fpnums)
          (if (null prev-thresh)
              (push 1.0 thresh)
              (push prev-thresh thresh))
          (setf prev-thresh curr-thresh))
        (if (= (vref ylabel indx) positive)
            (incf tpcumsum 1.0)
            (incf fpcumsum 1.0)))
      (push tpcumsum tpnums)
      (push fpcumsum fpnums)
      (push prev-thresh thresh))
    (setf tpnums (mapcar (lambda (x) (/ x (car tpnums))) tpnums))
    (setf fpnums (mapcar (lambda (x) (/ x (car fpnums))) fpnums))
    (values (nreverse fpnums) (nreverse tpnums) (nreverse thresh))))

(defun auc-score (x y)
  "(x[i], y[i]) being points of trapezoid; where X an Y are lists
Requires: X, Y in ascending order"
  (let ((len (length x))
        (auc 0.0))
    (dotimes (i (- len 1))
      ;; 1/2 * (x[i+1] - x[i]) * (y[i] + y[i+1])
      (incf auc (* 1/2 (- (nth (+ i 1) x) (nth i x)) (+ (nth i y) (nth (+ i 1) y)))))
    auc))

(defun roc-auc-score (ylabel ypred &optional (positive +1))
  (multiple-value-bind (fpr tpr) (roc-curve ylabel ypred positive)
    (auc-score fpr tpr)))
