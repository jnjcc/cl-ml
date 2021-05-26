;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; XGBoost Decision Tree impurity criterion for node split

(in-package #:cl-ml)

(defclass criterion-xgb (criterion-base)
  ;; `gamma' * J + 1/2 * `lambd' * sum(c^{2})
  ((gamma :initform 0 :initarg :gamma)
   (lambd :initform 1 :initarg :lambd)
   (pgsum :initform 0) ;; sum of first-order derivative, for parent
   (phsum :initform 0)
   (lgsum :initform 0)
   (lhsum :initform 0) ;; sum of second-order derivative, for left child
   (rgsum :initform 0)
   (rhsum :initform 0)))

(defmethod init-position-and-summary ((cxgb criterion-xgb) xframe)
  (with-slots (sbeg send spos pgsum phsum lgsum lhsum rgsum rhsum) cxgb
    (do ((idx sbeg (1+ idx)))
        ((>= idx send))
      (incf pgsum (get-first-order xframe idx))
      (incf phsum (get-second-order xframe idx)))
    (setf spos sbeg)
    (setf lgsum 0)
    (setf lhsum 0)
    (setf rgsum pgsum)
    (setf rhsum phsum)))

(defmethod const-label-p ((cxgb criterion-xgb) xframe)
  "`xframe' is type of xgb-frame"
  (with-slots (sbeg send gamma lambd pgsum phsum) cxgb
    (let ((g0 (get-first-order xframe sbeg))
          (h0 (get-second-order xframe sbeg)))
      (do ((idx sbeg (1+ idx)))
          ((>= idx send))
        (unless (and (float= g0 (get-first-order xframe idx))
                     (float= h0 (get-second-order xframe idx)))
          (return-from const-label-p (values nil nil))))
      ;; c* = -G / (H + lambda)
      (return-from const-label-p (values t (- (/ pgsum (+ phsum lambd))))))))

(defmethod reset-position ((cxgb criterion-xgb))
  (with-slots (sbeg spos pgsum phsum lgsum lhsum rgsum rhsum) cxgb
    (setf spos sbeg)
    (setf lgsum 0)
    (setf lhsum 0)
    (setf rgsum pgsum)
    (setf rhsum phsum)))

(defmethod update-position ((cxgb criterion-xgb) xframe newpos)
  (with-slots (spos pgsum phsum lgsum lhsum rgsum rhsum) cxgb
    (do ((idx spos (1+ idx)))
        ((>= idx newpos))
      (incf lgsum (get-first-order xframe idx))
      (incf lhsum (get-second-order xframe idx)))
    (setf rgsum (- pgsum lgsum))
    (setf rhsum (- phsum lhsum))
    (setf spos newpos)))

(defmethod proxy-impurity-gain ((cxgb criterion-xgb))
  (with-slots (lgsum lhsum rgsum rhsum lambd) cxgb
    ;; Gl^{2} / (Hl + lambda) + Gr^{2} / (Hr + lambda)
    (+ (/ (* lgsum lgsum) (+ lhsum lambd))
       (/ (* rgsum rgsum) (+ rhsum lambd)))))

(defmethod node-impurity ((cxgb criterion-xgb))
  (with-slots (pgsum phsum lambd) cxgb
    ;; G^{2} / (H + lambd)
    (/ (* pgsum pgsum) (+ phsum lambd))))

(defmethod children-impurity ((cxgb criterion-xgb) xframe)
  (declare (ignore xframe))
  (with-slots (lgsum lhsum rgsum rhsum lambd) cxgb
    (values (/ (* lgsum lgsum) (+ lhsum lambd))
            (/ (* rgsum rgsum) (+ rhsum lambd)))))

(defmethod impurity-gain ((cxgb criterion-xgb) impurity xframe)
  (with-slots (gamma) cxgb
    (multiple-value-bind (limp rimp) (children-impurity cxgb xframe)
      (- (* 1/2 (+ limp rimp (- impurity)))
         gamma))))

(defmethod node-value ((cxgb criterion-xgb))
  (with-slots (pgsum phsum lambd) cxgb
    (- (/ pgsum (+ phsum lambd)))))

;;; make-* functions
(defun make-criterion (type sbeg send &optional (gamma 0) (lambd 1))
  (ecase type
    (:gini (make-instance 'criterion-gini :sbeg sbeg :send send))
    (:entropy (make-instance 'criterion-entropy :sbeg sbeg :send send))
    (:mse (make-instance 'criterion-mse :sbeg sbeg :send send))
    (:xgb (make-instance 'criterion-xgb :sbeg sbeg :send send :gamma gamma :lambd lambd))))
