;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Gradient Boosting Lossess

(in-package #:cl-ml)

(deftype gbloss-type ()
  ;; squared loss, exp
  '(member :square :exp))

(defclass gbloss-base ()
  ;; gradient boosting loss function
  ((init-estimator :initform nil :documentation "initial estimator to predict X")))

(defgeneric init-fit-and-predict (gbase y)
  (:documentation "initial \hat{y} to minimize loss function"))
(defgeneric init-predict (gbase X))

(defgeneric negative-gradient (gbase y f)
  (:documentation "`y' as the original label; `f' as the additive model by this stage"))

(defgeneric update-leaf-and-predict (gbase dtreg X y residual fx eta)
  (:documentation "update leaf region, and update the additive model `fx'"))

;;; Least Squares Error
(defclass gbloss-least (gbloss-base)
  ())

(defmethod init-fit-and-predict ((gbleast gbloss-least) y)
  (with-slots (init-estimator) gbleast
    (setf init-estimator (mmean y :axis nil))
    (make-vector (nrow y) :initial-element init-estimator)))

(defmethod init-predict ((gbleast gbloss-least) X)
  (with-slots (init-estimator) gbleast
    (make-vector (nrow X) :initial-element init-estimator)))

(defmethod negative-gradient ((gbleast gbloss-least) y f)
  (m- y f))

(defmethod update-leaf-and-predict ((gbleast gbloss-least) dtreg X y residual fx eta)
  "Least Squares Error does not need to update leaf values"
  (let ((curpred (predict dtreg X)))
    (m*= curpred eta)
    (m+= fx curpred))
  fx)

;;; Least Absolute Deviation
(defclass gbloss-lad (gbloss-base)
  ())

(defmethod init-fit-and-predict ((gblad gbloss-lad) y)
  (with-slots (init-estimator) gblad
    (setf init-estimator (mquantile y 0.5))
    (make-vector (nrow y) :initial-element init-estimator)))

(defmethod init-predict ((gblad gbloss-lad) X)
  (with-slots (init-estimator) gblad
    (make-vector (nrow X) :initial-element init-estimator)))

(defmethod negative-gradient ((gblad gbloss-lad) y f)
  (let ((grad (make-vector (nrow y) :initial-element 1.0)))
    (do-vector (i y)
      (when (<= (vref y i) (vref f i))
        (setf (vref grad i) -1.0)))
    grad))

(defmethod update-leaf-and-predict ((gblad gbloss-lad) dtreg X y residual fx eta)
  (let ((leaf-regs (leaf-regions dtreg X))
        (yv (make-matrix-view y))
        (fxv (make-matrix-view fx))
        cur-leaf newval)
    (dolist (pair leaf-regs)
      (setf cur-leaf (car pair))
      (update-matrix-view yv :row-view (cdr pair))
      (update-matrix-view fxv :row-view (cdr pair))
      (setf newval (mquantile (m- yv fxv) 0.5))
      (update-dtree-data-value cur-leaf newval)
      (m+= fxv (* newval eta))))
  fx)

;;; Huber Loss
(defclass gbloss-huber (gbloss-base)
  ((alpha :initform 0.9 :initarg :alpha :documentation "quantile at which to extract score")
   (delta :initform nil)))

(defmethod init-fit-and-predict ((ghuber gbloss-huber) y)
  (with-slots (init-estimator) ghuber
    (setf init-estimator (mquantile y 0.5))
    (make-vector (nrow y) :initial-element init-estimator)))

(defmethod init-predict ((ghuber gbloss-huber) X)
  (with-slots (init-estimator) ghuber
    (make-vector (nrow X) :initial-element init-estimator)))

(defmethod negative-gradient ((ghuber gbloss-huber) y f)
  (let ((diff (make-vector (nrow y))))
    ;; diff = |y - f|
    (do-vector (i y)
      (if (>= (vref y i) (vref f i))
          (setf (vref diff i) (- (vref y i) (vref f i)))
          (setf (vref diff i) (- (vref f i) (vref y i)))))
    (with-slots (alpha delta) ghuber
      (setf delta (mquantile diff alpha))
      (do-vector (i diff)
        (if (<= (vref diff i) delta)
            (setf (vref diff i) (- (vref y i) (vref f i)))
            (setf (vref diff i) (* delta (signfn (vref diff i)))))))
    diff))

(defmethod update-leaf-and-predict ((ghuber gbloss-huber) dtreg X y residual fx eta)
  (error "TODO to implement..."))

;;; Binomial deviance
(defclass gbloss-binom (gbloss-base)
  ())

(defmethod init-fit-and-predict ((gbinom gbloss-binom) y)
  (with-slots (init-estimator) gbinom
    (let* ((class-probs (density (mbincount y)))
           (class1 (binvalue class-probs 1)))
      (setf init-estimator (log (/ class1 (- 1 class1))))
      (make-vector (nrow y) :initial-element init-estimator))))

(defmethod init-predict ((gbinom gbloss-binom) X)
  (with-slots (init-estimator) gbinom
    (make-vector (nrow X) :initial-element init-estimator)))

(defmethod negative-gradient ((gbinom gbloss-binom) y f)
  (mmap f #'sigmoid)
  (m- y f))

(defmethod update-leaf-and-predict ((gbinom gbloss-binom) dtreg X y residual fx eta)
  (let ((leaf-regs (leaf-regions dtreg X))
        (yv (make-matrix-view y))
        (fxv (make-matrix-view fx))
        (rv (make-matrix-view residual))
        cur-leaf newval)
    (dolist (pair leaf-regs)
      (setf cur-leaf (car pair))
      (update-matrix-view yv :row-view (cdr pair))
      (update-matrix-view rv :row-view (cdr pair))
      (update-matrix-view fxv :row-view (cdr pair))
      (setf newval 0.0)
      ;; (y - residual) * (1 - y + residual)
      (do-vector (i yv)
        (incf newval (* (- (vref yv i) (vref rv i))
                        (+ 1 (- (vref yv i)) (vref rv i)))))
      (unless (float= newval 0.0)
        (setf newval (/ (msum rv) newval)))
      (update-dtree-data-value cur-leaf newval)
      (m+= fxv (* newval eta))))
  fx)

;;; make-* functions
(defun make-gbloss (type &optional (alpha 0.9))
  (ecase type
    (:least (make-instance 'gbloss-least))
    (:lad (make-instance 'gbloss-lad))
    (:huber (make-instance 'gbloss-huber :alpha alpha))
    (:deviance (make-instance 'gbloss-binom))))
