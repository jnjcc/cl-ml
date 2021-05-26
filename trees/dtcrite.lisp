;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Decision Tree impurity criterion for node split

(in-package #:cl-ml)

(defclass criterion-base ()
  ((sbeg :initform 0 :initarg :sbeg
         :documentation "the first sample to be used on this node")
   (send :initform nil :initarg :send)
   (spos :initform 0
         :documentation "[`sbeg', `spos') on left; [`spos', `send') on right")))

(defgeneric init-position-and-summary (cbase dframe)
  (:documentation "init and summarize statistics on this node from `dframe'"))
(defgeneric const-label-p (cbase dframe)
  (:documentation "(values const-label-p label-class)"))
(defgeneric reset-position (cbase)
  (:documentation "reset split point at sample `sbeg' for a new feature"))
(defgeneric update-position (cbase dframe newpos)
  (:documentation "set split point at sample `newpos' of `dframe': [sbeg, newpos)"))
(defgeneric proxy-impurity-gain (cbase)
  (:documentation "the proxy impurity which neglects all constant terms"))
(defgeneric node-impurity (cbase))
(defgeneric children-impurity (cbase dframe))
(defgeneric impurity-gain (cbase impurity dframe)
  (:documentation "the actual (unweighted) impurity gain when a split occurs:
impurity - Dl / D * left_impurity - Dr / D * right_impurity
the final weight as (D / Dtotal)"))
(defgeneric node-value (cbase)
  (:documentation "(values label bincount) for classifier or (values pred nil)
for regressor"))

(defmethod impurity-gain ((cbase criterion-base) impurity dframe)
  (with-slots (sbeg send spos) cbase
    (let ((psize (- send sbeg))
          (lsize (- spos sbeg))
          (rsize (- send spos)))
      (multiple-value-bind (limp rimp) (children-impurity cbase dframe)
        (- impurity (* (/ lsize psize) limp) (* (/ rsize psize) rimp))))))

;;; regressor: mean suqared error impurity criterion
(defclass criterion-mse (criterion-base)
  ((psum :initform 0)
   (sqsum :initform 0) ;; squared parent sum
   (lsum :initform 0)
   (rsum :initform 0)))

(defmethod const-label-p ((cmse criterion-mse) dframe)
  (with-slots (sbeg send) cmse
    (let ((label0 (get-label dframe sbeg)))
      (do ((i sbeg (1+ i)))
          ((>= i send))
        (unless (float= label0 (get-label dframe i))
          (return-from const-label-p (values nil nil))))
      (return-from const-label-p (values t label0)))))

(defmethod init-position-and-summary ((cmse criterion-mse) dframe)
  (with-slots (sbeg send spos psum sqsum lsum rsum) cmse
    (let ((cur nil))
      (do ((idx sbeg (+ idx 1)))
          ((>= idx send))
        (setf cur (get-label dframe idx))
        (incf psum cur)
        (incf sqsum (* cur cur))))
    (setf spos sbeg)
    (setf lsum 0)
    (setf rsum psum)))

(defmethod reset-position ((cmse criterion-mse))
  (with-slots (sbeg spos psum lsum rsum) cmse
    (setf spos sbeg)
    (setf lsum 0)
    (setf rsum psum)))

(defmethod update-position ((cmse criterion-mse) dframe newpos)
  (with-slots (spos psum lsum rsum) cmse
    (do ((idx spos (+ idx 1)))
        ((>= idx newpos))
      (incf lsum (get-label dframe idx)))
    (setf rsum (- psum lsum))
    (setf spos newpos)))

(defmethod proxy-impurity-gain ((cmse criterion-mse))
  (with-slots (sbeg send spos lsum rsum) cmse
    (+ (/ (* lsum lsum) (- spos sbeg))
       (/ (* rsum rsum) (- send spos)))))

(defmethod node-impurity ((cmse criterion-mse))
  "E[y2] - (E[y])2"
  (with-slots (psum sqsum sbeg send) cmse
    (let ((sz (- send sbeg)))
      (- (/ sqsum sz) (* (/ psum sz) (/ psum sz))))))

(defmethod children-impurity ((cmse criterion-mse) dframe)
  (with-slots (sqsum lsum rsum sbeg send spos) cmse
    (let ((lsqsum 0)
          (rsqsum 0)
          (lsz (- spos sbeg))
          (rsz (- send spos)))
      (do ((idx sbeg (+ idx 1)))
          ((>= idx spos))
        (incf lsqsum (* (get-label dframe idx) (get-label dframe idx))))
      (setf rsqsum (- sqsum lsqsum))
      (values (- (/ lsqsum lsz) (* (/ lsum lsz) (/ lsum lsz)))
              (- (/ rsqsum rsz) (* (/ rsum rsz) (/ rsum rsz)))))))

(defmethod node-value ((cmse criterion-mse))
  (with-slots (psum sbeg send) cmse
    (values (/ psum (- send sbeg)) nil)))

;;; classifier: classifier impurity criterion
(defclass criterion-classifier (criterion-base)
  ((pbins :initform nil)
   (lbins :initform nil)
   (rbins :initform nil)))

(defmethod const-label-p ((crclf criterion-classifier) dframe)
  (with-slots (sbeg send) crclf
    (let ((class0 (get-label dframe sbeg)))
      (do ((i sbeg (1+ i)))
          ((>= i send))
        (unless (eq class0 (get-label dframe i))
          (return-from const-label-p (values nil nil))))
      (return-from const-label-p (values t class0)))))

(defmethod init-position-and-summary ((crclf criterion-classifier) dframe)
  (with-slots (sbeg send spos pbins lbins rbins) crclf
    (setf pbins (count-label dframe sbeg send))
    (setf spos sbeg)
    (setf lbins nil)
    (setf rbins (copy-tree pbins))))

(defmethod reset-position ((crclf criterion-classifier))
  (with-slots (sbeg spos pbins lbins rbins) crclf
    (setf spos sbeg)
    (setf lbins nil)
    (setf rbins (copy-tree pbins))))

(defmethod update-position ((crclf criterion-classifier) dframe newpos)
  (with-slots (spos lbins rbins) crclf
    (let ((cur nil))
      (do ((idx spos (+ idx 1)))
          ((>= idx newpos))
        (setf cur (get-label dframe idx))
        (binincf lbins cur)
        (bindecf rbins cur)))
    (setf spos newpos)))

(defmethod node-value ((crclf criterion-classifier))
  (with-slots (pbins) crclf
    (values (argmax pbins) pbins)))

;;; entropy impurity criterion
(defclass criterion-entropy (criterion-classifier)
  ())

(defmethod proxy-impurity-gain ((crent criterion-entropy))
  (with-slots (sbeg send spos lbins rbins) crent
    (let* ((lsize (- spos sbeg))
           (rsize (- send spos))
           (lprob (probability lbins lsize))
           (rprob (probability rbins rsize)))
      (+ (- (* lsize (entropy-list lprob)))
         (- (* rsize (entropy-list rprob)))))))

(defmethod node-impurity ((crent criterion-entropy))
  (with-slots (sbeg send pbins) crent
    (entropy-list (probability pbins (- send sbeg)))))

(defmethod children-impurity ((crent criterion-entropy) dframe)
  (declare (ignore dframe))
  (with-slots (sbeg send spos lbins rbins) crent
    (let* ((lsize (- spos sbeg))
           (rsize (- send spos))
           (lprob (probability lbins lsize))
           (rprob (probability rbins rsize)))
      (values (entropy-list lprob) (entropy-list rprob)))))

;;; gini impurity criterion
(defclass criterion-gini (criterion-classifier)
  ())

(defmethod proxy-impurity-gain ((cgini criterion-gini))
  (with-slots (sbeg send spos lbins rbins) cgini
    (let* ((lsize (- spos sbeg))
           (rsize (- send spos))
           (lprob (probability lbins lsize))
           (rprob (probability rbins rsize)))
      (+ (- (* lsize (gini-list lprob)))
         (- (* rsize (gini-list rprob)))))))

(defmethod node-impurity ((cgini criterion-gini))
  (with-slots (sbeg send pbins) cgini
    (gini-list (probability pbins (- send sbeg)))))

(defmethod children-impurity ((cgini criterion-gini) dframe)
  (declare (ignore dframe))
  (with-slots (sbeg send spos lbins rbins) cgini
    (let* ((lsize (- spos sbeg))
           (rsize (- send spos))
           (lprob (probability lbins lsize))
           (rprob (probability rbins rsize)))
      (values (gini-list lprob) (gini-list rprob)))))
