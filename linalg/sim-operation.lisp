;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: matrix properties and operations

(in-package #:cl-ml/linalg)

(defmethod mdet ((ma smatrix))
  (let ((copy (copy-matrix ma))
        (det 1))
    (gaussian-on-matrix copy)
    (do-matrix-row (i copy)
      (setf det (* det (mref copy i i))))
    det))

(defun %ensure-dimension-match (ma mb)
  (when (or (/= (nrow ma) (nrow mb))
            (/= (ncol ma) (ncol mb)))
    (error "dimension not match: (~A, ~A), (~A, ~A)"
           (nrow ma) (ncol ma) (nrow mb) (ncol mb))))

(defun %ensure-dimension-mult (ma mb)
  (when (/= (ncol ma) (nrow mb))
    (error "dimension not match for mult: (~A, ~A), (~A, ~A)"
           (nrow ma) (ncol ma) (nrow mb) (ncol mb))))

(defmethod m+= ((ma smatrix) (mb smatrix))
  (%ensure-dimension-match ma mb)
  (do-matrix (i j ma)
    (incf (mref ma i j) (mref mb i j))))

(defmethod m+= ((ma smatrix) (mb number))
  (do-matrix (i j ma)
    (incf (mref ma i j) mb)))

(defmethod m+ ((ma smatrix) mb &rest mrest)
  "smatrix + smatrix / smatrix + number"
  (let ((add (copy-matrix ma)))
    (m+= add mb)
    (dolist (mr mrest)
      (m+= add mr))
    add))

(defmethod m+ ((ma number) (mb smatrix) &rest mrest)
  (let ((add (copy-matrix mb)))
    (m+= add ma)
    (dolist (mr mrest)
      (m+= add mr))
    add))

(defmethod m+ ((ma number) (mb number) &rest mrest)
  (let ((sum (+ ma mb)))
    (if mrest
        (apply #'m+ (cons sum mrest))
        sum)))

(defmethod m-= ((ma smatrix) (mb smatrix))
  (%ensure-dimension-match ma mb)
  (do-matrix (i j ma)
    (decf (mref ma i j) (mref mb i j))))

(defmethod m-= ((ma smatrix) (mb number))
  (do-matrix (i j ma)
    (decf (mref ma i j) mb)))

(defmethod m- ((ma smatrix) mb &rest mrest)
  "smatrix - smatrix / smatrix - number"
  (let ((sub (copy-matrix ma)))
    (m-= sub mb)
    (dolist (mr mrest)
      (m-= sub mr))
    sub))

(defmethod m- ((ma number) (mb smatrix) &rest mrest)
  (let ((sub (copy-matrix mb)))
    (do-matrix (i j sub)
      (setf (mref sub i j) (- ma (mref mb i j))))
    (dolist (mr mrest)
      (m-= sub mr))
    sub))

(defmethod m*= ((ma smatrix) (mb smatrix))
  (%ensure-dimension-mult ma mb)
  (unless (= (ncol mb) (nrow mb))
    (error "m*= not applicable: (~A, ~A) (~A, ~A)" (nrow ma) (ncol ma)
           (nrow mb) (ncol mb)))
  (let ((vrow (make-row-vector (ncol ma))))
    (do-matrix-row (i ma)
      (fill-matrix vrow (mrv ma i))
      (do-matrix-col (j ma)
        (setf (mref ma i j) 0)
        (dotimes (k (ncol ma))
          (incf (mref ma i j) (* (vref vrow k) (mref mb k j))))))))

(defmethod m*= ((ma smatrix) (mb number))
  (do-matrix (i j ma)
    (setf (mref ma i j) (* (mref ma i j) mb))))

(defun %prod-out-place (ma mb)
  (let ((prod (make-matrix (nrow ma) (ncol mb) :initial-element 0)))
    (do-matrix (i j prod)
      (dotimes (k (ncol ma))
        (incf (mref prod i j) (* (mref ma i k) (mref mb k j)))))
    prod))

(defmethod %prod-try-in-place ((ma smatrix) (mb smatrix))
  "do matrix production in-place as much as possible"
  (let ((prod ma))
    (if (= (nrow mb) (ncol mb))
        (m*= ma mb)
        (setf prod (%prod-out-place ma mb)))
    prod))

(defmethod %prod-try-in-place ((ma smatrix) (mb number))
  (m*= ma mb))

(defmethod m* ((ma smatrix) (mb smatrix) &rest mrest)
  "do matrix production in-place as much as possible"
  (let ((prod (%prod-out-place ma mb)))
    (dolist (mr mrest)
      (setf prod (%prod-try-in-place prod mr)))
    prod))

(defmethod m* ((ma smatrix) (mb number) &rest mrest)
  (let ((prod (make-matrix (nrow ma) (ncol ma))))
    (do-matrix (i j prod)
      (setf (mref prod i j) (* (mref ma i j) mb)))
    (dolist (mr mrest)
      (setf prod (%prod-try-in-place prod mr)))
    prod))

(defmethod m* ((ma number) (mb smatrix) &rest mrest)
  (let ((prod (make-matrix (nrow mb) (ncol mb))))
    (do-matrix (i j prod)
      (setf (mref prod i j) (* (mref mb i j) ma)))
    (dolist (mr mrest)
      (setf prod (%prod-try-in-place prod mr)))
    prod))

(defmethod m* ((ma number) (mb number) &rest mrest)
  (let ((mult (* ma mb)))
    (if mrest
        (apply #'m* (cons mult mrest))
        mult)))

(defmethod m/ ((ma smatrix) (mb smatrix))
  (%ensure-dimension-match ma mb)
  (let ((div (make-matrix (nrow ma) (ncol ma))))
    (do-matrix (i j div)
      (setf (mref div i j) (/ (mref ma i j) (mref mb i j))))
    div))

(defmethod m/= ((ma smatrix) (mb smatrix))
  (%ensure-dimension-match ma mb)
  (do-matrix (i j ma)
    (setf (mref ma i j) (/ (mref ma i j) (mref mb i j)))))

(defmethod m/ ((ma smatrix) (mb number))
  (let ((div (make-matrix (nrow ma) (ncol ma))))
    (do-matrix (i j div)
      (setf (mref div i j) (/ (mref ma i j) mb)))
    div))

(defmethod m/= ((ma smatrix) (mb number))
  (do-matrix (i j ma)
    (setf (mref ma i j) (/ (mref ma i j) mb))))

(defmethod mt ((ma smatrix))
  (%make-transpose-matrix ma))

(defmethod minv ((ma smatrix))
  (%gauss-jordan ma))

(defmethod mmap ((ma smatrix) op)
  (do-matrix (i j ma)
    (setf (mref ma i j) (funcall op (mref ma i j))))
  ma)

(defmethod mreduce ((ma smatrix) op &key (axis 0) (key #'identity) (initial-value 0))
  (let ((reduced initial-value))
    (cond
      ((null axis) (do-matrix (i j ma)
                     (setf reduced (funcall op reduced (funcall key (mref ma i j))))))
      ;; on all elements of the same column
      ((= axis 0)
       (if (= (ncol ma) 1)
           (do-matrix (i j ma)
             (setf reduced (funcall op reduced (funcall key (mref ma i j)))))
           (progn
             (setf reduced (make-row-vector (ncol ma)
                                            :initial-element initial-value))
             (do-matrix (i j ma)
               (setf (vref reduced j) (funcall op (vref reduced j)
                                               (funcall key (mref ma i j))))))))
      ;; on all elements of the same row
      ((= axis 1)
       (if (= (nrow ma) 1)
           (do-matrix (i j ma)
             (setf reduced (funcall op reduced (funcall key (mref ma i j)))))
           (progn
             (setf reduced (make-vector (nrow ma) :initial-element initial-value))
             (do-matrix (i j ma)
               (setf (vref reduced i) (funcall op (vref reduced i)
                                               (funcall key (mref ma i j)))))))))
    reduced))

(defun %ensure-two-vectors (vx vy)
  (unless (and (svector-type vx) (svector-type vy))
    (error "vectors expected: (~A, ~A), (~A, ~A)"
           (nrow vx) (ncol vx) (nrow vy) (ncol vy))))

(defun %ensure-one-vector (vx)
  (unless (svector-type vx)
    (error "vector expected: (~A, ~A)" (nrow vx) (ncol vx))))

(defun %ensure-size-match (vx vy)
  (unless (= (* (nrow vx) (ncol vx)) (* (nrow vy) (ncol vy)))
    (error "vector size dimatch: (~A, ~A), (~A, ~A)"
           (nrow vx) (ncol vx) (nrow vy) (ncol vy))))

(defmethod v+ ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (m+ vx vy))

(defmethod v+= ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (m+= vx vy))

(defmethod v+ ((vx smatrix) (vy number))
  (%ensure-one-vector vx)
  (m+ vx vy))

(defmethod v+= ((vx smatrix) (vy number))
  (%ensure-one-vector vx)
  (m+= vx vy))

(defmethod v+ ((vx number) (vy smatrix))
  (%ensure-one-vector vy)
  (m+ vx vy))

(defmethod v- ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (m- vx vy))

(defmethod v-= ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (m-= vx vy))

(defmethod v- ((vx smatrix) (vy number))
  (%ensure-one-vector vx)
  (m- vx vy))

(defmethod v-= ((vx smatrix) (vy number))
  (%ensure-one-vector vx)
  (m-= vx vy))

(defmethod v- ((vx number) (vy smatrix))
  (%ensure-one-vector vy)
  (m- vx vy))

(defmethod v* ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (%ensure-dimension-match vx vy)
  (let ((prod (make-matrix (nrow vx) (ncol vx))))
    (do-matrix (i j prod)
      (setf (mref prod i j) (* (mref vx i j) (mref vy i j))))
    prod))

(defmethod v*= ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (%ensure-dimension-match vx vy)
  (do-matrix (i j vx)
    (setf (mref vx i j) (* (mref vx i j) (mref vy i j)))))

(defmethod v* ((vx smatrix) (vy number))
  "scalar multiplication of matrices"
  (%ensure-one-vector vx)
  (m* vx vy))

(defmethod v*= ((vx smatrix) (vy number))
  (%ensure-one-vector vx)
  (m*= vx vy))

(defmethod v* ((vx number) (vy smatrix))
  (%ensure-one-vector vy)
  (m* vx vy))

(defmethod v/ ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (m/ vx vy))

(defmethod v/= ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (m/= vx vy))

(defmethod v/ ((vx smatrix) (vy number))
  (%ensure-one-vector vx)
  (m/ vx vy))

(defmethod v/= ((vx smatrix) (vy number))
  (%ensure-one-vector vx)
  (m/= vx vy))

(defmethod vdot ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (%ensure-size-match vx vy)
  (let ((dot 0))
    (do-vector (i vx)
      (incf dot (* (vref vx i) (vref vy i))))
    dot))

(defmethod vouter ((vx smatrix) (vy smatrix))
  (%ensure-two-vectors vx vy)
  (let ((xtype (svector-type vx))
        (ytype (svector-type vy)))
    (cond
      ((and (eq xtype :column) (eq ytype :row)) (m* vx vy))
      ((and (eq xtype :column) (eq ytype :column)) (m* vx (mt vy)))
      ((and (eq xtype :row) (eq ytype :row)) (m* (mt vx) vy))
      ((and (eq xtype :row) (eq ytype :column) (m* (mt vx) (mt vy)))))))

(defmethod vmap ((vx smatrix) op)
  (%ensure-one-vector vx)
  (do-matrix (i j vx)
    (setf (mref vx i j) (funcall op (mref vx i j))))
  vx)

(defmethod vreduce ((vx smatrix) op &key (initial-value 0))
  (%ensure-one-vector vx)
  (let ((result initial-value))
    (do-matrix (i j vx)
      (setf result (funcall op result (mref vx i j))))
    result))
