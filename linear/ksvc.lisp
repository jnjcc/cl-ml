;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Non-Linear Support Vector Machine for classification

(in-package #:cl-ml)

(deftype kernel-type ()
  '(member :linear :poly :rbf :sigmoid))

(defclass kernel-svc (estimator)
  ((yalpha :initform nil :type smatrix :documentation "y[i] * alpha[i]")
   (rho :initform nil)
   (spvectors :initform nil :type smatrix :documentation "support vectors")
   (spvectnum :initform 0)
   (penalty :initform 1 :initarg :penalty :reader penalty
            :documentation "penalty in primal problem, a.k.a, the C parameter")
   (kernel :initform :linear :initarg :kernel :type kernel-type
           :documentation "kernel parameters")
   (gamma :initform 1 :initarg :gamma)
   (coef0 :initform 0 :initarg :coef0)
   (degree :initform 1 :initarg :degree)
   (tolerance :initform 1.0e-3 :initarg :tolerance)
   (epochs :initform 1000 :initarg :epochs)))


(defun linear-kernel (va vb)
  "<a, b>"
  (vdot va vb))

(defun poly-kernel (va vb &optional (gamma 1) (coef0 0) (degree 1))
  "(gamma * <a, b> + coef0)^{degree}"
  (expt (+ (* gamma (vdot va vb)) coef0) degree))

(defun rbf-kernel (va vb &optional (gamma 1))
  "exp(-gamma * ||a - b||^{2})"
  (let ((vminus (v- va vb)))
    (exp (* (- gamma) (vdot vminus vminus)))))

(defun sigmoid-kernel (va vb &optional (gamma 1) (coef0 0))
  "tanh(gamma * <a, b> + coef0)"
  (tanh (+ (* gamma (vdot va vb)) coef0)))

(defmethod kernel-fn-matrix ((ksvc kernel-svc))
  (with-slots (kernel gamma coef0 degree) ksvc
    (ecase kernel
      (:linear (lambda (X i j)
                 (linear-kernel (mrv X i) (mrv X j))))
      (:poly (lambda (X i j)
               (poly-kernel (mrv X i) (mrv X j) gamma coef0 degree)))
      (:rbf (lambda (X i j)
              (rbf-kernel (mrv X i) (mrv X j) gamma)))
      (:sigmoid (lambda (X i j)
                  (sigmoid-kernel (mrv X i) (mrv X j) gamma coef0))))))

(defmethod kernel-fn-vectors ((ksvc kernel-svc))
  (with-slots (kernel gamma coef0 degree) ksvc
    (ecase kernel
      (:linear #'linear-kernel)
      (:poly #'poly-kernel)
      (:rbf #'rbf-kernel)
      (:sigmoid #'sigmoid-kernel))))

;;; There are so many parameters to maintain, we need a class!
(defclass smo-solver ()
  ((penalty :initform 1)
   (tolerance :initform 1.0e-3)
   (m :initform nil)
   (alpha-status :initform nil :type smatrix)
   ;; the diagonal element of kernel matrix
   (qdiag :initform nil :type smatrix)
   (kernel-fn :initform nil :type smatrix)
   ;; the gradient vector
   (grad :initform nil :type smatrix)
   (gbar :initform nil :type smatrix)
   (tau :initform 1.0e-12)
   ;; the shrink indices
   (indices :initform nil :type list)
   (nactive :initform nil)))

(defmethod %update-alpha-status ((smo smo-solver) alpha i)
  (with-slots (penalty alpha-status) smo
    (let ((ai (vref alpha i)))
      (cond
        ((>= ai penalty) (setf (vref alpha-status i) :ubound))
        ((<= ai 0) (setf (vref alpha-status i) :lbound))
        (t (setf (vref alpha-status i) :free))))))

;; TODO: caching and shrinking
(defmethod %qmatrix-row ((smo smo-solver) X y i)
  (with-slots (indices nactive kernel-fn) smo
    (let ((Qrowi (make-vector nactive :initial-element 0))
          (yi (vref y i))
          yj)
      (do-active-alphas (j indices nactive)
        (setf yj (vref y j))
        (setf (vref Qrowi j) (* yi yj (funcall kernel-fn X i j))))
      Qrowi)))

(defmethod initialize-solver ((smo smo-solver) alpha X y pvec ksvc)
  "initialize solver from kernel-svc and training set"
  (with-slots (penalty m alpha-status qdiag grad gbar indices nactive kernel-fn) smo
    (setf m (nrow X))
    (setf kernel-fn (kernel-fn-matrix ksvc))
    (setf alpha-status (make-vector m :initial-element :free))
    (setf qdiag (make-vector m :initial-element 0))
    (do-vector (i qdiag)
      (setf (vref qdiag i) (funcall kernel-fn X i i)))
    (do-vector (i alpha-status)
      (%update-alpha-status smo alpha i))
    (setf indices (make-indices m))
    (setf nactive m)

    ;; initialize gradient
    (setf grad (copy-matrix pvec))
    (setf gbar (make-vector m :initial-element 0))
    (dotimes (i m)
      (let ((status (vref alpha-status i)))
        (unless (eq status :lbound)
          (let ((Qrowi (%qmatrix-row smo X y i))
                (alphi (vref alpha i)))
            (dotimes (j m)
              (incf (vref grad j) (* alphi (vref Qrowi j))))
            (when (eq status :ubound)
              (dotimes (j m)
                (incf (vref gbar j) (* penalty (vref Qrowi j)))))))))))

(defmethod select-working-set ((smo smo-solver) alpha X y)
  "Returns (values T nil nil) if optimal"
  (with-slots (alpha-status qdiag grad indices nactive tolerance tau) smo
    (let ((gmax :ninfty) ;; m(alpha)
          (gmax2 :ninfty) ;; -M(alpha)
          ival jval)
      (do-active-alphas (it indices nactive)
        (if (= (vref y it) +1)
            (unless (eq (vref alpha-status it) :ubound)
              (when (inf>= (- (vref grad it)) gmax)
                (setf gmax (- (vref grad it)))
                (setf ival it)))
            (unless (eq (vref alpha-status it) :lbound)
              (when (inf>= (vref grad it) gmax)
                (setf gmax (vref grad it))
                (setf ival it)))))
      (let ((Qrowi (%qmatrix-row smo X y ival))
            ;; a[t][s], b[t][s]
            quadcoef gradiff
            (objdiff-min :infty)
            (objdiff nil))
        (do-active-alphas (it indices nactive)
          (if (= (vref y it) +1)
              (unless (eq (vref alpha-status it) :lbound)
                (setf gradiff (+ gmax (vref grad it)))
                (when (inf>= (vref grad it) gmax2)
                  (setf gmax2 (vref grad it)))
                (when (> gradiff 0)
                  (setf quadcoef (+ (vref Qdiag ival) (vref Qdiag it)
                                    (* -2.0 (vref y ival) (vref Qrowi it))))
                  (if (> quadcoef 0)
                      (setf objdiff (- (/ (* gradiff gradiff) quadcoef)))
                      (setf objdiff (- (/ (* gradiff gradiff) tau))))
                  (when (inf<= objdiff objdiff-min)
                    (setf jval it)
                    (setf objdiff-min objdiff))))
              (unless (eq (vref alpha-status it) :ubound)
                (setf gradiff (- gmax (vref grad it)))
                (when (inf>= (- (vref grad it)) gmax2)
                  (setf gmax2 (- (vref grad it))))
                (when (> gradiff 0)
                  (setf quadcoef (+ (vref Qdiag ival) (vref Qdiag it)
                                    (* 2.0 (vref y ival) (vref Qrowi it))))
                  (if (> quadcoef 0)
                      (setf objdiff (- (/ (* gradiff gradiff) quadcoef)))
                      (setf objdiff (- (/ (* gradiff gradiff) tau))))
                  (when (inf<= objdiff objdiff-min)
                    (setf jval it)
                    (setf objdiff-min objdiff))))))
        (when (or (< (+ gmax gmax2) tolerance) (= jval -1))
          (return-from select-working-set (values t nil nil)))
        (return-from select-working-set (values nil ival jval))))))

(defmethod reconstruct-gradient ((smo smo-solver) alpha X y)
  (with-slots (grad alpha-status gbar pvec indices nactive tolerance tau) smo
    (when (= nactive (length indices))
      (return-from reconstruct-gradient))
    (do-shrunk-alphas (j indices nactive)
      (setf (vref grad j) (+ (vref gbar j) (vref pvec j))))
    (let ((m (nrow X))
          (nfree 0)
          Qrowi alphi)
      (do-active-alphas (j indices nactive)
        (when (eq (vref alpha-status j) :free)
          (incf nfree)))
      (if (> (* nfree m) (* 2 nactive (- m nactive)))
          (do-shrunk-alphas (i indices nactive)
            (setf Qrowi (%qmatrix-row smo X y i))
            (do-active-alphas (j indices nactive)
              (when (eq (vref alpha-status j) :free)
                (incf (vref grad j) (* (vref alpha j) (vref Qrowi j))))))
          (do-active-alphas (i indices nactive)
            (when (eq (vref alpha-status i) :free)
              (setf Qrowi (%qmatrix-row smo X y i))
              (setf alphi (vref alpha i))
              (do-shrunk-alphas (j indices nactive)
                (incf (vref grad j) (* alphi (vref Qrowi j))))))))))

(defmethod shrink-alphas ((smo smo-solver))
  )

(defmethod alphas-shrunk-p ((smo smo-solver))
  (with-slots (nactive m) smo
    (< nactive m)))

(defmethod unshrink-alphas ((smo smo-solver))
  (with-slots (nactive m) smo
    (setf nactive m)))

(defmethod two-variable-solve ((smo smo-solver) alpha i j Qrowi y)
  "Solve the two-variable sub-problem, update alpha[i], alpha[j]"
  (with-slots (penalty tau grad qdiag) smo
    (let (quadcoef delta)
      (if (/= (vref y i) (vref y j))
          (let (diff)
            (setf quadcoef (+ (vref Qdiag i) (vref Qdiag j) (* 2 (vref Qrowi j))))
            (when (<= quadcoef 0)
              (setf quadcoef tau))
            (setf delta (/ (- (+ (vref grad i) (vref grad j))) quadcoef))
            (setf diff (- (vref alpha i) (vref alpha j)))
            (incf (vref alpha i) delta)
            (incf (vref alpha j) delta)
            (if (> diff 0)
                (when (< (vref alpha j) 0)
                  (setf (vref alpha j) 0)
                  (setf (vref alpha i) diff))
                (when (< (vref alpha i) 0)
                  (setf (vref alpha i) 0)
                  (setf (vref alpha j) (- diff))))
            (if (> diff 0) ;; TODO: diff > C[i] - C[j]
                (when (> (vref alpha i) penalty) ;; alpha[i] > C[i]
                  (setf (vref alpha i) penalty)
                  (setf (vref alpha j) (- penalty diff)))
                (when (> (vref alpha j) penalty)
                  (setf (vref alpha j) penalty)
                  (setf (vref alpha i) (+ penalty diff)))))
          (let (sum)
            (setf quadcoef (+ (vref Qdiag i) (vref Qdiag j) (* -2 (vref Qrowi j))))
            (when (<= quadcoef 0)
              (setf quadcoef tau))
            (setf delta (/ (- (vref grad i) (vref grad j)) quadcoef))
            (setf sum (+ (vref alpha i) (vref alpha j)))
            (decf (vref alpha i) delta)
            (incf (vref alpha j) delta)

            (if (> sum penalty) ;; TODO: sum > C[i]
                (when (> (vref alpha i) penalty)
                  (setf (vref alpha i) penalty)
                  (setf (vref alpha j) (- sum penalty)))
                (when (< (vref alpha j) 0)
                  (setf (vref alpha j) 0)
                  (setf (vref alpha i) sum)))
            (if (> sum penalty) ;; TODO: sum > C[j]
                (when (> (vref alpha j) penalty)
                  (setf (vref alpha j) penalty)
                  (setf (vref alpha i) (- sum penalty)))
                (when (< (vref alpha i) 0)
                  (setf (vref alpha i) 0)
                  (setf (vref alpha j) sum))))))))

(defmethod maintain-gradient ((smo smo-solver) alpha i j Qrowi Qrowj alphi alphj)
  "`alphi' and `alphj' being the old alpha before (two-variable-solve)"
  (with-slots (m penalty alpha-status grad gbar indices nactive) smo
    (let ((deltai (- (vref alpha i) alphi))
          (deltaj (- (vref alpha j) alphj)))
      ;; update gradient
      (do-active-alphas (k indices nactive)
        (incf (vref grad k) (+ (* (vref Qrowi k) deltai) (* (vref Qrowj k) deltaj)))))
    (labels ((ubound-p (idx)
               (eq (vref alpha-status idx) :ubound)))
      ;; update gbar
      (let ((uboundi (ubound-p i))
            (uboundj (ubound-p j)))
        (%update-alpha-status smo alpha i)
        (%update-alpha-status smo alpha j)
        (unless (eq uboundi (ubound-p i))
          (if uboundi
              (dotimes (k m)
                (decf (vref gbar k) (* penalty (vref Qrowi k))))
              (dotimes (k m)
                (incf (vref gbar k) (* penalty (vref Qrowi k))))))
        (unless (eq uboundj (ubound-p j))
          (if uboundj
              (dotimes (k m)
                (decf (vref gbar k) (* penalty (vref Qrowj k))))
              (dotimes (k m)
                (incf (vref gbar k) (* penalty (vref Qrowj k))))))))))

(defmethod calculate-rho ((smo smo-solver) y)
  (with-slots (alpha-status grad indices nactive) smo
    (let ((nfree 0)
          (ygrad 0)
          (ygsum 0)
          (upbound :infty)
          (lobound :ninfty))
      (do-active-alphas (i indices nactive)
        (setf ygrad (* (vref y i) (vref grad i)))
        (ecase (vref alpha-status i)
          (:ubound (if (= (vref y i) -1)
                       (setf upbound (infmin upbound ygrad))
                       (setf lobound (infmax lobound ygrad))))
          (:lbound (if (= (vref y i) +1)
                       (setf upbound (infmin upbound ygrad))
                       (setf lobound (infmax lobound ygrad))))
          (:free (progn
                   (incf nfree)
                   (incf ygsum ygrad)))))
      (if (> nfree 0)
          (/ ygsum nfree)
          (/ (+ upbound lobound) 2)))))

(defmethod %smo-dual ((ksvc kernel-svc) X y)
  "Working Set Selection Using Second Order Information for Training
Support Vector Machines"
  (with-slots (yalpha rho spvectors spvectnum epochs) ksvc
    (let* ((m (nrow X))
           ;; 1/2 * alpha^{T} * Q * alpha + p^{T} * alpha
           (alpha (make-vector m :initial-element 0))
           (pvec (make-vector m :initial-element -1))
           (smo (make-instance 'smo-solver))
           (m (nrow X))
           (iters 0)
           ;; shrinking counter; working set {i, j}
           shrink-counter wi wj)
      (initialize-solver smo alpha X y pvec ksvc)

      (setf shrink-counter (min m 1000))
      (dotimes (epoch epochs)
        (setf iters epoch)
        (decf shrink-counter)
        (when (= shrink-counter 0)
          (setf shrink-counter (min m 1000)))
        (let ((optimal nil))
          (multiple-value-setq (optimal wi wj) (select-working-set smo alpha X y))
          (when optimal
            (reconstruct-gradient smo alpha X y)
            (unshrink-alphas smo)
            (multiple-value-setq (optimal wi wj) (select-working-set smo alpha X y)))
          (if optimal
              ;; break out of iterations
              (return)
              ;; do shrinking the next iteration
              (setf shrink-counter 1)))
        (let ((Qrowi (%qmatrix-row smo X y wi))
              (Qrowj (%qmatrix-row smo X y wj))
              (alphi (vref alpha wi))
              (alphj (vref alpha wj)))
          (two-variable-solve smo alpha wi wj Qrowi y)
          ;; TODO: here `Qrowi' and `Qrowj' must be full!
          (maintain-gradient smo alpha wi wj Qrowi Qrowj alphi alphj)))
      (when (>= iters epochs)
        ;; not optimal
        (when (alphas-shrunk-p smo)
          (reconstruct-gradient smo alpha X y)
          (unshrink-alphas smo)))

      ;; decision function: rho and alpha's
      (setf rho (calculate-rho smo y))
      (setf spvectnum 0)
      (do-vector (i alpha)
        (setf (vref alpha i) (* (vref y i) (vref alpha i)))
        (when (> (abs (vref alpha i)) 0)
          (incf spvectnum)))
      (setf spvectors (make-matrix spvectnum (ncol X) :initial-element 0))
      (setf yalpha (make-vector spvectnum :initial-element 0))
      (let ((j 0))
        (do-vector (i alpha)
          (when (> (abs (vref alpha i)) 0)
            (setf (mrv spvectors j) (mrv X i))
            (setf (vref yalpha j) (vref alpha i))
            (incf j)))))))

(defmethod fit ((ksvc kernel-svc) X &optional y)
  (declare (type smatrix X))
  (%smo-dual ksvc X y))

(defmethod predict-one ((ksvc kernel-svc) X i)
  (with-slots (yalpha rho spvectors spvectnum) ksvc
    (let ((kfn (kernel-fn-vectors ksvc))
          (fval 0))
      (dotimes (j spvectnum)
        (incf fval (* (vref yalpha j) (funcall kfn (mrv X i) (mrv spvectors j)))))
      (decf fval rho)
      (values (signfn fval) fval))))

(defmethod predict ((ksvc kernel-svc) X)
  (let ((pred (make-vector (nrow X) :initial-element +1)))
    (do-matrix-row (i X)
      (setf (vref pred i) (predict-one ksvc X i)))
    pred))
