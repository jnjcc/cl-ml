;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Neural Network > Network architecture > Multilayer perceptron

(in-package #:cl-ml)

;;; tf.contrib.learn.DNNClassifier
(defclass multilayer-perceptron (estimator)
  ((hidden-units :initform '(16) :initarg :hidden-units :type list
                 :documentation "list of number of units per hidden layer, with
length being (nlayers - 2): excluding input layer and output layer")
   (layer-nums :initform nil :reader layer-nums)
   (activation :initform :relu :initarg :activation
               :documentation "activation function")
   (optimizer :initform :sgd :initarg :optimizer
              :documentation "optimizer")
   (batch-size :initform 32 :initarg :batch-size)
   (epochs :initform 200 :initarg :epochs)
   (learn-rate :initform 0.01 :initarg :learn-rate)
   (mWi :initform nil :documentation "weights connecting input and 1st hidden layer")
   (mBi :initform nil)
   ;; NROW being hidden units of prev layer; NCOL being of next layer
   (mWhs :initform nil :documentation "list of weight matrices per layer, with
length being (nlayers - 1): exluding output layer")
   ;; row vector, with NCOL being hidden units of next layer
   (mBhs :initform nil)
   (mZcache :initform nil :documentation "cached Z for backward propagation")
   (mAcache :initform nil :documentation "cached A for backward propagation")
   (mWo :initform nil :documentation "connecting last hidden and outputy layer")
   (mBo :initform nil)))

(defmethod initialize-instance :after ((mlp multilayer-perceptron) &rest args)
  "skip input and output layer, as we do not know the input/output shape yet..."
  (declare (ignore args))
  (with-slots (hidden-units layer-nums activation mWhs mBhs mBi) mlp
    (setf layer-nums (+ (length hidden-units) 2))
    (setf activation (make-activation activation))
    (setf mBi (make-rand-row-vector (car hidden-units) 1.0))
    (dotimes (i (- layer-nums 3))
      (push (make-rand-matrix (nth i hidden-units) (nth (+ i 1) hidden-units) 1.0)
            mWhs)
      (push (make-rand-row-vector (nth (+ i 1) hidden-units) 1.0) mBhs))
    (setf mWhs (nreverse mWhs))
    (setf mBhs (nreverse mBhs))))

(defmethod input-layer ((mlp multilayer-perceptron))
  0)

(defmethod last-hidden-layer ((mlp multilayer-perceptron))
  (with-slots (layer-nums) mlp
    (- layer-nums 2)))

(defmethod output-layer ((mlp multilayer-perceptron))
  (with-slots (layer-nums) mlp
    (- layer-nums 1)))

(defmethod layer-weights ((mlp multilayer-perceptron) i)
  "input layer being 0, has no associated weights; output layer being (layer-nums - 1)"
  (with-slots (layer-nums mWi mBi mWhs mBhs mWo mBo) mlp
    (cond
      ((or (< i 0) (> (+ i 1) layer-nums)) (error "no such layer ~A in MLP" (+ i 1)))
      ((= i 0) (error "no weights associated with layer 0"))
      ((= i 1) (values mWi mBi))
      ((= (+ i 1) layer-nums) (values mWo mBo)) ;; output layer
      (t (values (nth (- i 2) mWhs) (nth (- i 2) mBhs))))))

(defmacro do-layers ((i mlp) &body body)
  "we have no interest in the input layer"
  `(do ((,i 1 (1+ ,i)))
       ((>= ,i (layer-nums ,mlp)))
     ,@body))

(defmacro do-layers-reverse ((i mlp) &body body)
  "we have no interest in the input layer"
  `(do ((,i (- (layer-nums ,mlp) 1) (1- ,i)))
       ((< ,i 1))
     ,@body))

(defmacro do-layer-weights ((mW mB mlp) &body body)
  (let ((idx-sym (gensym "IDX-")))
    `(let ((,mW nil) (,mB nil))
       ;; excluding input layer
       (do ((,idx-sym 1 (1+ ,idx-sym)))
           ((>= ,idx-sym (layer-nums ,mlp)))
         (multiple-value-setq (,mW ,mB) (layer-weights ,mlp ,idx-sym))
         ,@body))))

;;; Network construction
(defmethod build-network ((mlp multilayer-perceptron) X y)
  "fill in input and output layer"
  (error "not implemented in base class"))

;;; Cache initialization
(defmethod %init-forward-cache ((mlp multilayer-perceptron) X)
  (with-slots (mZcache mAcache) mlp
    (setf mZcache (list X))
    (setf mAcache (list X))
    (do-layer-weights (mW mB mlp)
      (push (make-matrix (nrow X) (ncol mW)) mZcache)
      (push (make-matrix (nrow X) (ncol mW)) mAcache))
    (setf mZcache (nreverse mZcache))
    (setf mAcache (nreverse mAcache))))

;;; Forward Propagation
(defun linear-combination (mX mW vB &optional (op #'+))
  "mX * mW + vB: broadcasting `vB' accross rows of (mX * mW)"
  (let ((mXW (m* mX mW)))
    (do-matrix-row (i mXW)
      (do-matrix-col (j mXW)
        (setf (mref mXW i j) (funcall op (mref mXW i j) (vref vb j)))))
    mXW))

(defmethod forward-linear ((mlp multilayer-perceptron) i mA)
  "Z = AW + b"
  (multiple-value-bind (mW mB) (layer-weights mlp i)
    (linear-combination mA mW mB)))

(defmethod forward-layer ((mlp multilayer-perceptron) i X &optional zCache)
  "feed forward X through layer i: (values A Z), where A = act(Z) = act(XW + b)
if the output layer, A = Z without activation: softmax might be out there!
if `zCache', cache Z"
  (with-slots (activation layer-nums) mlp
    (let ((mZ (forward-linear mlp i X)))
      (when zCache
        (fill-matrix zCache mZ))
      (if (= i (output-layer mlp))
          mZ
          (activate-matrix activation mZ)))))

(defmethod forward-output ((mlp multilayer-perceptron) mZ)
  "do activation on the output"
  (error "not implemented in base class"))

(defmethod forward-propagation ((mlp multilayer-perceptron) X)
  "feed forward and cache Z's of each layer"
  (with-slots (mZcache mAcache) mlp
    (let ((pred X))
      (do-layers (l mlp)
        (setf pred (forward-layer mlp l pred (nth l mZcache)))
        (fill-matrix (nth l mAcache) pred))
      (forward-output mlp pred))))

;;; Backward Propagation
(defmethod backward-output ((mlp multilayer-perceptron) mA y)
  "do loss gradient on the outupt and label: returns dA
for the output layer, dZ = dA"
  (error "not implemented in base class"))

(defun %element-wise-op (mA mZ &optional (op #'*))
  "NOTICE: will modify `mA'"
  (do-matrix (i j mA)
    (setf (mref mA i j) (funcall op (mref mA i j) (mref mZ i j))))
  mA)

;;; TODO: replace dA with dZ, for the last layer, dZ = dA
(defmethod backward-layer ((mlp multilayer-perceptron) i dZ)
  "Input: dZ[l]; Output: dZ[l-1], dW[l], db[l]"
  (with-slots (activation mZcache mAcache) mlp
    (let* ((Ap (nth (- i 1) mAcache))
           (Zp (nth (- i 1) mZcache))
           (m (nrow Zp))
           (mW nil)
           dAp dZp dW db)
      (multiple-value-setq (mW) (layer-weights mlp i))
      ;; dW = 1/m \dot Ap.T \dot dZ
      (setf dW (m* (/ 1 m) (mt Ap) dZ))
      ;; db = 1/m \dot sum(dZ)
      (setf db (m* (/ 1 m) (msum dZ :axis 0)))
      ;; dAp = dZ \dot W.T
      (setf dAp (m* dZ (mt mW)))
      ;; g'(Z)
      (activate-gradient-matrix activation Zp)
      ;; dZ = dA * g'(Z)
      (setf dZp (%element-wise-op dAp Zp))
      (values dZp dW db))))

(defun %update-weight (mW dW eta)
  (do-matrix (i j mW)
    (decf (mref mW i j) (* eta (mref dW i j))))
  mW)

(defmethod backward-propagation ((mlp multilayer-perceptron) dZ)
  "backward propagation"
  (with-slots (learn-rate) mlp
    (do-layers-reverse (i mlp)
      (multiple-value-bind (mW mB) (layer-weights mlp i)
        (multiple-value-bind (dZp dW dB) (backward-layer mlp i dZ)
          (%update-weight mW dW learn-rate)
          (%update-weight mB dB learn-rate)
          (setf dZ dZp))))))

(defmethod fit ((mlp multilayer-perceptron) X &optional y)
  (build-network mlp X y)
  (%init-forward-cache mlp X)
  (with-slots (epochs) mlp
    (let (mA dZ)
      (dotimes (i epochs)
        (setf mA (forward-propagation mlp X))
        (setf dZ (backward-output mlp mA y))
        (backward-propagation mlp dZ)
        (let ((pred (predict mlp X)))
          (format t "~A-th accuracy: ~A~%" i (accuracy-score y pred)))))))

(defmethod predict ((mlp multilayer-perceptron) X)
  "feed forward and no cache: for prediction"
  (let ((pred X))
    (do-layers (l mlp)
      (setf pred (forward-layer mlp l pred nil)))
    (forward-output mlp pred)))

;;; MLP classifier
(defclass mlp-classifier (multilayer-perceptron)
  ((nclasses :initform nil)))

(defmethod build-network ((mlpclf mlp-classifier) X y)
  (with-slots (hidden-units mWi mWo mBo nclasses) mlpclf
    (setf nclasses (length (mbincount y)))
    (setf mWi (make-rand-matrix (ncol X) (car hidden-units) 1.0))
    (setf mWo (make-rand-matrix (car (last hidden-units)) nclasses 1.0))
    (setf mBo (make-rand-row-vector nclasses 1.0))))

(defmethod forward-output ((mlpclf mlp-classifier) mZ)
  (softmax-on-matrix mZ))

(defmethod backward-output ((mlpclf mlp-classifier) mA y)
  (cross-entropy-gradient mA y)
  mA)

(defmethod predict-proba ((mlpclf mlp-classifier) X)
  (let ((pred X))
    (do-layers (l mlpclf)
      (setf pred (forward-layer mlpclf l pred nil)))
    (softmax-on-matrix pred)))

(defmethod predict ((mlpclf mlp-classifier) X)
  (let ((pred (predict-proba mlpclf X)))
    (margmax pred :axis 1)))

;;; MLP regressor
(defclass mlp-regressor (multilayer-perceptron)
  ())

(defmethod initialize-instance :after ((mlpreg mlp-regressor) &rest args)
  "skip input layer, as we do not know the input shape yet..."
  (declare (ignore args))
  (with-slots (hidden-units mWo mBo) mlpreg
    (setf mWo (make-rand-matrix (car (last hidden-units)) 1 1.0))
    (setf mBo (make-rand-row-vector 1 1.0))))

(defmethod build-network ((mlpreg mlp-regressor) X y)
  (declare (ignore y))
  (with-slots (hidden-units mWi) mlpreg
    (setf mWi (make-rand-matrix (ncol X) (car hidden-units) 1.0))))

(defmethod forward-output ((mlpreg mlp-regressor) mZ)
  mZ)

(defmethod backward-output ((mlpred mlp-regressor) mA y)
  )
