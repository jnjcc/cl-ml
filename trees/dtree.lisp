;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Decision Tree Estimator
;;;; TODO: multiclass support; algorithm optimization

(in-package #:cl-ml)

;;; Decision Tree
;; Decision Tree: node data (payload of the Binary Tree data structure)
(defclass dtree-node-data ()
  ((feature :initform nil :initarg :feature :reader feature
            :documentation "feature index, if NIL, then leaf")
   (threshold :initform nil :initarg :threshold :reader threshold
              :documentation "feature threshold")
   (value :initform 0.0 :initarg :value :reader value)
   ;; frequency of each label in classifier
   (binfreqs :initform nil :initarg :binfreqs :reader binfreqs)
   (impurity :initform 0.0 :initarg :impurity :reader impurity
             :documentation "node impurity, or MSE in regression tasks")
   (nsamples :initform 0 :initarg :nsamples :reader nsamples
             :documentation "number of samples in this node")))

(defmethod print-object ((ddata dtree-node-data) stream)
  (with-slots (feature threshold value nsamples) ddata
    (if feature
        (format stream "~A <= ~A (~A)" feature threshold nsamples)
        (format stream "leaf: ~A (~A)" value nsamples))))

(defmethod print-verbose ((ddata dtree-node-data)
                          &optional (stream t) (feafun #'identity) (labelfun #'identity))
  (with-slots (feature threshold value nsamples) ddata
    (if feature
        (format stream "~A <= ~A (~A)" (funcall feafun feature) threshold nsamples)
        (format stream "leaf: ~A (~A)" (funcall labelfun value) nsamples))))

;; Decision Tree: Binary Tree with dtree-node-data payload
(defun set-dtree-data (root &key feature threshold impurity nsamples value binfreqs)
  (let ((data (make-instance 'dtree-node-data :feature feature :threshold threshold
                             :impurity impurity :nsamples nsamples :value value
                             :binfreqs binfreqs)))
    (set-tree-data root data)))

(defun update-dtree-data-value (root newval)
  (let ((data (get-tree-data root)))
    (with-slots (value) data
      (setf value newval))))

(defun get-dtree-child (root X i)
  "(values leaf-tree node-data) OR (values child-tree nil)"
  (let ((node-data (get-tree-data root)))
    (with-slots (feature threshold) node-data
      (if (null feature)
          (values root node-data)
          (let ((fval (mref X i feature)))
            (if (<= fval threshold)
                (values (get-left-tree root) nil)
                (values (get-right-tree root) nil)))))))

(defun print-dtree-depth (root depth &optional (stream t)
                                       (feafun #'identity) (labelfun #'identity))
  (when root
    (format stream "~A" (repeat-string "| " depth))
    (print-verbose (get-tree-data root) stream feafun labelfun)
    (format stream "~%")
    (print-dtree-depth (get-left-tree root) (+ depth 1) stream feafun labelfun)
    (print-dtree-depth (get-right-tree root) (+ depth 1) stream feafun labelfun)))

;;; Decision Tree Estimator
(deftype criterion-type ()
  '(member :gini :entropy :gain-ratio :mse))

;; for Random Forest
(deftype rand-feature-type ()
  '(member :all :log2 :sqrt))

;;; TODO: max-leaf-nodes
(defclass dtree-estimator (estimator)
  ((droot :initform nil :reader droot :documentation "decision tree root node")
   (depth :initform 0 :reader depth)
   (nfeat :initform nil)
   (vimps :initform nil)
   (max-depth :initform nil :initarg :max-depth :reader max-depth)
   (min-samples-split :initform 2 :initarg :min-samples-split
                      :reader min-samples-split :documentation
                      "minimum number of samples a node must have before it can split")
   (min-samples-leaf :initform 1 :initarg :min-samples-leaf
                     :reader min-samples-leaf :documentation
                     "minimum number of samples a leaf node must have")
   (max-leaf-nodes :initform nil :initarg :max-leaf-nodes :reader max-leaf-nodes
                   :documentation "maximum number of leaf nodes: best-first-splitter")
   (max-features :initform :all :initarg :max-features :type rand-feature-type
                 :documentation "feature selection for Random Forest")
   (criterion :initform :gini :type criterion-type)))

(defmethod print-object ((dtree-est dtree-estimator) stream)
  (with-slots (droot) dtree-est
    (print-dtree-depth droot 0 stream)))

;;; Decision Tree Splitter
(defclass dtree-splitter ()
  ((dnode :initform nil :initarg :dnode :documentation "decision tree node")
   (criterion :initform nil :initarg :criterion)
   (crite-obj :initform nil :initarg :crite-obj
              :documentation "the `criterion-base' object")
   (rf-maxfeats :initform -1 :initarg :rf-maxfeats :documentation
                "maximum number of features to consider, for Random Forest")
   (depth :initform 1 :initarg :depth)
   (sbeg :initform 0 :initarg :sbeg
         :documentation "begin of sample index, indirect access")
   (send :initform nil :initarg :send)
   (fbeg :initform 0 :initarg :fbeg
         :documentation "begin of (non-constant) feature index, indirect access")
   (fend :initform nil :initarg :fend)))

(defun %get-max-feature (rand-feature n)
  (ecase rand-feature
    (:all n) ;; -1 means all the features
    (:log2 (log2 n))
    (:sqrt (sqrt n))))

(defun make-root-splitter (dtree-est X)
  "make the root splitter for dtree-estimator on training set X"
  (with-slots (droot criterion max-features) dtree-est
    (setf droot (make-binary-tree nil))
    (make-instance 'dtree-splitter :dnode droot :depth 1 :criterion criterion
                   :crite-obj (make-criterion criterion 0 (nrow X))
                   :rf-maxfeats (%get-max-feature max-features (ncol X))
                   :sbeg 0 :send (nrow X) :fbeg 0 :fend (ncol X))))

(defun make-left-splitter (splitter smid)
  "generate the left child [sbeg, smid) and make the corresponding splitter"
  (with-slots (dnode criterion rf-maxfeats depth sbeg fbeg fend) splitter
    (set-left-tree dnode (make-binary-tree nil))
    (make-instance 'dtree-splitter :dnode (get-left-tree dnode) :depth (+ depth 1)
                   :criterion criterion :crite-obj (make-criterion criterion sbeg smid)
                   :rf-maxfeats rf-maxfeats
                   :sbeg sbeg :send smid :fbeg fbeg :fend fend)))

(defun make-right-splitter (splitter smid)
  "generate the right child [smid, send) and make the corresponding splitter"
  (with-slots (dnode criterion rf-maxfeats depth send fbeg fend) splitter
    (set-right-tree dnode (make-binary-tree nil))
    (make-instance 'dtree-splitter :dnode (get-right-tree dnode) :depth (+ depth 1)
                   :criterion criterion :crite-obj (make-criterion criterion smid send)
                   :rf-maxfeats rf-maxfeats
                   :sbeg smid :send send :fbeg fbeg :fend fend)))

(defmethod shrink-feature ((splitter dtree-splitter) dframe j)
  (with-slots (fend) splitter
    (swap-feature dframe j (- fend 1))
    (decf fend 1)))

(defun %feature-equal (dframe fidx i j)
  (float= (get-feature-value dframe i fidx) (get-feature-value dframe j fidx)))

(defmethod choose-feature ((splitter dtree-splitter) dframe)
  (with-slots (crite-obj rf-maxfeats sbeg send fbeg fend depth) splitter
    (let ((bfidx nil) ;; best feature idx
          (bsidx nil) ;; best sample split point [`sbeg', `bsidx')
          (bgain nil) ;; best gain
          (bthrh nil) ;; best threshold
          (rf-fend fend)) ;; feature end for Random Forest
      (when (< 0 rf-maxfeats (- fend fbeg))
        (shuffle-feature dframe fbeg fend)
        ;; `rf-maxfeats' features to consider for Random Forest
        (setf rf-fend (+ fbeg rf-maxfeats)))
      (do ((fidx fbeg (1+ fidx)))
          ((>= fidx rf-fend))
        (reset-position crite-obj)

        (sort-frame dframe fidx sbeg send)
        (if (%feature-equal dframe fidx sbeg (- send 1))
            (shrink-feature splitter dframe fidx)
            (do ((sidx sbeg (1+ sidx)))
                ((>= sidx (- send 1)))
              (unless (%feature-equal dframe fidx sidx (+ sidx 1))
                (update-position crite-obj dframe (+ sidx 1))
                (let ((gain (proxy-impurity-gain crite-obj)))
                  (when (or (null bgain) (< bgain gain))
                    (setf bgain gain)
                    (setf bfidx fidx)
                    ;; `bsidx' belongs to the right child
                    (setf bsidx (+ sidx 1))
                    (setf bthrh (/ (+ (get-feature-value dframe sidx fidx)
                                      (get-feature-value dframe (+ sidx 1) fidx))
                                   2.0))))))))
      (when bfidx
        ;; found a good split feature, reorder data frame again!
        (sort-frame dframe bfidx sbeg send))
      (values bfidx bsidx bthrh bgain))))

(defmethod fill-dtree-data ((splitter dtree-splitter) dframe &key feature threshold)
  "(feature & threshold) OR leaf node"
  (with-slots (dnode sbeg send crite-obj) splitter
    (multiple-value-bind (value binfreqs) (node-value crite-obj)
      (if feature
          (set-dtree-data dnode :feature feature :threshold threshold :nsamples (- send sbeg)
                          :impurity (node-impurity crite-obj))
          (set-dtree-data dnode :value value :binfreqs binfreqs :nsamples (- send sbeg)
                          :impurity (node-impurity crite-obj))))))

(defmethod fill-dtree-const ((splitter dtree-splitter) dframe value)
  "special case of fill-dtree-data: all label is same"
  (with-slots (dnode sbeg send crite-obj) splitter
    (let ((nsamples (- send sbeg)))
      (set-dtree-data dnode :value value :binfreqs (list (cons value nsamples))
                      :nsamples nsamples :impurity (node-impurity crite-obj)))))

(defmethod split-node ((splitter dtree-splitter) dframe dtree-est)
  (with-slots (dnode depth sbeg send fbeg fend crite-obj) splitter
    (init-position-and-summary crite-obj dframe)
    ;; MAX-DEPTH, MIN-SAMPLES-SPLIT
    (when (or (and (max-depth dtree-est) (>= depth (max-depth dtree-est)))
              (and (min-samples-split dtree-est) (< (- send sbeg) (min-samples-split dtree-est))))
      (fill-dtree-data splitter dframe)
      (return-from split-node depth))

    ;; all sample belongs to the same class
    (multiple-value-bind (same-label label0) (const-label-p crite-obj dframe)
      (when same-label
        (fill-dtree-const splitter dframe label0)
        (return-from split-node depth)))

    ;; all feature is constant
    (when (>= fbeg fend)
      (fill-dtree-data splitter dframe)
      (return-from split-node depth))

    ;; best feature, best split point, best threshold
    (multiple-value-bind (bfidx bsidx bthrh) (choose-feature splitter dframe)
      (when bfidx ;; if found a best split
        (when (and (min-samples-leaf dtree-est)
                   (or (< (- bsidx sbeg) (min-samples-leaf dtree-est))
                       (< (- send bsidx) (min-samples-leaf dtree-est))))
          (fill-dtree-data splitter dframe)
          (return-from split-node depth))
        (fill-dtree-data splitter dframe :feature (get-feature-name dframe bfidx)
                         :threshold bthrh)
        (let (ldepth rdepth)
          ;; generate the left child and continue splitting
          (setf ldepth (split-node (make-left-splitter splitter bsidx) dframe dtree-est))
          (setf rdepth (split-node (make-right-splitter splitter bsidx) dframe dtree-est))
          (return-from split-node (max ldepth rdepth))))
      (unless bfidx ;; if not found a best split
        (fill-dtree-data splitter dframe)
        (return-from split-node depth)))))
