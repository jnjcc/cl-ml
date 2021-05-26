;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple matrix: Matrix transformations

(in-package #:cl-ml/linalg)

;;; 1) Gaussian elimination: three types of elementary row operations
(defun %swap-rows (ma i j)
  "swapping two rows"
  (declare (type smatrix ma))
  (do-matrix-col (k ma)
    (let ((tmp (mref ma i k)))
      (setf (mref ma i k) (mref ma j k))
      (setf (mref ma j k) tmp))))

(defun %row*= (ma i mul)
  "multiplying a row by a nonzero number"
  (declare (type smatrix ma))
  (do-matrix-col (j ma)
    (setf (mref ma i j) (* (mref ma i j) mul))))

(defun %row+= (ma to from &optional (mul 1))
  "adding a multiple of one row to another row"
  (declare (type smatrix ma))
  (do-matrix-col (j ma)
    (incf (mref ma to j) (* (mref ma from j) mul))))

(defun gaussian-on-matrix (mA)
  "Gaussian elimination on matrix; Requires: ncol >= nrow
NOTICE: will modify `mA' in-place; copy-matrix if necessary"
  (let ((n (nrow mA)))
    (do-matrix-row (i mA)
      (when (= (mref mA i i) 0)
        (do ((j (+ i 1) (1+ j)))
            ((>= j n) (return-from gaussian-on-matrix))
          (when (/= (mref mA j i) 0)
            (%swap-rows mA i j)
            (return))))
      (let ((base (/ 1 (mref mA i i)))
            (ratio nil))
        (do ((j (+ i 1) (1+ j)))
            ((>= j n))
          (setf ratio (* base (mref mA j i)))
          (%row+= mA j i (- ratio)))))))

(defun %gauss-jordan (ma)
  (declare (type smatrix ma))
  (if (/= (nrow ma) (ncol ma))
      (error "square matrix expected: (~A, ~A)" (nrow ma) (ncol ma))
      (let* ((n (nrow ma))
             ;; block matrix
             (bma (make-matrix n (* 2 n) :initial-element 0))
             (inv (make-matrix n n)))
        (do-matrix (i j ma)
          (setf (mref bma i j) (mref ma i j)))
        (do-matrix-row (i bma)
          (setf (mref bma i (+ n i)) 1))
        (do-matrix-row (i bma)
          (when (= (mref bma i i) 0)
            (do ((j (+ i 1) (1+ j)))
                ((>= j n) (error "matrix not invertible"))
              (when (/= (mref bma j i) 0)
                (%swap-rows bma i j)
                (return))))
          (let ((ratio (/ 1 (mref bma i i))))
            (%row*= bma i ratio)
            (do ((j 0 (1+ j)))
                ((>= j n))
              (unless (= i j)
                (%row+= bma j i (- (mref bma j i)))))))
        (do-matrix (i j inv)
          (setf (mref inv i j) (mref bma i (+ j n))))
        inv)))

;;; 2) Householder transformation
(defun householder (vx)
  "Householder matrix from vector `vx', H * vx = a * e: H = I - 2ww^{T} = I - beta * v * v^{T}
v = x + a * e, where a = ||x||; beta = 1 / a * v[1]
Returns: (beta, v, a)"
  (let (beta v aval eta)
    (do-vector (i vx)
      (if (= i 0)
          (setf eta (abs (vref vx i)))
          (setf eta (max eta (abs (vref vx i))))))
    (if (= eta 0)
        (values 0 (copy-matrix vx) 0)
        (progn
          (setf v (v/ vx eta))
          ;; a = norm = ||v||
          (setf aval (sqrt (vdot v v)))
          (when (< (vref v 0) 0)
            (setf aval (- aval)))
          ;; v1 = v1 + a
          (incf (vref v 0) aval)
          (setf beta (/ 1 (* aval (vref v 0))))
          ;; a = eta * a
          (setf aval (* eta aval))
          (values beta v aval)))))

(defun householder-on-vector (vx beta v &optional a)
  "Hx = (I - \beta * v * vT) * x = x - \beta * (vT * x) * v
NOTICE: will modify `vx' in-place; copy if necessary"
  (if a
      (do-vector (i vx)
        (setf (vref vx i) 0)
        (when (= i 0)
          (setf (vref vx i) (- a))))
      (let ((dot (vdot v vx)))
        (do-vector (i vx)
          (decf (vref vx i) (* beta dot (vref v i))))))
  vx)

(defun householder-on-vector-copy (vx beta v &optional a)
  "Hx = (I - \beta * v * vT) * x = x - \beta * (vT * x) * v"
  (declare (ignore a))
  (v- vx (v* (* beta (vdot v vx))
             v)))

(defun householder-on-matrix (mA beta v)
  "HA = A - \beta * v * (A^{T} * v)^{T}
By making A = I, we can get the actual Householder matrix
NOTICE: will modify A in-place; copy-matrix if necessary"
  (let ((sigma 0))
    (do-matrix-col (j mA)
      (setf sigma 0)
      (do-matrix-row (i mA)
        (incf sigma (* (vref v i) (mref mA i j))))
      (setf sigma (* beta sigma))
      (do-matrix-row (i mA)
        (decf (mref mA i j) (* sigma (vref v i)))))
    mA))

(defun householder-of-row-vector (vx)
  (householder (mt vx)))

(defun householder-on-row-vector (vx beta v &optional a)
  "xH: (MT VX) shares memory with VX"
  (householder-on-vector (mt vx) beta v a))

(defun householder-on-row-matrix (mA beta v)
  "AH = ((AH)^{T})^{T} = (H^{T} * A^{T})^{T} = (H * A^{T})^{T}
\(MT A) shares memory with A"
  (householder-on-matrix (mt mA) beta v))

(defun householder-matrix (beta v)
"H = I - \beta * v * vT"
  (let ((H (make-identity-matrix (nrow v))))
    (householder-on-matrix H beta v)))

;;; 3) Givens rotation
(defun givens-from-value (x y)
  "[[c s] [-s c]] * [[x] [y]] = [[val] [0]]:
-s * x + c * y = 0"
  (let (c s)
    (if (= y 0)
        (setf c 1
              s 0)
        (if (>= (abs y) (abs x))
            (let ((tmp (/ x y)))
              (setf s (/ 1 (sqrt (+ 1 (* tmp tmp)))))
              (setf c (* s tmp)))
            (let ((tmp (/ y x)))
              (setf c (/ 1 (sqrt (+ 1 (* tmp tmp)))))
              (setf s (* c tmp)))))
    (values c s (+ (* c x) (* s y)))))

(defun givens-from-row-value (x y)
  "[x y] * [[c s] [-s c]] = [val 0] <=>
[[c -s] [s c]] * [[x] [y]] = [[val] [[0]]:
s * x + c * y = 0"
  (multiple-value-bind (c s sigma) (givens-from-value x y)
    (values c (- s) sigma)))

(defun givens (vx i j)
  "Givens matrix from vector `vx': vy = G(i, j, theta) * vx:
vy[i] = c * vx[i] + s * vx[j]; vy[j] = 0"
  (multiple-value-bind (c s sigma) (givens-from-value (vref vx i) (vref vx j))
    (values i j c s sigma)))

(defun givens-on-value (x y c s)
  "[[c s] [-s c]] * [[x] [y]]"
  (values (+ (* c x) (* s y))
          (+ (* (- s) x) (* c y))))

(defun givens-on-row-value (x y c s)
  "[x y] * [[c s] [-s c]]"
  (values (+ (* c x) (* (- s) y))
          (+ (* s x) (* c y))))

(defun givens-on-square (a b e f c s)
  "G * SQ2 = [[c s] [-s c]] * [[a b] [e f]"
  (multiple-value-bind (newa newe) (givens-on-value a e c s)
    (multiple-value-bind (newb newf) (givens-on-value b f c s)
      (values newa newb newe newf))))

(defun givens-on-row-square (a b e f c s)
  "SQ2 * G = [[a b] [e f]] * [[c s] [-s c]]"
  (multiple-value-bind (newa newb) (givens-on-row-value a b c s)
    (multiple-value-bind (newe newf) (givens-on-row-value e f c s)
      (values newa newb newe newf))))

(defun givens-on-vector (vx i j c s)
  "vy = G * vx: vy[i] = c * vx[i] + s * vx[j]; vy[j] = -s * vx[i] + c * vx[j]
NOTICE: will modify `vx' in-place; copy if necessary"
  (multiple-value-bind (newx newy) (givens-on-value (vref vx i) (vref vx j) c s)
    (setf (vref vx i) newx)
    (setf (vref vx j) newy))
  vx)

(defun givens-on-row-vector (vx i j c s)
  "vx * G
NOTICE: will modify `vx' in-place; copy if necessary"
  (multiple-value-bind (newx newy) (givens-on-value (vref vx i) (vref vx j) c s)
    (setf (vref vx i) newx)
    (setf (vref vx j) newy))
  vx)

(defun givens-on-matrix (mA i j c s)
  "GA: only change row I and J of A; G is the form of [[c s] [-s c]]
NOTICE: will modify A in-place; copy-matrix if necessary"
  (let (alpha beta)
    (do-matrix-col (k mA)
      (setf alpha (mref mA i k))
      (setf beta (mref mA j k))
      (setf (mref mA i k) (+ (* c alpha) (* s beta)))
      (setf (mref mA j k) (+ (* (- s) alpha) (* c beta)))))
  mA)

(defun givens-on-row-matrix (mA i j c s)
  "AG: will only change column I and J of A; G is the form of [[c s] [-s c]]
NOTICE: will modify A in-place; copy-matrix if necessary"
  (let (alpha beta)
    (do-matrix-row (k mA)
      (setf alpha (mref mA k i))
      (setf beta (mref mA k j))
      (setf (mref mA k i) (+ (* c alpha) (* (- s) beta)))
      (setf (mref mA k j) (+ (* s alpha) (* c beta)))))
  mA)

(defun givens-matrix (n i j c s)
  "NOTICE: by using -s, we can get transpose matrix of G"
  (let ((idm (make-identity-matrix n)))
    (setf (mref idm i j) s)
    (setf (mref idm j i) (- s))
    (setf (mref idm i i) c)
    (setf (mref idm j j) c)
    idm))
