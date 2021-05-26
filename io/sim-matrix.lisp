;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Simple io: read smatrix from csv file

(in-package #:cl-ml/io)

(defun read-matrix (fpath &key (start 0) end (header-p nil) (delimiter #\,)
                         colclass (label -1) labclass)
  "Read from row [`start', `end') of `fpath'; `header-p' if the first line is header;
`colclass' as class of columns: :string being string, otherwise numeric;
`label' as the label column index; `labclass' as label class"
  (let* ((examples (read-file-lines fpath))
         (nrow (length examples))
         (vars (split-string (nth 0 examples) delimiter))
         (ncol (length vars))
         (ma nil)
         (vy nil))
    ;; header as the first row
    (when header-p
      (incf start)
      (decf nrow))
    (when (and start end (< (- end start) nrow))
      (setf nrow (- end start)))

    (when label
      (when (< label 0)
        (incf label ncol))
      (decf ncol)
      (setf vy (make-vector nrow)))

    (setf ma (make-matrix nrow ncol))
    (do-example-row (example ridx examples :start start :end end)
      (setf vars (split-string example delimiter))
      (when label
        (if (eq labclass :string)
            (setf (vref vy ridx) (nth label vars))
            (setf (vref vy ridx) (read-from-string (nth label vars)))))
      (do-variable-col (var cidx vars :label label)
        (if (eq (nth cidx colclass) :string)
            (setf (mref ma ridx cidx) var)
            (setf (mref ma ridx cidx) (read-from-string var)))))
    (values ma vy)))
