;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; file io

(in-package #:cl-ml/io)

(defun read-file-content (fpath)
  (with-open-file (stream fpath :direction :input :if-does-not-exist nil)
    (unless stream
      (error "error reading file ~A~%" fpath))
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun read-file-lines (fpath)
  ;; there will be an extra empty line
  (butlast (split-string (read-file-content fpath) #\Newline)))
