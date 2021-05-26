;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Common Data Structure: String

(in-package #:cl-ml/algo)

(defun repeat-string (string n)
  (format nil "~v@{~A~:*~}" n string))

(defun join-strings (strings &optional (delimiter #\Space))
  (format nil (format nil "~A~A~A" "~{~A~^" delimiter "~}") strings))

(defun split-by-char (str delimiter)
  (let ((cols nil)
        (sbeg 0)
        (slen (length str)))
    (dotimes (send slen)
      (when (char= (char str send) delimiter)
        (push (subseq str sbeg send) cols)
        (setf sbeg (+ send 1))))
    ;; one last part
    (push (subseq str sbeg slen) cols)
    (nreverse cols)))

(defun split-by-string (str delimiter)
  (let ((cols nil)
        (sbeg 0)
        (slen (length str))
        (dlen (length delimiter)))
    ;; forever-loop
    (do ((dbeg 0)) (nil)
      (setf dbeg (search delimiter str :start2 sbeg))
      (unless dbeg
        (return))
      (push (subseq str sbeg dbeg) cols)
      (setf sbeg (+ dbeg dlen)))
    ;; one last part
    (push (subseq str sbeg slen) cols)
    (nreverse cols)))

(defun split-string (str &optional (delimiter #\,))
  (etypecase delimiter
    (character (split-by-char str delimiter))
    (string (split-by-string str delimiter))))

(defun split-string-if (str predicate)
  "NOTICE: `predicate' only accepts character"
  (let ((cols nil)
        (start 0)
        (slen (length str)))
    (dotimes (end slen)
      (when (funcall predicate (char str end))
        (push (subseq str start end) cols)
        (setf start (+ end 1))))
    ;; one last part
    (push (subseq str start slen) cols)
    (nreverse cols)))

(defun replace-string (str old new)
  (let ((seqs (split-string str old)))
    (join-strings seqs new)))

(defun replace-string-if (str predicate new)
  "NOTICE: `predicate' only accepts character"
  (let ((seqs (split-string-if str predicate)))
    (join-strings seqs new)))
