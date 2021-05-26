;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Lexical analysis, with #\Space being word delimiter

(in-package #:cl-ml/io)

(defun alpha-word-p (word)
  (every #'alpha-char-p word))

(defun upper-word-p (word)
  (every #'upper-case-p word))

(defun lower-word-p (word)
  (every #'lower-case-p word))

(defun title-word-p (word)
  (and (upper-case-p (char word 0))
       (lower-word-p (subseq word 1))))

(defun digit-word-p (word)
  (every #'digit-char-p word))

(defun alnum-word-p (word)
  (every #'alphanumericp word))

(defun make-adjustable-string ()
  (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))

(defun read-next-word (stream &key (eow #\Space) (eol #\Newline))
  "`eow': end-of-word; `eol': end-of-line
Returns: :eof if end-of-file; :eol if end-of-line; word otherwise"
  (discard-chars-if stream (lambda (ch) (eq ch eow)))
  (let ((next (peek-next-char stream nil :eof)))
    (cond
      ((eq next :eof) (return-from read-next-word :eof))
      ((eq next eol) (progn
                       (discard-next-char stream)
                       (return-from read-next-word :eol)))))
  (let ((word (make-adjustable-string)))
    (with-output-to-string (wstream word)
      (do-char-until (chr stream (lambda (x)
                                   (or (eq x eow) (eq x eol))))
        (write-char chr wstream))
      word)))

(defmacro do-word ((word stream &key (eow #\Space) (eol #\Newline)) &body body)
  "`word' will be :eol if end-of-line"
  `(do ((,word (read-next-word ,stream :eow ,eow :eol ,eol)
               (read-next-word ,stream :eow ,eow :eol ,eol)))
       ((eq ,word :eof))
     ,@body))
