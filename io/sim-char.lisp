;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Character

(in-package #:cl-ml/io)

(defun space-char-p (chr)
  (or (char= chr #\Space) (char= chr #\Tab) (char= chr #\Newline)
      (char= chr #\Return)))

(defun peek-non-space (stream &optional (eof-error-p t) (eof-value nil))
  "peek next non-whitespace character"
  (peek-char t stream eof-error-p eof-value))

(defun peek-next-char (stream &optional (eof-error-p t) (eof-value nil))
  "peek next char, maybe whitespace"
  (peek-char nil stream eof-error-p eof-value))

(defun discard-next-char (stream &optional (eof-error-p t) (eof-value nil))
  "read next char and discard it by returning `nil'"
  (read-char stream eof-error-p eof-value)
  nil)

(defun stream-eof-p (stream)
  (let ((next (peek-next-char stream nil :eof)))
    (eq next :eof)))

(defun read-next-char (stream)
  "Returns: :eof if end-of-file; char otherwise"
  (read-char stream nil :eof))

(defun read-chars-if (stream predicate)
  (let ((chars nil))
    (do ((next (peek-next-char stream nil :eof) (peek-next-char stream nil :eof)))
        ((or (eq next :eof)
             (not (funcall predicate next))))
      (push (read-char stream nil :eof) chars))
    (nreverse chars)))

(defun discard-chars-if (stream predicate)
  (do ((next (peek-next-char stream nil :eof) (peek-next-char stream nil :eof)))
      ((or (eq next :eof)
           (not (funcall predicate next))))
    (discard-next-char stream nil)))

(defun read-chars-until (stream predicate)
  (let ((chars nil))
    (do ((next (peek-next-char stream nil :eof) (peek-next-char stream nil :eof)))
        ((or (eq next :eof)
             (funcall predicate next)))
      (push (read-char stream nil :eof) chars))
    (nreverse chars)))

(defun discard-chars-until (stream predicate)
  (do ((next (peek-next-char stream nil :eof) (peek-next-char stream nil :eof)))
      ((or (eq next :eof)
           (funcall predicate next)))
    (discard-next-char stream nil)))

(defmacro do-char ((char stream) &body body)
  (let ((next (gensym "NEXT-")))
    `(do ((,next (peek-next-char ,stream nil :eof) (peek-next-char ,stream nil :eof))
          (,char nil))
         ((eq ,next :eof))
       (setf ,char (read-char ,stream nil :eof))
       ,@body)))

(defmacro do-char-if ((char stream predicate) &body body)
  (let ((next (gensym "NEXT-"))
        (pred (gensym "PRED-")))
    `(do ((,next (peek-next-char ,stream nil :eof) (peek-next-char ,stream nil :eof))
          (,pred ,predicate)
          (,char nil))
         ((or (eq ,next :eof)
              (not (funcall ,pred ,next))))
       (setf ,char (read-char ,stream nil :eof))
       ,@body)))

(defmacro do-char-until ((char stream predicate) &body body)
  (let ((next (gensym "NEXT-"))
        (pred (gensym "PRED-")))
    `(do ((,next (peek-next-char ,stream nil :eof) (peek-next-char ,stream nil :eof))
          (,pred ,predicate)
          (,char nil))
         ((or (eq ,next :eof)
              (funcall ,pred ,next)))
       (setf ,char (read-char ,stream nil :eof))
       ,@body)))
