;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;
;;;; Syntactic analysis, with #\Space being word delimiter, #\Newline being sentence delimiter

(in-package #:cl-ml/io)

(defun read-next-sentence (stream &key (max-len nil) (eow #\Space) (eol #\Newline))
  "Returns: :eof if no more sentence; or list of words (could be empty list)
if `max-len' non-nil, only `max-len' words allowed"
  (let ((sentence nil)
        (eof-word nil))
    (do ((word (read-next-word stream :eow eow :eol eol)
               (read-next-word stream :eow eow :eol eol)))
        ((or (eq word :eol)
             (and max-len (> (length sentence) max-len))))
      (when (eq word :eof)
        (setf eof-word t)
        (return))
      (push word sentence))
    (if (and eof-word (null sentence))
        :eof
        (nreverse sentence))))

(defmacro do-sentence ((sentence stream &key (eow #\Space) (eol #\Newline)) &body body)
  "`sentence' will be NIL if empty sentence"
  `(do ((,sentence (read-next-sentence ,stream :eow ,eow :eol ,eol)
                   (read-next-sentence ,stream :eow ,eow :eol ,eol)))
       ((eq ,sentence :eof))
     ,@body))
