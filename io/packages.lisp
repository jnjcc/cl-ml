;;;; Copyright (c) 2012-2015 jnjcc, Yste.org. All rights reserved.
;;;;

(in-package #:cl-user)

(defpackage #:cl-ml/io
  (:nicknames #:ml/io)
  (:use #:cl #:cl-ml/linalg #:cl-ml/algo #:cl-ml/graph)
  (:export #:space-char-p
           #:peek-non-space #:peek-next-char #:discard-next-char #:stream-eof-p
           #:read-next-char
           #:read-chars-if #:read-chars-until #:discard-chars-if #:discard-chars-until
           #:do-char #:do-char-if #:do-char-until

           #:alpha-word-p #:upper-word-p #:lower-word-p #:title-word-p #:digit-word-p #:alnum-word-p
           #:read-next-word #:do-word
           #:read-next-sentence #:do-sentence

           #:split-string
           #:read-file-content #:read-file-lines

           #:do-example-from-file #:do-example-row #:do-variable-col
           #:read-matrix #:read-graph))
