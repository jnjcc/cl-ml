cl-ml
=====================
Machine Learning in Common Lisp

Unfortunately, this package is _NOT_ production ready.

For one thing, most of the linear algebra stuff is written from scratch,
which is definitely _NOT_ efficient enough.

Usage
---------

Tests
---------
```sh
cd ~/quicklisp/local-projects/
git clone https://github.com/jnjcc/cl-dataset
git clone https://github.com/jnjcc/cl-ml
ln -s cl-dataset/ml cl-ml/test/dataset
```

```lisp
(ql:quickload :cl-ml)
(ql:quickload :cl-ml-test)
(cl-ml/test:run-all-tests)
```

COPYING
---------
Copyright (c) 2012-2015 jnjcc, [Yste.org](http://www.yste.org)

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
