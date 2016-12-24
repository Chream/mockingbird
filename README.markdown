# Cl-Stub

This package provides to macros for unit testing ```with-stub``` and ```with-mock```. They are used when specified functions in a test
should not be computed but should instead return a provided constant value.

## Usage

(in-package :cl-user)
(uiop:define-package :my-project
    (:use :closer-common-lisp
          :prove
          :cl-stub))
(in-package :my-project)

(defun foo (x) x)
(defun bar (x y) (+ x (foo x)))

```bar``` calls ```foo``` but for unit testing
only ```bar```one can do

(with-stubs ((foo 10))
  (is (bar 1 2)))


(with-stub)


## Installation

## Author

* Christopher Eames (Chream) (chream@gmx.com)

## Copyright

Copyright (c) 2016 Christopher Eames (Chream) (chream@gmx.com)
