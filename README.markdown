# Cl-Mock

This package provides some useful stubbing and mocking macros for unit testing. Used when specified functions in a test should not be computed but should instead return a provided constant value.

## Usage
```
(in-package :cl-user)
(uiop:define-package :my-project
    (:use :closer-common-lisp
          :prove
          :cl-mock))
(in-package :my-project)

(defun foo (x) x)
(defun bar (x y) (+ x (foo x)))

(with-stubs ((foo 10))
  (is (foo 1 2) 10)) ;; --> T
  (is (bar 1 2) 2))  ;; --> T Only lexically stubbed!

(with-dynamic-stubs ((foo 10))
  (is (foo 1 2) 10)) ;; --> T
  (is (bar 1 2) 11))  ;; --> T Dynamically stubbed!

(with-mocks (foo bar)
  (is (foo 5) nil)    ;; --> T
  (is (bar 10) nil))  ;; --> T Args dont need to match!

(with-dynamic-mocks (foo bar)
  ...)
```

The arguments passed to mocked or stubbed functions are also saved.

```
(with-stubs ((foo 5))
  (foo 4 5)
  (call-times-for 'foo)                  ;; --> 1
  (verify-call-times-for 'foo 1)         ;; --> T
  (nth-mock-args-for 1 'foo)             ;; --> '(4 5)
  (verify-nth-call-args-for 1 'foo 4 5)  ;; --> T
  (verify-first-call-args-for 'foo 4 5)  ;; --> T
  (clear-calls))                         ;; --> no-value
```

These also work for the dynamic and mocking variants.

-------------------------------------------------------------------

It is also possible to mock/stub individual methods.

```
(with-method-stubs ((foo (x y) 'is-stubbed)
                    (foo ((x aclass) (y aclass)) 'aclass-stubbed)
  ...)

```

The calls to methods are currently NOT saved so the above verification functions can not be used.



## Installation

Clone this repository and put into asdf load path then
```
(ql:quickload :cl-mock)
```

To run tests:
```
(ql:test-system :cl-mock)
```

## Author

* Christopher Eames (Chream) (chream@gmx.com)

## Copyright

Copyright (c) 2016 Christopher Eames (Chream) (chream@gmx.com)
