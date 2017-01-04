;;;; cl-mock/t/main.lisp

(in-package :cl-user)
(uiop:define-package  :cl-mock/t/main
    (:use :closer-common-lisp
          :prove
          :cl-mock/t/all)
  (:mix :fare-utils
        :uiop
        :alexandria)
  (:documentation
   "")

  (:export ))

(in-package :cl-mock/t/main)

(plan 35)
(run-tests)
(with-methods-tests)
(finalize)
