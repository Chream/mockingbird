;;;; mockingbird/t/main.lisp

(in-package :cl-user)
(uiop:define-package  :mockingbird/t/main
    (:use :closer-common-lisp
          :prove
          :mockingbird/t/all)
  (:mix :fare-utils
        :uiop
        :alexandria)
  (:documentation
   "")

  (:export ))

(in-package :mockingbird/t/main)

(plan 36)
(run-tests)
(with-methods-tests)
(finalize)
