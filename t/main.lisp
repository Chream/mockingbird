;;;; cl-mock/t/main.lisp

(in-package :cl-user)
(uiop:define-package  :cl-mock/t/main
    (:use :closer-common-lisp
          :cl-mock/t/all)
  (:mix :fare-utils
        :uiop
        :alexandria)
  (:documentation
   "")

  (:export ))

(in-package :cl-mock/t/main)

(run-tests)
