;;;; cl-stub/t/all.lisp

(in-package :cl-user)
(uiop:define-package  :cl-stub/t/all
    (:nicknames :cl-stub.test)
  (:use :closer-common-lisp)
  (:use-reexport :cl-stub/t/utils
                 :cl-stub/t/cl-stub)
  (:documentation
   ""))
