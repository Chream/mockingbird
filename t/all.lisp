;;;; cl-mock/t/all.lisp

(in-package :cl-user)
(uiop:define-package  :cl-mock/t/all
    (:nicknames :cl-mock.test)
  (:use :closer-common-lisp)
  (:use-reexport :cl-mock/t/cl-mock)
  (:documentation
   ""))

(in-package :cl-mock.test)
