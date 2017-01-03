;;;; cl-mock/t/all.lisp

(in-package :cl-user)
(uiop:define-package  :cl-mock/t/all
    (:nicknames :cl-mock.test)
  (:use :closer-common-lisp)
  (:use-reexport :cl-mock/t/cl-mock
                 :cl-mock/t/with-methods)
  (:documentation
   ""))

(in-package :cl-mock.test)
