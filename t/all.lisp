;;;; mockingbird/t/all.lisp

(in-package :cl-user)
(uiop:define-package  :mockingbird/t/all
    (:nicknames :mb.test)
  (:use :closer-common-lisp)
  (:use-reexport :mockingbird/t/functions
                 :mockingbird/t/methods)
  (:documentation
   ""))

(in-package :mb.test)
