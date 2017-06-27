;;;; mockingbird/src/all.lisp

(in-package :cl-user)
(uiop:define-package  :mockingbird/src/all
    (:nicknames :mockingbird :mb)
  (:use :closer-common-lisp)
  (:use-reexport :mockingbird/src/functions
                 :mockingbird/src/methods)
  (:documentation
   ""))
