;;;; mockingbird/src/conditions.lisp

(in-package :cl-user)
(uiop:define-package :mockingbird/src/conditions
    (:nicknames :mockingbird.conditions)
    (:use :closer-common-lisp)
  (:mix :fare-utils
        :uiop
        :alexandria)
  (:import-from :trivial-arguments
                :arglist)
  (:documentation
   "")

  (:export :undefined-function
           :undefined-stub-function-error))

(in-package :mockingbird.conditions)

;;;;;;;;;;;;;;;;;;;
;;; Conditions. ;;;
;;;;;;;;;;;;;;;;;;;

(define-condition undefined-stub-function (undefined-function) ()
  (:report (lambda (condition stream)
             (format stream "The defined stub function for ~s does not ~
                          have a defined original." (cell-error-name condition))))
  (:documentation "Error: "))

(defun undefined-stub-function-error (name)
  "The error function for undefined-stub-function."
  (error 'undefined-stub-function :name name))
