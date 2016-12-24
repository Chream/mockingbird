;;;; cl-stub/t/cl-stub.lisp

(in-package :cl-user)
(uiop:define-package  :cl-stub/t/cl-stub
    (:use :closer-common-lisp
          :prove
          :cl-stub)
  (:mix :fare-utils
        :uiop
        :alexandria)
  (:documentation
   "")

  (:export :init-test :with-stubs-test))

(in-package :cl-stub/t/cl-stub)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-stub)' in your Lisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameters/Constants/Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun foo (x) x)
  (defun bar (x y) (+ x y))

  (defun baz (x)
    (+ (foo x) x))
  (defun bazz (x)
    (+ (foo x) (baz x))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Test functions. ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun init-test ()
  (diag "Testing in with-stubs-test.")
  (is (foo 5) 5 "is default.")
  (is (bar 5 10) 15 "is default.")
  (is (baz 15) 30 "is default.")
  (is (bazz 30) 90 "is default."))

(defun with-stubs-test ()
  (diag "Testing in with-stubs-test.")
  (subtest "Basic stub tests."
    (with-stubs
        ((foo 99) (bar 99) (baz 99)
         (bazz 99))
      (is (foo 5) 99 "is stubbed.")
      (is (bar 5 10) 99 "is stubbed.")
      (is (baz 15) 99 "is stubbed.")
      (is (bazz 30) 99 "is stubbed.")))
  (finalize))

(plan nil)

(finalize)
