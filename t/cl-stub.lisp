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

(setf prove:*enable-colors* t)
(setf *default-reporter* :list)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun foor (x) x)
  (defun barr (x y) (+ x y))

  (defun baz (x)
    (+ (foor x) x))
  (defun bazz (x)
    (+ (foor x) (baz x))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Test functions. ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun init-test ()
  (diag "Testing in with-stubs-test.")
  (subtest "Init tests."
    (is (foor 5) 5 "returns correctly.")
    (is (barr 5 10) 15 "returns correctly.")
    (is (baz 15) 30 "returns correctly.")
    (is (bazz 30) 90 "returns correctly.")))

(defun *mock-calls*-init-test ()
  (diag "Testing *mock-calls* variable.")
  (with-stubs ()
    (is *mock-calls* '() "init is nil.")
    (flet ((f ()
             (setf *mock-calls* (acons 'test 5 *mock-calls*))))
      (let ((expected (acons 'test 5 '())))
        (f)
        (is *mock-calls* expected "is special.")
        (is (funcall  (lambda () *mock-calls*))
            expected "inner function readable.")))))
(defun with-stubs-test ()
  (diag "Testing in with-stubs-test.")
  (subtest "Basic stub tests for (with-stubs)."
    (with-stubs
        ((foor 99) (barr 99) (baz 99)
         (bazz 99))
      (is (foor 5) 99 "is lexically stubbed.")
      (is *mock-calls* (acons "FOOR" (list '(5)) '())
          "*mocks-calls* is special.")
      (is (barr 5 10) 99 "is lexically stubbed.")
      (is (baz 15) 99 "is lexically stubbed.")
      (is (baz 20) 99 "is lexically stubbed.")
      (is (bazz 30) 99 "is lexically  stubbed.")
      (subtest "Get call times with (call-times-for)."
        (is (call-times-for 'foor) 1 "1 call time for foor.")
        (is (call-times-for 'barr) 1 "1 call time for barr")
        (is (call-times-for 'baz) 2 "2 call times for baz")
        (is (call-times-for 'bazz) 1 "1 call time for bazz"))
      (subtest "Verify call times with (verify-call-times-for)."
        (ok (verify-call-times-for 'foor 1) "runnable.")
        (ok (verify-call-times-for 'foor 1) "1 call.")
        (ok (verify-call-times-for 'barr 1) "1 call.")
        (ok (verify-call-times-for 'baz 2) "2 call.")
        (ok (verify-call-times-for 'bazz 1) "1 call.")))))

(defun with-mocks-test ()
  (diag "Testing in with-mocks-test.")
  (subtest "Basic mocks tests."
    (with-mocks (foor barr)
      (is (foor 5) nil "is nil.")
      (is (barr 10 15) nil "is nil."))))

(plan 6)

(init-test)
(*mock-calls*-init-test)
(with-stubs-test)
(with-mocks-test)


(finalize)
