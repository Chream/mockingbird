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


(defun foo (x) x)
(defun bar (x y) (+ x y))
(defun baz (x)
    (+ (foo x) (bar x x)))
(defun add (x y) (+ x y))
(defun sub (x y) (- x y))
(defun mul (x y) (* x y))
(defun fun () (funcall (lambda () 'fun)))
(defun lam () (lambda () 'lam))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Test functions. ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun init-test ()
  (diag "Testing in with-stubs-test.")
  (subtest "Init tests."
    (is (foo 5) 5 "(foo) returns correctly.")
    (is (bar 5 10) 15 "(bar) returns correctly.")
    (is (baz 30) 90 "returns correctly.")
    (is (add 30 30) 60 "(add) returns correctly.")
    (is (sub 30 30) 0 "(sub) returns correctly.")
    (is (mul 30 30) 900 "(mul) returns correctly.")
    (is (fun) 'fun "(fun) returns correctly.")
    (is-type (lam) 'function "(add) returns correctly.")))

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

(defun call-times-for-test ()
  (diag "Testing in (call-times-for).")
  (clear-calls)
  (with-stubs ((foo 10) (bar 20))
    (foo 5)
    (bar 10)
    (foo 10)
    (add 3 4)
    (is (call-times-for 'foo) 2 "foo calls 2.")
    (is (call-times-for 'bar) 1 "bar calls 1.")
    (is (call-times-for 'add) 0 "add calls 1 (not stubbed.).")))

(defun verify-call-times-for-test ()
  (diag "Testing in (verify-call-times-for-test).")
  (clear-calls)
  (with-stubs ((foo 10) (bar 20))
    (foo 234)
    (bar 1234)
    (foo 234)
    (bar 2345)
    (bar 2345)
    (ok (verify-call-times-for 'foo 2) "foo calls 2,")
    (ok (verify-call-times-for 'bar 3) "bar calls 3.")))

(defun clear-calls-test ()
  (diag "Testing on (clear-calls-test).")
  (with-stubs ((foo 10) (bar 20))
    (foo 10)
    (foo 20)
    (ok (verify-call-times-for 'foo 2) "Is counted.")
    (is (clear-calls) (values) "no return (clear-cells).")
    (ok (verify-call-times-for 'foo 0) "Is cleared.")))

(defun with-stubs-test ()
  (diag "Testing in (with-stubs-test).")
  (subtest "Testing (with-stubs) return values."
    (with-stubs
        ((foo 99) (bar 99) (baz 99))
      (is (foo 5) 99 "(foo) is lexically stubbed.")
      (is *mock-calls* (acons "FOO" (list '(5)) '())
          "*mocks-calls* is special.")
      (is (bar 5 10) 99 "(bar) is lexically stubbed.")
      (is (baz 10) 99 "(foo) and (bar) not specially stubbed.")))
  (subtest "Testing call times."
    (call-times-for-test)
    (verify-call-times-for-test))
  (subtest "Testing stub expressions."
    (with-stubs
        ((add (+ 5 6)) (sub (- 3 4)) (mul (* 2 10))
         (fun (funcall (lambda () 10))) (lam (lambda (x) (+ x 1))))
      (is (add 10) 11 "Addition ok.")
      (is (sub 20) -1 "Substraction ok.")
      (is (mul 5) 20 "Multiplication ok.")
      (is (fun 1) 10 "Simple funcalled lambda expression ok.")
      (is (lam 20) 21 "Lambda expression ok.")))

  (subtest "Testing nested (with-stubs)"
    (with-stubs ((foo 100) (bar 200))
      (foo 10)
      (bar 20)
      (ok (verify-call-times-for 'foo 1) "Outer call times (foo) ok.")
      (ok (verify-call-times-for 'bar 1) "Outer call times (bar) ok.")
      (with-stubs ((foo 10) (bar 20))
        (is (foo 5) 10 "Nested integer shadow (foo) ok.")
        (is (bar 5) 20 "Nested integer shadow (bar) ok.")
        (ok (verify-call-times-for 'foo 2) "nested call times (foo) ok.")
        (ok (verify-call-times-for 'bar 2) "nested call times (bar) ok."))()))
  (subtest "Testing (clear-calls)."
    (clear-calls-test)))

(defun with-mocks-test ()
  (diag "Testing in with-mocks-test.")
  (subtest "Testing mock function return values."
    (with-mocks (foo bar)
      (is (foo 5) nil "is nil.")
      (is (bar 10 15) nil "is nil.")
      (is (baz 10) 30 "is not special.")))
  (subtest "Testing nested (with-mock),"
    (with-mocks (foo bar)
      (with-mocks (foo bar)
        (is (foo 20) nil "foo is nil.")
        (is (bar 10) nil "bar is nil")
        (is (baz 20) 60 "not special.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun with-dynamic-stubs-test ()                     ;;
;;   (diag "Testing in (with-dynamic-stubs-test).")      ;;
;;   (with-dynmaic-stubs                                 ;;
;;       ((add 10) (sub 5) (mul 'mul)                    ;;
;;        (lam (lambda () 'rebound)) (baz 10)            ;;
;;        (foo 5) (bar 10))                              ;;
;;     (is (add 5 3) 10 "(add) is lexically rebound.")   ;;
;;     (is (sub 2 3) 5 "(sub) Is lexiaclly rebound")     ;;
;;     (is (lam 2) 'rebound "(lam) is lexically bound.") ;;
;;     (is (baz 50) 15 "is dynamicly bound")))           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plan 11)
(init-test)
(*mock-calls*-init-test)
(with-stubs-test)
(with-mocks-test)

(finalize)
