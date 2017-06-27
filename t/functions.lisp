;;;; mockingbird/t/mockingbird.lisp

(in-package :cl-user)
(uiop:define-package :mockingbird/t/functions
    (:use :closer-common-lisp
          :prove
          :mockingbird)
  (:documentation
   "")

  (:export :run-tests))

(in-package :mockingbird/t/functions)

;; NOTE: To run this test file, execute `(asdf:test-system :mockingbird)' in your Lisp.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parameters/Constants/Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setf prove:*enable-colors* t)
;; (setf *default-reporter* :list)
;; (setf prove:*debug-on-error* t)

(defun run-tests ()
  (init-test)
  (*mock-calls*-init-test)
  (with-stubs-test)
  (with-mocks-test)
  (with-dynamic-stubs-test)
  (with-dynamic-mocks-test))

(defun foo (x) x)
(defun bar (x y) (+ x y))
(defun baz (x)
  (+ (foo x) (bar x x)))
(defun fom (x)
  (list (foo x)))
(defun fob (x)
  (bar x x))
(defun add (x y) (+ x y))
(defun sub (x y) (- x y))
(defun mul (x y) (* x y))
(defun fun () (funcall (lambda () 'fun)))
(defun lam () (lambda () 'lam))
(defun sym () 'some-symbol)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Test functions. ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun init-test ()
  (diag "Init tests.")
  (is (foo 5) 5 "(foo) returns correctly.")
  (is (bar 5 10) 15 "(bar) returns correctly.")
  (is (baz 30) 90 "(baz) returns correctly.")
  (is (fom 5) '(5) "(fom) returns correctly.")
  (is (fob 5) 10 "(fob) returns correctly.")
  (is (add 30 30) 60 "(add) returns correctly.")
  (is (sub 30 30) 0 "(sub) returns correctly.")
  (is (mul 30 30) 900 "(mul) returns correctly.")
  (is (fun) 'fun "(fun) returns correctly.")
  (is-type (lam) 'function "(add) returns correctly.")
  (is (sym) 'some-symbol "(sym) returns correctly"))

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

(defun mock-args-test ()
  (diag "Testing mock arguments collection in (with-stubs).")
  (with-stubs ((foo 5))
    (foo 1)
    (foo 2 3)
    (foo "string")
    (foo 'symbol)
    (is (nth-mock-args-for 1 'foo) '(1) "First args was 1 ok.")
    (is (nth-mock-args-for 2 'foo) '(2 3) "Second args was 1 ok.")
    (is (nth-mock-args-for 3 'foo) '("string") "Third args was 1 ok.")
    (is (nth-mock-args-for 4 'foo) '(symbol) "Fourth arg was 1 ok.")))

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
        ((foo 99) (bar 99) (baz 99) (sym 'another-symbol))
      (is (foo 5) 99 "(foo) is lexically stubbed.")
      (is *mock-calls* (acons 'foo (list '(5)) '())
          "*mocks-calls* is special.")
      (is (bar 5 10) 99 "(bar) is lexically stubbed.")
      (is (sym 3) 'another-symbol "(sym) is lexically stubbed.")
      (is (baz 10) 99 "(foo) and (bar) not specially stubbed.")))
  (subtest "Testing call times."
    (call-times-for-test)
    (verify-call-times-for-test))
  (subtest "Testing stub expressions."
    (with-stubs
        ((add (+ 5 6)) (sub (- 3 4)) (mul (* 2 10))
         (fun (funcall (lambda () 10))) (lam (lambda (x) (+ x 1)))
         (sym 'another-symbol))
      (is (add 10) 11 "Addition ok.")
      (is (sub 20) -1 "Substraction ok.")
      (is (mul 5) 20 "Multiplication ok.")
      (is (fun 1) 10 "Simple funcalled lambda expression ok.")
      (is (lam 20) 21 "Lambda expression ok.")
      (is (sym) 'another-symbol "Symbol expression ok.")))

  (subtest "Testing nested (with-stubs)"
    (with-stubs ((foo 100) (bar 200))
      (foo 10)
      (bar 20)
      (is (call-times-for 'foo) 1 "Outer call times (foo) = 1 ok.")
      (is (call-times-for 'bar) 1 "Outer call times (bar) = 1 ok.")
      (with-stubs ((foo 10) (bar 20))
        (is (foo 5) 10 "Nested integer shadow (foo) ok.")
        (is (bar 5) 20 "Nested integer shadow (bar) ok.")
        (is (call-times-for 'foo) 2 "Nested call times (foo) = 2 ok.")
        (is (call-times-for 'bar) 2 "Nested call times (bar) = 2 ok."))
      (is (call-times-for 'foo) 2 "Outer call times (foo) = 2 ok.")
      (is (call-times-for 'bar) 2 "Outer call times (bar) = 2 ok.")))
  (subtest "Testing (clear-calls)."
    (clear-calls-test))
  (subtest "Testing argument capture."
    (mock-args-test)))

(defun with-mocks-test ()
  (diag "Testing in (with-mocks-test.)")
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

(defun call-times-for-dynamic-test ()
  (diag "Testing in (call-times-for).")
  (clear-calls)
  (with-dynamic-stubs ((foo 10) (bar 20))
    (foo 5)
    (bar 10)
    (foo 10)
    (add 3 4)
    (baz 10)
    (is (call-times-for 'foo) 3 "foo calls 2.")
    (is (call-times-for 'bar) 2 "bar calls 1.")
    (is (call-times-for 'add) 0 "add calls 0 (not stubbed).")))

(defun verify-call-times-for-dynamic-test ()
  (diag "Testing in (verify-call-times-for-test).")
  (clear-calls)
  (with-dynamic-stubs ((foo 10) (bar 20))
    (foo 234)
    (bar 1234)
    (foo 234)
    (bar 2345)
    (bar 2345)
    (baz 10)
    (ok (verify-call-times-for 'foo 3) "foo calls 2.")
    (ok (verify-call-times-for 'bar 4) "bar calls 3.")))

(defun dynamic-mock-args-test ()
  (diag "Testing mock arguments collection in (with-stubs).")
  (with-dynamic-stubs ((foo 5))
    (foo 1)
    (foo 2 3)
    (fom "string")
    (fom 'symbol)
    (is (nth-mock-args-for 1 'foo) '(1) "First args was 1 ok.")
    (is (nth-mock-args-for 2 'foo) '(2 3) "Second args was 1 ok.")
    (is (nth-mock-args-for 3 'foo) '("string") "Third args was 1 ok.")
    (is (nth-mock-args-for 4 'foo) '(symbol) "Fourth arg was 1 ok.")
    (ok (verify-nth-call-args-for 2 'foo 2 3) "nth arg verify ok.")
    (ok (verify-first-call-args-for 'foo 1) "First arg verify ok.")))

(defun with-dynamic-stubs-test ()
  (diag "Testing in (with-dynamic-stubs-test).")
  (subtest "Testing initial functions."
    (init-test))
  (subtest "Testing dynamic binding."
    (with-dynamic-stubs
        ((foo 99) (bar 98))
      (is (foo 123) 99 "(foo) is lexically rebound.")
      (is (bar 23) 98 "(bar) Is lexiaclly rebound.")
      (is (baz 50) 197 "(foo) and (bar) is dynamically bound"))
    (subtest "Testing functions correctly rebound."
      (init-test)))
  (subtest "Testing with stub expressions."
    (with-dynamic-stubs
        ((add (+ 6 5)) (sub (- 3 4)) (mul (* 2 10))
         (fun (funcall (lambda () 10))) (lam (lambda (x) (+ x 1)))
         (sym 'another-symbol))
      (is (add 10) 11 "Addition ok.")
      (is (sub 20) -1 "Subtraction ok.")
      (is (mul 5) 20 "Multiplication ok.")
      (is (fun 1) 10 "Simple funcalled lambda expression ok.")
      (is (lam 20) 21 "Lambda expression ok.")
      (is (sym 234) 'another-symbol "Symbol expression ok.")
      (is (add) 11 "(add) no arg ok,")
      (is (handler-case (sym)
            (type-error (c) (type-error-datum c)))
          'another-symbol
          "Wierd error handled ('prove' related?) ok.")))
  (subtest "Testing dynamic call times."
    (call-times-for-dynamic-test)
    (verify-call-times-for-dynamic-test))
  (subtest "Testing nested (with-dynamic-stubs)"
    (with-dynamic-stubs ((foo 100) (bar 200))
      (foo 10)
      (bar 20)
      (baz 10)
      (is (call-times-for 'foo) 2 "Outer call times = 2 (foo) ok.")
      (is (call-times-for 'bar) 2 "Outer call times = 2 (bar) ok.")
      (with-dynamic-stubs ((foo 10) (bar 20))
        (is (foo 5) 10 "Nested integer shadow (foo) ok.")
        (is (bar 5) 20 "Nested integer shadow (bar) ok.")
        (is (baz 5) 30 "Nested integer shadow (bar) ok.")
        (is (call-times-for 'foo) 4 "nested call times (foo) ok.")
        (is (call-times-for 'bar) 4 "nested call times (bar) ok."))
      (is (call-times-for 'foo) 4 "Outer call times = 4 (foo) ok.")
      (is (call-times-for 'bar) 4 "Outer call times = 4 (bar) ok.")))
  (subtest "Testing argument capture."
    (dynamic-mock-args-test)))

(defun with-dynamic-mocks-test ()
  (diag "Testing in (with-dynamic-mocks-test).")
  (subtest "Testing in (with-dynamic-mocks)."
    (with-dynamic-mocks (foo bar)
      (is (foo 5) nil "foo is mocked.")
      (is (bar 10) nil "bar is mocked.")
      (is (fom 5) '(nil) "(foo) dynamically mocked ok.")
      (is (fob 5) nil "(bar) dynamically mocked ok."))))
