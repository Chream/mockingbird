;;;; cl-mock/t/with-method-stubs.lisp

(in-package :cl-user)
(uiop:define-package  :cl-mock/t/with-methods
    (:use :closer-common-lisp
          :prove
          :cl-mock/src/all)
  (:mix :fare-utils
        :uiop
        :alexandria)
  (:import-from :cl-mock/src/cl-mock
                :undefined-stub-method)
  (:documentation
   "")

  (:export :with-methods-tests))

(in-package :cl-mock/t/with-methods)

;;;;;;;;;;;;;;;;;;
;;; Main tests ;;;
;;;;;;;;;;;;;;;;;;

(defun with-methods-tests ()
  (init-tests)
  (with-method-stubs-test))

;;;;;;;;;;;;;;;;;;
;;; Parameters ;;;
;;;;;;;;;;;;;;;;;;

(defparameter *class-objects*
  (list
   (defclass aclass () ())
   (defclass baclass (aclass) ())
   (defclass caclass (aclass) ())))

(defparameter *foo-gf*
  (defgeneric foo (x y)))

(defparameter *original-method-objects*
  (list
   (defmethod foo (x y)
     (declare (ignore x y))
     'not-stubbed)
   (defmethod foo ((x aclass) (y aclass))
     (declare (ignore x y))
     '(aclass aclass))
   (defmethod foo ((x baclass) (y aclass))
     (declare (ignore x y))
     '(baclass aclass))
   (defmethod foo ((x aclass) (y baclass))
     (declare (ignore x y))
     '(aclass baclass))
   (defmethod foo ((x baclass) (y baclass))
     (declare (ignore x y))
     '(baclass baclass))))

(defun bar (x y)
  (foo x y))


(defparameter *aclass* (make-instance 'aclass))
(defparameter *baclass* (make-instance 'baclass))
(defparameter *caclass* (make-instance 'caclass))

(defun stub-def (fn-name spes1 spes2)
  `(,fn-name ((x ,spes1) (y ,spes2))
             '(,spes1 ,spes2)))

(defparameter *method-defs*
  (list (stub-def 'foo 'baclass 'baclass)
        (stub-def 'foo 'aclass 'aclass)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests Implementation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(macrolet ((var-test (arg)
             `(is-type ,(conc-symbol '* arg '*)
                       ',arg
                       ,(format nil "*~S* is type '~S." arg arg)))
           (init-test (arg1 arg2)
             `(is (foo ,(conc-symbol '* arg1 '*)
                       ,(conc-symbol '* arg2 '*))
                  '(,arg1 ,arg2) ,(format nil "(foo *~S* *~S*) is '(~S ~S)."
                                          arg1 arg2 arg1 arg2))))
  (defun init-tests ()
    (diag "Testing in (init-tests).")
    (subtest "Testing variables."
      (var-test aclass)
      (var-test baclass)
      (var-test caclass))
    (subtest "Testing helper functions."
      (is (foo 1 2) 'not-stubbed "(foo 1 2) is 'not-stubbed.")
      (is (bar 1 2) 'not-stubbed "(bar) returns correctly.")
      (init-test aclass aclass)
      (init-test baclass aclass)
      (init-test aclass baclass)
      (init-test baclass baclass))
    (subtest "Testing misc functions."
      (gf-metaobjects-from-test)
      (method-spec-from-test)
      (method-metaobjects-from-test)
      (add/remove-methods-for-test))))

(defun with-method-stubs-test ()
  (diag "Testing in (with-method-stubs-test).")
  (with-method-stubs ((foo (x y) 'is-stubbed)
                      (foo ((x aclass) (y aclass)) 'aclass-stubbed))

    (subtest "Testing environment."
      (is (length (generic-function-methods *foo-gf*)) 5
          "5 defined methods.")
      (is-error (with-method-stubs ((foo ((x caclass) y) ()))
                  (foo 1 2)) 'undefined-stub-method
                  "`undefined-stub-method' error signaled ok."))

    (subtest "Testing binding and returns."
      (is (foo 1 2) 'is-stubbed "(foo 1 2) is lexically stubbed.")
      (is (bar 1 2) 'is-stubbed "(foo 1 2) is dynamically stubbed.")))

  (subtest "Testing rebinding of methods."
    (init-tests)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function tests/Utils. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gf-metaobjects-from-test ()
  (diag "Testing in (gf-metaobjects-from-test).")
  (is (gf-metaobjects-from *method-defs*) (list *foo-gf* *foo-gf*)
      (format nil "(gf-metaobjects-from) got 2x 'foo generic function."))
  nil)

(defun method-spec-from-test ()
  (diag "Testing in (method-spec-from-test).")
  (is (method-spec-from *method-defs*)
      '((DEFMETHOD FOO
            ((X BACLASS) (Y BACLASS))
          '(BACLASS BACLASS))
        (DEFMETHOD FOO
            ((X ACLASS) (Y ACLASS))
          '(ACLASS ACLASS)))
      "(method-metaobjects-spec-from) returns correctly.")
  nil)

(defun method-metaobjects-from-test ()
  (diag "Testing in (method-metaobjects-from-test).")
  (handler-bind ((warning (lambda (c)
                            (declare (ignore c))
                            (muffle-warning))))
    (is (length (method-metaobjects-from *method-defs*))
        2)))

(defun add/remove-methods-for-test ()
  (diag "Testing in (add/remove-methods-for-test).")
  (let ((method-objects (generic-function-methods *foo-gf*)))
    (is (length method-objects) 5 "5 methods for generic n (foo).")
    (remove-methods-for *foo-gf* method-objects)
    (is (length (generic-function-methods *foo-gf*)) 0
        "(remove-methods-for) removed methods successfully.")
    (add-methods-for *foo-gf* method-objects)
    (is (length (generic-function-methods *foo-gf*))
        (length method-objects)
        "(add-methods-for) added methods successfully."))
  nil)
