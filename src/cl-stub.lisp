;;;; cl-stub/src/cl-stub.lisp

(in-package :cl-user)
(uiop:define-package :cl-stub/src/cl-stub
    (:use :closer-common-lisp)
  (:mix :fare-utils
        :uiop
        :alexandria
        :trivial-types
        :flexi-streams
        :bit-smasher
        :nibbles)
  (:import-from :let-over-lambda
                :defmacro!)
  (:import-from :trivial-arguments
                :arglist)
  (:documentation
   "")

  (:export :stub-fn :with-stubs :with-mocks :with-shadow
           :*mock-calls*
           :call-times-for
           :verify-call-times-for
           :clear-calls))

(uiop:define-package :cl-stub/src/mock
    (:nicknames :mock)
  (:use :closer-common-lisp))

(in-package :cl-stub/src/cl-stub)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Language macros ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defmacro fn-names-from (fdefs)
  `(mapcar #'first ,fdefs))

(defmacro returns-from (fdefs)
  `(mapcar #'second ,fdefs))

(defmacro register-mock-call-for (fn args)
  `(rplacd (mock-calls-spec-for ,fn)
           (append (list ,args)
                   (mock-calls-for ,fn))))

(defmacro register-mock-fn (fn)
  `(setf *mock-calls* (acons (string ,fn) '() *mock-calls*)))

(defmacro mock-fn-registered-p (fn)
  `(mock-calls-spec-for ,fn))

(defmacro mock-calls-spec-for (fn-name)
    "Accessor function for the special *mock-calls* variable.
   Returns the full spec."
    `(assoc (string ,fn-name) *mock-calls* :test #'string=))

(defmacro mock-calls-for (fn-name)
    "Accessor function for the special *mock-calls* variable.
   Returns the value associated with the function."
    `(cdr (mock-calls-spec-for ,fn-name)))

(defmacro call-times-for (fn-name)
    `(length (assoc-value *mock-calls* (string ,fn-name)
                          :test #'string=)))

(defun defined-fns-bound-p (fdefs)
  (with-simple-restart (ignore "Ignore error.")
      (let ((fn-names (fn-names-from fdefs)))
        (mapcar (lambda (name)
                  (if (fboundp name)
                      t
                      (undefined-stub-function-error name)))
                fn-names))
      t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic function stubbing and mocking ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mock-calls* '())

(defun flet-spec (fn-defs)
  (let* ((fn-names (fn-names-from fn-defs))
         (stub-returns (returns-from fn-defs))
         (args (gensym)))
    (mapcar (lambda (name stub-return)
                 `(,name (&rest ,args)
                        (unless (mock-fn-registered-p ',name)
                          (register-mock-fn ',name))
                        (register-mock-call-for ',name ,args)
                        (typecase ,stub-return
                          (function (apply ,stub-return ,args))
                          (t  ,stub-return))))
            fn-names stub-returns)))

(defmacro with-stubs ((&rest fdefs) &body body)
 "The stub macro. Lexically binds a new stub function in place
  and returns a constant supplied value. Calls are counted and
  the arguments are saved in the dynamic variable *mock-calls*."
 `(let* ((*mock-calls* (if (boundp '*mock-calls*)
                                 *mock-calls*
                                 '())))
    (declare (special *mock-calls*))
    (defined-fns-bound-p ',fdefs)
    (flet  ,(flet-spec fdefs)
        ,@body)))

(defmacro with-mocks ((&rest fn-names) &body body)
  "The mock macro. Lexically binds a new mock function which
   will return nil. Calls are counted and the arguments are saved
   in the dynamic variable *mock-calls*. "
  `(with-stubs ,(mapcar #'list
                          fn-names
                          (make-list (length fn-names)
                                     :initial-element nil))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Verify call times ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro verify-call-times-for (fn-name number)
  `(progn
     (= ,number (call-times-for ,fn-name))))

(defun nth-arglist-for (n fn-name)
  ())

(defmacro verify-nth-call-args-for (n fn-name &rest args)
    `(is (equal ,args (nth (decf ,n) (assoc-value *mock-calls* ,fn-name)))))

(defmacro verify-first-call-args-for (fn-name &rest args)
    `(verify-nth-call-args-for 1 ,fn-name ,@args))


(defun clear-calls ()
    (declare (special *mock-calls*))
    (setf *mock-calls* '()))


;; http://stackoverflow.com/questions/3074812/common-lisp-redefine-an-existing-function-within-a-scope
(defmacro! with-shadow ((fname fun) &body body)
  "Shadow the function named fname with fun. Any call to fname
   within body will use fun, instead of the default function for fname.
   This macro is intentionally unhygienic: fun-orig is the anaphor,
   and can be used in body to access the shadowed function"
  `(let ((fun-orig))
       (cond ((fboundp ',fname)
              (setf fun-orig (symbol-function ',fname))
              (setf (symbol-function ',fname) ,fun)
              (unwind-protect (progn ,@body)
                (setf (symbol-function ',fname) fun-orig)))
             (t
              (setf (symbol-function ',fname) ,fun)
              (unwind-protect (progn ,@body)
                (fmakunbound ',fname))))))


;;;;;;;;;;;;;;
;;; Error. ;;;
;;;;;;;;;;;;;;

(define-condition undefined-stub-function (undefined-function) ()
  (:report (lambda (condition stream)
             (format stream "The defined stub function for ~s does not ~
                          have a defined original." (cell-error-name condition))))
  (:documentation
   (format nil "Error: ")))

(defun undefined-stub-function-error (name)
  "The error function for undefined-stub-function."
  (error 'undefined-stub-function :name name))
