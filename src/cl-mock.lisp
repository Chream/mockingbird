;;;; cl-mock/src/cl-mock.lisp

(in-package :cl-user)
(uiop:define-package :cl-mock/src/cl-mock
    (:use :closer-common-lisp)
  (:mix :fare-utils
        :uiop
        :alexandria)
  (:import-from :trivial-arguments
                :arglist)
  (:documentation
   "")

  (:export :with-stubs :with-mocks
           :with-dynamic-stubs :with-dynamic-mocks
           :*mock-calls*
           :call-times-for
           :nth-mock-args-for
           :verify-nth-call-args-for
           :verify-first-call-args-for
           :verify-call-times-for
           :clear-calls

           :undefined-function
           :undefined-stub-function-error))

(in-package :cl-mock/src/cl-mock)

;;;;;;;;;;;;;;;;;;
;;; Interface. ;;;
;;;;;;;;;;;;;;;;;;

(defun call-times-for (fn-name)
  (declare (special *mock-calls*))
  (length (assoc-value *mock-calls* (string fn-name)
                       :test #'string=)))

(defun verify-call-times-for (fn-name number)
  (= number (call-times-for fn-name)))

(defun nth-mock-args-for (n fn-name)
  (nth (1- n) (reverse (mock-calls-for fn-name))))

(defun verify-nth-call-args-for (n fn-name &rest args)
  (declare (special *mock-calls*))
  (equal args (nth-mock-args-for n fn-name)))

(defmacro verify-first-call-args-for (fn-name &rest args)
  `(verify-nth-call-args-for 1 ,fn-name ,@args))

(defun clear-calls ()
  (declare (special *mock-calls*))
  (setf *mock-calls* '())
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lexical stubbing and mocking macros ;;;
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
                        (etypecase ,stub-return
                          (function (apply ,stub-return ,args))
                          (atom  ,stub-return))))
            fn-names stub-returns)))

(defmacro with-stubs ((&rest fdefs) &body body)
 "The stub macro. Lexically binds a new stub function in place
  and returns a constant supplied value. Calls are counted and
  the arguments are saved in the dynamic variable *mock-calls*."

 `(locally
      ;; TODO Remove the warnings in a better way. Currently all
      ;;      warnings are removed..
      (declare #+sbcl(sb-ext:muffle-conditions sb-kernel::simple-warning))
    (let* ((*mock-calls* (if (boundp '*mock-calls*)
                             *mock-calls*
                             '())))
      (declare (special *mock-calls*))
      (defined-fns-bound-p ',fdefs)
      (flet  ,(flet-spec fdefs)
        ,@body))))

(defmacro with-mocks ((&rest fn-names) &body body)
  "The mock macro. Lexically binds a new mock function which
   will return nil. Calls are counted and the arguments are saved
   in the dynamic variable *mock-calls*. "
  `(with-stubs ,(mapcar #'list
                          fn-names
                          (make-list (length fn-names)
                                     :initial-element nil))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dynamic stubbing and mocking macros ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun stub-fns-from (fdefs)
  (let* ((stub-returns (returns-from fdefs))
        (fn-names (fn-names-from fdefs))
        (argss (loop :repeat (length fn-names) :collect (gensym))))
    (mapcar (lambda (name stub-return args)
                 `(lambda (&rest ,args)
                  (unless (mock-fn-registered-p ',name)
                    (register-mock-fn ',name))
                  (register-mock-call-for ',name ,args)
                  (restart-case
                      (etypecase ,stub-return
                        (function (apply ,stub-return ,args))
                        (atom ,stub-return))
                    (just-return-value () ,stub-return))))
            fn-names stub-returns argss)))

(defun replace-fn-bindings-spec (fdefs temp-fn-vars)
  (let ((fn-names (fn-names-from fdefs))
        (stub-fns (stub-fns-from fdefs)))
    `(progn
       ,@(mapcar (lambda (fname stub-fn temp-var)
                 `(cond ((fboundp ',fname)
                         (setf (symbol-function ',temp-var)
                               (symbol-function ',fname))
                         (setf (symbol-function ',fname) ,stub-fn))
                        (t
                         (undefined-stub-function-error ',fname))))
               fn-names stub-fns temp-fn-vars))))

(defun rebind-original-fn-bindings-spec (fdefs temp-fn-vars)
  (let ((fn-names (fn-names-from fdefs)))
    `(progn
       ,@(mapcar
          (lambda (fname temp-var)
            `(if (fboundp ',temp-var)
                 (setf (symbol-function ',fname)
                       (symbol-function ',temp-var))
                 (error (format nil "Temp var '~S is unbound. ~
                                     Something went wrong."
                                ',temp-var))))
               fn-names temp-fn-vars))))

(defmacro with-dynamic-stubs ((&rest fdefs) &body body)
  (let* ((fn-names (fn-names-from fdefs))
         (temp-fn-vars (loop :for fn in fn-names
                          :collect (gensym (format nil "~S-orig" fn)))))
    `(locally
         ;; TODO Remove the warnings in a better way. Currently all
         ;;      warnings are removed..
         (declare #+sbcl(sb-ext:muffle-conditions sb-kernel::simple-warning))
       (let ((*mock-calls* (if (boundp '*mock-calls*)
                               *mock-calls*
                               '())))
         (declare (special *mock-calls* ,@temp-fn-vars))
         (defined-fns-bound-p ',fdefs)
         ,(replace-fn-bindings-spec fdefs temp-fn-vars)
         (unwind-protect (progn ,@body)
           ,(rebind-original-fn-bindings-spec fdefs temp-fn-vars))))))

(defmacro with-dynamic-mocks ((&rest fn-names) &body body)
  `(with-dynamic-stubs ,(mapcar #'list
                                fn-names
                                (make-list (length fn-names)
                                           :initial-element nil))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;
;;; Language/Utils ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun fn-names-from (fdefs)
  (mapcar #'first fdefs))

(defun returns-from (fdefs)
  (mapcar #'second fdefs))

(defun register-mock-call-for (fn args)
  (rplacd (mock-calls-spec-for fn)
           (append (list args)
                   (mock-calls-for fn))))

(defun register-mock-fn (fn)
  (setf *mock-calls* (acons (string fn) '() *mock-calls*)))

(defun mock-fn-registered-p (fn)
  (mock-calls-spec-for fn))

(defun mock-calls-spec-for (fn-name)
    "Accessor function for the special *mock-calls* variable.
   Returns the full spec."
    (assoc (string fn-name) *mock-calls* :test #'string=))

(defun mock-calls-for (fn-name)
    "Accessor function for the special *mock-calls* variable.
   Returns the value associated with the function."
    (cdr (mock-calls-spec-for fn-name)))

(defun defined-fns-bound-p (fdefs)
  (with-simple-restart (ignore "Ignore error.")
      (let ((fn-names (fn-names-from fdefs)))
        (mapcar (lambda (name)
                  (if (fboundp name)
                      t
                      (undefined-stub-function-error name)))
                fn-names))
      t))


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
