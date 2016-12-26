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
           :verify-call-times-for))

(in-package :cl-stub/src/cl-stub)


;;;;;;;;;;;;;;;;;;;;;;;
;;; Language macros ;;;
;;;;;;;;;;;;;;;;;;;;;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro fn-names-from (fdefs)
    `(mapcar #'first ,fdefs))
  (defmacro args-from (fns)
    `(mapcar #'arglist ,fns))

  (defmacro returns-from (fdefs)
    `(mapcar #'second ,fdefs))

  (defmacro make-stub-fns-from (fn-names stub-returns)
    `(mapcar #'stub-fn ,fn-names ,stub-returns))

  (defmacro register-mock-call-for (fn args)
    `(rplacd (mock-calls-spec-for ,fn)
             (append (list ,args)
                     (mock-calls-for ,fn))))

  (defmacro register-mock-fn (fn)
    `(setf *mock-calls* (acons (string ,fn) '() *mock-calls*)))

  (defmacro mock-fn-registered-p (fn)
    `(mock-calls-spec-for ,fn))

  (defmacro make-stub-fn-name-for (fn)
    `(conc-symbol ,fn '-stub (gensym)))

  (defun mock-calls-spec-for (fn-name)
    "Accessor function for the special *mock-calls* variable.
   Returns the full spec."
    (assoc (string fn-name) *mock-calls* :test #'string=))

  (defmacro mock-calls-for (fn-name)
    "Accessor function for the special *mock-calls* variable.
   Returns the value associated with the function."
    `(cdr (mock-calls-spec-for ,fn-name)))

  (defun call-times-for (fn-name)
    (length (assoc-value *mock-calls* (string fn-name)
                         :test #'string=)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic function stubbing and mocking ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defvar *mock-calls* '())

  (defun stub-fn (the-function return-value)
    "Returns a stub `named-function' interned into a temprary name.
   The function also closes over the dynammic *mock-calls* variable
   which keeps track of calls and arguments in a alist. function name
   is the key."
    (let* ((stub-fn-name (make-stub-fn-name-for the-function)))
      (setf (symbol-function stub-fn-name)
            (lambda (&rest args)
              (unless (mock-fn-registered-p the-function)
                (register-mock-fn the-function))
              (register-mock-call-for the-function args)
              return-value))
      (intern (string stub-fn-name))
      stub-fn-name))

  (defmacro with-stubs ((&rest fdefs) &body body)
    "The stub macro. Lexically binds a new stub function in place
   and returns a constant supplied value. Calls are counted and
   the arguments are saved in the dynamic variable *mock-calls*."
    (let* ((fn-names (fn-names-from fdefs))
           (fn-args (mapcar #'arglist fn-names))
           (stub-returns (returns-from fdefs))
           (stub-fns (make-stub-fns-from fn-names stub-returns)))
      `(let* ((*mock-calls* (if (boundp *mock-calls*)
                                *mock-calls*
                                '())))
         (declare (special *mock-calls*))
         (flet  ,(mapcar
                  (lambda  (name args stub)
                    (list name args `(funcall #',stub ,@args)))
                  fn-names fn-args stub-fns)
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
                (fmakunbound ',fname)))))))
