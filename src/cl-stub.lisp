;;;; cl-stub/src/cl-stub.lisp

(in-package :cl-user)
(uiop:define-package  :cl-stub/src/cl-stub
    (:use :closer-common-lisp)
  (:mix :fare-utils
        :uiop
        :alexandria)
  (:import-from :let-over-lambda
                :defmacro!)
  (:import-from :trivial-arguments
                :arglist)
  (:documentation
   "")

  (:export :stub-fn :with-stubs :with-mocks :with-shadow))

(in-package :cl-stub/src/cl-stub)

(defun stub-fn (the-function return-value)
  "Returns a stub `function' interned into a temprary name.
   The function also closes over the dynammic *mock-calls* variable
   which keeps track of calls and arguments in a alist. function name
   is the key."
  (let* ((fn-name (fare-utils:conc-symbol the-function 'most-positive-double-float (gensym))))
    (setf (symbol-function fn-name)
          (lambda (&rest args)
            (declare (special *mock-calls*))
            (unless (assoc the-function *mock-calls*)
              (setf *mock-calls* (acons the-function '() *mock-calls*)))
            (rplacd (assoc the-function *mock-calls*)
                    (append (list args)
                            (assoc-value *mock-calls* the-function)))
            return-value))
    fn-name))

(defmacro with-stubs ((&rest fdefs) &body body)
  "The stub macro. Lexically binds a new stub function in place
   and returns a constant supplied value. Calls are counted and
   the arguments are saved in the dynamic variable *mock-calls*."
  (let* ((fn-names (mapcar #'first fdefs))
         (fn-args (mapcar #'arglist fn-names))
         (stub-returns (mapcar #'second fdefs))
         (stub-fns (mapcar #'stub-fn fn-names stub-returns)))
    `(let* ((*mock-calls* (if (boundp *mock-calls*)
                              *mock-calls*
                              '())))
       (declare (special *mock-calls*))
       (flet  ,(mapcar (lambda  (name args stub)
                         (list name args `(apply #',stub (list ,@args))))
                       fn-names fn-args stub-fns)
         ,@body))))

(defmacro with-mocks ((&rest fn-names) &body body)
  "The mock macro. Lexically binds a new mock function which
   will return nil. Calls are counted and the arguments are saved
   in the dynamic variable *mock-calls*. "
  `(with-stubs ,(mapcar #'list fn-names (make-list (length fn-names) :initial-element nil))
     ,@body))

;; http://stackoverflow.com/questions/3074812/common-lisp-redefine-an-existing-function-within-a-scope
(defmacro! with-shadow ((fname fun) &body body)
  "Shadow the function named fname with fun
   Any call to fname within body will use fun, instead of the default function for fname.
   This macro is intentionally unhygienic:
   fun-orig is the anaphor, and can be used in body to access the shadowed function"
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
