;;;; cl-mock/src/with-methods.lisp

(in-package :cl-user)
(uiop:define-package  :cl-mock/src/with-methods
    (:use :closer-common-lisp)
  (:mix :fare-utils
        :uiop
        :alexandria)
  (:import-from :closer-mop
                :compute-applicable-methods-using-classes
                :method-specializers)
  (:import-from :cl-mock/src/cl-mock
                :undefined-stub-method
                :undefined-stub-function-error)
  (:documentation
   "")

  (:export :with-method-stubs

           :specializers/list-from
           :gf-metaobjects-from
           :applicable-methods-metaobjects-from
           :method-spec-from
           :method-metaobjects-from
           :remove-methods-for
           :add-methods-for

           ;; Mop extensions
           :generic-function-method-combination-type-name
           :generic-function-method-combination-options
           :method-combination-type-name
           :method-combination-options))

(in-package :cl-mock/src/with-methods)

;;;;;;;;;;;;;;;;;;
;;; Interface. ;;;
;;;;;;;;;;;;;;;;;;

(defmacro with-method-stubs ((&rest method-defs) &body body)
  (with-gensyms (stub-method-objects
                 applicable-method-objects/list
                 gf-objects)
    `(let* ((,gf-objects (gf-metaobjects-from ',method-defs))
            (,stub-method-objects (method-metaobjects-from ',method-defs))
            (,applicable-method-objects/list
             (applicable-methods-metaobjects-from ',method-defs)))
       (unwind-protect
            (progn
              ;; Remove originals.
              (mapcar #'remove-methods-for
                      ,gf-objects
                      ,applicable-method-objects/list)
              ;; Add stub methods.
              (mapcar #'add-method
                      ,gf-objects
                      ,stub-method-objects)
              ,@body)
         (progn
           ;; Remove stub methods.
           (mapcar #'remove-method
                   ,gf-objects
                   ,stub-method-objects)
           ;; Add original methods.
           (mapcar #'add-methods-for
                   ,gf-objects
                   ,applicable-method-objects/list))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Language/Utils ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defun specializers/list-from (method-objects)
  (mapcar #'method-specializers method-objects))

(defun gf-metaobjects-from (method-defs)
  (mapcar (lambda (method-def)
            (symbol-function (first method-def)))
          method-defs))

(defun applicable-methods-metaobjects-from (method-defs)
  (let* ((generic-fns (gf-metaobjects-from method-defs))
         (method-objects (method-metaobjects-from method-defs))
         (specializers-list (specializers/list-from method-objects)))
    (mapcar
     (lambda (generic-fn specializers)
       (let ((method-comb (generic-function-method-combination-type-name
                           generic-fn))
             (comb-options (generic-function-method-combination-options
                            generic-fn))
             (all-applicable-methods
              (compute-applicable-methods-using-classes
               generic-fn specializers)))
         (let ((result (filter-applicable-methods method-comb
                                                  comb-options
                                                  all-applicable-methods
                                                  specializers)))
           (unless result
             (undefined-stub-method-error
              generic-fn
              specializers))
           result)))
     generic-fns specializers-list)))

;;;;;;;;;;;;;;;;;;;;;;
;;; MOP extension. ;;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric method-combination-type-name (method-combination))
(defgeneric method-combination-type-options (method-combination))
(defgeneric generic-function-method-combination-type-name (generic-functions))
(defgeneric generic-function-method-combination-options (generic-functions))

(defmethod method-combination-type-name ((object method-combination))
  #+sbcl (sb-pcl::method-combination-type-name object)
  #+ccl (ccl::method-combination-name object))

(defmethod method-combination-options ((object method-combination))
  #+sbcl (sb-pcl::method-combination-options object)
  #+ccl (ccl::method-combination-options object))

(defmethod generic-function-method-combination-type-name
    ((object generic-function))
  (method-combination-type-name
   (generic-function-method-combination object)))

(defmethod generic-function-method-combination-options
    ((object generic-function))
  (method-combination-options
   (generic-function-method-combination object)))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric filter-applicable-methods
    (method-comb-spec comb-options method-objects specializers))

(defmethod filter-applicable-methods
    ((method-comb-spec (eql 'standard)) comb-options
     all-applicable-methods specializers)
  (let ((applicable-method (first all-applicable-methods)))
    (if (equal (method-specializers applicable-method)
               specializers)
        (list applicable-method)
        nil)))

(defmethod filter-applicable-methods
    (method-comb comb-options method-objects specializers)
  (method-combination-error
   "Error in (filter-applicable-methods ~S S).~
    Can not stub/mock methods for method combination ~S ~
    for method ~S with specializers ~S."
   method-comb method-objects method-comb (generic-function-name
                                           (method-generic-function
                                            (first method-objects)))
   specializers))

;;;;;;;;;;;;;;;;;;;;;;;;;

(defun method-metaobjects-from (method-defs)
  (let* ((gf-objects (gf-metaobjects-from method-defs))
         (original-methods/list (mapcar #'generic-function-methods
                                        gf-objects)))
    (unwind-protect
         (progn
           (mapcar (lambda (method-def)
                     (assert (eq (first method-def) 'defmethod))
                     (assert (= (length method-def) 4))
                     (handler-bind ((warning (lambda (c)
                                                (declare (ignore c))
                                                (muffle-warning))))
                       (eval method-def))) ;; unsafe?
                   (method-spec-from method-defs)))
      (mapcar (lambda (gf methods)
                (remove-methods-for gf (generic-function-methods gf))
                (add-methods-for gf methods))
              gf-objects original-methods/list))))

(defun method-spec-from (method-defs)
  (mapcar (lambda (method-def)
            `(defmethod ,@method-def))
          method-defs))

(defun remove-methods-for
    (generic-function-object applicable-methods-objects)
  (dolist (applicable-method applicable-methods-objects)
    (remove-method generic-function-object applicable-method)))

(defun add-methods-for
    (generic-function-object applicable-methods-objects)
  (dolist (applicable-method applicable-methods-objects)
    (add-method generic-function-object applicable-method)))

;;;;;;;;;;;;;;;;;;;
;;; Conditions. ;;;
;;;;;;;;;;;;;;;;;;;

(define-condition undefined-stub-method (undefined-function)
  ((specializers :initarg :specializers
                 :reader undefined-stub-error-specializers
                 :type list))
  (:report (lambda (condition stream)
             (format stream
                     "The defined stub method for ~&~S~& with ~
                     with specializers ~&~S~& does not have a ~
                     defined original."
                     (cell-error-name condition)
                     (undefined-stub-error-specializers condition))))
  (:documentation "Error: "))

(defun undefined-stub-method-error (name specializers)
  "The error function for undefined-stub-function."
  (error 'undefined-stub-method
         :name name
         :specializers specializers))
