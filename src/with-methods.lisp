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
  (:documentation
   "")

  (:export :with-method-stubs

           :specializers/list-from
           :gf-metaobjects-from
           :applicable-methods-metaobjects-from
           :method-spec-from
           :method-metaobjects-from
           :remove-methods-for
           :add-methods-for))

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
;;; objects/Utils ;;;
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
       (compute-applicable-methods-using-classes
        generic-fn specializers))
     generic-fns specializers-list)))

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
