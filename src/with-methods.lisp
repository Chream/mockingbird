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

  (:export :with-method-stubs))

(in-package :cl-mock/src/with-methods)

;;;;;;;;;;;;;;;;;;
;;; Interface. ;;;
;;;;;;;;;;;;;;;;;;

(defmacro with-method-stubs ((&rest method-defs) &body body)
  (with-gensyms (stub-method-objects applicable-methods-objects)
    (let ((gf-objects (gf-metaobjects-from method-defs)))
      `(let ((,stub-method-objects
              ,(method-metaobjects-from method-defs))
             (,applicable-methods-objects
              (applicable-methods-metaobjects-from ,stub-method-objects)))
         ;; Remove originals.
         (mapcar #'remove-methods-for
                 ,gf-objects
                 ,applicable-methods-objects)
         (unwind-protect (progn ,@body)
           ;; Remove stub methods
           (mapcar #'remove-method
                   ,gf-objects
                   ,stub-method-objects)
           ;; Add original methods
           (mapcar #'add-methods-for
                   ,gf-objects
                   ,applicable-methods-objects))))))

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
  (let ((generic-fns (gf-metaobjects-from method-defs))
        (specializers-list (specializers/list-from method-defs)))
    (mapcar
     (lambda (generic-fn specializers)
       (closer-mop:compute-applicable-methods-using-classes
        generic-fn specializers))
     generic-fns specializers-list)))

(defun method-metaobjects-from (method-defs)
  `(list ,@(mapcar (lambda (method-def)
                     `(defmethod ,@method-def))
                   method-defs)))

(defun remove-methods-for
    (generic-function-object applicable-methods-objects)
  (dolist (applicable-method applicable-methods-objects)
    (remove-method generic-function-object applicable-method)))

(defun add-methods-for
    (generic-function-object applicable-methods-objects)
  (dolist (applicable-method applicable-methods-objects)
    (add-method generic-function-object applicable-method)))
