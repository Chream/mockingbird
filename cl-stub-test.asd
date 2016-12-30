#|
  This file is a part of cl-stub project.
  Copyright (c) 2016 Christopher Eames (Chream) (chream@gmx.com)
|#

(in-package :cl-user)
(defpackage cl-stub-test-asd
  (:use :cl :asdf))
(in-package :cl-stub-test-asd)

;;;; cl-stub/cl-stub-test.asd

#-asdf3.1 (error "asdf 3.1 or higher is required.")
(asdf:defsystem cl-stub-test
  :class :package-inferred-system
  :description "Test system for cl-stub."
  :author "Christopher Eames (Chream)"
  :license ""
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:cl-stub
               :prove
               "cl-stub/t/all")
  :components
  ((:test-file "t/main"))
  :perform (test-op (o s)
                    (uiop:symbol-call :prove '#:run s)
                    (asdf:clear-system s)))




(register-system-packages
 "closer-mop"
 '(:c2mop :closer-common-lisp :c2cl :closer-common-lisp-user :c2cl-user))


;; (defsystem cl-stub-test
;;   :author "Christopher Eames (Chream)"
;;   :license ""
;;   :depends-on (:cl-stub
;;                :prove)
;;   :components ((:module "t"
;;                         :components
;;                         ((:test-file "cl-stub"))))
;;   :description "Test system for cl-stub"

;;   :defsystem-depends-on (:prove-asdf)
;;   :perform (test-op :after (op c)
;;                     (funcall (intern #.(string :run-test-system) :prove-asdf) c)
;;                     (asdf:clear-system c)))
