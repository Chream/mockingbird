#|
  This file is a part of cl-mock project.
  Copyright (c) 2016 Christopher Eames (Chream) (chream@gmx.com)
|#

(in-package :cl-user)
(defpackage cl-mock-test-asd
  (:use :cl :asdf))
(in-package :cl-mock-test-asd)

;;;; cl-mock/cl-mock-test.asd

#-asdf3.1 (error "asdf 3.1 or higher is required.")
(asdf:defsystem cl-mock-test
  :class :package-inferred-system
  :description "Test system for cl-mock."
  :author "Christopher Eames (Chream)"
  :license ""
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:cl-mock
               :prove
               "cl-mock/t/all")
  :components
  ((:test-file "t/main"))
  :perform (test-op (o s)
                    (uiop:symbol-call :prove '#:run s)
                    (asdf:clear-system s)))




(register-system-packages
 "closer-mop"
 '(:c2mop :closer-common-lisp :c2cl :closer-common-lisp-user :c2cl-user))


;; (defsystem cl-mock-test
;;   :author "Christopher Eames (Chream)"
;;   :license ""
;;   :depends-on (:cl-mock
;;                :prove)
;;   :components ((:module "t"
;;                         :components
;;                         ((:test-file "cl-mock"))))
;;   :description "Test system for cl-mock"

;;   :defsystem-depends-on (:prove-asdf)
;;   :perform (test-op :after (op c)
;;                     (funcall (intern #.(string :run-test-system) :prove-asdf) c)
;;                     (asdf:clear-system c)))
