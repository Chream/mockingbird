#|
  This file is a part of mockingbird project.
  Copyright (c) 2016 Christopher Eames (Chream) (chream@gmx.com)
|#

(in-package :cl-user)
(defpackage mockingbird-test-asd
  (:use :cl :asdf))
(in-package :mockingbird-test-asd)

;;;; mockingbird/mockingbird-test.asd

#-asdf3.1 (error "asdf 3.1 or higher is required.")
(asdf:defsystem mockingbird-test
  :class :package-inferred-system
  :description "Test system for mockingbird."
  :author "Christopher Eames (Chream)"
  :license ""
  :defsystem-depends-on (:prove-asdf)
  :depends-on (:mockingbird
               :prove
               "mockingbird/t/all")
  :components
  ((:test-file "t/main"))
  :perform (test-op :after (o s)
                    (uiop:symbol-call :prove '#:run s)
                    (asdf:clear-system s)))

(register-system-packages
 "closer-mop"
 '(:c2mop :closer-common-lisp :c2cl :closer-common-lisp-user :c2cl-user))
