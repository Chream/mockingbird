;;;; cl-stub/cl-stub.asd

#|
  This file is a part of cl-stub project.
  Copyright (c) 2016 Christopher Eames (Chream) (chream@gmx.com)
|#

#|
  Author: Christopher Eames (Chream) (chream@gmx.com)
|#

(in-package :cl-user)
(defpackage cl-stub-asd
  (:use :cl :asdf))
(in-package :cl-stub-asd)

#-asdf3.1 (error "asdf 3.1 or higher is required.")
(asdf:defsystem cl-stub
  :version "0.1"
  :class :package-inferred-system
  :description ""
  :author "Christopher Eames (Chream)"
  :license ""
  :depends-on ("cl-stub/src/all")
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op "cl-stub-test"))))

(register-system-packages
 "closer-mop"
 '(:c2mop :closer-common-lisp :c2cl :closer-common-lisp-user :c2cl-user))
