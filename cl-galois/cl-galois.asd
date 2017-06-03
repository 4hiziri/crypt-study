#|
  This file is a part of cl-galois project.
  Copyright (c) 2017 4hiziri
|#

#|
  tool for Galois Field GF(2^8)

  Author: 4hiziri
|#

(in-package :cl-user)
(defpackage cl-galois-asd
  (:use :cl :asdf))
(in-package :cl-galois-asd)

(defsystem cl-galois
  :version "0.1"
  :author "4hiziri"
  :license ""
  :depends-on (:cl-annot :cl-crypto-util)
  :components ((:module "src"
                :components
                ((:file "cl-galois"))))
  :description "tool for Galois Field GF(2^8)"
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
  :in-order-to ((test-op (test-op cl-galois-test))))
