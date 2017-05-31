#|
  This file is a part of cl-crypto-util project.
  Copyright (c) 2017 4hiziri
|#

#|
  util for crypto-study

  Author: 4hiziri
|#

(in-package :cl-user)
(defpackage cl-crypto-util-asd
  (:use :cl :asdf))
(in-package :cl-crypto-util-asd)

(defsystem cl-crypto-util
  :version "0.1"
  :author "4hiziri"
  :license ""
  :depends-on (:cl-annot)
  :components ((:module "src"
                :components
                ((:file "cl-crypto-util"))))
  :description "util for crypto-study"
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
  :in-order-to ((test-op (test-op cl-crypto-util-test))))
