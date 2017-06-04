#|
  This file is a part of aes project.
  Copyright (c) 2017 4hiziri ()
|#

#|
  for study

  Author: 4hiziri ()
|#

(in-package :cl-user)
(defpackage aes-asd
  (:use :cl :asdf))
(in-package :aes-asd)

(defsystem aes
  :version "0.1"
  :author "4hiziri"
  :license ""
  :depends-on (:cl-galois
               :cl-crypto-util
	       :cl-annot)
  :components ((:module "src"
                :components
                ((:file "aes"))))
  :description "for study"
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
  :in-order-to ((test-op (test-op aes-test))))
