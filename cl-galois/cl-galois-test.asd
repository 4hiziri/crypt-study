#|
  This file is a part of cl-galois project.
  Copyright (c) 2017 4hiziri
|#

(in-package :cl-user)
(defpackage cl-galois-test-asd
  (:use :cl :asdf))
(in-package :cl-galois-test-asd)

(defsystem cl-galois-test
  :author "4hiziri"
  :license ""
  :depends-on (:cl-galois
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-galois"))))
  :description "Test system for cl-galois"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
