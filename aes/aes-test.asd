#|
  This file is a part of aes project.
  Copyright (c) 2017 4hiziri ()
|#

(in-package :cl-user)
(defpackage aes-test-asd
  (:use :cl :asdf))
(in-package :aes-test-asd)

(defsystem aes-test
  :author "4hiziri"
  :license ""
  :depends-on (:aes
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "aes"))))
  :description "Test system for aes"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
