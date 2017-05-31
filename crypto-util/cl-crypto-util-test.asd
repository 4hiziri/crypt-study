#|
  This file is a part of cl-crypto-util project.
  Copyright (c) 2017 4hiziri
|#

(in-package :cl-user)
(defpackage cl-crypto-util-test-asd
  (:use :cl :asdf))
(in-package :cl-crypto-util-test-asd)

(defsystem cl-crypto-util-test
  :author "4hiziri"
  :license ""
  :depends-on (:cl-crypto-util
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-crypto-util"))))
  :description "Test system for cl-crypto-util"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
