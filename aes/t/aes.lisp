(in-package :cl-user)
(defpackage aes-test
  (:use :cl
        :aes
        :prove))
(in-package :aes-test)

;; NOTE: To run this test file, execute `(asdf:test-system :aes)' in your Lisp.

(plan nil)

(subtest "sub-bytes"
  (let ((input #2A((#x19 #xa0 #x9a #xe9)
		   (#x3d #xf4 #xc6 #xf8)
		   (#xe3 #xe2 #x8d #x48)
		   (#xbe #x2b #x2a #x08)))
	(output #2A((#xd4 #xe0 #xb8 #x1e)
		    (#x27 #xbf #xb4 #x41)
		    (#x11 #x98 #x5d #x52)
		    (#xae #xf1 #xe5 #x30))))
    (ok (equalp (aes::sub-bytes input)
		output))))

(finalize)
