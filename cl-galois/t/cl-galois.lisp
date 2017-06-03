(in-package :cl-user)
(defpackage cl-galois-test
  (:use :cl
        :cl-galois
        :prove))
(in-package :cl-galois-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-galois)' in your Lisp.

(plan nil)

(diag "Run cl-galois test")

(subtest "gf-mod"
  (let ((test-val 11129)
	(ret 193))
    (is (cl-galois:gf-mod test-val cl-galois::*mx*)
	ret)))

(subtest "gf-mult-unfinited"
  (let ((test-val1 #x57)
	(test-val2 #x83)
	(res #b10101101111001))
    (ok (equalp (print (cl-galois::gf-mult-unfinited test-val1 test-val2))
		res))))

(finalize)
