(in-package :cl-user)
(defpackage cl-galois-test
  (:use :cl
        :cl-galois
        :prove))
(in-package :cl-galois-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-galois)' in your Lisp.

(plan nil)

(diag "Run cl-galois test")

(subtest "gf-add"
  (let ((test-val1 #x57)
	(test-val2 #x83)
	(result #xd4))
    (is (cl-galois:gf-add test-val1 test-val2)
	result)
    (is (cl-galois:gf-add test-val1 test-val2 result)
	0)
    (ok (<= (integer-length (cl-galois:gf-add test-val1 test-val2)) 8))))

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

(subtest "gf-mult"
  (let ((test1 #x57)
	(test2 #x83)
	(result #xc1))
    (is (cl-galois:gf-mult test1 test2)
	result)
    (ok (<= (integer-length (cl-galois:gf-mult test1 test2)) 8))))

(subtest "gf-power"
  (let ((test 2)
	(num1 0)
	(num2 1)
	(num3 2)
	(res1 1)
	(res2 2)
	(res3 4))
    (is (cl-galois:gf-power test num1)
	res1)
    (is (cl-galois:gf-power test num2)
	res2)
    (is (cl-galois:gf-power test num3)
	res3)))

(subtest "gf-inv"
  (let ((test 123))
    (ok (= (cl-galois:gf-mult (cl-galois:gf-inv test) test)
	   1))))

(finalize)
