(in-package :cl-user)
(defpackage aes-test
  (:use :cl
        :aes
        :prove))
(in-package :aes-test)

;; NOTE: To run this test file, execute `(asdf:test-system :aes)' in your Lisp.

(plan nil)

(defparameter test-input #2A((#x19 #xa0 #x9a #xe9)
			     (#x3d #xf4 #xc6 #xf8)
			     (#xe3 #xe2 #x8d #x48)
			     (#xbe #x2b #x2a #x08)))

(defparameter after-subbyte #2A((#xd4 #xe0 #xb8 #x1e)
				(#x27 #xbf #xb4 #x41)
				(#x11 #x98 #x5d #x52)
				(#xae #xf1 #xe5 #x30)))

(defparameter after-shiftrows #2A((#xd4 #xe0 #xb8 #x1e)
				  (#xbf #xb4 #x41 #x27)
				  (#x5d #x52 #x11 #x98)
				  (#x30 #xae #xf1 #xe5)))
(defparameter after-mixcolumns #2A((4 224 72 40)
				   (102 203 248 6)
				   (129 25 211 38)
				   (229 154 122 76)))

(defparameter test-128-key #x2b7e151628aed2a6abf7158809cf4f3c)
(defparameter test-word #x09cf4f3c)
(defparameter after-rotword #xcf4f3c09)
(defparameter after-subword #x8a84eb01)
(defparameter after-xor-rcon #x8b84eb01)

(subtest "divide-num-bits"
  (let ((test-val #x123456)
	(res (list #x123 #x456)))
    (is (aes::divide-num-bits test-val (* 4 3))
	res)))

(subtest "divide-num-fixlen"
  (let ((test-val #x1234)
	(res (list #x0 #x0 #x12 #x34)))
    (is (aes::divide-num-fixlen test-val 8 32)
	res)))

(subtest "accumulate-bits"
  (let ((test (list #x12 #x34))
	(res #x1234))
    (is (aes::accumulate-bits test 8)
	res)))

(subtest "sub-bytes"
  (ok (equalp (aes::sub-bytes test-input)
	      after-subbyte)))

(subtest "inv-sub-bytes"
  (ok (equalp (aes::inv-sub-bytes after-subbyte)
	      test-input)))

(subtest "shift-rows"
  (ok (equalp (aes::shift-rows after-subbyte)
	      after-shiftrows)))

(subtest "inv-shift-rows"
  (ok (equalp (aes::inv-shift-rows after-shiftrows)
	      after-subbyte)))

(subtest "mix-columns"
  (ok (equalp (aes::mix-columns after-shiftrows)
	      after-mixcolumns)))

(subtest "inv-mix-columns"
  (ok (equalp (aes::inv-mix-columns after-mixcolumns)
	      after-shiftrows)))

(subtest "rotword"
  (is (aes::rot-word test-word)
      after-rotword))

(subtest "subword"
  (is (aes::sub-word after-rotword)
      after-subword))

(subtest "xor-rcon"
  (is (logxor (aes::r-con 1) after-subword)
      after-xor-rcon))

(subtest "key-expansion"
  (let ((val-4th #xa0fafe17))
    (is (aref (aes::key-expansion test-128-key) 4)
	val-4th)))

(subtest "add-round-key"
  (let ((round-keys (list #x00010203 #x04050607 #x08090a0b #x0c0d0e0f))
	(state #2A((0 0 0 0)
		   (0 0 0 0)
		   (0 0 0 0)
		   (0 0 0 0)))
	(res #2A((0 4 8 12)
		 (1 5 9 13)
		 (2 6 10 14)
		 (3 7 11 15))))
    (ok (equalp (aes::add-round-key state round-keys)
		res))))

(subtest "input-state"
  (let ((input #x12345678)
	(state #2A((0 0 0 #x12)
		   (0 0 0 #x34)
		   (0 0 0 #x56)
		   (0 0 0 #x78))))    
    (ok (equalp (aes::input-state input)
		state))))

(subtest "output"
  (let ((state #2A((0 0 0 #x12)
		   (0 0 0 #x34)
		   (0 0 0 #x56)
		   (0 0 0 #x78)))
	(output #x12345678))
    (is (aes::state-output state)
	output)))

(subtest "encryption and decryption"
  (let ((val #xdeadbeaf))    
    (is (aes:decrypt (aes:encrypt val test-128-key) test-128-key)
	val)))

(finalize)
