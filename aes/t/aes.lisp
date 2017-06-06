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

;(subtest "add-roundkey"  )

(finalize)
