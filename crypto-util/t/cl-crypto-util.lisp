(in-package :cl-user)
(defpackage cl-crypto-util-test
  (:use :cl
        :cl-crypto-util
        :prove))
(in-package :cl-crypto-util-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-crypto-util)' in your Lisp.

(plan nil)

(subtest "random-list"
  (let* ((test-num 100)
	 (list (cl-crypto-util:random-list test-num)))
    (ok (= (length list) test-num))
    (ok (reduce (lambda (x y) (and x y))
		(mapcar (lambda (x) (< x test-num)) list)))
    (ok (dolist (e list t)
	  (when (/= (count e list) 1)
	    (return nil))))))

(subtest "concat-bit"
  (let ((test-arr1 #*1111)
	(test-arr2 #*0000)
	(res1 #*11110000)
	(res2 #*00001111))
    (ok (equalp (cl-crypto-util:concat-bit test-arr1 test-arr2)
		res1))
    (ok (equalp (cl-crypto-util:concat-bit test-arr2 test-arr1)
		res2))))

(subtest "translate-vec-by-vec"
  (let ((a-vec #(2 4 6))
	(map-vec '(1 0 2))
	(res-vec #(4 2 6)))
    (ok (equalp (cl-crypto-util:translate-vec-by-vec a-vec map-vec)
		res-vec))))

(subtest "limits-list"
  (let ((test-list '(2 3 0 1 3))
	(test-len1 10)
	(test-len2 3))
    (is (length (cl-crypto-util:limits-list test-list test-len1))
	test-len1)
    (is (length (cl-crypto-util:limits-list test-list test-len2))
	test-len2)))

(subtest "array-shift-left, bit-lrotate, bit-shift"
  (let ((test-array #(1 2 3 4 5))
	(res-pad0-array #(4 5 0 0 0))
	(res-nopad-array #(4 5 1 2 3))
	(shift-num 3)
	(shift-num-over 10))
    (ok (equalp (cl-crypto-util:array-shift-left test-array
						 shift-num	
						 :padding-elem 0)
		res-pad0-array))
    (ok (equalp (cl-crypto-util:array-shift-left test-array
						 shift-num
						 :padding-p nil)
		res-nopad-array))
    (ok (equalp (cl-crypto-util:array-shift-left test-array
						 shift-num-over
						 :padding-p nil)
		test-array))))

(subtest "bit-shift-len"
  (let ((test-arr #*1111)
	(test-len 4)
	(res #*11110000))
    (ok (equalp (bit-shift-len test-arr test-len)
		res))))

(subtest "int-bit"
  (let* ((test-len1 8)
	 (test-len2 4)
	 (test-val #b11001100)
	 (res-bit1 #*11001100)
	 (res-bit2 #*1100))
    (ok (equalp (cl-crypto-util:int-bit test-val test-len1)
		res-bit1))
    (ok (equalp (cl-crypto-util:int-bit test-val test-len2)
		res-bit2))))

(subtest "bit-int"
  (let ((test-arr #*1100110)
	(test-int #b1100110))
    (is (cl-crypto-util:bit-int (int-bit test-int))
	test-int)
    (is (cl-crypto-util:bit-int test-arr)
	test-int)))

(subtest "divide-array"
  (let ((test-array #(1 2 3 4 5 6 7 8 9 0))
	(divide-num 3)
	(res (list #(1 2 3) #(4 5 6) #(7 8 9) #(0))))
    (ok (equalp (cl-crypto-util:divide-array test-array divide-num)
		res))))

(subtest "encode-ascii, decode-ascii"
  (let ((test-str "test-test"))
    (is (cl-crypto-util:decode-ascii (cl-crypto-util:encode-ascii test-str))
	test-str)
    (ok (bit-vector-p (cl-crypto-util:encode-ascii test-str)))))

(subtest "inc-bit"
  (let ((test-arr #*010)
	(res-arr #*011))
    (ok (equalp (cl-crypto-util:inc-bit test-arr)
		res-arr))))

(subtest "set-bytes"
  (let ((arr1 #(1 2 3 4 5))
	(arr2 #(4 6 8))
	(pos 1)
	(res #(1 4 6 8 5)))
    (ok (equalp (cl-crypto-util:set-bytes arr1 arr2 pos)
		res))))

(subtest "bit-index-list"
  (let ((test-arr #*1010)
	(res (list 1 3)))
    (is (cl-crypto-util:bit-index-list test-arr)
	res)))

(subtest "word-to-bytes-seq"
  (let ((test-word #*11111000111101001111001011110001)
	(res (list #*11111000 #*11110100 #*11110010 #*11110001)))
    (ok (equalp (cl-crypto-util:word-to-bytes-seq test-word)
		res))))

(subtest "mapbyte"
  (let ((test-word #*00000111010110111100110100010101)
	(res (bit-not #*00000111010110111100110100010101)))
    (ok (equalp (cl-crypto-util:mapbyte (lambda (byte) (bit-not byte))
					test-word)
		res))))

(subtest "func-to-vector"
  (flet ((test-func (x)
	   (* (1+ x) 2)))
    (let ((res #(2 4 6)))
      (ok (equalp (cl-crypto-util:func-to-vector #'test-func 0 3)
		  res)))))

(subtest "inv-table"
  (let ((test-arr #(3 1 0 2))
	(res #(2 1 3 0)))
    (ok (equalp (cl-crypto-util:inv-table test-arr)
		res))))

(finalize)
