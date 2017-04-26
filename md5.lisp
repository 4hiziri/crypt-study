(defparameter *md5-bl* 512)
(defparameter *list-for-table* (loop repeat 64
				     for i from 0
				     collect (floor (* (expt 2 32) (abs (sin (1+ i)))))))
(defparameter *table* (make-array 64 :initial-contents *list-for-table*))
(defparameter *shift* (list (loop repeat 4 append '(7 12 17 22))
			    (loop repeat 4 append '(5 9 14 20))
			    (loop repeat 4 append '(4 11 16 23))
			    (loop repeat 4 append '(6 10 15 21))))
(defparameter *A* (int2bit #x01234567 32))
(defparameter *B* (int2bit #x89abcdef 32))
(defparameter *C* (int2bit #xfedcba98 32))
(defparameter *D* (int2bit #x76543210 32))

(defun padding-length-zero (msg-len)
  (let* ((limit (- *md5-bl* 64 1))
	 (uncorrected-ans (- limit msg-len)))
    (if (> msg-len limit)
	(+ uncorrected-ans *md5-bl*)
	uncorrected-ans)))

(defun padding-except-msg-length (msg-len)
  (let ((pad-zero-len (padding-length-zero msg-len)))
    (make-array  (1+ pad-zero-len)
		 :element-type 'bit
		 :initial-contents (cons 1 (loop repeat pad-zero-len collect 0)))))

(defun padding-msg (msg msg-len)
  "return 1024 or 512 bits"
  (let ((pad (padding-except-msg-length (length msg))))
    (concat-bit-array msg pad (int2bit msg-len 64))))

(defun f (x y z)
  (bit-ior (bit-and x y)
	   (bit-and (bit-not x) z)))

(defun g (x y z)
  (bit-ior (bit-and x z)
	   (bit-and (bit-not z) y)))

(defun h (x y z)
  (bit-xor (bit-xor x y) z))

(defun i (x y z)
  (bit-xor y
	   (bit-ior x (bit-not z))))

(defun mutate (a-int b-int func-ret s xk ti)
  (int2bit (+ b-int
	      (bit-lrotate (int2bit (+ a-int func-ret xk ti) 32) s))
	   32))

(defun round-16 (a b c d blocks shift f g)
  (labels ((inner-loop (a b c d i shift)
	     (if (= i 16)
		 (list a b c d)
		 (let ((func-ret (mutate (bit2int a)
					 (bit2int b)
					 (funcall f b c d)
					 (car shift)
					 )))
		   (inner-loop d ))))))
  (let ((sum-a 0)
	(sum-b 0)
	(sum-c 0)
	(sum-d 0))
    (loop for i from 0 to 15
	  for g = i then i)))

(defun md5 (msg)
  )
