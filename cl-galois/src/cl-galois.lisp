(in-package :cl-user)
(defpackage cl-galois
  (:use :cl :cl-annot :cl-crypto-util))
(in-package :cl-galois)

(cl-annot:enable-annot-syntax)

(defparameter *mx* #b100011011)

@export
(defun gf-mod (b1 b2 &optional (len 8))
  (labels ((below-powered (n1 n2 prev-n1)
	     (if (> n1 n2)
		 prev-n1
		 (below-powered (ash n1 1) n2 n1)))
	   (inner-loop (n1 n2)
	     (cond ((= (length (int-bit n1)) 9) (int-bit (logxor n1 n2) len))
		   ((< n1 n2) (int-bit n1 len))
		   (t (inner-loop (logxor n1 (below-powered n2 n1 n2)) n2)))))
    (inner-loop (bit-int b1) (bit-int b2))))

(defun gf-mod (n1 n2)
  (labels ((below-powered (n1 n2 prev-n1)
	     (if (> n1 n2)
		 prev-n1
		 (below-powered (ash n1 1) n2 n1)))
	   (inner-loop (n1 n2)
	     (cond ((= (integer-length n1) 9) (logxor n1 n2))
		   ((< n1 n2) n1)
		   (t (inner-loop (logxor n1 (below-powered n2 n1 n2)) n2)))))
    (inner-loop n1 n2)))

(defun gf-mult-unfinited (b1 b2)
  ;; if b2 is 0, cannot calc
  ;; so simply return 0 as bit-vector
  (if (reduce (lambda (x y) (and x y)) (map 'list (lambda (x) (= x 0)) b2))
      (make-array (+ (length b1) (length b2)) :element-type 'bit :initial-element 0)
      (let ((b2-pos-1 (cl-crypto-util:bit-index-list b2))
	    (max-len (+ (length b1) (length b2))))
	(reduce #'bit-xor
		(mapcar (lambda (x) (cl-crypto-util:bit-shift b1 x))
			b2-pos-1)))))

(defun gf-mult-unfinited (n1 n2)
  (let ((exponents (cl-crypto-util:bit-indexes n2))
	(acc 0))
    (print exponents)
    (dolist (e exponents acc)
      (setf acc (logxor acc (ash n1 e))))))

@export
(defun gf-mult (b1 b2)
  (gf-mod (gf-mult-unfinited b1 b2)
	  (int-bit *mx* 9)
	  (length b1)))

@export
(defun gf-power (bit-array num)
  (if (= num 0)
      (int-bit 1 (length bit-array))
      (let ((acc bit-array))
	(dotimes (n (1- num) (gf-mod acc (int-bit *mx* 9) (length bit-array)))
	  (setf acc (gf-mult-unfinited bit-array acc))))))

@export
(defun gf-power (bit-array num)
  (if (= num 0)
      (int-bit 1 (length bit-array))
      (gf-mod (bit-shift-pad-0 bit-array (1- num)) #*100011011 (length bit-array))))

@export
(defun gf-inv (bit-array)
  (if (equal bit-array #*00000000)
      #*00000000
      (loop for i from 1 to 255
	    for b = (int-bit i 8) then (int-bit i 8)
	    when (equal (gf-mult b bit-array) #*00000001)
	      do (return b))))
