(load './util.lisp')

(defparameter *mx* #b100011011)

(defun gf-mod (b1 b2 len)
  (labels ((below-powered (n1 n2 prev-n1)
	     (if (> n1 n2)
		 prev-n1
		 (below-powered (ash n1 1) n2 n1)))
	   (inner-loop (n1 n2)
	     (cond ((= (length (int2bit n1)) 9) (int2bit (logxor n1 n2) len))
		   ((< n1 n2) (int2bit n1 len))
		   (t (inner-loop (logxor n1 (below-powered n2 n1 n2)) n2)))))
    (inner-loop (bit2int b1) (bit2int b2))))

(defun gf-mult-unfinited (b1 b2)
  (let ((b2-pos-1 (bit-index-list b2))
	(max-len (+ (length b1) (length b2))))
    (reduce #'bit-xor (mapcar (lambda (x) (bit-shift b1 x max-len)) b2-pos-1))))

(defun gf-mult (b1 b2)
  (gf-mod (gf-mult-unfinited b1 b2)
	  (int2bit *mx* 9)
	  (length b1)))

(defun gf-power (bit-array num)
  (if (= num 0)
      (int2bit 1 (length bit-array))
      (let ((acc bit-array))
	(dotimes (n (1- num) (gf-mod acc (int2bit *mx* 9) (length bit-array)))
	  (setf acc (gf-mult-nm bit-array acc))))))
