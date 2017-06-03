(in-package :cl-user)
(defpackage cl-crypto-util
  (:use :cl :cl-annot))
(in-package :cl-crypto-util)

(cl-annot:enable-annot-syntax)

@export
(defun random-list (num)
  "generate list. 
length = num. 
element < num.
each element is different."
  (let ((ret nil))
    (loop for r = (random num) then (random num)
	  when (= (length ret) num)
	    do (return ret)
	  when (not (member r ret))
	    do (push r ret))))

@export
(defun concat-bit (&rest bit-arrays)
  (reduce (lambda (x y) (concatenate 'bit-vector x y)) bit-arrays))

@export
(defun translate-vec-by-vec (vector mapping-list)
  "return vector: nth-elem = vector[mapping-list[n]]"
  (let ((res-list (mapcar (lambda (x) (aref vector x)) mapping-list)))
    (make-array (length mapping-list) :element-type (array-element-type vector)
				      :initial-contents res-list)))



@export
(defun limits-list (list num)
  "regulate length of list to num. if list is expanded, padding 0 to its head"
  (let ((len (length list)))
    (if (> num len)
	(append (loop repeat (- num len) collect 0) list)
	(subseq list 0 num))))

@export
(defun array-shift-left (array shift-num &key (padding-p t) (padding-elem 0))
  "Shift num array, padding 0 right of array"
  (let* ((len (length array))
	 (ret (copy-seq array))
	 (finit-shift-num (mod shift-num len)))
    (dotimes (n finit-shift-num)
      (let ((dst-i (- len (- finit-shift-num n)))
	    (src-i n))
	(setf (aref ret dst-i) (if padding-p padding-elem (aref array src-i)))))
    (dotimes (c (- len finit-shift-num) ret)
      (setf (aref ret c) (aref array (+ c finit-shift-num))))))

@export
(defun bit-lrotate (bit count)
  (array-shift-left bit count :padding-p nil))

@export
(defun bit-shift (bit count)
  (array-shift-left bit count :padding-p nil))

@export
(defun bit-shift-len (bit-array len)
  "return bit-array which length is len and padding 0"
  (concat-bit bit-array (int-bit 0 len)))

;; fast but complication
(defun int-bit (num &optional (len nil len-p))
  (loop for bits = nil then (push r bits)
	for q = num then (/ (- q r) 2)
        for r = (mod num 2) then (mod q 2)
	when (or (= q 0) (= q 1))
	  do (let* ((res (cons q bits))
		    (size (if len-p len (length res))))
	       (return (make-array size
				   :element-type 'bit
				   :initial-contents (limits-list res size))))))

@export
(defun int-bit (num &optional (len nil len-p))
  "integer -> bit-array. len is length of bit."
  (let ((len (if len-p len (integer-length num))))    
    (make-array len
		:element-type 'bit
		:initial-contents (loop for i downfrom (1- len) to 0
					collect (if (logbitp i num) 1 0)))))

@export
(defun bit-int (bits)
  "bit-array -> integer"
  (let ((list (coerce bits 'list))
	(ret 0))
    (dolist (b list ret)
      (setf ret (ash ret 1))
      (incf ret b))))

;; I think this is useless
@export
(defun old-slash-block (bits &optional (size 64))
  (let ((len (length bits))
	(c 0)	
	(ret nil)
	(acc nil))
    (dotimes (index len (reverse ret))
      (let ((b (aref bits index)))
	(push b acc)
	(incf c)
	(when (= c size)
	  (push (make-array size :element-type 'bit :initial-contents (reverse acc)) ret)
	  (setf c 0 acc nil))))
    (if (= c 0)
	(reverse ret)
	(reverse (cons
		  (concat-bit
		   (reverse acc)
		   (make-array (- size c) :element-type 'bit :initial-element 0))
		  ret)))))

@export
(defun divide-array (array block-size)
  "not padding"
  (let ((arr-len (length array)))
    (loop for i from 0 below arr-len by block-size
	  collect (subseq array i (min (+ i block-size) arr-len)))))

@export
(defun encode-ascii (string &optional (byte 1))
  "for string"
  (apply #'concat-bit
	 (mapcar (lambda (x) (int-bit x (* 8 byte)))
		 (mapcar #'char-code (coerce string 'list)))))

@export
(defun decode-ascii (bits &optional (byte 1))
  "for string"
  (coerce (mapcar #'code-char
		  (remove-if (lambda (x) (= 0 x))
			     (mapcar #'bit-int (divide-array bits (* byte 8)))))
	  'string))

@export
(defun inc-bit (bits)
  (int-bit (1+ (bit-int bits)) (length bits)))

@export
(defun set-bytes (arr1 arr2 pos)  
  "set arr2 to part of arr1 at pos"
  (let ((ret (copy-seq arr1)))
    (dotimes (n (length arr2) ret)
      (setf (aref ret (+ pos n)) (aref arr2 n)))))

@export
(defun bit-index-list (bit-array)
  "WARN: bit-array[0] is msb!"
  (loop with len = (length bit-array)
	for i from 0 below len
	when (= (aref bit-array (- (1- len) i))
		1)
	  collect i))

@export
(defun bit-indexes (num)
  (let ((ret nil))
    (dotimes (i (integer-length num) (reverse ret))
      (when (logbitp i num)
	(push i ret)))))

@export
(defun word-to-bytes-seq (word)
  (divide-array word 8))

@export
(defun mapbyte (func word)
  (apply (lambda (x y z w) (concatenate 'bit-vector x y z w))
	 (mapcar func (word-to-bytes-seq word))))

;; not used
(defun divide-list (list num)
  (loop repeat (ceiling (/ (length list) num))
	collect (loop repeat num
		      for i = (pop list) then (pop list)
		      while i
		      collect i)))

@export
(defun func-to-vector (func start count)
  (let ((init (loop for i from 0 below count collect (funcall func i))))    
    (make-array (- count start) :initial-contents init)))

@export
(defun inv-table (vector)
  (let ((ret (make-array (length vector))))
    (dotimes (l (length vector) ret)
      (setf (aref ret (aref vector l)) l))))



