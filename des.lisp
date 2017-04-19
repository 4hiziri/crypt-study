(defun mapping ()
  "generate mapping-list expansion-permutation"
  (let ((ret (list (cons 31 (loop for i from 0 to 4 collect i)))))
    (loop repeat 6
	  for i = 3 then (+ i 4)
	  do (push (loop repeat 6 for j from i collect j) ret))
    (apply #'append
	   (reverse (cons (append (loop for i from 27 to 31 collect i) '(1)) ret)))))

(defparameter mapping-list '(31 0 1 2 3 4 3 4 5 6 7 8 7 8 9 10 11 12 11 12 13 14 15 16 15 16 17 18 19 20 19 20 21 22 23 24 23 24 25 26 27 28 27 28 29 30 31 1))

(defparameter perm-table '(8 17 11 31 2 9 14 18 26 6 22 5 0 23 16 1 7 28 27 29 30 10 21 24 19 3 15 25 12 13 4 20))

(defun random-bit-list (num)
  (let ((ret nil))    
    (loop for r = (random num) then (random num)
	  when (= (length ret) num)
	    do (return ret)
	  when (not (member r ret))
	    do (push r ret))))

(defparameter round-key-table
  (make-array '(4 16) :initial-contents '((8 13 10 2 1 0 12 14 15 11 9 4 6 5 3 7)
					  (8 0 3 6 14 4 9 1 11 10 12 13 2 5 7 15)
					  (9 0 10 7 2 13 8 5 3 12 1 15 11 4 6 14)
					  (5 14 12 1 8 11 15 7 9 0 3 10 2 4 6 13))))

(defun regulate-list (list num)
  "regulate length of list to num. if list is expanded, padding 0 to its head"
  (let ((len (length list)))
    (if (> num len)
	(append (loop repeat (- num len) collect 0) list)
	(subseq list 0 num))))

(defun int2bit (num &optional (len nil len-p))
  "integer -> bit-array. len is length of bit."
  (loop for bits = nil then (push r bits)
	for q = num then (/ (- q r) 2)
        for r = (mod num 2) then (mod q 2)
	when (or (= q 0) (= q 1))
	  do (let* ((res (cons q bits))
		    (size (if len-p len (length res))))
	       (return (make-array size
				   :element-type 'bit
				   :initial-contents (regulate-list res size))))))

(defun bit2int (bits)
  (let ((list (coerce bits 'list))
	(ret 0))
    (dolist (b list ret)
      (setf ret (ash ret 1))
      (incf ret b))))

(defun expansion-permutation (bits-32)
  "32bit -> 48bit"
  (let ((mapping mapping-list)
	(bits-48 (make-array 48 :element-type 'bit)))
    (dotimes (index 48 bits-48)
      (setf (aref bits-48 index) (aref bits-32 (car mapping)))
      (setf mapping (cdr mapping)))))

(defun key-mixing (bits-48 round-key)
  (bit-xor bits-48 round-key))

(defun substitution (round-key)
  (flet ((convert (i j)
	   (let ((table round-key-table))
	     (aref table i j))))
    (let ((ret (make-array 32 :element-type 'bit)))
      (loop repeat 8
	    for i = 0 then (+ i 6)
	    for c = 0 then (+ c 4) do
	      (let ((bits-4 (int2bit (convert (+ (ash (aref round-key i) 1)
						 (aref round-key (+ i 5)))
					      (bit2int (subseq round-key
							       (1+ i)
							       (+ i 1 4))))
				     4)))
	        (loop for j from 0 to 3 do (setf (aref ret (+ c j))
						 (aref bits-4 j)))))
      ret)))

(defun permutation ()
  (let ((table perm-table))
    ))

(defun des-encryption (plain-text-64bits)
  )
