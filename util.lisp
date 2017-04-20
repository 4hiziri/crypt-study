(defun random-bit-list (num)
  (let ((ret nil))    
    (loop for r = (random num) then (random num)
	  when (= (length ret) num)
	    do (return ret)
	  when (not (member r ret))
	    do (push r ret))))

(defun mapping ()
  "generate mapping-list expansion-permutation"
  (let ((ret (list (cons 31 (loop for i from 0 to 4 collect i)))))
    (loop repeat 6
	  for i = 3 then (+ i 4)
	  do (push (loop repeat 6 for j from i collect j) ret))
    (apply #'append
	   (reverse (cons (append (loop for i from 27 to 31 collect i) '(1)) ret)))))

(defun map2vec (map-table vector)
  (make-array (length map-table) :element-type (array-element-type vector)
				 :initial-contents (mapcar (lambda (x) (aref vector x)) map-table)))

(defun concat-bit-array (&rest bit-arrays)
  (reduce (lambda (x y) (concatenate 'bit-vector x y)) bit-arrays))

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
  "bit-array -> integer"
  (let ((list (coerce bits 'list))
	(ret 0))
    (dolist (b list ret)
      (setf ret (ash ret 1))
      (incf ret b))))

(defun array-shift-left (array num)
  (let ((len (length array)))
    (dotimes (c (- len num))
      (setf (aref array c) (aref array (+ c num))))
    (dotimes (c num array)
      (setf (aref array (+ (- len num) c)) 0))))

(defun slash-block (bits &optional (size 64))
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
	(reverse (cons (concat-bit-array (reverse acc)
					 (make-array (- size c) :element-type 'bit :initial-element 0))
		       ret)))))

(defun encode-ascii (string &optional (byte 1))
  "for string"
  (apply #'concat-bit-array
	 (mapcar (lambda (x) (int2bit x (* 8 byte)))
		 (mapcar #'char-code (coerce string 'list)))))

(defun decode-ascii (bits &optional (byte 1))
  "for string"
  (coerce (mapcar #'code-char
		  (remove-if (lambda (x) (= 0 x))
			     (mapcar #'bit2int (slash-block bits (* byte 8))))) 'string))
