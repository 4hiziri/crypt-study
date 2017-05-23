(defun random-map-list (num)
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
  (let ((map (mapcar (lambda (x) (aref vector x)) map-table)))    
    (make-array (length map-table) :element-type (array-element-type vector)
				   :initial-contents map)))

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
  "padding 0 last of array"
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

(defun bit-lrotate (bit count)
  (let ((need-rotate (mod count (length bit)))
	(bit-list (coerce bit 'list))
	(acc nil))
    (dotimes (c need-rotate (make-array (length bit)
				  :element-type 'bit
				  :initial-contents (append bit-list (reverse acc))))
      (push (car bit-list) acc)
      (setf bit-list (cdr bit-list)))))

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

(defun inc-bit-array (bits)
  (int2bit (1+ (bit2int bits)) (length bits)))

(defun set-bytes (a b pos)
  "set b to part of a"
  (dotimes (n (length b) a)
    (setf (aref a (+ pos n)) (aref b n))))

(defun bit-index-list (bit-array)
  "! bit-array[0] is msb !"
  (loop with len = (length bit-array)
	for i from 0 below len
	when (= (aref bit-array (- (1- len) i))
		1)
	  collect i))

(defun bit-shift (bit-array count len)
  (int2bit (ash (bit2int bit-array) count) len))

(defun bit-shift-pad-0 (bit-array num)
  (concat-bit-array bit-array (int2bit 0 num)))

(defun gf-power (bit-array num)
  (if (= num 0)
      (int2bit 1 (length bit-array))
      (gf-mod (bit-shift-pad-0 bit-array (1- num)) #*100011011 (length bit-array))))

;; integer as bit
(defun map-byte (func bytes bit-len &optional (bit-num 8))
  (let ((rest bytes)
	(acc 0))
    (dotimes (n (truncate bit-len bit-num) acc)
      (setf acc (+ acc
		   (ash (funcall func (logand rest (1- (expt 2 bit-num))))
			(* bit-num n)))
	    rest (ash rest (- bit-num))))))
