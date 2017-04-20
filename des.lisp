(load "./util.lisp")

(defparameter mapping-table '(31 0 1 2 3 4
			      3 4 5 6 7 8
			      7 8 9 10 11 12
			      11 12 13 14 15 16
			      15 16 17 18 19 20
			      19 20 21 22 23 24
			      23 24 25 26 27 28
			      27 28 29 30 31 1))

(defun expansion-permutation (bits-32 &optional (mapping mapping-table))
  "32bit -> 48bit, based on mapping-table"
  (map2vec mapping bits-32))

(defun key-mixing (bits-48 round-key)
  (bit-xor bits-48 round-key))

(defparameter s-box-tables
  (list (make-array '(4 16) :initial-contents '((8 13 10 2 1 0 12 14 15 11 9 4 6 5 3 7)
						(8 0 3 6 14 4 9 1 11 10 12 13 2 5 7 15)
						(9 0 10 7 2 13 8 5 3 12 1 15 11 4 6 14)
						(5 14 12 1 8 11 15 7 9 0 3 10 2 4 6 13)))
	(make-array '(4 16) :initial-contents '((5 15 8 3 1 10 0 9 7 11 4 14 12 13 2 6)
						(13 11 10 15 7 9 5 14 12 2 3 4 8 1 6 0)
						(11 14 0 2 8 6 13 1 12 9 7 5 3 15 4 10)
						(10 11 12 1 15 0 8 6 13 7 5 3 14 2 9 4)))
	(make-array '(4 16) :initial-contents '((15 5 2 9 6 0 7 4 1 11 12 14 13 3 8 10)
						(14 9 5 13 0 6 7 10 3 4 15 2 11 1 12 8)
						(4 8 1 9 15 5 12 0 13 3 14 7 2 11 6 10)
						(10 7 1 9 5 0 6 11 13 12 8 14 3 15 4 2)))
	(make-array '(4 16) :initial-contents '((4 10 2 12 6 7 3 15 13 5 9 1 11 0 14 8)
						(12 2 7 14 9 4 13 6 15 10 5 3 8 1 11 0)
						(6 12 5 2 14 3 1 7 15 11 13 10 8 0 9 4)
						(11 7 15 6 10 3 0 13 8 5 2 12 4 9 1 14)))
	(make-array '(4 16) :initial-contents '((11 8 5 6 10 13 7 9 3 4 0 1 14 2 15 12)
						(13 7 5 15 14 0 6 2 10 9 12 3 1 4 8 11)
						(13 8 2 15 12 6 1 5 4 9 10 7 11 3 14 0)
						(15 5 3 11 4 12 13 6 7 10 14 9 2 0 8 1)))
	(make-array '(4 16) :initial-contents '((7 15 9 10 13 0 6 5 12 3 11 1 8 14 2 4)
						(4 8 6 0 14 3 12 5 10 11 15 2 1 9 7 13)
						(12 7 13 10 11 1 6 4 14 15 3 0 2 9 5 8)
						(12 2 1 8 15 10 5 3 11 14 13 6 0 9 7 4)))
	(make-array '(4 16) :initial-contents '((3 15 1 9 8 4 7 10 6 13 2 14 11 12 0 5)
						(8 4 3 6 0 11 13 10 7 9 5 14 12 15 1 2)
						(1 0 13 14 5 11 9 4 2 10 3 15 8 7 6 12)
						(10 1 6 11 8 2 15 0 14 13 12 9 5 7 3 4)))
	(make-array '(4 16) :initial-contents '((8 9 3 4 6 13 2 1 15 14 5 0 10 7 12 11)
						(4 5 10 9 8 15 2 14 3 1 0 12 11 6 13 7)
						(6 3 10 0 2 7 12 8 11 9 5 15 1 13 14 4)
						(9 6 5 11 10 14 2 7 0 8 4 1 3 13 12 15)))))
(defun substitution (bits-48 &optional (s-box-tables s-box-tables))
  (flet ((convert (bits-6 table)
	   (aref table
		 (+ (ash (aref bits-6 0) 1)
			  (aref bits-6 5))
		 (bit2int (subseq bits-6 1 5)))))
    (let ((ret nil))
      (loop repeat 8
	    for s in s-box-tables
	    for i = 0 then (+ i 6)
	    do (push (int2bit (convert (subseq bits-48 i (+ i 6)) s) 4) ret))
      (apply #'concat-bit-array (reverse ret)))))

(defparameter perm-table '(8 17 11 31 2 9 14 18 26 6 22 5 0 23 16 1 7 28 27 29 30 10 21 24 19 3 15 25 12 13 4 20))
(defun permutation (bits-32 &optional (table perm-table))
  (map2vec table bits-32))

(defparameter lmap '(8 3 27 24 23 11 19 6 4 16 21 20 0 9 15 14 13 10 18 2 26 1 12 17))
(defparameter rmap '(5 19 24 13 25 3 6 4 11 22 12 1 21 2 26 23 9 18 15 7 27 0 16 20))
(defun compress (lbits-28 rbits-28 &optional (l-table lmap) (r-table rmap))
  (concat-bit-array (map2vec l-table lbits-28) (map2vec r-table rbits-28)))

(defun key-generate (lbits-28 rbits-28)
  (compress lbits-28 rbits-28))

(defun keys-16 (key-56)
  (let ((lkey (subseq key-56 0 28))
	(rkey (subseq key-56 28 56))
	(keys nil))
    (dotimes (r 16 (reverse keys))
      (let ((shift (if (or (= r 0) (= r 1) (= r 8) (= r 15)) 2 1)))	
	(setf lkey (array-shift-left lkey shift))
	(setf rkey (array-shift-left rkey shift))
	(push (key-generate lkey rkey) keys)))))

(defun feistel (f bits-64 keys)
  (let ((l-bits-32 (subseq bits-64 0 32))
	(r-bits-32 (subseq bits-64 32 64)))
    (dolist (key keys (concat-bit-array r-bits-32 l-bits-32))
      (setf l-bits-32 (bit-xor l-bits-32 (funcall f r-bits-32 key)))
      (rotatef l-bits-32 r-bits-32))))

(defun des-process (bits-64 keys)
  (flet ((round-func (bits-32 round-key)
	   (permutation (substitution (key-mixing (expansion-permutation bits-32) round-key)))))
    (feistel #'round-func bits-64 keys)))

(defun des-encryption (bits-64 key-56)
  (des-process bits-64 (keys-16 key-56)))

(defun des-decryption (bits-64 key-56)
  (des-process bits-64 (reverse (keys-16 key-56))))

(defun des-ede3-encryption (bits-64 key-168)
  (let ((key1-56 (subseq key-168 0 56))
	(key2-56 (subseq key-168 56 112))
	(key3-56 (subseq key-168 112 168)))
    (des-encryption
     (des-decryption
      (des-encryption bits-64 key1-56)
      key2-56)
     key3-56)))

(defun des-ede3-decryption (bits-64 key-168)
  (let ((key1-56 (subseq key-168 0 56))
	(key2-56 (subseq key-168 56 112))
	(key3-56 (subseq key-168 112 168)))
    (des-decryption
     (des-encryption
      (des-decryption bits-64 key3-56)
      key2-56)
     key1-56)))
