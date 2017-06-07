(in-package :cl-user)
(defpackage aes
  (:use :cl))
(in-package :aes)

(cl-annot:enable-annot-syntax)

(defparameter Nk 4)
(defparameter Nb 4)
(defparameter Nr 10)

;; util

(defun word-bytes (word)
  (loop repeat 4
	for rest-word = word then (mod rest-word i)
	for i = (expt 2 24) then (ash i -8)	
	for byte = (truncate rest-word i)
	collect byte))

(defun bytes-word (bytes)
  (let ((ret 0))    
    (dolist (byte bytes ret)
      (setf ret (+ (ash ret 8) byte)))))

(defun mapbyte (func word)
  (bytes-word (mapcar func (word-bytes word))))

(defun matrix-sbox (i)
  (labels ((rotate (v)
	     (if (logbitp 7 v)
		 (+ (ash v 1) 1)
		 (ash v 1)))
	   (inner-loop (v i)
	     (if (<= i 0)
		 v
		 (inner-loop (rotate v) (1- i)))))
    (let ((iv #b11110001))
      (mod (inner-loop iv i) (expt 2 8)))))

(defparameter *matrix-sbox* #(#b11110001
			      #b11100011
			      #b11000111
			      #b10001111
			      #b00011111
			      #b00111110
			      #b01111100
			      #b11111000))

(defun calc-sub-byte (byte)
  "This function substitudes byte to byte by some mathmatical process."  
  (let ((acc 0)
	(inv-num (cl-galois:gf-inv byte))
	(salt #b01100011)
	(byte-len 8))
    (dotimes (i byte-len (logxor acc salt))
      		   ;; logcount returns number of bit-1.
		   ;; so GF(2) addition n1 + n2 + n3 becomes (logcount n1n2n3) % 2
      (incf acc (ash (mod (logcount (logand inv-num (aref *matrix-sbox* i)))
			  2)
		     i)))))

;; sub-byte's input range is 0 <= input <= 255, so it turns into vector
(defparameter *sub-byte-table*  
  #(99 124 119 123 242 107 111 197 48 1 103 43 254 215 171 118 202 130 201 125
    250 89 71 240 173 212 162 175 156 164 114 192 183 253 147 38 54 63 247 204 52
    165 229 241 113 216 49 21 4 199 35 195 24 150 5 154 7 18 128 226 235 39 178
    117 9 131 44 26 27 110 90 160 82 59 214 179 41 227 47 132 83 209 0 237 32 252
    177 91 106 203 190 57 74 76 88 207 208 239 170 251 67 77 51 133 69 249 2 127
    80 60 159 168 81 163 64 143 146 157 56 245 188 182 218 33 16 255 243 210 205
    12 19 236 95 151 68 23 196 167 126 61 100 93 25 115 96 129 79 220 34 42 144
    136 70 238 184 20 222 94 11 219 224 50 58 10 73 6 36 92 194 211 172 98 145
    149 228 121 231 200 55 109 141 213 78 169 108 86 244 234 101 122 174 8 186
    120 37 46 28 166 180 198 232 221 116 31 75 189 139 138 112 62 181 102 72 3
    246 14 97 53 87 185 134 193 29 158 225 248 152 17 105 217 142 148 155 30 135
    233 206 85 40 223 140 161 137 13 191 230 66 104 65 153 45 15 176 84 187 22))

(defun sub-byte (byte)
  (aref *sub-byte-table* byte))

(defun map-state (state func)
  "substitude each byte in state."
  (let ((ret (make-array (list 4 Nb))))
    (dotimes (r 4 ret)
      (dotimes (c Nb)
	(setf (aref ret r c) (funcall func (aref state r c)))))))

(defun sub-bytes (state)
  "substitude each byte in state."
  (map-state state #'sub-byte))

(defparameter *inv-sub-byte-table*
  #(82 9 106 213 48 54 165 56 191 64 163 158 129 243 215 251 124 227 57 130 155
    47 255 135 52 142 67 68 196 222 233 203 84 123 148 50 166 194 35 61 238 76
    149 11 66 250 195 78 8 46 161 102 40 217 36 178 118 91 162 73 109 139 209 37
    114 248 246 100 134 104 152 22 212 164 92 204 93 101 182 146 108 112 72 80
    253 237 185 218 94 21 70 87 167 141 157 132 144 216 171 0 140 188 211 10 247
    228 88 5 184 179 69 6 208 44 30 143 202 63 15 2 193 175 189 3 1 19 138 107 58
    145 17 65 79 103 220 234 151 242 207 206 240 180 230 115 150 172 116 34 231
    173 53 133 226 249 55 232 28 117 223 110 71 241 26 113 29 41 197 137 111 183
    98 14 170 24 190 27 252 86 62 75 198 210 121 32 154 219 192 254 120 205 90
    244 31 221 168 51 136 7 199 49 177 18 16 89 39 128 236 95 96 81 127 169 25
    181 74 13 45 229 122 159 147 201 156 239 160 224 59 77 174 42 245 176 200 235
    187 60 131 83 153 97 23 43 4 126 186 119 214 38 225 105 20 99 85 33 12 125))

(defun inv-sub-byte (byte)
  (aref *inv-sub-byte-table* byte))

(defun inv-sub-bytes (state)
  (map-state state #'inv-sub-byte))

;; how implement?
;; Does it depend Nb
(defun shift (r) r)

(defun shift-rows (state)
  (let ((ret (make-array (list 4 Nb))))
    (dotimes (c Nb)
      (setf (aref ret 0 c) (aref state 0 c)))
    (loop for r from 1 to 3 do
      (dotimes (c Nb)
	(setf (aref ret r c) (aref state r (mod (+ c (shift r)) Nb)))))
    ret))

(defun inv-shift-rows (state)
  (let ((ret (make-array (list 4 Nb))))
    (dotimes (c Nb)
      (setf (aref ret 0 c) (aref state 0 c)))
    (loop for r from 1 to 3 do
      (dotimes (c Nb)
	(setf (aref ret r (mod (+ c (shift r)) Nb)) (aref state r c))))
    ret))

(defun mix-columns-by-matrix (state matrix)
  (let ((ret (make-array (list 4 Nb))))
    (dotimes (c Nb ret)
      (dotimes (r 4)
	(setf (aref ret r c)
	      (reduce #'logxor
		      (loop for i from 0 to 3
			    collect (cl-galois:gf-mult (aref matrix r i) (aref state i c)))))))))

(defun mix-columns (state)
  (let ((trans-mat #2A((#b00000010 #b00000011 #b00000001 #b00000001)
		       (#b00000001 #b00000010 #b00000011 #b00000001)
		       (#b00000001 #b00000001 #b00000010 #b00000011)
		       (#b00000011 #b00000001 #b00000001 #b00000010))))
    (mix-columns-by-matrix state trans-mat)))

(defun inv-mix-columns (state)
  (let ((trans-mat #2A((#b00001110 #b00001011 #b00001101 #b00001001)
		       (#b00001001 #b00001110 #b00001011 #b00001101)
		       (#b00001101 #b00001001 #b00001110 #b00001011)
		       (#b00001011 #b00001101 #b00001001 #b00001110))))
    (mix-columns-by-matrix state trans-mat)))

;; not tested
(defun add-round-key (state round-keys)
  (let ((ret (make-array (list 4 Nb)))
	(keys round-keys))
    (dotimes (c Nb ret)
      (let ((round-key (pop keys)))
	(dotimes (r 4)
	  (let* ((offset (- 32 (* (1+ r) 8)))
		 (key (truncate round-key offset)))
	    (setf round-key (mod round-key offset)
		  (aref ret r c) (logxor (aref state r c) key))))))))

(defun sub-word (word)
  (mapbyte #'sub-byte word))

(defun rot-word (word)
  (multiple-value-bind (upper8 lower24) (truncate word (expt 2 24))
    (+ (* lower24 (expt 2 8)) upper8)))

(defun r-con (i)
  (let ((base-byte 2))
    (ash (cl-galois:gf-power base-byte (1- i)) 24)))

(defun key-expansion (key)
  (flet ((inner-key-divided (key)
	   "32 * Nk bit key -> list of 32bits block"
	   (loop for i from 0 below (* 32 Nk) by 32
		 collect (subseq key i (+ i 32)))))
    (let ((round-keys (make-array (* Nb (1+ Nr))))
	  (key-blocks (inner-key-divided key)))
      (dotimes (n Nk) ;; set keys to first Nk words of round-key
	(setf (aref round-keys n) (pop key-blocks)))
      (loop for i from Nk below (* Nb (1+ Nr))
	    for tmp = (aref round-keys (1- i)) then (aref round-keys (1- i))
	    do (setf (aref round-keys i)
		     (bit-xor (aref round-keys (- i Nk))
			      (cond ((= (mod i Nk) 0) (bit-xor (sub-word (rot-word tmp))
							       (r-con (/ i Nk))))
				    ((and (> Nk 6) (= (mod i Nk) 4)) (sub-word tmp))
				    (t tmp)))))
      round-keys)))

(defparameter test-key (cl-crypto-util:int-bit #x2b7e151628aed2a6abf7158809cf4f3c 128))

(defun input-to-state (input)
  "vector to 2-dimention arr"
  (let ((state (make-array (list 4 Nb))))
    (dotimes (r 4 state)
      (dotimes (c Nb)
	(setf (aref state r c) (aref input (+ r (* 4 c))))))))

(defun state-to-output (state)
  (let ((output (make-array (* 4 Nb))))
    (dotimes (r 4 (apply #'concat-bit-array (coerce output 'list)))
      (dotimes (c Nb)
	(setf (aref output (+ r (* 4 c))) (aref state r c))))))

@export
(defun encrypt (input key)
  (flet ((inner-block-to-byte-list (input)
	   (make-array 16 :initial-contents(loop for i from 0 below 128 by 8
						 collect (subseq input i (+ i 8))))))
    (let ((state (input-to-state (inner-block-to-byte-list input))) ;; copy input to state
	  (round-keys (key-expansion key))) ;; get round-keys      
      (setf state (add-round-key state (coerce (subseq round-keys 0 Nb) 'list)))
      (loop for r from 1 to (1- Nr) do
	(setf state (add-round-key (mix-columns (shift-rows (sub-bytes state)))
				   ;; get Nb-round-keys
				   (coerce (subseq round-keys (* r Nb) (* (1+ r) Nb)) 'list))))      
      (state-to-output (add-round-key (shift-rows (sub-bytes state))
				      (coerce (subseq round-keys (* Nr Nb) (* (1+ Nr) Nb)) 'list))))))

@export
(defun decrypt (input key)
  (flet ((inner-block-to-byte-list (input)
	   (make-array 16 :initial-contents(loop for i from 0 below 128 by 8
						 collect (subseq input i (+ i 8))))))
    (let ((state (input-to-state (inner-block-to-byte-list input)))
	  (round-keys (key-expansion key)))
      (setf state (add-round-key state (coerce (subseq round-keys (* Nr Nb) (* (1+ Nr) Nb)) 'list)))
      (loop for r downfrom (1- Nr) to 1 do
	(setf state (inv-mix-columns
		     (add-round-key (inv-sub-bytes (inv-shift-rows state))
				    (coerce (subseq round-keys (* r Nb) (* (+ r 1) Nb)) 'list)))))
      (state-to-output (add-round-key (inv-sub-bytes (inv-shift-rows state))
				      (coerce (subseq round-keys 0 Nb) 'list))))))



(defparameter test-input (cl-crypto-util:int-bit #x3243f6a8885a308d313198a2e0370734 128))
(defparameter test-key (cl-crypto-util:int-bit #x2b7e151628aed2a6abf7158809cf4f3c 128))
(defparameter test-output (cl-crypto-util:int-bit #x3925841d02dc09fbdc118597196a0b32 128))

(defun output-hex (state)
  (let ((ret (make-array (list 4 Nb))))    
    (dotimes (r 4 ret)
      (dotimes (c Nb)
	(setf (aref ret r c) (format nil "~x" (cl-crypto-util:bit-int (aref state r c))))))))
