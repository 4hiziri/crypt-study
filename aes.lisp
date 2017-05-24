;; Rijndael
;; use integer as bits
;(load "./util.lisp")
;(load "./gf.lisp")

(defun bits-matrix (bits)
  (let ((mat (make-array '(4 4))))
    (dotimes (i 4 mat)
      (dotimes (j 4)
	(let ((head (+ (* 32 i) (* j 8))))
	  (setf (aref mat j i) (subseq bits head (+ head 8))))))))

(defparameter Nk 4)
(defparameter Nb 4)
(defparameter Nr 12)

(defparameter *subbyte-map*
  #(116 240 67 157 27 191 253 66 61 175 113 215 170 125 2 197 69 31 119 222 237
    242 95 187 109 174 74 107 105 239 183 29 229 44 234 134 0 149 99 144 68 136 10
    51 82 181 221 77 249 78 245 64 42 89 34 164 8 236 85 151 94 35 114 97 1 165 15
    87 185 123 108 3 55 159 139 162 112 19 70 167 43 156 25 216 4 189 211 33 133
    190 223 235 154 120 86 26 213 252 160 131 206 180 205 232 47 176 14 209 56 36
    83 80 75 63 138 147 48 111 17 141 195 92 39 57 212 73 150 54 214 184 126 115
    32 179 248 173 122 118 188 166 50 41 11 217 59 12 93 130 227 230 7 129 178 254
    79 62 171 155 72 192 186 53 194 90 91 247 233 104 207 199 148 200 251 158 96
    16 84 241 40 46 224 168 201 38 228 127 163 5 219 196 152 88 169 6 117 23 244
    106 203 58 102 231 250 65 243 198 60 100 30 255 81 238 220 49 208 124 18 21 52
    128 110 135 182 225 140 76 161 142 177 13 202 204 146 121 98 193 103 172 145
    153 37 246 137 9 226 28 143 71 22 218 20 132 101 210 24 45))

(defparameter *matrix-sbox* #(#*10001111
			      #*11000111
			      #*11100011
			      #*11110001
			      #*11111000
			      #*01111100
			      #*00111110
			      #*00011111))

(defun sub-byte (byte)
  "substitude byte to byte by some mathmatical process."
  (let ((ret (make-array 8 :element-type 'bit))
	;; original text's indexing is reversed from lisp implement
	;; so reveres input, output and some parameter
	(byte (reverse (gf-inv byte))) 
	(salt #*01100011))
    (dotimes (i 8 (bit-xor (reverse ret) salt))
      (setf (aref ret i) (reduce (lambda (x y) (logxor x y))
				 (bit-and byte (aref *matrix-sbox* i)))))))
;; :TODO make hash-map
(defun sub-bytes (state)
  "substitude each byte in state."
  (let ((ret (make-array (list 4 Nb))))    
    (dotimes (r 4 ret)
      (dotimes (c Nb)
	(setf (aref ret r c) (sub-byte (aref state r c)))))))

;; :TODO make hash-map
(defun inv-sub-byte (byte)
  )

;; #2A((1 2 3 4)
;;     (5 6 7 8)
;;     (9 10 11 12)
;;     (13 14 15 16))

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

(defparameter input #2A((#*00011001 #*10100000 #*10011010 #*11101001)
			(#*00111101 #*11110100 #*11000110 #*11111000)
			(#*11100011 #*11100010 #*10001101 #*01001000)
			(#*10111110 #*00101011 #*00101010 #*00001000)))

(make-array '(4 4) :initial-contents
	    (list (list (int2bit #x19 8) (int2bit #xa0 8) (int2bit #x9a 8) (int2bit #xe9 8))
		  (list (int2bit #x3d 8) (int2bit #xf4 8) (int2bit #xc6 8) (int2bit #xf8 8))
		  (list (int2bit #xe3 8) (int2bit #xe2 8) (int2bit #x8d 8) (int2bit #x48 8))
		  (list (int2bit #xbe 8) (int2bit #x2b 8) (int2bit #x2a 8) (int2bit #x08 8))))

'("D4" "E0" "B8" "1E"
  "27" "BF" "B4" "41"
  "11" "98" "5D" "52"
  "AE" "F1" "E5" "30")

'("D4" "E0" "B8" "1E"
  "BF" "B4" "41" "27"
  "5D" "52" "11" "98"
  "30" "AE" "F1" "E5")

'("04" "E0" "48" "28"
  "66" "CB" "F8" "06"
  "81" "19" "D3" "26"
  "E5" "9A" "7A" "4C")

(defparameter test #2A((#*00000010 #*00000011 #*00000001 #*00000001)
		       (#*00000001 #*00000010 #*00000011 #*00000001)
		       (#*00000001 #*00000001 #*00000010 #*00000011)
		       (#*00000011 #*00000001 #*00000001 #*00000010)))

(defun mix-columns-by-matrix (state matrix)
  (let ((ret (make-array (list 4 Nb))))
    (dotimes (c Nb ret)
      (dotimes (r 4)
	(setf (aref ret r c)
	      (reduce #'bit-xor
		      (loop for i from 0 to 3
			    collect
			    (gf-mult (aref matrix r i) (aref state i c)))))))))

(defun mix-columns (state)
  (let ((trans-mat #2A((#*00000010 #*00000011 #*00000001 #*00000001)
		       (#*00000001 #*00000010 #*00000011 #*00000001)
		       (#*00000001 #*00000001 #*00000010 #*00000011)
		       (#*00000011 #*00000001 #*00000001 #*00000010))))
    (mix-columns-by-matrix state trans-mat)))

(defun inv-mix-columns (state)
  (let ((trans-mat #2A((#*00001110 #*00001011 #*00001101 #*00001001)
		       (#*00001001 #*00001110 #*00001011 #*00001101)
		       (#*00001101 #*00001001 #*00001110 #*00001011)
		       (#*00001011 #*00001101 #*00001001 #*00001110))))
    (mix-columns-by-matrix state trans-mat)))

(defun add-round-key (state round-keys)
  (let ((ret (make-array (list 4 Nb)))
	(keys round-keys))
    (dotimes (c Nb ret)
      (let ((key-blocks (word-to-bytes-seq (pop keys))))	  
	(dotimes (r 4)
	  (setf (aref ret r c) (bit-xor (aref state r c) (pop key-blocks))))))))

(defun sub-word (word)
  (mapbyte word #'sub-byte))

(defun rot-word (word)
  (apply (lambda (x1 x2 x3 x4) (concatenate 'bit-vector x2 x3 x4 x1))
	 (word-to-bytes-seq word)))

(defun r-con (i)
  (let ((base-byte (int2bit 2 8)))
    (apply #'concat-bit-array (cons (gf-power base-byte (1- i))
				    (loop repeat 3 collect (int2bit 0 8))))))

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

(defparameter test-key (int2bit #x2b7e151628aed2a6abf7158809cf4f3c 128))

(defun input-to-state (input)
  "vector to 2-dimention arr"
  (let ((state (make-array (list 4 Nb))))
    (dotimes (r 4 state)
      (dotimes (c Nb)
	(setf (aref state r c) (aref input (+ r (* 4 c))))))))

(defun state-to-output (state)
  (let ((output (make-array (* 4 Nb))))
    (dotimes (r 4 (bit2int (apply #'concat-bit-array (coerce output 'list))))
      (dotimes (c Nb)
	(setf (aref output (+ r (* 4 c))) (aref state r c))))))

(defun encrypt (input key)
  (flet ((inner-block-to-byte-list (input)
	   (make-array 16 :initial-contents(loop for i from 0 below 128 by 8
						 collect (subseq input i (+ i 8))))))
    (let ((state (input-to-state (inner-block-to-byte-list input)))
	  (round-keys (key-expansion key)))
      (setf state (add-round-key state (coerce (subseq round-keys 0 Nb) 'list)))
      (dotimes (r (- Nr 2))
	(setf state (add-round-key (mix-columns (shift-rows (sub-bytes state)))
				   (coerce (subseq round-keys (* (+ r 1) Nb) (* (+ r 2) Nb)) 'list))))
      (state-to-output (add-round-key (shift-rows (sub-bytes state))
				      (coerce (subseq round-keys (* (- Nr 1) Nb) (* Nr Nb)) 'list))))))

(defun decrypt (input key)
  (flet ((inner-block-to-byte-list (input)
	   (make-array 16 :initial-contents(loop for i from 0 below 128 by 8
						 collect (subseq input i (+ i 8))))))
    (let ((state (input-to-state (inner-block-to-byte-list input)))
	  (round-keys (reverse (key-expansion key))))
      (setf state (add-round-key state (coerce (subseq round-keys 0 Nb) 'list)))
      (dotimes (r (- Nr 2))
	(setf state (inv-mix-columns
		     (add-round-key (inv-sub-bytes (inv-shift-rows state))
				    (coerce (subseq round-keys (* (+ r 1) Nb) (* (+ r 2) Nb)) 'list)))))
      (state-to-output (add-round-key (inv-sub-bytes (inv-shift-rows state))
				      (coerce (subseq round-keys (* (- Nr 1) Nb) (* Nr Nb)) 'list))))))
