;; Rijndael
;; use integer as bits
(load "./util.lisp")
(load "./gf.lisp")

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

(defun sub-bytes (state)
  "substitude each byte in state."
  (let ((ret (make-array (list 4 Nb))))    
    (dotimes (r 4 ret)
      (dotimes (c Nb)
	(setf (aref ret r c) (sub-byte (aref state r c)))))))

;; #2A((1 2 3 4)
;;     (5 6 7 8)
;;     (9 10 11 12)
;;     (13 14 15 16))

;; how implement?
;; Does it depend Nb
(defun shift (r)
  r)

(defun shift-rows (state)
  (let ((ret (make-array (list 4 Nb))))
    (dotimes (c Nb)
      (setf (aref ret 0 c) (aref state 0 c)))
    (loop for r from 1 to 3 do
      (dotimes (c Nb)
	(setf (aref ret r c) (aref state r (mod (+ c (shift r)) Nb)))))
    ret))

(defparameter input #2A((#*00011001 #*10100000 #*10011010 #*11101001)
			(#*00111101 #*11110100 #*11000110 #*11111000)
			(#*11100011 #*11100010 #*10001101 #*01001000)
			(#*10111110 #*00101011 #*00101010 #*00001000)))

("D4" "E0" "B8" "1E"
 "27" "BF" "B4" "41"
 "11" "98" "5D" "52"
 "AE" "F1" "E5" "30")

(make-array '(4 4) :initial-contents
	    (list (list (int2bit #x19 8) (int2bit #xa0 8) (int2bit #x9a 8) (int2bit #xe9 8))
		  (list (int2bit #x3d 8) (int2bit #xf4 8) (int2bit #xc6 8) (int2bit #xf8 8))
		  (list (int2bit #xe3 8) (int2bit #xe2 8) (int2bit #x8d 8) (int2bit #x48 8))
		  (list (int2bit #xbe 8) (int2bit #x2b 8) (int2bit #x2a 8) (int2bit #x08 8))))



(defparameter test #2A((#*00000010 #*00000011 #*00000001 #*00000001)
		       (#*00000001 #*00000010 #*00000011 #*00000001)
		       (#*00000001 #*00000001 #*00000010 #*00000011)
		       (#*00000011 #*00000001 #*00000001 #*00000010)))

(defun mix-columns (state)
  (let ((trans-mat #2A((#*00000010 #*00000011 #*00000001 #*00000001)
		       (#*00000001 #*00000010 #*00000011 #*00000001)
		       (#*00000001 #*00000001 #*00000010 #*00000011)
		       (#*00000011 #*00000001 #*00000001 #*00000010)))
	(ret (make-array (list 4 Nb))))
    (dotimes (c Nb ret)
      (dotimes (r 4)
	(setf (aref ret r c)
	      (reduce #'bit-xor
		      (loop for i from 0 to 3
			    collect
			    (gf-mult (aref trans-mat r i) (aref state i c)))))))))

(defun add-round-key (bytes-4 round-keys)
  (let ((ret (make-array '(4 4))))
    (dotimes (i 4 ret)
      (dotimes (j 4)
	(setf (aref ret j i) (bit-xor (aref bytes-4 j i)
				      (subseq (aref round-keys i)
					      (* j 8)
					      (* (1+ j) 8))))))))

(defun sub-word (word &optional (map *subbyte-map*))
  (let ((ret nil))
    (dotimes (i 4 (apply #'concat-bit-array (reverse ret)))
      (push (sub-byte (subseq word (* i 8) (* (1+ i) 8)) map) ret))))

(defun rot-word (word)
  (bit-lrotate word 8))

(defun r-con (i)
  (let ((base-byte (int2bit 2 8)))
    (concat-bit-array (gf-power base-byte (1- i))
		      (loop repeat 3 collect (int2bit 0 8)))))



(defun key-expansion (key)
  )

(defun input-to-state (input)
  "vector to 2-dimention arr"
  (let ((state (make-array (list 4 Nb))))
    (dotimes (r 4)
      (dotimes (c Nb)
	(setf (aref state r c) (aref input (+ r (* 4 c))))))))

(defun state-to-output (state)
  (let ((output (make-array (* 4 Nb))))
    (dotimes (r 4)
      (dotimes (c Nb)
	(setf (aref output (+ r (* 4 c))) (aref state r c))))))

(defun encrypt (input key)  
  (let ((state (input-to-state input))
	(round-keys (key-expansion key)))
    ;; add-round-key should have side-effect?
    ;; or implement this macro?
    ;; or extracting first 4 element as method
    (setf state (add-round-key state round-keys))
    (dotimes (n (- Nr 2))
      (setf (add-round-key (mix-columns
			    (shift-rows
			     (sub-bytes state)))
			   round-keys ;; how handle?
			   )))

    (add-round-key (shift-rows (sub-bytes state)) round-keys)
    (state-to-output)))
