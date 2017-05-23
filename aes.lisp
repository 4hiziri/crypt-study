;; Rijndael
;; use integer as bits
(load "./util.lisp")

(defun bits-matrix (bits)
  (let ((mat (make-array '(4 4))))
    (dotimes (i 4 mat)
      (dotimes (j 4)
	(let ((head (+ (* 32 i) (* j 8))))
	  (setf (aref mat j i) (subseq bits head (+ head 8))))))))

(defparameter Nk 4)
(defparameter Nb 4)
(defparameter Nr 12)

(d>0;95;0cefparameter *subbyte-map*
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
  (let ((ret (make-array 8 :element-type 'bit))
	(salt #*11000110))
    (dotimes (i 8 (bit-xor ret salt))
      (setf (aref ret i) (reduce (lambda (x y) (logxor x y))
				 (bit-xor byte (aref *matrix-sbox* i)))))))

(defun sub-bytes (state)
  )

(defun sub-byte (byte map &optional (get (lambda (arr x) (aref arr x))))
  "convert 8bit to another 8bit by map's index - value"
  (int2bit (funcall get map (bit2int byte)) 8))

(defun sub-bytes (bytes-matrix map)
  "each byte coverted by map"
  (let ((ret (make-array (array-dimensions bytes-matrix))))
    (dotimes (i 4 ret)
      (dotimes (j 4)
	(setf (aref ret i j) (sub-byte (aref bytes-matrix i j) map))))))

(defun shift-rows (bytes-mat)
  (dotimes (n 3 bytes-mat)
    (let ((i (1+ n)))
      (dotimes (j i)
	(rotatef (aref bytes-mat i 0)
		 (aref bytes-mat i 1)
		 (aref bytes-mat i 2)
		 (aref bytes-mat i 3))))))

(defun mix-column (column)
  (flet ((gf-mul-cof (b1-vec b2-vec index)
	   (gf-mult (nth index b1-vec) (nth index b2-vec))))
    (let* ((mix-coff (list #*00000010 #*00000011 #*00000001 #*00000001))
	   (ret nil))
      (dotimes (n 4 (reverse ret))
	(push (reduce #'bit-xor
		      (loop for i from 0 to 3
			    collect (gf-mul-cof column mix-coff i)))
	      ret)
	(rotatef (nth 3 mix-coff)
		 (nth 2 mix-coff)
		 (nth 1 mix-coff)
		 (nth 0 mix-coff))))))

(defun mix-columns (bytes-4)
  (let ((ret (make-array '(4 4))))
    (dotimes (n 4 ret)
      (let ((column (mix-column (loop for i from 0 to 3 collect (aref bytes-4 i n)))))
	(dotimes (m 4)
	  (setf (aref ret m n) (nth m column)))))))

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
