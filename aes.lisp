;; Rijndael
;; use integer as bits
(load "./util.lisp")

(defun bits-matrix (bits)
  (let ((mat (make-array '(4 4))))
    (dotimes (i 4 mat)
      (dotimes (j 4)
	(let ((head (+ (* 32 i) (* j 8))))
	  (setf (aref mat j i) (subseq bits head (+ head 8))))))))

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

(defun sub-byte (byte map &optional (get (lambda (arr x) (aref arr x))))
  "convert 8bit to another 8bit by map's index - value"
  (int2bit (funcall get map (bit2int byte)) 8))

(defun sub-bytes (bytes-matrix map)
  "each byte coverted by map"
  (let ((ret (make-array (array-dimensions bytes-matrix))))
    (dotimes (i 4 ret)
      (dotimes (j 4)
	(setf (aref ret i j) (sub-byte (aref bytes-matrix i j) map))))))

(defun shift-rows-map ()
  "shift each 4bytes"
  (let* ((len 128)
	 (ret (make-array len))
	 (c 0))
    (loop repeat 4 for i from 0 by 32 do
      (loop repeat 4
	    for b-i from 0
	    for j from 0 by 8 do
	      (dotimes (n 8)
		(setf (aref ret (mod (+ i j n (* b-i 32)) 128)) c)
		(incf c))))
    ret))

(defparameter *shift-rows-map* #(0 1 2 3 4 5 6 7
				 104 105 106 107 108 109 110 111
				 80 81 82 83 84 85 86 87
				 56 57 58 59 60 61 62 63
				 32 33 34 35 36 37 38 39
				 8 9 10 11 12 13 14 15
				 112 113 114 115 116 117 118 119
				 88 89 90 91 92 93 94 95
				 64 65 66 67 68 69 70 71
				 40 41 42 43 44 45 46 47
				 16 17 18 19 20 21 22 23
				 120 121 122 123 124 125 126 127
				 96 97 98 99 100 101 102 103
				 72 73 74 75 76 77 78 79
				 48 49 50 51 52 53 54 55
				 24 25 26 27 28 29 30 31))



(defun shift-rows (bytes &optional (map *shift-rows-map*))
  "shift each 4bytes"
  (let ((ret (make-array 128 :element-type 'bit)))    
    (dotimes (n 128 ret)
      (setf (aref ret n) (aref bytes (aref map n))))))

(defun shift-rows (bytes-mat)
  (dotimes (n 3 bytes-mat)
    (let ((i (1+ n)))
      (dotimes (j i)
	(rotatef (aref bytes-mat i 0)
		 (aref bytes-mat i 1)
		 (aref bytes-mat i 2)
		 (aref bytes-mat i 3))))))

(defun mix-columns (bytes-4)
  )
