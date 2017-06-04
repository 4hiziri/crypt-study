(in-package :cl-user)
(defpackage cl-galois
  (:use :cl :cl-annot :cl-crypto-util))
(in-package :cl-galois)

(cl-annot:enable-annot-syntax)

(defparameter *mx* #b100011011)

@export
(defun gf-add (num &rest rest)
  (let ((acc num))
    (dolist (n rest acc)
      (setf acc (logxor acc n)))))

@export
(defun gf-mod (n1 n2)
  (labels ((below-powered (n1 n2 prev-n1)
	     (if (> n1 n2)
		 prev-n1
		 (below-powered (ash n1 1) n2 n1)))
	   (inner-loop (n1 n2)
	     (cond ((= (integer-length n1) 9) (logxor n1 n2))
		   ((< n1 n2) n1)
		   (t (inner-loop (logxor n1 (below-powered n2 n1 n2)) n2)))))
    (inner-loop n1 n2)))

(defun gf-mult-unfinited (n1 n2)
  (let ((exponents (cl-crypto-util:bit-indexes n2))
	(acc 0))
    (dolist (e exponents acc)
      (setf acc (logxor acc (ash n1 e))))))

@export
(defun gf-mult (num &rest rest)
  (let ((acc num))
    (dolist (n rest acc)
      (setf acc (gf-mod (gf-mult-unfinited acc n) *mx*)))))

@export
(defun gf-power (num exponent)
  (if (< exponent 1)
      1
      (gf-mod (ash num (1- exponent)) *mx*)))

(defun gf-bruteforce-inv (num)
  "This function calculate gf-inverse of num by brute-force."
  (if (/= num 0)
      (loop for i from 1 to 255	    
	    when (equal (gf-mult i num) 1)
	      do (return i))
      0))

@export
(defun gf-inv (num)
  "This function has lookup vector, which is 256 length"
  (let ((table #(0 1 141 246 203 82 123 209 232 79 41 192 176 225 229 199 116 180 170 75 153
    43 96 95 88 63 253 204 255 64 238 178 58 110 90 241 85 77 168 201 193 10 152
    21 48 68 162 194 44 69 146 108 243 57 102 66 242 53 32 111 119 187 89 25 29
    254 55 103 45 49 245 105 167 100 171 19 84 37 233 9 237 92 5 202 76 36 135
    191 24 62 34 240 81 236 97 23 22 94 175 211 73 166 54 67 244 71 145 223 51
    147 33 59 121 183 151 133 16 181 186 60 182 112 208 6 161 250 129 130 131 126
    127 128 150 115 190 86 155 158 149 217 247 2 185 164 222 106 50 109 216 138
    132 114 42 20 159 136 249 220 137 154 251 124 46 195 143 184 101 72 38 200 18
    74 206 231 210 98 12 224 31 239 17 117 120 113 165 142 118 61 189 188 134 87
    11 40 47 163 218 212 228 15 169 39 83 4 27 252 172 230 122 7 174 99 197 219
    226 234 148 139 196 213 157 248 144 107 177 13 214 235 198 14 207 173 8 78
    215 227 93 80 30 179 91 35 56 52 104 70 3 140 221 156 125 160 205 26 65 28)))
    (aref table num)))
