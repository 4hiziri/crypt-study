(load "./util.lisp")
(load "./des.lisp")

;; ecb-mode
(defun ecb (data key-56 cipher)
  (let ((blocks (slash-block data)))
    (apply #'concat-bit-array
	   (mapcar (lambda (b) (funcall cipher b key-56)) blocks))))

(defun ecb-encrypt (data key-56 encrypt-f)
  (ecb data key-56 encrypt-f))

(defun ecb-decrypt (data key-56 decrypt-f)
  (ecb data key-56 decrypt-f))

;; cbc-mode

(defparameter *iv* (make-array 64 :element-type 'bit
				  :initial-contents (loop repeat 64 collect (random 2))))
(defun cbc (data key-56 block-converter get-next iv)
  (let ((blocks (slash-block data))
	(ret nil))
    (dolist (b blocks (apply #'concat-bit-array (reverse ret)))
      (let ((c (funcall block-converter iv b key-56)))
	(push c ret)
	(setf iv (funcall get-next b c))))))

(defun cbc-encrypt (data key-56 encrypt-func &optional (iv *iv*))
  (cbc data
       key-56
       (lambda (prev next key)
	 (funcall encrypt-func (bit-xor prev next) key))
       (lambda (prev next) next)
       iv))

(defun cbc-decrypt (data key-56 decrypt-func &optional (iv *iv*))
  (cbc data
       key-56
       (lambda (prev next key)
	 (bit-xor prev (funcall decrypt-func next key)))
       (lambda (prev next) prev)
       iv))

;; cfb-mode
(defun cfb (data key encrypt get-iv iv)
  (let ((blocks (slash-block data))
	(prev iv)
	(ret nil))
    (dolist (b blocks (apply #'concat-bit-array (reverse ret)))
      (push (bit-xor (funcall encrypt prev key) b) ret)
      (setf prev (funcall get-iv b (car ret))))))

(defun cfb-encrypt (data key encrypt &optional (iv *iv*))
  (cfb data key encrypt (lambda (p n) n) iv))

(defun cfb-decrypt (data key encrypt &optional (iv *iv*))
  (cfb data key encrypt (lambda (p n) p) iv))

;; ofb-mode
(defun ofb (data key encrypt &optional (iv *iv*))
  (let ((blocks (slash-block data))
	(stream-key iv)
	(ret))
    (dolist (b blocks (apply #'concat-bit-array (reverse ret)))
      (setf stream-key (funcall encrypt stream-key key))
      (push (bit-xor b stream-key) ret))))

(defun ofb-encrypt (data key encrypt &optional (iv *iv*))
  (ofb data key encrypt iv))

(defun ofb-decrypt (data key encrypt &optional (iv *iv*))
  (ofb data key encrypt iv))

;; ctr-mode
(defparameter *nonce* #*00110101010000111111001010111100)
(defun ctr (data key encrypt &optional (nonce *nonce*))
  (let ((blocks (slash-block data))
	(counter (concat-bit-array nonce (int2bit 0 32)))
	(ret nil))
    (dolist (b blocks (apply #'concat-bit-array (reverse ret)))
      (push (bit-xor (funcall encrypt counter key) b) ret)
      (setf counter (inc-bit-array counter)))))

(defun ctr-encrypt (data key encrypt &optional (nonce *nonce*))
  (ctr data key encrypt nonce))

(defun ctr-decrypt (data key encrypt &optional (nonce *nonce*))
  (ctr data key encrypt nonce))

;;; easy-test
;; password is "password", use shasum as hash-function
(defparameter password-hash #xc8fed00eb2e87f1cee8e90ebbe870c190ac3848c)

(defun crypt-on-mode (mode crypt-func)
  "(mode src key-56 block-to-block-convert-function)
(crypt-func 64-bits-array key-56)"
  (lambda (data key)
    (funcall mode data key crypt-func)))

(defun test (string pass-num key-num encrypt decrypt)
  (let* ((key (int2bit pass-num key-num))
	 (row-data (encode-ascii string))
	 (cipher (funcall encrypt row-data key)))
    (decode-ascii (funcall decrypt cipher key))))

(defun test-des-ecb (string)
  (test string
	password-hash
	56
	(crypt-on-mode #'ecb-encrypt #'des-ede1-encryption)
	(crypt-on-mode #'ecb-decrypt #'des-ede1-decryption)))

(defun test-ede2-ecb (string)
  (test string
	password-hash
	112
	(crypt-on-mode #'ecb-encrypt #'des-ede2-encryption)
	(crypt-on-mode #'ecb-decrypt #'des-ede2-decryption)))

(defun test-ede3-ecb (string)
  (test string
	password-hash
	168
	(crypt-on-mode #'ecb-encrypt #'des-ede3-encryption)
	(crypt-on-mode #'ecb-decrypt #'des-ede3-decryption)))

(defun test-des-cbc (string)
  (test string
	password-hash
	56
	(crypt-on-mode #'cbc-encrypt #'des-encryption)
	(crypt-on-mode #'cbc-decrypt #'des-decryption)))

(defun test-des-cfb (string)
  (test string
	password-hash
	56
	(crypt-on-mode #'cfb-encrypt #'des-encryption)
	(crypt-on-mode #'cfb-decrypt #'des-encryption)))

(defun test-des-ofb (string)
  (test string
	password-hash
	56
	(crypt-on-mode #'ofb-encrypt #'des-encryption)
	(crypt-on-mode #'ofb-decrypt #'des-encryption)))

(defun test-des-ctr (string)
  (test string
	password-hash
	56
	(crypt-on-mode #'ctr-encrypt #'des-encryption)
	(crypt-on-mode #'ctr-decrypt #'des-encryption)))
