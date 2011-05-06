
;; http://en.wikipedia.org/wiki/PNG_file_format
;; http://garethrees.org/2007/11/14/pngcrush/
;; http://www.ietf.org/rfc/rfc1951.txt
;; http://drj11.wordpress.com/2007/11/20/a-use-for-uncompressed-pngs/
;; http://www.w3.org/TR/PNG/#11IHDR

;; for letters start chunk
;; 1) uppercase letter = critical
;; 2) uppercase = public
;; 3) must be uppercase
;; 4) lowercase = safe to copy

;; chunk:
;; length ub4, chunk-type ub4, chunk-data length*ub, crc ub4

;; IHDR 73 72 68 82 first chunk with header

;; width ub4, height ub4, bit-depth=8, color-type=0,
;; compression-method=0, filter-method=0, interlace-method=0
 
;; IDAT 73 68 65 84 image

;; ZLIB
;; compression-method-and-flags=#b0111 1000=#x78, flags= #x9c , 
;; CMF*256+FLG must be multiple of 31
#+nil ;; FLG & #b11111 =
(mod (* 257 #x78) 31)
#+nil
(logand #x9c #b11111)

;; FCHECK=11100 no preset dictionary, default algorithm

;; inside #0500 for n=5
;; followed by two's complement #xfaff

;; datablocks n*1byte, check-value ub4 (adler)

;; DEFLATE
;; header-bits (3) BFINAL1, BTYPE2=00

;; ignores all data until next byte boundary, then
;; LEN2,NLEN2, LEN*byte data

;; IEND 73 69 78 68 marks end of image

(defun list->array (ls)
  (declare (values (simple-array (unsigned-byte 8) 1) &optional))
  (make-array (length ls) :element-type '(unsigned-byte 8)
	      :initial-contents ls))

(defun ub32->ub8 (b)
  (loop for i below 4 collect
       (ldb (byte 8 (* 8 i)) b)))
(defun ub16->ub8 (b)
  (loop for i below 2 collect
       (ldb (byte 8 (* 8 i)) b)))

#+nil
(ub4->ub1 #xff010203)

(defun ihdr (width height &optional (bit-depth 8) (color-type 0)
	     (compression-method 0) (filter-method 0) (interlace-method 0))
  (declare (type (unsigned-byte 32) width height)
	   (type (unsigned-byte 8) bit-depth color-type compression-method
		 filter-method interlace-method)
	   (values (simple-array (unsigned-byte 8) 1) &optional))
  (let* ((data (list width height bit-depth color-type compression-method filter-method
		     interlace-method)))
    (list->array (append (ub16->ub8 (length data))
			 (list 73 72 68 82)
			 data 
			 (ub32->ub8 (crc (list->array data)))))))

(defun idat (zlib-data)
  (declare (type (simple-array (unsigned-byte 8) 1) zlib-data)
	   (values (simple-array (unsigned-byte 8) 1) &optional))
  (concatenate '(simple-array (unsigned-byte 8) 1)
	       (list->array (append (ub16->ub8 (length zlib-data))
				    (list 73 68 65 84)))
	       zlib-data
	       (list->array (ub32->ub8 (crc zlib-data)))))

(defun zlib (deflate-data)
  (declare (type (simple-array (unsigned-byte 8) 1) deflate-data)
	   (values (simple-array (unsigned-byte 8) 1) &optional))
  (concatenate '(simple-array (unsigned-byte 8) 1)
	       (list->array (list #x78 #x9c))
	       deflate-data
	       (list->array (ub32->ub8 (adler deflate-data)))))

(defun deflate (image-data)
  (concatenate '(simple-array (unsigned-byte 8) 1)
	       (list->array (append (list #b10000000)
				    (ub16->ub8 (length image-data))
				    (ub16->ub8 (- (length image-data)))))
	       image-data))

(defun iend ()
  (list->array (list 73 69 78 68)))

(defun write-png (fn image-data)
  (declare (type (simple-array (unsigned-byte 8) 2) image-data))
  (with-open-file (s fn :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create
		     :element-type '(unsigned-byte 8))
   (destructuring-bind (h w) (array-dimensions image-data)
     (write-sequence 
      (concatenate '(simple-array (unsigned-byte 8) 1)
		   (list->array '(137 80 78 71 13 10 26 10)) ;; signature
		   (ihdr w h)
		   (idat (zlib (deflate (sb-ext:array-storage-vector image-data))))
		   (iend))
      s))
   nil))

(let* ((h 100)
       (w 100)
       (data (make-array (list h w) :element-type '(unsigned-byte 8))))
  (write-png "/dev/shm/o.png" data))

(let ((crc-table 
       (make-array 
	256 :element-type '(unsigned-byte 32)
	:initial-contents
	(loop for n below 256 collect
	     (let ((c n))
	       (declare (type (unsigned-byte 32) c))
	       (dotimes (k 8)
		 (setf c (if (< 0 (logand c 1))
			     (logxor #xedb88320 (ash c -1))
			     (ash c -1))))
	       c)))))
  (defun update-crc (buf &optional (crc #xffffffff))
    (declare (type (simple-array (unsigned-byte 8) 1) buf)
	     (type (unsigned-byte 32) crc)
	     (values (unsigned-byte 32) &optional))
    (dotimes (n (length buf))
      (setf crc
	    (logxor (aref crc-table (logand (logxor crc
						    (aref buf n)) 
					    #xff))
		    (ash crc -8))))
    crc)
  (defun crc (buf)
    (declare (type (simple-array (unsigned-byte 8) 1) buf)
	     (values (unsigned-byte 32) &optional))
    (logxor (update-crc buf) #xffffffff)))

#+nil
(crc (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(0))) ; 3523407757
#+nil
(crc (make-array 1 :element-type '(unsigned-byte 8) :initial-contents '(1))) ; 2768625435
#+nil
(crc (make-array 
      65 :element-type '(unsigned-byte 8)
      :initial-contents 
      (mapcar #'(lambda (x) (char-code x)) 
	      (coerce "Twas brillig and the slithy toves did gyre and gimble in the wabe" 
		      'list)))) ; 4186783197


(defun adler (buf)
  (declare (type (simple-array (unsigned-byte 8) 1) buf)
	   (values (unsigned-byte 32) &optional))
  (let ((a 1)
	(b 0)
	(mod-adler 65521))
    (declare (type (unsigned-byte 32) a b))
    (dotimes (i (length buf))
      (setf a (mod (+ a (aref buf i))
		   mod-adler)
	    b (mod (+ b a)
		   mod-adler)))
    (logior (ash b 16)
	    a)))

#+nil
(adler (make-array 
	4 :element-type '(unsigned-byte 8)
	:initial-contents 
	(mapcar #'(lambda (x) (char-code x)) 
		(coerce "blah" 
			'list)))) ;; #x03fa0198