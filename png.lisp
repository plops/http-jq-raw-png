(in-package :png)
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

;; all numbers in network byte order (most significant byte first)
;; crc includes chunk-type and chunk-data (but not length)

;; IHDR 73 72 68 82 first chunk with header

;; width ub4, height ub4, bit-depth=8, color-type=0,
;; compression-method=0, filter-method=0, interlace-method=0
 
;; IDAT 73 68 65 84 image

;; ZLIB
;; compression-method-and-flags=#b0111 1000=#x78, flags= #x9c ,
;; CM=8 bits 0 to 3
;; CINFO=base-2 log of lz77 window size - 8 
;; 2^(CINFO+8) is window size, e.g. CINFO=3 -> 2048, CINFO=7 -> 32768
;; CMF*256+FLG must be multiple of 31

;; FCHECK=11100 no preset dictionary, default algorithm

;; inside #0500 for n=5
;; followed by two's complement #xfaff

;; datablocks n*1byte, check-value ub4 (adler)

;; DEFLATE
;; header-bits (3) BFINAL1, BTYPE2=00

;; ignores all data until next byte boundary, then
;; LEN2,NLEN2, LEN*byte data

;; IEND 73 69 78 68 marks end of image
;; test file http://www.libpng.org/pub/png/PngSuite/basn0g08.png
;; http://www.chrfr.de/software/midp_png.html

(defun list->array (ls)
  (declare (values (simple-array (unsigned-byte 8) 1) &optional))
  (make-array (length ls) :element-type '(unsigned-byte 8)
	      :initial-contents ls))

(defun ub32->ub8 (b)
  (reverse
   (loop for i below 4 collect
	(ldb (byte 8 (* 8 i)) b))))
(defun ub16->ub8 (b)
  (reverse
   (loop for i below 2 collect
	(ldb (byte 8 (* 8 i)) b))))

(defun ihdr (width height &optional (bit-depth 8) (color-type 0)
	     (compression-method 0) (filter-method 0) (interlace-method 0))
  (declare (type (unsigned-byte 32) width height)
	   (type (unsigned-byte 8) bit-depth color-type compression-method
		 filter-method interlace-method)
	   (values (simple-array (unsigned-byte 8) 1) &optional))
  (let* ((data (append (ub32->ub8 width)
		       (ub32->ub8 height)
		       (list bit-depth color-type compression-method 
				      filter-method interlace-method)))
	 (signature (list 73 72 68 82)))
    (list->array (append (ub32->ub8 (length data))
			 signature
			 data
			 (ub32->ub8 (crc (list->array (append signature
							      data))))))))

(defun idat (zlib-data)
  (declare (type (simple-array (unsigned-byte 8) 1) zlib-data)
	   (values (simple-array (unsigned-byte 8) 1) &optional))
  (let* ((signature (list 73 68 65 84))
	 (dat (concatenate '(simple-array (unsigned-byte 8) 1) 
			   (list->array signature)
			   zlib-data)))
    (concatenate '(simple-array (unsigned-byte 8) 1)
		 (list->array (ub32->ub8 (length zlib-data)))
		 dat
		 (list->array (ub32->ub8 (crc dat))))))



(defun deflate (buf &optional (is-last-p nil))
  (concatenate '(simple-array (unsigned-byte 8) 1)
	       (list->array (append (list (if is-last-p 1 0))
				    (reverse (ub16->ub8 (length buf)))
				    (reverse (ub16->ub8 (+ (- (length buf)) 
							   -1)))))
	       buf))

(defun zlib (buf)
  (declare (type (simple-array (unsigned-byte 8) 1) buf)
	   (values (simple-array (unsigned-byte 8) 1) &optional))
  (let* ((blocksize 2048)
	 (cmf #x38) ;78
	 (nblocks (ceiling (length buf)
			   blocksize))
	 (result (make-array (+ 2 (* 5 nblocks) (length buf))
			     :element-type '(unsigned-byte 8)))
	 (result-i 0))
    (setf (aref result 0) cmf
	  (aref result 1) (mod (- 31 (mod (ash cmf 8) 31)) 31)
	  result-i 1)
    (dotimes (b nblocks)
      (let* ((pos (* b blocksize))
	     (a (deflate 
		    (subseq buf pos (min (length buf) 
					 (+ pos blocksize)))
		    (= b (1- nblocks)))))
	(dotimes (k (length a))
	  (setf (aref result (incf result-i))
		(aref a k)))))
    (concatenate '(simple-array (unsigned-byte 8) 1)
		 result
		 (list->array (ub32->ub8 (adler buf))))))

(defun iend ()
  (let ((signature (list 73 69 78 68)))
    (list->array (append (ub32->ub8 0) 
			 signature 
			 (ub32->ub8 (crc (list->array signature)))))))

(defun png (image-data)
  (declare (type (simple-array (unsigned-byte 8) 2) image-data))
  (destructuring-bind (h w) (array-dimensions image-data)
    (concatenate '(simple-array (unsigned-byte 8) 1)
		 (list->array '(137 80 78 71 13 10 26 10)) ;; signature
		 (ihdr (- w 1) h)
		 (idat (zlib (sb-ext:array-storage-vector image-data)))
		 (iend))))

(defun write-png (fn image-data)
  (declare (type (simple-array (unsigned-byte 8) 2) image-data))
  (with-open-file (s fn :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create
		     :element-type '(unsigned-byte 8))
   (write-sequence (png image-data) s)
   nil))

#+nil
(let* ((h 322)
       (w 300)
       (data (make-array (list h (+ 1 w)) 
			 :element-type '(unsigned-byte 8))))
  ;; write 0 infront of every line       4.1.3. IDAT Image data
  (dotimes (j h)
    (setf (aref data j 0) 0)
    (dotimes (i w)
      (setf (aref data j (+ 1 i)) (mod i (min 255 w)))))
  (write-png "/dev/shm/o.png" data))

