#.(require :serv)

(defpackage :run
  (:use :cl :png :serv :sb-bsd-sockets :sb-posix))
(in-package :run)

(let* ((index (get-answer (read-file "index.html")))
       (jquery (get-answer (read-file "jquery.js")
			    "text/javascript"))
       (canvas (get-answer (read-file "canvas.js")
			    "text/javascript"))
       (favicon (get-answer (read-file "favicon.ico")
			    "image/vnd.microsoft.icon"))
       (im-h 16)
       (im-w 16)
       (image (make-array (list im-h im-w 3) :element-type '(unsigned-byte 8))))
  (defparameter *q* (list canvas jquery index))
  (defun handle-connection (s)
    (let ((sm (socket-make-stream (socket-accept s)
				  :output t
				  :input t
				  :element-type 
				  :default ; bivalent stream for binary and text
					;'character
					;'(unsigned-byte 8)
					;:buffering :none
				  )))
      (let ((r (read-get-request sm)))
	(format t "read request for: '~a'~%" r) 
	;; 200 means Ok: request fullfilled, document follows
	(when-bind* ((slash (position #\/ r)))
	  (cond ((string= "/test.ppm" r)
		 (dotimes (j im-h)
		   (dotimes (i im-w)
		     (setf (aref image j i 0) (mod (+ i j) 255))))
		 (write-sequence (get-answer
				  (string->ub8
				   (format nil 
					   "P6~%~d ~d~%255~%" im-h im-w))
				  "image/x-portable-pixmap"
				  ;"image/ppm"
				  )
				 sm)
		 (write-sequence (sb-ext:array-storage-vector image) sm))
		((string= "/ajax.json" r)
		 (write-sequence (get-answer
				  (string->ub8
				   (format nil 
					   "{\"x\":~a,\"y\":~a,\"time\":~a}" (random 200)
					   (random 200) (get-internal-real-time)
					   ))
				  "application/json")
				 sm))
		((or (string= "/" r) (string= "/index.html" r))
		 (write-sequence index sm))
		((string= "/jquery.js" r) (write-sequence jquery sm))
		((string= "/canvas.js" r) (write-sequence canvas sm))
		((string= "/favicon.ico" r) (write-sequence favicon sm))
		(t (format sm "error"))))
	(force-output sm)
	(close sm)))))
;; unfortunately browsers don't display pgm files
;; rfc2083 PNG, rfc1951 deflate could be an alternative
;; it looks like i don't have to compress/modify data for png at all
;; i just have to write the right header
;; http://drj11.wordpress.com/2007/11/20/a-use-for-uncompressed-pngs/
;; http://gareth-rees.livejournal.com/9988.html Smallest possible transparent PNG

;; ideas for zooming into image
;; http://deepliquid.com/projects/Jcrop/demos.php
;; http://www.mind-projects.it/projects/jqzoom/demos.php#demo5
#+nil
(progn
  (defvar *keep-running* t)
  (sb-thread:make-thread (lambda ()
			   (defvar s (init-serv))
			   (loop while *keep-running* do
			      (handle-connection s))
			   (socket-close s))
			 :name "server"))
#+nil
(setf *keep-running* nil)