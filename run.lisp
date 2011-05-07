#.(require :serv)

(defpackage :run
  (:use :cl :png :serv :sb-bsd-sockets))
(in-package :run)

(let* ((index (get-answer (read-file "index.html")))
       (jquery (get-answer (read-file "jquery.js")
			    "text/javascript"))
       (canvas (get-answer (read-file "canvas.js")
			    "text/javascript"))
       (favicon (get-answer (read-file "favicon.ico")
			    "image/vnd.microsoft.icon"))
       (im-h 200)
       (im-w 310)
       (image (make-array (list im-h im-w) :element-type '(unsigned-byte 8))))
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
	  (cond ((string= "/test.png" r)
		 (dotimes (j im-h)
		   (dotimes (i im-w)
		     (setf (aref image j i) (mod i 255))))
		 (write-sequence (get-answer (png image)
					     "image/png")
				 sm))
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