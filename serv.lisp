(require :asdf)
(require :sb-posix)
(require :sb-bsd-sockets)

(defpackage :serv
  (:use :cl :sb-bsd-sockets))
(in-package :serv)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (speed 0)))

(defun init-serv ()
  (let ((s (make-inet-socket :stream :tcp)))
    (setf (sockopt-reuse-address s) t)
    (socket-bind s (make-inet-address "127.0.0.1") 8080)
    (socket-listen s 5)
    s))

(defun read-get-request (sm)
  (loop for line = (read-line sm)
	while line
	do (let ((index (search "GET" line)))
	     (when index
	       (return-from read-get-request
		 (let ((start (+ index 1 (length "GET"))))
		   (subseq line
			 start
			 (search " " line :start2 start)))))))
  (error "no GET found in request"))

; parsing idioms taken from cl-opengl enum stuff

(defmacro when-bind ((var value) &body body)
  "Bind VAR to VALUE and evaluate BODY when VALUE is non-nil."
  `(let ((,var ,value))
    (when ,var
      ,@body)))

(defmacro when-bind* (bindings &body body)
  "Recursively apply WHEN-BIND to each binding in BINDINGS. Evaluate BODY when 
all bindings are non-nil."
  (if (null (cdr bindings))
      `(when-bind ,(car bindings)
        ,@body)
      `(when-bind ,(car bindings)
        (when-bind* ,(cdr bindings)
         ,@body))))


;; i copied that from CERN
;; webstatlib.lisp in cds-invenio-0.90.1.tar.gz
(defun string-substitute (string substring replacement-string)
  "Taken from c.l.l."
  (let ((substring-length (length substring))
        (last-end 0)
        (new-string ""))
    (do ((next-start
          (search substring string)
          (search substring string :start2 last-end)))
        ((null next-start)
         (concatenate 'string new-string (subseq string last-end)))
      (declare (type (or integer null) next-start))
      (setq new-string
            (concatenate 'string
                         new-string
                         (subseq string last-end next-start)
                         replacement-string))
      (setq last-end (+ next-start substring-length)))))

(defun string-decode-url (string)
  "Return string where URL is unquoted, that is %22 is substituted by
a double quote, etc."
  (setf string (string-substitute string "%20" " "))
  (setf string (string-substitute string "+" " "))
  (setf string (string-substitute string "%2B" "+"))
  (setf string (string-substitute string "%22" "\""))
  (setf string (string-substitute string "%23" "#"))
  (setf string (string-substitute string "%25" "%"))
  (setf string (string-substitute string "%26" "&"))
  (setf string (string-substitute string "%27" "'"))
  (setf string (string-substitute string "%28" "("))
  (setf string (string-substitute string "%29" ")"))
  (setf string (string-substitute string "%2A" "*"))
  (setf string (string-substitute string "%2C" ","))
  (setf string (string-substitute string "%2F" "/"))
  (setf string (string-substitute string "%3A" ":"))
  (setf string (string-substitute string "%3D" "="))
  (setf string (string-substitute string "%3E" ">"))
  (setf string (string-substitute string "%3F" "?"))
  (setf string (string-substitute string "%5B" "["))
  (setf string (string-substitute string "%5C" "\\"))
  (setf string (string-substitute string "%5D" "]")))

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
		  (cond ((string= "/" r) 
			 (format sm "HTTP/1.1 200 OK~%Content-type: text/html~%~%")
			  (format sm "<html><body bgcolor=\"#9791c0\"><img src=\"map.png\">")
			 (format sm "<p><b>~a</b></p>" (get-internal-real-time))
			 (format sm "<p>map data from open street map</p></body></html>")) 
			((string= r "/chat?satz=")
			 (format sm "HTTP/1.1 200 OK~%Content-type: text/html~%~%")
			 (format sm "nothing written")
			 )
			((string= "/map.png" r)
			  (format sm "HTTP/1.1 200 OK~%Caontent-type: image/png~%~%")
			)
			 (t (format sm "error"))))
      (force-output sm)
      (close sm))))

#+mo;
(progn
  (defvar s (init-serv))
  (loop
     (handle-connection s))
  (socket-close s))

 