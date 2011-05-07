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
  "Recursively apply WHEN-BIND to each binding in BINDINGS. Evaluate
BODY when all bindings are non-nil."
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
  (macrolet ((q ()
	       (let ((conv '(("%20" " ") ("+" " ")   ("%2B" "+") ("%22" "\"")
			     ("%23" "#") ("%25" "%") ("%26" "&") ("%27" "'")
			     ("%28" "(") ("%29" ")") ("%2A" "*") ("%2C" ",")
			     ("%2F" "/") ("%3A" ":") ("%3D" "=") ("%3E" ">")
			     ("%3F" "?") ("%5B" "[") ("%5C" "\\") ("%5D" "]"))))
		 `(progn
		    ,@(loop for (a b) in conv collect
			   `(setf string (string-substitute string ,a ,b)))))))
    (q)))

(defun read-file (fn)
 (with-open-file (s fn :direction :input :element-type '(unsigned-byte 8))
   (let ((buf (make-array (file-length s) :element-type '(unsigned-byte 8))))
     (read-sequence buf s)
     buf)))

(defun string->ub8 (s)
  (let* ((n (length s))
	 (r (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n)
      (setf (aref r i) (char-code (char s i))))
    r))

#+nil
(string->ub8 "ess")


(defun get-answer (str &optional (type "text/html"))
  (concatenate '(simple-array (unsigned-byte 8) 1)
	       (string->ub8 (format nil "HTTP/1.1 200 OK~%Content-type: ~a~%~%"
				    type))
	       str))


