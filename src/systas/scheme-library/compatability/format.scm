;;; format.scm - Format a la Scheme 48
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998 Tom Lord
;;;
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;;



(define-module (compatability format)
  :use-module (data-structures string-fun))



;;s format port-spec format-string . arguments
;;
;; Prints the arguments to the port as directed by the string.  `port-spec'
;; should be either:
;;
;; 	An output port.  The output is written directly to the port.
;; 	The result of the call to `format' is undefined.
;;
;; 	#T.  The output is written to the current output port.  The
;; 	result of the call to `format' is undefined.
;;
;; 	#F.  The output is written to a string, which is then the
;; 	value returned from the call to `format'.
;;
;; Characters in `format-string' which are not preceded by a ~ are
;; written directly to the output.  Characters preceded by a ~ have
;; the following meaning (case is irrelevant; ~a and ~A have the same
;; meaning):
;;
;; 	~~ prints a single ~
;; 	~A prints the next argument using `display'
;; 	~D prints the next argument as a decimal number
;; 	~S prints the next argument using `write'
;; 	~% prints a newline character
;; 	~? performs a recursive call to `format' using the next two 
;;	   arguments as the string and the list of arguments
;;
;; This operator, in Scheme48, is not supported:
;;
;; 	~& prints a `newline' character if the previous printed character 
;;	   was not one (this is implemented using `fresh-line')
;;
(define-public (format port-spec format-string . arguments)
  (let ((port (case port-spec
		((#t)		(current-output-port))
		((#f)		(%% %make-string-port 'O_WRONLY))
		(else		port-spec)))

	(format-parts (separate-fields-matching-regexp "[^~]\\+\\|~."
						       format-string
						       list)))
    (let loop ((format-parts format-parts)
	       (arguments arguments))
      (if (not format-parts)
	  (if (eq? #f port-spec)
	      (let ((answer (string-port->string port)))
		(close-port port)
		answer))
	  (let ((f (car format-parts)))
	    (if (or (= 0 (string-length f))
		    (not (char=? #\~ (string-ref f))))
		(begin (display f port)
		       (loop (cdr format-parts) arguments))
		(cond
		 ((string=? "~~" f)			(display "~" port)
							(loop (cdr format-parts) arguments))
		 ((string-ci=? "~A" f)			(display (car arguments) port)
							(loop (cdr format-parts) (cdr arguments)))
		 ((string-ci=? "~D" f)			(if (not (number? (car arguments)))
							    (error 'number-expected-by-format
								   f (car arguments)))
							(write (car arguments) port)
							(loop (cdr format-parts) (cdr arguments)))
		 ((string-ci=? "~S" f)			(write (car arguments) port)
							(loop (cdr format-parts) (cdr arguments)))
		 ((string-ci=? "~%" f)			(newline port)
							(loop (cdr format-parts) arguments))
		 ((string-ci=? "~?" f)			(apply format
							       port (car arguments) (cadr arguments))
							(loop (cdr format-parts) (cddr arguments)))
		 (#t					(error 'unrecognized-format-directive
							       f)))))))))
