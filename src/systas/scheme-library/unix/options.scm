;;; options.scm - scheme command-line option parsing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (unix options)
  :use-module (standard string-parsing))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command-line argument editting
;;
;; These procedures edit a command-line argument list to remove
;; an option that has just been processed.
;;

;; (option-shift args)
;; 
;; Remove a single argument from a command-line argument list.  
;; For example:
;; 
;;	(option-shift '("prog" "--abc" "-def"))
;;	=> ("prog" "-def")
;;
;;	(option-shift '("prog"))
;;	=> ("prog")
;;
(define-public (option-shift args)
  (if (not (cdr args))
      args
      (cons (car args) (cddr args))))


;; (option-shift-char args)
;; 
;; Remove a single character argument from a command-line argument list.
;; For example:
;;
;; 	(option-shift-char '("prog" "-abc" "-def"))
;;	=> ("prog" "-bc" "-def")
;;
;; 	(option-shift-char '("prog" "-a" "-def"))
;;	=> ("prog" "-def")
;;
;; 	(option-shift-char '("prog"))
;;	=> ("prog")
;;
;; 	(option-shift-char '("prog" "foo"))
;;	=> ("prog" "foo")
;;
(define-public (option-shift-char args)
  (cond
   ((or (not (cdr args))
	(not (char=? #\- (string-ref (cadr args)))))		args)
   ((= 2 (string-length (cadr args)))				(option-shift args))
   (#t								`(,(car args)
								  ,(string-append "-" (make-shared-substring (cadr args) 2))
								  ,@(cddr args)))))


;; (next-option options-desc args)
;;
;; Parse the next option in `args' according to `options-desc'.
;;
;; Return four values:
;;
;;		option-identifier
;;		option
;;		option-argument
;;		remaining-args
;; 
;; The option identifer is one of the option identifiers from
;; `options-desc' or a special identifier as described below.
;;
;; `options-desc' is a list of the form:
;; 
;;	((identifier . parameters) ...)
;; 
;; Each option is given a unique (according to `eqv?') identifier and
;; is described by a collection of optional keyword paramters.  Valid
;; keywords are:
;;
;;	:char c			The option can be specified "-c".
;;				`c' may be of one of the forms:
;;					#\c
;;					"c"
;;					"carg"
;;					"c arg"
;;				The latter two forms provide documentation
;;				(in the message generated by `options-help')
;;				that the option requires an argument.
;;
;;	:long-name name		The option can be specified "--name".
;;				`name' can have a suffix of the form
;;				"=foo" as in:
;;					:long-name "opt=foo"
;;				In that case, the long-name of the
;;				option is the text to the left of the "=",
;;				by the complete text is used when formatting
;;				a help message.
;;
;;	:requires-argument	The option requires an argument of any 
;;				of the forms:
;;					"-cargument"
;;					"-c" "argument"
;;				        "--name=argument" 
;;				        "--name" "argument"
;;
;;	:documentation		One line of text describing the option.
;;
;;
;; To obtain help messages that fit on an 80 character display,
;; documentation and long-option names should be kept short.
;; To provide more than one line of documentation for an option
;; or to include a blank line in the help message, use pseudo-options
;; with no :char or :long-name keyword as in this example:
;;
;;	( ...
;;	 (output-file :char "o file"
;;		      :long-name "output-file=file"
;;		      :documentation "The output file.  This file must")
;;	 (output-file :documentation "not already exist.")
;;	 (blank-line)
;;	 ...
;;	)
;;
;;
;; Some option identifiers are special should not occur in an options 
;; description, though they may be returned by `next-option':
;;
;;	'-			The next argument is simply "-"
;;	'--			The next argument is simply "--"
;;	'unrecognized		The next option is not recognized
;;	#f			The next argument is not an option.
;;
;; When `return' is called, it is passed both the option identifier and
;; any argument that was provided with the option.  It is up to the caller
;; to detect missing or superfluous option arguments.
;; 
(define-public (next-option options-desc args)
  (if (null? (cdr args))
      (values #f #f #f args)
      (let* ((arg (cadr args)))
	(cond
	 ((not (char=? #\- (string-ref arg)))		(values #f #f #f args))
	 ((string=? "-" arg)				(values '- arg #f (option-shift args)))
	 ((string=? "--" arg)				(values '-- arg #f (option-shift args)))
	 ((string-prefix=? "--" arg)			(next-long-option options-desc args))
	 (#t						(next-char-option options-desc args))))))


(define (next-long-option options-desc args)
  (let* ((arg (cadr args))
	 (sans-dashes (make-shared-substring arg 2))
	 (option-name (split-before-char #\= sans-dashes first-value))
	 (eq-argument (split-after-char #\= sans-dashes second-value))
	 (matching-desc (option-desc-long-ref options-desc option-name)))
    (cond
     ((not matching-desc)				(values 'unrecognized arg #f args))
     ((option-requires-argument? matching-desc)		(cond
							 (eq-argument		(values (option-identifier matching-desc)
											arg
											eq-argument
											(option-shift args)))
							 ((null? (cddr args))	(values (option-identifier matching-desc)
											arg
											#f
											(option-shift args)))
							 (#t			(values (option-identifier matching-desc)
											arg
											(caddr args)
											(option-shift (option-shift args))))))
     (#t						(values (option-identifier matching-desc)
								arg
								eq-argument
								(option-shift args))))))


(define (next-char-option options-desc args)
  (let* ((arg (cadr args))
	 (sans-dash (make-shared-substring arg 1))
	 (option-name (string-ref sans-dash))
	 (same-arg-argument (and (< 1 (string-length sans-dash)) (make-shared-substring sans-dash 1)))
	 (matching-desc (option-desc-char-ref options-desc option-name)))
    (cond
     ((not matching-desc)				(values 'unrecognized
								arg
								#f
								args))
     ((option-requires-argument? matching-desc)		(cond
							 (same-arg-argument	(values (option-identifier matching-desc)
											arg
											same-arg-argument
											(option-shift args)))
							 ((null? (cddr args))	(values (option-identifier matching-desc)
											arg
											#f
											(option-shift args)))
							 (#t			(values (option-identifier matching-desc)
											arg
											(caddr args)
											(option-shift (option-shift args))))))
     (#t						(values (option-identifier matching-desc)
								arg
								#f
								(option-shift-char args))))))



;; (options-help options-desc)
;; 
;; Return a help message for the options described by `options-desc'.
;; 
(define-public (options-help options-desc)
  (let* ((columns (map (lambda (desc)
			 (list (or (and=> (option-char-name-spec desc) (lambda (s) (string-append "-" s)))
				   "")
			       (or (and=> (option-long-name-spec desc) (lambda (s) (string-append "--" s)))
				   "")
			       (or (option-documentation desc) "")))
		       options-desc))
	 (column0-width (+ 2 (apply max (map (lambda (d) (string-length (car d))) columns))))
	 (column1-width (+ 2 (apply max (map (lambda (d) (string-length (cadr d))) columns)))))
    (apply string-append
	   (map (lambda (line)
		  (apply-to-args line
		    (lambda (c l d)
		      (string-append (pad-to-length column0-width c)
				     (pad-to-length column1-width l)
				     d
				     "\n"))))
		columns))))



(define (pad-to-length n s)
  (string-append s (make-string (max 0 (- n (string-length s))) #\space)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Option Description Parsing
;;


(define (option-identifier spec) (car spec))
(define (option-requires-argument? spec) (memq :requires-argument spec))
(define (option-documentation spec) (kw-arg-ref spec :documentation))
(define (option-char-name spec) (and=> (option-char-name-spec spec)
				       (lambda (s)
					 (if (char? s)
					     s
					     (string-ref s)))))
(define (option-long-name spec) (and=> (option-long-name-spec spec)
				       (lambda (long-name) (split-before-char #\= long-name first-value))))
(define (option-char-name-spec spec) (kw-arg-ref spec :char))
(define (option-long-name-spec spec) (kw-arg-ref spec :long-name))

;; (option-desc-long-ref desc long-name)
;; 
;; Return the clause of options description that pertains to
;; the named option.
;; 
(define (option-desc-long-ref desc long-name)
  (let loop ((desc desc))
    (and desc
	 (let ((this-long-name (option-long-name (car desc))))
	   (if (and this-long-name
		    (string-prefix=? long-name this-long-name)
		    (or (= (string-length long-name) (string-length this-long-name))
			(let ((c (string-ref this-long-name (string-length long-name))))
			  (case c
			    ((#\space #\tab #\=)		#t)
			    (else				#f)))))
	       (car desc)
	       (loop (cdr desc)))))))


;; (option-desc-char-ref desc char-name)
;; 
;; Return the clause of options description that pertains to
;; the named option.
;; 
(define (option-desc-char-ref desc char)
  (let loop ((desc desc))
    (and desc
	 (let ((this-char (option-char-name (car desc))))
	   (if (and this-char (char=? this-char char))
	       (car desc)
	       (loop (cdr desc)))))))
      



(define sample
  '((input-file 	:char "i file"
			:long-name "input=file"
			:requires-argument
			:documentation "Read input from FILE.")
    (output-file	:char "o file"
			:long-name "output=file"
			:requires-argument
			:documentation "Write output to FILE.")
    (quiet		:char #\q
			:long-name "quiet"
			:documentation "Avoid unecessary output.")
    (quiet		:documentation "Truncate error messages.")
    (blank-line)
    (x			:char #\x
			:documentation "Peform mysterious operation.")))

