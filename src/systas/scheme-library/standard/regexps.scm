;;; fast-sre.scm: regexp and structured regexp utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001, 2002 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (standard regexps))

;;; Note: the (standard char-set) library is not current supported
;;; by this sre syntax.
;;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Posix Basic Regular Expressions (BRE) With Rx Extensions
;;; 

;; (regexp-quote s)
;; 
;; Return a regexp (in rx-extended BRE syntax) which matches `s'
;; literally, not treating any characters in `s' as regexp operators.
;; 
(define-public (regexp-quote s)
  (apply string (map (lambda (c) (or (assq-ref regexp-names c) c)) (string->list s))))


(define regexp-names '((#\\ . "\\\\")
		       (#\. . "\\.")
		       (#\^ . "\\^")
		       (#\$ . "\\$")
		       (#\* . "\\*")
		       (#\[ . "\\[")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Structured Regular Expressions
;;; 
;;; 
;;; A structured regexp is a recursively defined list structure.
;;; The general form is:
;;;
;;;	structured-regexp := (<operator> <parameter> ...)
;;;	parameter	  := <integer>
;;;			  |  <character>
;;;			  |  <string>
;;;			  |  <keyword>
;;;			  |  <structured-regexp>
;;;
;;; The valid operators are:
;;;
;;;	operator	  := const	; a string constant
;;;			  |  any	; any character
;;;			  |  []		; character set
;;;			  |  [^]	; negated character set
;;;			  |  ^		; start anchor
;;;			  |  $		; end anchor
;;;			  |  ?		; optional sub-expression
;;;			  |  *		; repeated sub-expression
;;;			  |  +		; non-empty, repeated sub-expression
;;;			  |  {}		; a counted sub-expression
;;;			  |  =		; parenthesized subexpression
;;;			  |  &		; sub-expression concatenation
;;;			  |  |		; alternative sub-expressions
;;;			  |  @		; parenthesized subexpression back-reference
;;;			  |  /		; the "cut" operator
;;; 			  | !		; the symbolicly labeled "cut" operator
;;;
;;; As a short-hand, some structured regexps can be abbreviated:
;;;
;;; 	(const "string") == "string"
;;;	(* any)		 == *.
;;;	(^ ($ subexp))	 == (^$ subexp)
;;; 
;;; Each operator has its own syntax, so the precise syntax of a structured
;;; regexp is:
;;;
;;;	structured-regexp :=	(const <string>)
;;;			  |	([] <character-set-element> ...)
;;;			  |	([^] <character-set-element> ...)
;;;			  |	(^ <structured-regexp> ...)
;;;			  |	($ <structured-regexp> ...)
;;;			  |	(? <structured-regexp> ...)
;;;			  |	(* <structured-regexp> ...)
;;;			  |	(+ <structured-regexp> ...)
;;;			  |	({} <integer> <integer> <structured-regexp> ...)
;;;			  |	(& <structured-regexp> ...)
;;;			  |	(| <structured-regexp> ...)
;;;			  |	(= [<subexpression-label>] <structured-regexp> ...)
;;;			  |	(@ <subexpression-label>)
;;;			  |	(/ <integer>)
;;;			  |	(! [<cut-label>] <structured-regexp> ...)
;;;
;;;	character-set-element	:=	string
;;;				|	character
;;;				|	(character . character)	; a range of characters
;;;				|	<character-set> ; see the `(standard char-set-lib)' module
;;;
;;; 	subexpresion-label	:=	<keyword> ; (a keyword)
;;; 	cut-label		:=	<keyword> ; (a keyword)
;;;
;;; A `pick-spec' specifies values to be returned from `regexec' or a
;;; procedure returned by `regexec-function'.  It has the form:
;;; 
;;; 	pick-spec	:=	#f	; return #t if a match is found, #f otherwise
;;; 
;;;			|	#t	; return #f or a list `(before match after)'
;;; 					; that is the partition of the string implied
;;;					; by a successful match
;;; 
;;; 			|	<recursive-pick-spec>
;;; 
;;; 
;;; A `recursive-pick-spec' is:
;;; 
;;; 	recursive-pick-spec :=	<rps-elt>	; return only the value implied by `rps-elt'
;;; 			    |	(<rps-elt> ...) ; return a list of values implied by 
;;; 						; the list of `rps-elt's.
;;;
;;; An `rps-elt' is:
;;; 
;;; 	rps-elt		:=	<part>	; return the indicated part of the string
;;;					; (see below)
;;; 
;;; 			|	(<start-point> <end-point>) ; return the substring starting
;;;					; at  `<start-point>' and ending immediately
;;;					; before `<end-point>' (see below)
;;; 
;;; 
;;; 			|	state-label ; return the state label of the DFA ending
;;;					; state.  If the match terminated at a `cut'
;;;					; operator (`/' in sre notation), this is
;;; 					; the integer argument to that operator.
;;; 
;;; 			|	?	; the keyword of the terminating cut label or #f
;;; 
;;; 			|	<keyword> ; return the keyword literally.  This is useful
;;;					; for labeling elements in a `recursive-pick-spec'
;;;					; which is a list.
;;; 
;;; A `part' indicates the entire match, a parenthesized
;;; subexpression, or the substring that preceeds a match, or the
;;; substring that follows a match:
;;; 
;;; 	part		:=	0	; the entire match
;;; 
;;; 			|	<n>	; (an integer) the `nth' parenthesized subexpression
;;; 
;;; 			|	(@ <keyword>) ; the subexpression labeled by `<keyword>'
;;;
;;; 			|	<	; (the symbol '<') the substring preceeding the match
;;; 
;;; 			|	>	; (the symbol '>') the substring following the match
;;; 
;;; A `point' indicates a specific position within the string.  There
;;; are two kinds of `point': a `start-point' and and `end-point' that together
;;; specify a substring of the string:
;;; 
;;; 	start-point	:=	<part>		; the beginning of the indicated match part.
;;; 			| 	<any-point> 	; (see below)
;;; 
;;; 	end-point	:=	<part>		; the end of the indicated match part.
;;; 			| 	<any-point> 	; (see below)
;;; 
;;; 	any-point	:=	(<part> 0)	; the beginning of the indicated match part
;;;			|	(<part> 1)	; the end of the indicated match part
;;; 
;;;
;;; An example pick spec that returns a list of substrings of the original string:
;;; 
;;; 	(0		; the entire match
;;; 
;;;	 (< 0)		; from the start of the string to the end of the match
;;; 
;;;	 (2 >)		; from the start of subexpression 2 to the end of the string
;;; 
;;;	 (@ :username)	; the subexpression labeled `:username'
;;; 
;;; 	 ((@ :username)	  ; from the start of the subexpression labeled `:username'
;;;	  (@ :directory)) ; ... to the end of the subexpression labeled `:directory'
;;; 			  
;;;	 ((2 1) 		; from the end of subexpression 2 
;;; 	  ((@ :directory) 0)))  ; ... to the beginning of the subexpression labeled :directory
;;;			       
;;; 	


;; (structured-regexp->procedure sre . kws)
;; 
;; Return a function which compares strings to the regexp specified by
;; sre (a structured regexp).
;; 
;; `sre' is a structured regexp.
;; 
;; Keyword arguments: 
;; 
;; 	:cflags cflags		 #f or flags to `regcomp'.
;; 
;; 	:eflags eflags		 #f or flags to `regexec'.
;; 
;; 	:pick-spec pick-spec	 an SRE pick spec.
;; 
(define-public (structured-regexp->procedure sre . kws)
  (let ((cflags 	(kw-arg-ref kws :cflags))
	(eflags		(kw-arg-ref kws :eflags))
	(pick-spec	(kw-arg-ref kws :pick-spec)))
    (compile-regexp sre
		    (lambda (pattern n-subexps subexp-labels n-cuts cut-labels)
		      (let ((regexp 	(regcomp pattern cflags))
			    (pick 	(compile-match-pick pick-spec subexp-labels cut-labels)))
			(lambda (string :optional return error-return)
			  (regexec regexp string pick eflags return error-return)))))))


;; (compile-match-pick labels subexp-labels cut-labels)
;; 
;; Compile an SRE `pick-spec' to a `pick-spec' suitable for use with
;; `regexec'.
;; 
(define (compile-match-pick pick-spec subexp-labels cut-labels)
  (let ((cl	(apply vector 'cut cut-labels)))
    (let compile-match-pick ((pick-spec pick-spec))
      (cond
       ((null? pick-spec)			())
       ((eq? '? pick-spec)			cl)
       ((not (pair? pick-spec))			pick-spec)
       ((eq? (car pick-spec) '@)		(kw-arg-ref subexp-labels (cadr pick-spec)))
       (#t					(cons (compile-match-pick (car pick-spec))
						      (compile-match-pick (cdr pick-spec))))))))


;; (compile-regexp sre return)
;; 
;; Compile the structured regular expression `sre' and invoke `return':
;; 
;; 	(return string n-subexpressions subexpression-labels)
;; 
;; where:
;; 
;; `string' is the regexp compiled to (rx extended) "basic regular
;; expression (bre)" notation.
;; 
;; `n-subexpressions' is the number of parenthesized subexpressions.
;; 
;; `subexpression-labels' is a keyword list:
;; 
;; 	(<keyword> <n> <keyword> <n> ...)
;; 
;; in which each subexpression label (a keyword) is followed by the
;; corresponding subexpression number.
;; 
;; This is a comparatively low-level function.  Consider using
;; `structured-regexp->procedure' instead.
;; 
(define-public (compile-regexp sre return)

  ;; This procedure tail recurses with additional arguments:
  ;; 
  (define (compile-regexp
	   sre return
	   preceeding-n-subexpressions preceeding-subexpression-labels
	   preceeding-n-cut-labels preceeding-cut-labels)
			  

    (define (length-check n list)
      (if (not (= n (length list)))
	  (error "bad structured regexp syntax" list)))

    (define (implicit-& sre :optional permits-label?)
      (let* ((op	(car sre))
	     (label	(and permits-label? (cdr sre) (keyword? (cadr sre)) (cadr sre)))
	     (args	(if label (cddr sre) (cdr sre))))
	(if (not args)
	    (error "bad structured regexp syntax" sre))
	(if label
	    `(,op ,label (& ,@args))
	    `(,op (& ,@args)))))

    (cond
     ((symbol? sre)	(set! sre `(,sre)))
     ((string? sre)	(set! sre `(const ,sre))))

    (case (car sre)
      ((const)		(length-check 2 sre)
			(return (regexp-quote (cadr sre))
				preceeding-n-subexpressions preceeding-subexpression-labels
				preceeding-n-cut-labels preceeding-cut-labels))
      ((any)		(length-check 1 sre)
			(return "."
				preceeding-n-subexpressions preceeding-subexpression-labels
				preceeding-n-cut-labels preceeding-cut-labels))
      (([])		(return (regexp-cset (cdr sre))
				preceeding-n-subexpressions preceeding-subexpression-labels
				preceeding-n-cut-labels preceeding-cut-labels))
      (([^])		(return (regexp-cset (cdr sre) :negate)
				preceeding-n-subexpressions preceeding-subexpression-labels
				preceeding-n-cut-labels preceeding-cut-labels))
      ((^$)		(compile-regexp `(^ ($ ,@(cdr sre)))
					return
					preceeding-n-subexpressions
					preceeding-subexpression-labels
					preceeding-n-cut-labels
					preceeding-cut-labels))
      ((^)		(set! sre (implicit-& sre))
			(compile-regexp (cadr sre)
					(lambda (subexp-string
						 subexp-n-subexpressions subexp-subexpression-labels
						 subexp-n-cut-labels subexp-cut-labels)
					  (return (string-append "[[:(^[[:(" subexp-string "):]]):]]")
						  subexp-n-subexpressions
						  subexp-subexpression-labels
						  subexp-n-cut-labels
						  subexp-cut-labels))
					preceeding-n-subexpressions
					preceeding-subexpression-labels
					preceeding-n-cut-labels
					preceeding-cut-labels))
      (($)		(set! sre (implicit-& sre))
			(compile-regexp (cadr sre)
					(lambda (subexp-string
						 subexp-n-subexpressions subexp-subexpression-labels
						 subexp-n-cut-labels subexp-cut-labels)
					  (return (string-append "[[:([[:(" subexp-string "):]]$):]]")
						  subexp-n-subexpressions
						  subexp-subexpression-labels
						  subexp-n-cut-labels
						  subexp-cut-labels))
					preceeding-n-subexpressions
					preceeding-subexpression-labels
					preceeding-n-cut-labels
					preceeding-cut-labels))
      ((?)		(set! sre (implicit-& sre))
			(compile-regexp (cadr sre)
					(lambda (subexp-string
						 subexp-n-subexpressions subexp-subexpression-labels
						 subexp-n-cut-labels subexp-cut-labels)
					  (return (string-append "[[:(" subexp-string "):]]\\?")
						  subexp-n-subexpressions
						  subexp-subexpression-labels
						  subexp-n-cut-labels
						  subexp-cut-labels))
					preceeding-n-subexpressions
					preceeding-subexpression-labels
					preceeding-n-cut-labels
					preceeding-cut-labels))
      ((*.)		(length-check 1 sre)
			(compile-regexp `(* any)
					return
					preceeding-n-subexpressions
					preceeding-subexpression-labels
					preceeding-n-subexpressions
					preceeding-cut-labels))
      ((*)		(set! sre (implicit-& sre))
			(compile-regexp (cadr sre)
					(lambda (subexp-string
						 subexp-n-subexpressions subexp-subexpression-labels
						 subexp-n-cut-labels subexp-cut-labels)
					  (return (string-append "[[:(" subexp-string "):]]*")
						  subexp-n-subexpressions
						  subexp-subexpression-labels
						  subexp-n-cut-labels
						  subexp-cut-labels))
					preceeding-n-subexpressions
					preceeding-subexpression-labels
					preceeding-n-cut-labels
					preceeding-cut-labels))
      ((+)		(set! sre (implicit-& sre))
			(compile-regexp (cadr sre)
					(lambda (subexp-string
						 subexp-n-subexpressions subexp-subexpression-labels
						 subexp-n-cut-labels subexp-cut-labels)
					  (return (string-append "[[:(" subexp-string "):]]\\+")
						  subexp-n-subexpressions
						  subexp-subexpression-labels
						  subexp-n-cut-labels
						  subexp-cut-labels))
					preceeding-n-subexpressions
					preceeding-subexpression-labels
					preceeding-n-cut-labels
					preceeding-cut-labels))
      (({})		(if (> 4 (length sre))
			    (error "bad structured regexp syntax" sre))
			(let ((low (cadr sre))
			      (high (caddr sre))
			      (subexps (cdddr sre)))
			  (compile-regexp `(& ,@subexps)
					  (lambda (subexp-string
						   subexp-n-subexpressions subexp-subexpression-labels
						   subexp-n-cut-labels subexp-cut-labels)
					    (return (string-append "[[:(" subexp-string "):]]\\{" (number->string low) "," (number->string high) "\\}")
						    subexp-n-subexpressions
						    subexp-subexpression-labels
						    subexp-n-cut-labels
						    subexp-cut-labels))
					  preceeding-n-subexpressions
					  preceeding-subexpression-labels
					  preceeding-n-cut-labels
					  preceeding-cut-labels)))
      ((&)		(case (length sre)
			  ((1)			(return ""
							preceeding-n-subexpressions preceeding-subexpression-labels
							preceeding-n-cut-labels preceeding-cut-labels))
			  ((2)			(compile-regexp (cadr sre) return
								preceeding-n-subexpressions preceeding-subexpression-labels
								preceeding-n-cut-labels preceeding-cut-labels))
			  ((3)			(compile-regexp
						 (cadr sre)
						 (lambda (left-string
							  left-n-subexpressions left-subexpression-labels
							  left-n-cut-labels left-cut-labels)
						   (compile-regexp
						    (caddr sre)
						    (lambda (right-string
							     right-n-subexpressions right-subexpression-labels
							     right-n-cut-labels right-cut-labels)
						      (return (string-append "[[:(" left-string "):]][[:(" right-string "):]]")
							      right-n-subexpressions
							      right-subexpression-labels
							      right-n-cut-labels
							      right-cut-labels))
						    left-n-subexpressions
						    left-subexpression-labels
						    left-n-cut-labels
						    left-cut-labels))
						 preceeding-n-subexpressions
						 preceeding-subexpression-labels
						 preceeding-n-cut-labels
						 preceeding-cut-labels))
			  (else			(compile-regexp `(& ,(cadr sre) (& ,@(cddr sre)))
								return
								preceeding-n-subexpressions
								preceeding-subexpression-labels
								preceeding-n-cut-labels
								preceeding-cut-labels))))
      ((|)		(case (length sre)
			  ((1)			(return ""
							preceeding-n-subexpressions preceeding-subexpression-labels
							preceeding-n-cut-labels preceeding-cut-labels))
			  ((2)			(compile-regexp `(| ,(cadr sre) "") return
								preceeding-n-subexpressions preceeding-subexpression-labels
								preceeding-n-cut-labels preceeding-cut-labels))
			  ((3)			(compile-regexp
						 (cadr sre)
						 (lambda (left-string
							  left-n-subexpressions left-subexpression-labels
							  left-n-cut-labels left-cut-labels)
						   (compile-regexp
						    (caddr sre)
						    (lambda (right-string
							     right-n-subexpressions right-subexpression-labels
							     right-n-cut-labels right-cut-labels)
						      (return (string-append "[[:(" left-string "):]]\\|[[:(" right-string "):]]")
							      right-n-subexpressions
							      right-subexpression-labels
							      right-n-cut-labels
							      right-cut-labels))
						    left-n-subexpressions
						    left-subexpression-labels
						    left-n-cut-labels
						    left-cut-labels))
						 preceeding-n-subexpressions
						 preceeding-subexpression-labels
						 preceeding-n-cut-labels
						 preceeding-cut-labels))
			  (else			(compile-regexp `(| ,(cadr sre) (| ,@(cddr sre)))
								return
								preceeding-n-subexpressions
								preceeding-subexpression-labels
								preceeding-n-cut-labels
								preceeding-cut-labels))))
      ((=)		(set! sre (implicit-& sre #t))
			(let* ((params (cdr sre))
			       (label (and (= 2 (length params))
					   (car params)))
			       (subexp (car (last-pair params))))
			  (if (and label (not (keyword? label)))
			      (error "label is not a keyword" label))
			  (compile-regexp subexp
					  (lambda (subexp-string
						   subexp-n-subexpressions subexp-subexpression-labels
						   subexp-n-cut-labels subexp-cut-labels)
					    (return (string-append "\\(" subexp-string "\\)")
						    subexp-n-subexpressions
						    subexp-subexpression-labels
						    subexp-n-cut-labels
						    subexp-cut-labels))
					  (+ 1 (or preceeding-n-subexpressions 0))
					  (append (and label (list label (+ 1 preceeding-n-subexpressions)))
						  preceeding-subexpression-labels)
					  preceeding-n-subexpressions
					  preceeding-cut-labels)))

      ((!)		(set! sre (implicit-& sre #t))
			(let* ((params (cdr sre))
			       (label (and (= 2 (length params))
					   (car params)))
			       (subexp (car (last-pair params))))
			  (if (and label (not (keyword? label)))
			      (error "label is not a keyword" label))
			  (compile-regexp subexp
					  (lambda (subexp-string
						   subexp-n-subexpressions subexp-subexpression-labels
						   subexp-n-cut-labels subexp-cut-labels)
					    (return (string-append "[[:([[:(" subexp-string "):]][[:cut " (number->string (1+ preceeding-n-cut-labels)) ":]]):]]")
						    subexp-n-subexpressions
						    subexp-subexpression-labels
						    subexp-n-cut-labels
						    subexp-cut-labels))
					  preceeding-n-subexpressions
					  preceeding-subexpression-labels
					  (1+ preceeding-n-cut-labels)
					  (append preceeding-cut-labels (list label)))))

      ((@)		(length-check 2 sre)
			(let* ((label (cadr sre))
			       (position (kw-arg-ref preceeding-subexpression-labels label)))
			  (if (or (not position)
				  (< position 0)
				  (> position 9))
			      (throw 'too-many-subexps-for-backreference label))
			  (return (string-append "\\" (->string position))
				  preceeding-n-subexpressions preceeding-subexpression-labels
				  preceeding-n-cut-labels preceeding-cut-labels)))
      ((/)		(length-check 2 sre)
			(return (string-append "[[:cut " (number->string (cadr sre)) ":]]")
				preceeding-n-subexpressions preceeding-subexpression-labels
				preceeding-n-cut-labels preceeding-cut-labels))

      (else		(throw 'unrecognized-structured-regexp sre))))

  (compile-regexp sre return 0 '() 1 (list #t)))


;; (regexp-cset elts :optional negate?)
;; 
;; Compile an SRE character set specification to an rx-extended BRE
;; character set.
;; 
(define (regexp-cset elts :optional negate?)
  (let ((specials (apply append (map (lambda (elt)
				       (cond
					((not (string? elt))	'())
					((eq? elt #\-)		(list elt))
					((eq? elt #\])		(list elt))
					((pair? elt)		(let* ((from (car elt))
								       (to (cadr elt))
								       (dash? (or (eq? from #\-) (eq? to #\-)))
								       (bracket? (or (eq? from #\]) (eq? to #\]))))
								  (cond
								   ((and dash? bracket?)	'(#\- #\]))
								   (dash?			'(#\-))
								   (bracket?			'(#\]))
								   (#t				()))))
					((string? elt)		(let* ((dash? (string-index elt #\-))
								       (bracket? (string-index elt #\])))
								  (cond
								   ((and dash? bracket?)	'(#\- #\]))
								   (dash?			'(#\-))
								   (bracket?			'(#\]))
								   (#t				()))))
					(#t			())))
				     elts))))
    (string-append "["
		   (if negate?
		       "^"
		       "")
		   (if (memq #\] specials)
		       "]"
		       "")
		   (apply string
			  (map (lambda (elt)
				 (cond
				  ((symbol? elt)		(string-append "[:" elt ":]"))
				  ((eq? elt #\-)		"")
				  ((eq? elt #\[)		"")
				  ((char? elt)			elt)
				  ((string? elt)		(apply string
								       (map (lambda (c)
									      (if (eq? #\\ c)
										  "\\\\"
										  c))
									    (delq! #\- (delq #\] (string->list elt))))))
				  ((pair? elt)			(string-append
								 (case (car elt)
								   ((#\-)		".")
								   ((#\])		"\\\\")
								   (else		(car elt)))
								 "-"
								 (case (cadr elt)
								   ((#\-)		",")
								   ((#\])		"[")
								   (else		(cadr elt)))))
				  (#t				(throw 'unrecognized-character-set-element elt))))
			       elts))
		   (if (memq #\- specials)
		       "-"
		       "")
		   "]")))

;;; tag: Tom Lord Sat Apr 20 19:56:21 2002 (standard/regexps.scm)
;;;
