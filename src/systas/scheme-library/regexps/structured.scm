;;; structured.scm - structured regexps
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



;;; This isn't quite right.
;;; 
;;; Olin's is closer.
;;; 
;;; Use this module only if you don't mind rewriting its uses later,
;;; when Olin's SRE system is adapted for use here.
;;; 




(define-module (regexps structured)
  :use-module (data-structures ratlist))


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
;;; As a short-hand, a structured regexp with the operator `const' can
;;; be abbreviated:
;;;
;;; 	(const "string") == "string"
;;;
;;; The valid operators are:
;;;
;;;	operator	  := const	; a string constant
;;;			  |  []		; character set
;;;			  |  [^]	; negated character set
;;;			  |  ^		; start anchor
;;;			  |  $		; end anchor
;;;			  |  ?		; optional sub-expression
;;;			  |  *		; repeated sub-expression
;;;			  |  +		; non-empty, repeated sub-expression
;;;			  |  {}		; a counted sub-expression
;;;			  |  &		; sub-expression concatenation
;;;			  |  |		; alternative sub-expressions
;;;			  |  =		; parenthesized subexpression
;;;			  |  @		; parenthesized subexpression back-reference
;;;			  |  /		; the "cut" operator
;;;
;;; Each operator has its own syntax, so the precise syntax of a structured
;;; regexp is:
;;;
;;;	structured-regexp :=	(const <string>)
;;;			  |	([] <character-set-element> ...)
;;;			  |	([^] <character-set-element> ...)
;;;			  |	(^)
;;;			  |	($)
;;;			  |	(? <structured-regexp>)
;;;			  |	(* <structured-regexp>)
;;;			  |	(+ <structured-regexp>)
;;;			  |	({} <integer> <integer> <structured-regexp>)
;;;			  |	(& <structured-regexp> <structured-regexp>)
;;;			  |	(| <structured-regexp> <structured-regexp>)
;;;			  |	(= [<subexpression-label>] <structured-regexp>)
;;;			  |	(@ <subexpression-label>)
;;;			  |	(/ <integer>)
;;;
;;;	character-set-element	:=	string
;;;				|	character
;;;				|	(character . character)		; a range of characters
;;;
;;;
;;;



(define-public (regexec-function sre . rest)
  (let*	((cflags 		(and rest (car rest)))
	 (rest			(and rest (cdr rest)))
	 (eflags		(and rest (car rest)))
	 (rest			(and rest (cdr rest)))
	 (pick-provided?	rest)
	 (pick			(and pick-provided? (car rest))))
    (compile-regexp sre
		    (lambda (pattern n-subexps subexp-labels)
		      (let ((regexp (regcomp pattern cflags)))
			(if pick-provided?
			    (let ((default-pick (compile-match-pick pick subexp-labels)))
			      default-pick
			      (lambda (string :optional return error-return)
				(regexec regexp string default-pick eflags return error-return)))
			    (lambda (string :optional pick return error-return)
			      (let ((match-pick (compile-match-pick pick subexp-labels)))
				(regexec regexp string match-pick eflags return error-return)))))))))

(define (compile-match-pick labels subexp-labels)
  (cond
   ((null? labels)			())
   ((not (pair? labels))		labels)
   ((eq? (car labels) '@)		(kw-arg-ref subexp-labels (cadr labels)))
   (#t					(cons (compile-match-pick (car labels) subexp-labels)
					      (compile-match-pick (cdr labels) subexp-labels)))))

;; (return <string> <n-subexpressions> <subexpression-labels>)
;; 
(define-public (compile-regexp sre return :optional preceeding-n-subexpressions preceeding-subexpression-labels)
  (define (length-check n list)
    (if (not (= n (length list)))
	(throw 'bad-regexp-syntax list)))

  (if (string? sre)
      (set! sre `(const ,sre)))
  (case (car sre)
    ((const)		(length-check 2 sre)
			(return (regexp-quote (cadr sre)) preceeding-n-subexpressions preceeding-subexpression-labels))
    (([])		(return (regexp-cset (cdr sre)) preceeding-n-subexpressions preceeding-subexpression-labels))
    (([^])		(return (regexp-cset (cdr sre) :negate) preceeding-n-subexpressions preceeding-subexpression-labels))
    ((^)		(length-check 1 sre)
			(return "^" preceeding-n-subexpressions preceeding-subexpression-labels))
    (($)		(length-check 1 sre)
			(return "$" preceeding-n-subexpressions preceeding-subexpression-labels))
    ((?)		(length-check 2 sre)
			(compile-regexp (cadr sre)
					(lambda (subexp-string subexp-n-subexpressions subexp-subexpression-labels)
					  (return (string-append "[[:(" subexp-string "):]]\\?")
						  subexp-n-subexpressions
						  subexp-subexpression-labels))
					preceeding-n-subexpressions
					preceeding-subexpression-labels))
    ((*)		(length-check 2 sre)
			(compile-regexp (cadr sre)
					(lambda (subexp-string subexp-n-subexpressions subexp-subexpression-labels)
					  (return (string-append "[[:(" subexp-string "):]]*")
						  subexp-n-subexpressions
						  subexp-subexpression-labels))
					preceeding-n-subexpressions
					preceeding-subexpression-labels))
    ((+)		(length-check 2 sre)
			(compile-regexp (cadr sre)
					(lambda (subexp-string subexp-n-subexpressions subexp-subexpression-labels)
					  (return (string-append "[[:(" subexp-string "):]]\\+")
						  subexp-n-subexpressions
						  subexp-subexpression-labels))
					preceeding-n-subexpressions
					preceeding-subexpression-labels))
    (({})		(length-check 4 sre)
			(let ((low (cadr sre))
			      (high (caddr sre))
			      (subexp (cadddr sre)))
			  (compile-regexp subexp
					  (lambda (subexp-string subexp-n-subexpressions subexp-subexpression-labels)
					    (return (string-append "[[:(" subexp-string "):]]{" (number->string low) "," (number->string high) "}")
						    subexp-n-subexpressions
						    subexp-subexpression-labels))
					preceeding-n-subexpressions
					preceeding-subexpression-labels)))
    ((&)		(case (length sre)
			  ((1)			(return "" preceeding-n-subexpressions preceeding-subexpression-labels))
			  ((2)			(compile-regexp (cadr sre) return preceeding-n-subexpressions preceeding-subexpression-labels))
			  ((3)			(compile-regexp
						 (cadr sre)
						 (lambda (left-string left-n-subexpressions left-subexpression-labels)
						   (compile-regexp
						    (caddr sre)
						    (lambda (right-string right-n-subexpressions right-subexpression-labels)
						      (return (string-append "[[:(" left-string "):]][[:(" right-string "):]]")
							      right-n-subexpressions
							      right-subexpression-labels))
						    left-n-subexpressions
						    left-subexpression-labels))
						 preceeding-n-subexpressions
						 preceeding-subexpression-labels))
			  (else			(compile-regexp `(& ,(cadr sre) (& ,@(cddr sre)))
								return
								preceeding-n-subexpressions
								preceeding-subexpression-labels))))
    ((|)		(case (length sre)
			  ((1)			(return "" preceeding-n-subexpressions preceeding-subexpression-labels))
			  ((2)			(compile-regexp `(| ,(cadr sre) "") return preceeding-n-subexpressions preceeding-subexpression-labels))
			  ((3)			(compile-regexp
						 (cadr sre)
						 (lambda (left-string left-n-subexpressions left-subexpression-labels)
						   (compile-regexp
						    (caddr sre)
						    (lambda (right-string right-n-subexpressions right-subexpression-labels)
						      (return (string-append "[[:(" left-string "):]]\\|[[:(" right-string "):]]")
							      right-n-subexpressions
							      right-subexpression-labels))
						    left-n-subexpressions
						    left-subexpression-labels))
						 preceeding-n-subexpressions
						 preceeding-subexpression-labels))
			  (else			(compile-regexp `(| ,(cadr sre) (| ,@(cddr sre)))
								return
								preceeding-n-subexpressions
								preceeding-subexpression-labels))))
    ((=)		(if (< (length sre) 3)
			    (length-check 2 sre)
			    (length-check 3 sre))
			(let* ((params (cdr sre))
			       (label (and (= 2 (length params))
					   (car params)))
			       (subexp (car (last-pair params))))
			  (compile-regexp subexp
					  (lambda (subexp-string subexp-n-subexpressions subexp-subexpression-labels)
					    (return (string-append "\\(" subexp-string "\\)")
						    subexp-n-subexpressions
						    subexp-subexpression-labels))
					  (+ 1 (or preceeding-n-subexpressions 0))
					  (append (and label (list label (+ 1 (or preceeding-n-subexpressions 0))))
						  preceeding-subexpression-labels))))
    ((@)		(length-check 2 sre)
			(let* ((label (cadr sre))
			       (position (kw-arg-ref preceeding-subexpression-labels label)))
			  (if (or (not position)
				  (< position 0)
				  (> position 9))
			      (throw 'too-many-subexps-for-backreference label))
			  (return (string-append "\\" (->string position)) preceeding-n-subexpressions preceeding-subexpression-labels)))
    ((/)		(length-check 2 sre)
			(return (string-append "[[:cut " (number->string (cadr sre)) ":]]") preceeding-n-subexpressions preceeding-subexpression-labels))

    (else		(throw 'unrecognized-structured-regexp sre))))


(define (regexp-quote s)
  (apply string (map (lambda (c) (or (assq-ref regexp-names c) c)) (string->list s))))

(define regexp-names '((#\\ . "\\\\")
		       (#\. . "\\.")
		       (#\^ . "\\^")
		       (#\$ . "\\$")
		       (#\* . "\\*")
		       (#\[ . "\\[")))

(define (regexp-cset elts :optional negate?)
  (let ((specials (apply append (map (lambda (elt)
				       (cond
					((eq? elt #\-)		(list elt))
					((eq? elt #\[)		(list elt))
					((pair? elt)		(let* ((from (car elt))
								       (to (cadr elt))
								       (dash? (or (eq? from #\-) (eq? to #\-)))
								       (bracket? (or (eq? from #\[) (eq? to #\[))))
								  (cond
								   ((and dash? bracket?)	'(#\- #\[))
								   (dash?			'(#\-))
								   (bracket?			'(#\[))
								   (#t				()))))
					((string? elt)		(let* ((dash? (string-index elt #\-))
								       (bracket? (string-index elt #\[)))
								  (cond
								   ((and dash? bracket?)	'(#\- #\[))
								   (dash?			'(#\-))
								   (bracket?			'(#\[))
								   (#t				()))))
					(#t			())))
				     elts))))
    (string-append "["
		   (if negate?
		       "^"
		       "")
		   (if (memq #\[ specials)
		       "["
		       "")
		   (apply string
			  (map (lambda (elt)
				 (cond
				  ((eq? elt #\-)		"")
				  ((eq? elt #\[)		"")
				  ((char? elt)			elt)
				  ((string? elt)		(apply string
								       (map (lambda (c)
									      (if (eq? #\\ c)
										  "\\\\"
										  c))
									    (set-differq (string->list elt) '(#\- #\[)))))
				  ((pair? elt)			(string-append
								 (case (car elt)
								   ((#\-)		(integer->char (1+ (char->integer (car elt)))))
								   ((#\[)		"\\\\")
								   (else		(car elt)))
								 "-"
								 (case (cadr elt)
								   ((#\- #\[)		(integer->char (1- (char->integer (cadr elt)))))
								   (else		(cadr elt)))))
				  (#t				(throw 'unrecognized-character-set-element elt))))
			       elts))
		   (if (memq #\- specials)
		       "-"
		       "")
		   "]")))
