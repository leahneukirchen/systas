;;; string-parsing.scm - Carving up strings.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (standard string-parsing)
  :use-module (standard regexps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dividing Strings Into Fields
;;; 
;;; The names of these functions are very regular.
;;; Here is a grammar of a call to one of these:
;;;
;;;   <string-function-invocation>
;;;   := (<action>-<seperator-disposition>-<seperator-determination> <seperator-param> <str> [<ret>] . <kws>)
;;;
;;; <str>    = the string
;;;
;;; <ret>    = The continuation.  String functions generally return
;;;	       multiple values by passing them to this procedure in a 
;;;	       tail-recursive manner.
;;;
;;; <action> =    split
;;;		| separate-fields
;;;
;;;		"split" means to divide a string into (at most) two parts.
;;;			<ret> will be called with two arguments.
;;;
;;;		"separate-fields" means to divide a string into as many
;;;			parts as possible.  "separate-fields" procedures
;;;			do not accept a `ret' parameter and always
;;;			return a list of fields.
;;; 
;;; <kws>    = a list of keywords and keyword arguments
;;;			:substring -- Use `substring' to create the strings
;;;				      in the return value.  Otherwise, use
;;;				      `make-shared-substring'.   With this
;;;				      keyword, none of the strings returned
;;;				      share state with `<str>'.  Without this
;;;				      keyword, all of the strings returned
;;;				      share state with `<str>'.
;;;
;;; <seperator-disposition> = 	  before
;;;				| after
;;;				| discarding
;;;				| at-prefix
;;;				| discarding-prefix
;;;				| at-postfix
;;;				| discading-postfix
;;;
;;;		"before" means to find fields on both sides of
;;;			the separator and to leave the seperator
;;;			attached to the beginning of the field to its
;;;			right.
;;; 
;;;		"after" means to find fields on both sides of
;;;			the separator and to leave the seperator
;;;			attached to the end of the field to its left.
;;;			
;;;		"discarding" means to find fields on both sides of
;;;			the separator and to discard seperators.
;;; 
;;; 		"at-prefix" means to find fields to the right of 
;;;			separators and to leave separators attached
;;;			to their fields.  The separator may be omitted
;;;			from the first field.
;;;
;;; 		"discarding-prefix" means to find fields to the right 
;;;			of separators and to leave separators attached
;;;			to their fields.  The separator may be omitted
;;;			from the first field.
;;;
;;; 		"at-postfix" means to find fields to the left of 
;;;			separators and to leave separators attached
;;;			to their fields.  The separator may be omitted
;;;			from the last field.
;;;
;;; 		"discarding-postfix" means to find fields to the left
;;;			of separators and to leave separators attached
;;;			to their fields.  The separator may be omitted
;;;			from the last field.
;;; 
;;; 		"discarding-surrounding" means to find fields between
;;;			pairs of separators and to discard separators.
;;;			The separator may be omitted from the beginning
;;;			and end of the string.
;;;
;;; <seperator-determination> =	  char
;;;				| regexp
;;;
;;;		"char" means to use a particular character as field seperator.
;;;
;;; <seperator-param> = A parameter that completes the meaning of the determinations.
;;;			For example, if the determination is "char", then this parameter
;;;			says which character.  If it is "predicate", the parameter is the
;;;			predicate.
;;;
;;;
;;; Examples:
;;;
;;;		(separate-fields-discarding-char #\, "foo, bar, baz, , bat" list)
;;;		=> ("foo" " bar" " baz" " " " bat")
;;;
;;;		(split-after-char #\- 'an-example-of-split list)
;;;		=> ("an-" "example-of-split")
;;; 
;;; As an alternative to using a determination "predicate", or to
;;; trying to do anything complicated with these functions, consider
;;; using regular expressions.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Split by the First Occurence of a Particular Character
;;;

;; (char-splitter l-inc r-inc)
;; 
;; Return a procedure that implements the -before, -after,
;; and -discarding field splitters for character separated
;; fields.
;; 
(define ((char-splitter l-inc r-inc) char str ret . kws)
  (let ((substr		(if (memq :substring kws) substring make-shared-substring))
	(end (string-index str char)))
    (if end
	(ret (substr str 0 (+ l-inc end))
	     (substr str (+ r-inc end)))
	(ret (substr str) #f))))


;; (split-before-char c str ret . kws)
;; 
;; If character `c' does not occur in string `str', invoke `ret'
;; this way:
;; 
;; 	(ret (make-shared-substring str) #f)
;; 
;; Otherwise, if `c' occurs at position `n', invoke `ret' 
;; this way:
;; 
;; 	(ret (make-shared-substring str 0 n)
;;	     (make-shared-substring str n))
;; 
;; If the keyword `:substring' is provided, use `substring' instead
;; of `make-shared-substring'.
;; 
(define-public split-before-char (char-splitter 0 0))


;; (split-after-char c str ret . kws)
;; 
;; If character `c' does not occur in string `str', invoke `ret'
;; this way:
;; 
;; 	(ret (make-shared-substring str) #f)
;; 
;; Otherwise, if `c' occurs at position `n', invoke `ret' 
;; this way:
;; 
;; 	(ret (make-shared-substring str 0 (1+ n))
;;	     (make-shared-substring str (1+ n)))
;; 
;; If the keyword `:substring' is provided, use `substring' instead
;; of `make-shared-substring'.
;; 
(define-public split-after-char (char-splitter 1 1))


;; (split-discarding-char c str ret . kws)
;; 
;; If character `c' does not occur in string `str', invoke `ret'
;; this way:
;; 
;; 	(ret (make-shared-substring str) #f)
;; 
;; Otherwise, if `c' occurs at position `n', invoke `ret' 
;; this way:
;; 
;; 	(ret (make-shared-substring str 0 n)
;;	     (make-shared-substring str (1+ n)))
;; 
;; If the keyword `:substring' is provided, use `substring' instead
;; of `make-shared-substring'.
;; 
(define-public split-discarding-char (char-splitter 0 1))


;; (split-at-prefix-char c str ret . kws)
;; 
;; If character `c' does not occur in string `str', invoke `ret'
;; this way:
;; 
;; 	(ret (make-shared-substring str) #f)
;; 
;; Otherwise, search for `c' beginning at position 1 in `str' 
;; (ignoring the first character of `str').  If `c' is found at 
;; position `n', invoke `ret' this way:
;; 
;; 	(ret (make-shared-substring str 0 n)
;;	     (make-shared-substring str n))
;; 
;; If the keyword `:substring' is provided, use `substring' instead
;; of `make-shared-substring'.
;; 
(define-public (split-at-prefix-char c str ret . kws)
  (let ((substr		(if (memq :substring kws) substring make-shared-substring)))
    (cond
     ((string-null? str)		(ret (substr str) #f))
     (#t				(let ((index (string-index str c 1)))
					  (if (not index)
					      (ret (substr str) #f)
					      (ret (substr str 0 index)
						   (substr str index))))))))


;; (split-discarding-prefix-char c str ret . kws)
;; 
;; If character `c' does not occur in string `str', invoke `ret'
;; this way:
;; 
;; 	(ret (make-shared-substring str) #f)
;; 
;; Otherwise, search for `c' beginning at position 1 in `str' 
;; (ignoring the first character of `str').  If `c' is found at 
;; position `n', and `c' is not the first character of `str', 
;; invoke `ret' this way:
;; 
;; 	(ret (make-shared-substring str 0 n)
;;	     (make-shared-substring str (1+ n)))
;; 
;; If `c' is the first character of `str', invoke `ret' this way:
;; 
;; 
;; 	(ret (make-shared-substring str 1 n)
;;	     (make-shared-substring str (1+ n)))
;; 
;; If the keyword `:substring' is provided, use `substring' instead
;; of `make-shared-substring'.
;; 
(define-public (split-discarding-prefix-char c str ret . kws)
  (let ((substr		(if (memq :substring kws) substring make-shared-substring)))
    (cond
     ((string-null? str)		(ret (substr str) #f))
     (#t				(let ((index (string-index str c 1)))
					  (if (not index)
					      (ret (if (char=? c (string-ref str)) (substr str 1) str) #f)
					      (ret (substr str (if (char=? c (string-ref str)) 1 0) index)
						   (substr str (1+ index)))))))))


;; (split-at-postfix-char c str ret . kws)
;; 
;; If character `c' does not occur in string `str', invoke `ret'
;; this way:
;; 
;; 	(ret (make-shared-substring str) #f)
;; 
;; Otherwise, search for `c' in `str'.  If `c' is found at 
;; position `n', and `n' is not `(1- (string-length str))',
;; invoke `ret' this way:
;; 
;; 	(ret (make-shared-substring str 0 (1+ n))
;;	     (make-shared-substring str (1+ n)))
;; 
;; If `n' is `(1- (string-length str))' invoke `ret' this way:
;; 
;; 	(ret (make-shared-substring str 1 n)
;;	     #f)
;; 
;; If the keyword `:substring' is provided, use `substring' instead
;; of `make-shared-substring'.
;; 
(define-public (split-at-postfix-char c str ret . kws)
  (let ((substr		(if (memq :substring kws) substring make-shared-substring)))
    (cond
     ((string-null? str)		(ret (substr str) #f))
     (#t				(let ((index (string-index str c)))
					  (cond
					   ((not index)				(ret (substr str) #f))
					   ((< index (1- (string-length str)))	(ret (substr str 0 (1+ index))
										     (substr str (1+ index))))
					   (#t					(ret (substr str) #f))))))))


;; (split-discarding-postfix-char c str ret . kws)
;; 
;; If character `c' does not occur in string `str', invoke `ret'
;; this way:
;; 
;; 	(ret (make-shared-substring str) #f)
;; 
;; Otherwise, search for `c' in `str'.  If `c' is found at 
;; position `n', and `n' is not `(1- (string-length str))',
;; invoke `ret' this way:
;; 
;; 	(ret (make-shared-substring str 0 n)
;;	     (make-shared-substring str (1+ n)))
;; 
;; If `n' is `(1- (string-length str))' invoke `ret' this way:
;; 
;; 	(ret (make-shared-substring str 0 n)
;;	     #f)
;; 
;; If the keyword `:substring' is provided, use `substring' instead
;; of `make-shared-substring'.
;; 
(define-public (split-discarding-postfix-char c str ret . kws)
  (let ((substr		(if (memq :substring kws) substring make-shared-substring)))
    (cond
     ((string-null? str)		(ret (substr str) #f))
     (#t				(let ((index (string-index str c)))
					  (cond
					   ((not index)				(ret (substr str) #f))
					   ((< index (1- (string-length str)))	(ret (substr str 0 index)
										     (substr str (1+ index))))
					   (#t					(ret (substr str 0 index) #f))))))))


;; THIS LOOKS WRONG
;; 
(define-public (split-discarding-surrounding-char c str ret . kws)
  (let ((from		(if (and (< 0 (string-length str))
				 (char=? c (string-ref str)))
			    1
			    0)))
  (apply split-before-char
	 c
	 (make-shared-substring str from)
	 (lambda (a b)
	   (if (and b (= 1 (string-length b)) (char=? c (string-ref b)))
	       (ret a #f)
	       (ret a b)))
	 kws)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Split by the First Occurence of a Particular Regexp
;;;

;; (sre-splitter sre pick-spec . sre-kws)
;; 
;; `sre' is a strucutred regexp.  `pick-spec' specifies two values to
;; return for matches of `sre'. `sre-kws' are additional keywords for
;; `structured-regexp->procedure'.
;; 
;; Return a procedure `split':
;; 
;; 	(split s ret)
;; 
;; Where `s' is a string and `ret' is a procedure.  `split' invokes
;; `ret' with two arguments.  If `s' matches `sre', the `pick-spec'
;; should specify the two values.  If `s' does not match `sre', `ret'
;; is invoked:
;; 
;; 	(ret s #f)
;; 
(define-public (sre-splitter sre pick-spec . sre-kws)
  (let ((reproc		(apply structured-regexp->procedure sre :pick-spec pick-spec sre-kws)))
    (lambda (s ret)
      (let ((match 	(reproc s)))
	(if (not match)
	    (ret s #f)
	    (apply-to-args match
	      (lambda (a b) (ret a b))))))))


;; (split-before-whitespace str ret)
;; 
;; If `str' contains whitespace, invoke:
;; 
;; 	(ret a b)
;; 
;; where `a' and `b' are left and right substrings of `str', 
;; split before the whitespace.
;; 
;; Otherwise, invoke:
;; 
;; 	(ret str #f)
;; 
(define-public split-before-whitespace		(sre-splitter `([] space)
							      '(< (0 >))))

;; (split-after-whitespace str ret)
;; 
;; If `str' contains whitespace, invoke:
;; 
;; 	(ret a b)
;; 
;; where `a' and `b' are left and right substrings of `str', 
;; split after the whitespace.
;; 
;; Otherwise, invoke:
;; 
;; 	(ret str #f)
;; 
(define-public split-after-whitespace		(sre-splitter `(+ ([] space))
							      '((< 0) >)))

;; (split-discarding-whitespace str ret)
;; 
;; If `str' contains whitespace, invoke:
;; 
;; 	(ret a b)
;; 
;; where `a' and `b' are left and right substrings of `str', split
;; before and after the whitespace (the whitespace itself is
;; discarded).
;; 
;; Otherwise, invoke:
;; 
;; 	(ret str #f)
;; 
(define-public split-discarding-whitespace	(sre-splitter `(+ ([] space))
							      '(< >)))



;; (separate-fields proc str)
;; 
;; `proc' is a proedure of two arguments:
;; 
;; 	(proc str ret)
;; 
;; It invokes `ret' with two arguments:
;; 
;; 	(ret l r)
;; 
;; such that `l' is a substring of a prefix of `str' and `r' is a
;; substring of the corresponding suffix of `r' or #f.
;; 
;; `separate-fields' repeatedly applies `proc' to `str' and `r' until
;; `r' is #f.  It returns a list of `l', in the same order they occur
;; in `str'.
;; 
(define-public (separate-fields proc str)
  (let ((answer		(cons () ())))
    (let loop ((str	str)
	       (pos	answer))
      (proc str
	    (lambda (l r)
	      (set-cdr! pos (cons l ()))
	      (if (not r)
		  (cdr answer)
		  (loop r (cdr pos))))))))


;; (separate-fields-before-char ch str . kws)
;; 
;; Repeatedly use `split-before-char' to divide `str' into fields.
;; Return the list of fields.
;; 
(define-public (separate-fields-before-char ch str . kws)
  (separate-fields (lambda (str ret) (apply split-before-char ch str ret kws))
		   str))


;; (separate-fields-after-char ch str . kws)
;; 
;; Repeatedly use `split-after-char' to divide `str' into fields.
;; Return the list of fields.
;; 
(define-public (separate-fields-after-char ch str . kws)
  (separate-fields (lambda (str ret) (apply split-after-char ch str ret kws))
		   str))


;; (separate-fields-discarding-char ch str . kws)
;; 
;; Repeatedly use `split-discarding-char' to divide `str' into fields.
;; Return the list of fields.
;; 
(define-public (separate-fields-discarding-char ch str . kws)
  (separate-fields (lambda (str ret) (apply split-discarding-char ch str ret kws))
		   str))


;; (separate-fields-at-prefix-char ch str . kws)
;; 
;; Repeatedly use `split-at-prefix-char' to divide `str' into fields.
;; Return the list of fields.
;; 
(define-public (separate-fields-at-prefix-char ch str . kws)
  (separate-fields (lambda (str ret) (apply split-at-prefix-char ch str ret kws))
		   str))


;; (separate-fields-discarding-prefix-char ch str . kws)
;; 
;; Repeatedly use `split-discarding-prefix-char' to divide `str' into fields.
;; Return the list of fields.
;; 
(define-public (separate-fields-discarding-prefix-char ch str . kws)
  (separate-fields (lambda (str ret) (apply split-discarding-prefix-char ch str ret kws))
		   str))


;; (separate-fields-at-postfix-char ch str . kws)
;; 
;; Repeatedly use `split-at-postfix-char' to divide `str' into fields.
;; Return the list of fields.
;; 
(define-public (separate-fields-at-postfix-char ch str . kws)
  (separate-fields (lambda (str ret) (apply split-at-postfix-char ch str ret kws))
		   str))


;; (separate-fields-discarding-postfix-char ch str . kws)
;; 
;; Repeatedly use `split-discarding-postfix-char' to divide `str' into fields.
;; Return the list of fields.
;; 
(define-public (separate-fields-discarding-postfix-char ch str . kws)
  (separate-fields (lambda (str ret) (apply split-discarding-postfix-char ch str ret kws))
		   str))


;; (separate-fields-discarding-surrounding-char ch str . kws)
;; 
;; Repeatedly use `split-discarding-surrounding-char' to divide `str'
;; into fields.  Return the list of fields.
;; 
(define-public (separate-fields-discarding-postfix-char ch str . kws)
  (separate-fields (lambda (str ret) (apply split-discarding-surrounding-char ch str ret kws))
		   str))

;; (separate-fields-before-whitespace str . kws)
;; 
(define-public (separate-fields-before-whitespace str)
  (separate-fields split-before-whitespace str))

;; (separate-fields-after-whitespace str . kws)
;; 
(define-public (separate-fields-after-whitespace str)
  (separate-fields split-after-whitespace str))

;; (separate-fields-discarding-whitespace str . kws)
;; 
(define-public (separate-fields-discarding-whitespace str)
  (separate-fields split-discarding-whitespace str))




;; (string->lines str . kws)
;; 
;; 	(apply separate-fields-at-postfix-char #\nl str kws)
;; 
(define-public (string->lines str . kws)
  (apply separate-fields-at-postfix-char #\nl str kws))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String Prefix Predicates
;;;
;;; Very simple:
;;;
;;; (define-public ((string-prefix-predicate pred?) prefix str)
;;;  (and (<= (string-length prefix) (string-length str))
;;;	  (pred? prefix (make-shared-substring str 0 (string-length prefix)))))
;;;
;;; (define-public string-prefix=? (string-prefix-predicate string=?))
;;;

(define-public ((string-prefix-predicate pred?) prefix str)
  (and (<= (string-length prefix) (string-length str))
       (pred? prefix (make-shared-substring str 0 (string-length prefix)))))

(define-public string-prefix=? (string-prefix-predicate string=?))
(define-public string-prefix<? (string-prefix-predicate string<?))
(define-public string-prefix>? (string-prefix-predicate string>?))
(define-public string-prefix<=? (string-prefix-predicate string<=?))
(define-public string-prefix>=? (string-prefix-predicate string>=?))

(define-public string-prefix-ci=? (string-prefix-predicate string-ci=?))
(define-public string-prefix-ci<? (string-prefix-predicate string-ci<?))
(define-public string-prefix-ci>? (string-prefix-predicate string-ci>?))
(define-public string-prefix-ci<=? (string-prefix-predicate string-ci<=?))
(define-public string-prefix-ci>=? (string-prefix-predicate string-ci>=?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strippers
;;;
;;; stripper 	:= sans-<location>-<part>
;;; 
;;; location	:= surrounding
;;;		|  leading
;;;		|  trailing
;;;
;;; part	:= whitespace
;;; 		|  blanks
;;; 
;;; Also provided:
;;; 
;;; 	has-final-newline?
;;; 	sans-final-newline
;;;


(define-public (sans-surrounding-whitespace s)
  (sans-trailing-whitespace (sans-leading-whitespace s)))

(define-public (sans-surrounding-blanks s)
  (sans-trailing-blanks (sans-leading-blanks s)))

(define-public sans-trailing-whitespace
  (structured-regexp->procedure `(? (* any) ([^] space)) :pick-spec 0))


(define-public sans-trailing-blanks
  (structured-regexp->procedure `(? (* any) ([^] blank)) :pick-spec 0))


(define-public sans-leading-whitespace
  (structured-regexp->procedure `(^ (* ([] space))) :pick-spec '>))

(define-public sans-leading-blanks
  (structured-regexp->procedure `(^ (* ([] blank))) :pick-spec '>))


(define-public (has-final-newline? s) 
  (and (< 0 (string-length s)) (char=? #\nl (string-ref s (+ -1 (string-length s))))))


(define-public (sans-final-newline s)
  (if (has-final-newline? s)
      (make-shared-substring s 0 (+ -1 (string-length s)))
      s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unparsing Strings
;;; 
;;; String constructors which are the inverses of various string parsers.
;;; 
;;; 	join-fields-with char/symbol/string field ...
;;; 	join-fields-with-prefix
;;; 	join-fields-with-postfix
;;; 	join-fields-with-surrounding
;;; 
;;; 

(define-public (ensure-trailing-newline str)
  (if (has-final-newline? str)
      str
      (string-append str "\n")))

(define-public (join-fields-with sep . fields)
  (xreduce (lambda (a b) (string-append a sep b))
	   ""
	   fields))

(define-public (join-fields-with-prefix sep . fields)
  (xfold (lambda (a b) (string-append a sep b))
	 ""
	 fields))

(define-public (join-fields-with-postfix sep . fields)
  (xfold (lambda (a b) (string-append a b sep))
	 ""
	 fields))

(define-public (join-fields-with-surrounding sep . fields)
  (string-append sep
		 (apply join-fields-with sep fields)
		 sep))

(define-public (xreduce f lidentity list)
  (cond
   ((null? list)	lidentity)
   (#t			(xfold f (car list) (cdr list)))))

(define-public (xfold xkons xknil . lists)
  (if (or-map null? lists)
      xknil
      (apply xfold xkons (apply xkons xknil (map car lists)) (map cdr lists))))
