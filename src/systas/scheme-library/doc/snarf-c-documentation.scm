;;; tag: Tom Lord Tue Dec  4 14:59:32 2001 (doc/snarf-c-documentation.scm)
;;;
;;; snarf-c-documentation.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc snarf-c-documentation)
  :use-module (doc old-pdml)
  :use-module (doc old-xdml)
  :use-module (doc hdml)
  :use-module (unix file-utils)
  :use-module (unix untabify)
  :use-module (unix filenames)
  :use-module (data-structures string-fun)
  :use-module (data-structures ratlist)
  :use-module (regexps subst)
  :use-module (regexps cached)
  :use-module (regexps structured))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Documentation Comments in C
;; 
;; This file defines structured regular expressions for extracting
;; documentation comments from C code.
;;
;; 

(define-public (c-source->embedded-documentation str)
  
  (define (c-source->embedded-documentation str)
    (let ((possible-doc (regexec (once (regcomp (compile-regexp `(& (^) ,(not-documentation-pattern)) first-value)))
				 str
				 '>
				 'REG_NO_SUBEXP_REPORTING)))
      (and possible-doc
	   (regexec (once (regcomp (compile-regexp (documentation-comment-pattern) first-value)))
		    possible-doc
		    '(0 >)
		    'REG_NO_SUBEXP_REPORTING
		    (lambda (doc rest)
		      (cons doc (c-source->embedded-documentation rest)))
		    (lambda () ())))))

  (apply string-append (c-source->embedded-documentation str)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C Syntax for Documentation Comments
;;
;; C source code is seen as an alternating sequence of "not
;; documentation" and "documentation".
;; 
;; "Not documentation" is nearly arbitrary text, including C string
;; and character constants, CPP directives, and ordinary CPP
;; comments.
;; 
;; "documentation" is specially formatted comments and non-comment
;; text embedded between specially formatted comments.
;; 
;; Documentation always begins with a comment with a type tag.  The
;; type tag is a simple Scheme identifier.  It immediately follows the
;; initial decoration of the comment which is a mixture of whitespace
;; and astrixes ending with an asterix.  For example:
;;
;;	/*c vu_open	The type tag s "c"
;; 
;;	/************	The type tag is "h0"
;;	 ***h0 "Hey"
;; 
;;	/* c vu_open	Because of the space after the asterix, 
;;			there is no type tag here.  This is not
;; 			a documentation comment.
;; 
;; With one exception, every comment line of a documentation comment
;; must begin with any number of spaces and tabs, followed by an
;; asterix.
;;
;;	/*x some documentation
;;	 * first line.
;;	 * second line.
;;	 */
;;
;; The exception is a comment line that introduces embedded
;; non-comment text.  Such a line contains only spaces and tabs
;; followed by:
;;
;;	insert*/
;; 
;; All of the text that follows is added to the documentation until a
;; line is encountered that begins (after any number of spaces and
;; tabs):
;;
;;	/*end-insert
;; 
;; Documentation comment syntax resumes immediately after the second
;; "]" or on the next line if the line following the "]" is blank.  If
;; the rest of the line is not blank, the first non-whitespace
;; character must be an asterix, just as if the beginning of the line
;; where immediately after the "]".  For example, the documentation
;; for this function is its code:
;;
;;
;;   Example 1:
;;
;;	/*c add
;;	 * int add (int x, int y);
;;	 *
;;	 * This is a very simple function.
;;	 *
;;	 insert*/
;;	int
;;	add (int x, int y)
;;	{
;;	  return x + y;
;;	}
;;
;;	/*end-insert
;; 	 *
;;	 * See also: `subtract'.
;;	 */
;;
;;
;;   Example 2:
;;
;;	/*c add
;;	 * int add (int x, int y);
;;	 *
;;	 * This is a very simple function.
;;	 *
;;	 insert*/
;;	int
;;	add (int x, int y)
;;	{
;;	  return x + y;
;;	}
;;
;;	/*end-insert * See also: `subtract'.
;;	 */
;; 
;; The sole purpose of this tool is to extract the documentation text
;; from a source file and produce a string which is the concatenation
;; of that documentation.  That string has further structure, defined
;; by the syntax of "ccml": the "C Comment Mark-up Language", but that
;; further structure is of no interest here.
;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Program Text Which is Not Documentation
;;;

;; not-documentation-pattern
;;
;; A pattern describing what to skip over when searching for documentation
;; comments.
;;
(define (not-documentation-pattern)		`(* (| ,(string-constant-pattern)
						       ,(character-constant-pattern)
						       ,(not-comment-pattern)
						       ,(ordinary-comment-pattern))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation Comments with Embedded Program Text
;;;

;; documentation-comment-pattern
;;
;; A pattern that matches an entire documentation comment, including
;; embedded program text.
;;
;;
(define (documentation-comment-pattern)		`(& ,(leading-comment-decoration-pattern)
						    ,(documentation-tag-pattern)
						    (& (* ([^] "\n")) "\n")
						    ,(documentation-body-pattern)
						    ,(linear-space-pattern)
						    "*/"
						    ,(linear-space-pattern)
						    (? "\n")))


;; leading-comment-decoration-pattern
;; 
;; In a C documentation comment, this pattern matches the text from
;; the comment start to the tag that identifies what type of
;; documentation the comment contains.
;; 
;; This is "/*", followed by any combination of "*" and whitespace,
;; followed by a final "*" which may be the same as the "*" in the
;; "/*".  For example:
;;
;;	/*c int main (int argc, char ** argv);   ... */
;;      ^^
;;	  \
;;         indicated characters are the leading-comment-decoration
;;	  /
;;	  vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;;	/*******************************************************
;;	*h1 Section Eight ...*/
;;      ^^
;;
(define (leading-comment-decoration-pattern) 	`(& "/*"
						    (? (& (* ([] "* \t\n"))
							  "*"))))

;; documentation-tag-pattern
;; 
;; In a C documentation comment, this pattern matches the first alphanumeric
;; text which serves as a type tag for the documentation comment.
;; 
;; For example:
;;
;;	/*******************************************************
;;	*(h1 Section Eight ...*/
;;       ^^
;;	   \
;;          indicated characters are the documentation-tag
;;	   /
;;	  v
;;	/*(c int main (int argc, char ** argv);   ... */
;;
(define (documentation-tag-pattern)
  `(& "(" ,(letters-pattern) (* (| "-" ,(alnum-pattern)))))


;; documentation-body-pattern
;; 
;; Match the complete text of a documentation comment.  This may
;; include embedded code.
;;
(define (documentation-body-pattern)		`(* (& ,(linear-space-pattern)
						       (| "*\n"
							  "\n"
							  (& "*" ([^] "\n/") (* ([^] "\n")) "\n")
							  (& "insert"
							     ,(linear-space-pattern)
							     "*/"
							     ,(linear-space-pattern)
							     "\n"
							     ,(not-documentation-pattern)
							     ,(leading-comment-decoration-pattern)
							     "end-insert"
							     (? (& ,(linear-space-pattern)
								   "\n")))
							  (& ([^] "\n\t *") (* ([^] "\n")) "\n")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String and Character Constants
;;; 
;;; The pattern of of a C string constant.
;;;

;; string-constant-pattern
;;
;; A quoted list of `string-element-pattern'
;;
(define (string-constant-pattern) 		`(& "\""  (* ,(string-element-pattern)) "\""))


;; character-constant-pattern
;; 
;; The pattern of a C character constant.
;; 
(define (character-constant-pattern) 		`(& "\'" ,(character-name-pattern) "\'"))


;; string-element-pattern
;;
;; The pattern of one character in a string.
;;
(define (string-element-pattern)		`(| ,(string-or-character-element-pattern)
						    ;; Strings can include the character #\'
						    ;;
						    "\'"))
;; character-name-pattern
;; 
;; The pattern of the inside of a C character constant.
;; 
(define (character-name-pattern) 		`(| ,(string-or-character-element-pattern)
						    ;; Characters can include the character #\"
						    ;;
						    "\""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Everything but Comments and String or Character Constants
;;; 

;; not-comment-pattern
;; 
;; Matches many characters which can not begin a string constant,
;; character constant, or a comment.
;;
(define (not-comment-pattern) 			`(* ,(not-comment-element-pattern)))


;; not-comment-element-pattern
;; 
;; Matches one or two characters which can not begin a string
;; constant, character constant, or a comment.
;;
(define (not-comment-element-pattern) 		`(| ([^] "\"'/") (& "/" ([^] "*"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ordinary C Comments
;;; 

;; ordinary-comment-pattern
;;
;; The pattern of a C comment which is not a documentation comment.
;;
(define (ordinary-comment-pattern) 		`(& ,(leading-comment-decoration-pattern)
						    (| "/"
						       (& (| ([^] " \t\n/*(" ,(lowercase-range) ,(uppercase-range))
							     (& ,(mandatory-whitespace-pattern)
								([^] " \t\n*")))
							  (* (| ([^] "*") (& "*" ([^] "/"))))
							  "*/"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpful Regexp Parts
;;; 

;; whitespace-pattern
;; 
;; Any number of spaces, tabs, and newlines.
;; 
(define (whitespace-pattern) 				'(* ([] " \t\n")))

;; mandatory-whitespace-pattern
;; 
;; Any number of spaces, tabs, and newlines.
;; 
(define (mandatory-whitespace-pattern)			'(+ ([] " \t\n")))

;; linear-space-pattern
;; 
;; Any number of spaces and tabs.
;; 
(define (linear-space-pattern) 				'(* ([] " \t")))

;; mandatory-linear-space-pattern
;; 
;; Any non-0 number of spaces and tabs.
;; 
(define (mandatory-linear-space-pattern)		'(+ ([] " \t")))

;; octal-digit-range
;;
;; For use in regexps with the `[]' and `[^]' operators
;;
(define (octal-digit-range)	 		'(#\0 #\7))

;; lowercase-range
;;
;; For use in regexps with the `[]' and `[^]' operators.
;;
(define (lowercase-range)			'(#\a #\z))


;; uppercase-range
;;
;; For use in regexps with the `[]' and `[^]' operators.
;;
(define (uppercase-range)			'(#\A #\Z))

;; letters-pattern
;;
(define (letters-pattern)			`([] ,(lowercase-range) ,(uppercase-range)))


;; alnum-pattern
;;
(define (alnum-pattern)				`([] ,(lowercase-range)
						     ,(uppercase-range)
						     (#\0 #\9)))


;; string-or-character-element-pattern
;;
;; A regexp that matches the inside of a character constant
;; (except for the character #\") or one (logical) character on the
;; inside of string constant (except for the character #\').
;;
(define (string-or-character-element-pattern)	`(|
						  
						  ;; An octal character constant
						  ;;
						  (& "\\"
						     ([] ,(octal-digit-range))
						     (? (& ([] ,(octal-digit-range))
							   (? ([] ,(octal-digit-range))))))

						  ;; Or an escaped character (not octal)
						  ;;
						  (& "\\" ([^] ,(octal-digit-range)))

						  ;; Or a normal character.
						  ;;
						  (| ([^] "\\\\'\""))))








(define timers (make-hash-table))
(define (reset-timers) (set! timers (make-hash-table)))
(define (show-timers)
  (vector-for-each (lambda (bucket)
		     (for-each (lambda (key-value)
				 (write `(,(car key-value)
					  ,@(if (not (pair? (cdr key-value)))
						(list (cdr key-value))
						(list (cddr key-value) :active))))
				 (newline))
			       bucket))
		   timers))
					      
(define (timer-start v :optional value) (hashq-set! timers v (cons (get-internal-run-time) (hashq-ref timers v))) value)
(define (timer-end v :optional value)
  (let* ((x (hashq-ref timers v))
	 (elapsed (- (get-internal-run-time) (car x)))
	 (prev (or (cdr x) 0))
	 (cum (+ prev elapsed)))
    (hashq-set! timers v cum)
    value))
(define (timer-end-pk msg v :optional value)
  (let* ((x (hashq-ref timers v))
	 (elapsed (- (get-internal-run-time) (car x)))
	 (prev (or (cdr x) 0))
	 (cum (+ prev elapsed)))
    (hashq-set! timers v cum)
    (pk 'timer v elapsed cum)
    value))


(define-public (cdml-parse filename str return nesting-rules :optional containing-tag)
  (if (pair? str)
      (parse-hdml (car str)
		  (lambda (tag parameters . sub-docs)
		    (if (and containing-tag
			     (not (memq tag (assq-ref nesting-rules containing-tag))))
			(return () str)
			(return (car str) (cdr str)))))
      ((once (regexec-function `(? ,(documentation-tag-pattern))
			       #f
			       #f
			       '0))
       (regexec (once (regcomp (compile-regexp `(& (^) (? ,(leading-comment-decoration-pattern))) first-value)))
		str
		'>
		'REG_NO_SUBEXP_REPORTING)
       (lambda (tag)
	 (if (= 0 (string-length tag))
	     (return () str)
	     (let ((tag (string->symbol (make-shared-substring tag 1)))
		   (first-comment-regexp-fn (once (regexec-function `(& "\n"
									,(linear-space-pattern)
									(| (& "insert*/" ,(linear-space-pattern) "\n" (/ 2))
									   (& "*/" ,(linear-space-pattern) "\n" (/ 1))))
								    #f
								    #f
								    '( (< 0)
								       >
								       state-label))))
		   (comment-sans-decoration
		    (lambda (comment)
		      (let* ((sans-last-line 			(make-shared-substring comment
										       0
										       (1+
											(string-rindex comment
												       #\nl
												       0
												       (string-rindex comment #\nl)))))
			     (sans-beginning-of-line-decoration	(regexp-subst (once (regcomp "\n[ \t]*\\* \\?"))
									      sans-last-line
									      (lambda (s) "\n")
									      :global))
			     (sans-leading-decoration		(regexp-subst (once (cached-regexp (compile-regexp (leading-comment-decoration-pattern) first-value)))
									      sans-beginning-of-line-decoration
									      (lambda (s) ""))))
			sans-leading-decoration))))

	       (if (and containing-tag
			(not (memq tag '(include-documentation)))
			(not (memq tag (assq-ref nesting-rules containing-tag))))
		   (return () str)
		   (first-comment-regexp-fn
		    str
		    (lambda (first-comment rest-of-string insert-tag)
		      (with-input-from-string (comment-sans-decoration first-comment)
			(lambda ()
			  (let* ((header (read))
				 (rest-of-first-comment (fd->string (current-input-port)))
				 (rest-of-first-comment (regexec (once (cached-regexp "[[:([\t ]*\n):]]\\?"))
								 rest-of-first-comment
								 '>)))
			    (cond
			     ((and (pair? header)
				   (eq? 'include-documentation (car header))
				   (pair? (cdr header))
				   (null? (cddr header))
				   (string? (cadr header)))	(let* ((included (cadr header))
								       (included (if (filename-absolute? included)
										     included
										     (in-vicinity (filename-directory filename)
												  included)))
								       (str (untabify-string (file->string included)))
								       (docstr (c-source->embedded-documentation str)))
								  (cdml-parse included
									      docstr
									      (lambda (doc ign)
										doc
										(parse-hdml doc
											    (lambda (tag parameters . sub-documents)
											      (if (and containing-tag
												       (memq tag (assq-ref nesting-rules containing-tag)))
												  (return doc rest-of-string)
												  (return () (cons doc rest-of-string))))))
									      nesting-rules)))
			     (#t
			      (let loop ((insert? (= 2 insert-tag))
					 (sub-document-list (reverse (cdml-parse-paragraphs rest-of-first-comment :first-preformatted)))
					 (rest-of-string rest-of-string))
				(if (not insert?)
				    (let sub-documents-loop ((sub-document-list sub-document-list)
							     (rest-of-string rest-of-string))
				      (cdml-parse filename
						  rest-of-string
						  (lambda (sub-document rest-of-string)
						    (if (not sub-document)
							(return (cons header (reverse sub-document-list))
								rest-of-string)
							(sub-documents-loop (cons sub-document sub-document-list)
									    rest-of-string)))
						  nesting-rules
						  tag))
				    ((once (regexec-function `(& (= :last-newline "\n")
								 ,(linear-space-pattern)
								 "/*"
								 ,(linear-space-pattern)
								 "end-insert")
							     #f
							     #f
							     '( (< (@ :last-newline))
								((0 1) >) )))
				     rest-of-string
				     (lambda (insert rest-of-string)
				       (first-comment-regexp-fn
					rest-of-string
					(lambda (first-comment-next-part rest-of-string insert-tag)
					  (loop (= 2 insert-tag)
						(append (reverse (cdml-parse-paragraphs (comment-sans-decoration first-comment-next-part)))
							(cons `((insert) ,insert) sub-document-list))
						rest-of-string)))))))))))))))))))))
				     


(define (cdml-parse-paragraphs str . options)
  (let* ((paragraph-list (separate-fields-discarding-regexp-pt "\n\\([[:([ \t]*\n):]]\\+\\)"
							       1
							       (regexec (once (regcomp "^[[:([ \t]*\n):]]*")) str '>)
							       list))
	 (first-paragraph (and paragraph-list
			       (memq :first-preformatted options)
			       (regexec (once (regcomp "^[ \t]*[^\n \t]")) str #f)
			       (car paragraph-list)))
	 (normal-paragraphs (remove-if string-null?
				       (if first-paragraph
					   (cdr paragraph-list)
					   paragraph-list))))
    (append (and first-paragraph
		 (if (char=? #\+ (string-ref first-paragraph))
		     (list (cdml-translate-paragraph 'text (make-shared-substring first-paragraph 1)))
		     `(((insert) ,first-paragraph))))
	    (map (lambda (normal-paragraph) (cdml-translate-paragraph 'p normal-paragraph)) normal-paragraphs))))
	

(define (cdml-translate-paragraph tag paragraph)
  (cond
   ((string-null? paragraph)				`((p) ,paragraph))
   ((char-whitespace? (string-ref paragraph))		`((insert) ,paragraph))
   (#t
    `((,tag ,@(and (char-lower-case? (string-ref paragraph)) '(:noindent)))
	,@(let loop ((remaining paragraph)
		     (so-far ()))
	    (if (string-null? remaining)
		(reverse so-far)
		(regexec (once (regcomp "``[[:([^']\\|'[^']):]]*''\\|\\^[^^]*\\^\\|\\*[^*]*\\*\\|\\\\[^/]*/\\|url:\"[^\"]*\"\\|xref:\"[^\"]*\"\\|`[^'`][^']*'\\|{[^}]*}\\|\"[^\"]*\"\\|#t\\|#f\\|[\n\t (]-\\?[0-9]\\+[\n\t\ :;.,)]\\|#\\\\[a-zA-Z0-9]\\+\\||||\\||\\*|\\||[*|$~]\\?[][_?0-9a-zA-Z()<>{}[.\\.]$^+[.-.]*/ ,.:]*|"))
			 remaining
			 '(< 0 >)
			 #f
			 (lambda (ordinary special remaining)
			   (let ((so-far (if ordinary
					     (cons ordinary so-far)
					     so-far)))
			     (loop remaining
				   (case (string-ref special)
				     ((#\nl #\tab #\space)	`(,(string (string-ref special (1- (string-length special))))
								  ((output) ,(make-shared-substring special 1 (1- (string-length special))))
								  ,(string (string-ref special))
								  ,@so-far))
				     ((#\^)			`(((emph) ,(make-shared-substring special 1 (1- (string-length special))))
								  ,@so-far))
				     ((#\*)			`(((stress) ,(make-shared-substring special 1 (1- (string-length special))))
								  ,@so-far))
				     ((#\\)			`(((topic) ,(make-shared-substring special 1 (1- (string-length special))))
								  ,@so-far))
				     ((#\`)			(cond
								 ((char=? #\` (string-ref special 1))
								  `(,(string-append "\"" (make-shared-substring special 2 -2) "\"")
								    ,@so-far))
								 (#t
								  `(((output) ,(make-shared-substring special 1 (1- (string-length special))))
								    ,@so-far))))
				     ((#\{)			`(((output) ,(make-shared-substring special 1 (1- (string-length special))))
								  ,@so-far))
				     ((#\")			(let ((term (make-shared-substring special 1 (1- (string-length special)))))
								  (pk 'definition term)
								  `(((definition :term ,term) ,term)
								    ,@so-far)))
				     ((#\x)			(let ((reference (make-shared-substring special 6 (1- (string-length special)))))
								  `(((ref :name ,(normalize-label reference)))
								    ,@so-far)))
				     ((#\u)			(let ((reference (make-shared-substring special 5 (1- (string-length special)))))
								  `(((hyperlink :url ,reference)  ,reference)
								    ,@so-far)))
				     ((#\|)			(cond
								 ((and (char=? #\* (string-ref special 1)) (< 3 (string-length special)))
								  `(((index :term ,(make-shared-substring special 2 (1- (string-length special))) :primary))
								    ,@so-far))
								 ((and (char=? #\~ (string-ref special 1)) (< 3 (string-length special)))
								  `(((index :term ,(make-shared-substring special 2 (1- (string-length special)))))
								      ,@so-far))
								 ((and (char=? #\$ (string-ref special 1)) (< 3 (string-length special)))
								  `(((index :term ,(make-shared-substring special 2 (1- (string-length special)))
									    :categories (function)))
								      ,@so-far))
								 (#t
								  `(((index :term ,(make-shared-substring special 1 (1- (string-length special)))))
								      ,@so-far))))
				     (else			`(((output) ,special)
								  ,@so-far))))))
			 (lambda ()
			   (reverse (cons remaining so-far))))))))))


(define (normalize-label str)
  (apply string-append-with-separator
	 " "
	 (separate-fields-discarding-regexp (cached-regexp "[\n\t ]\\+")
					    (sans-surrounding-whitespace str)
					    list)))



(define ((make-cdml-reader nesting-rules) file)
  (let* ((str (untabify-string (file->string file)))
	 (docstr (c-source->embedded-documentation str)))
    (cdml-parse file
		docstr
		(lambda (parsed-ouput remaining-input)
		  (if (pair? remaining-input)
		      (throw 'improper-nesting (car remaining-input)))
		  parsed-ouput)
		nesting-rules)))

(xdml-name-reader 'c-comment (make-cdml-reader old-pdml-nesting-rules))

