;;; tag: Tom Lord Tue Dec  4 14:59:31 2001 (doc/parse-c-documentation.scm)
;;;
;;; snarf-c-documentation.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc parse-c-documentation)
  :use-module (doc pdml)
  :use-module (doc xdml)
  :use-module (doc hdml)
  :use-module (doc markup)
  :use-module (unix file-utils)
  :use-module (unix untabify)
  :use-module (unix filenames)
  :use-module (data-structures string-fun)
  :use-module (data-structures ratlist)
  :use-module (regexps subst)
  :use-module (regexps cached)
  :use-module (regexps structured))



(define (once x) x)

;;(s c-source->embedded-documentation)
;; (c-source->embedded-documentation str)
;; 
;; !!! actually returns a concatenation of strings
;; 
;; Given a string of C source code, return a list of strings, each of
;; which is a documentation comment.  The strings returned are shared
;; substrings of `str'.
;; 
;; A comment that begins with a match for:
;; 
;;	^[ \t]*/\(\*[ \n\t*]*\)\?\*(
;; 
;; begins a documentation comment.  Example matches:
;;
;;	/*(
;;
;;	/*************************
;;	 *(
;;
;; Example non-matches:
;;
;;	/*Foo		-- `F' does not match `('
;;
;;	/*************************
;;	 * (		-- ` ' does not match `('
;; 
;; A "documentation comment" may be extended to include source code if
;; it ends with a line matching:
;;
;;	^[ \t]*insert*/[ \t]*\n
;; 
;; In that case, all text up to a line matching:
;;
;;	^[ \t]*/*(end-insert)[ \t]*\n
;; 
;; is considered to be part of the documentation (even though it is
;; not enclosed in the comment itself). The comment continues
;; following "/*(end-insert)[ \t]*\n".  For example, in:
;; 
;;    /*(c #s"enum rx_opcode" :category type)
;;     * enum rx_opcode;
;;     * 
;;     * This type represents opcodes for a regular expression DFA virtual
;;     * machine.
;;     * 
;;     insert*/
;;     enum rx_opcode
;;     {
;;       rx_cache_miss = 1,
;;       [...]
;;     };
;;     /*(end-insert)
;;      *
;;	* `enum rx_opcode' is used by the function `rx_frob'.
;;      */
;; 
;; the declaration of `enum rx_opcode' is part of both the code (seen
;; by a C compiler), and the documentation.  In the documentation, the
;; declaration is treated as pre-formatted text
;;
;;

(define-public (c-source->embedded-documentation str)
  (define (build-list str)
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
		      (cons doc (build-list rest)))
		    (lambda () ())))))

  (apply string-append-with-separator
	 "\n"
	 (build-list str)))


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
						    ,(documentation-lparen-pattern)
						    ,(documentation-body-pattern)
						    "*/"))


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
;;	/*(c main)
;;      ^^
;;	  \
;;         indicated characters are the leading-comment-decoration
;;	  /
;;	  vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;;	/*******************************************************
;;	 *(h1 "Section Eight"
;;      ^^
;;
(define (leading-comment-decoration-pattern) 	`(& "/*"
						    (? (& (* ([] "* \t\n"))
							  "*"))))

;; documentation-lparen-pattern
;; 
;; 	"("
;;
(define (documentation-lparen-pattern) "(")


;; documentation-body-pattern
;; 
;; Match the complete text of a documentation comment.  This may
;; include embedded code.
;;

(define (documentation-body-pattern)		`(& ,(documentation-body-line-pattern)
						    (* (& "\n"
							  ,(documentation-body-line-pattern)))))

(define (documentation-body-line-pattern)	`(| (& (* (| ([^] "\n*")
							     (& "*" ([^] "\n/"))))
						       (? "*"))
						    (& ,(linear-space-pattern)
						       "insert"
						       ,(linear-space-pattern)
						       "*/"
						       ,(linear-space-pattern)
						       "\n"
						       ,(not-documentation-pattern)
						       ,(leading-comment-decoration-pattern)
						       "(end-insert)"
						       ,(linear-space-pattern))))


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
						       (& (| ([^] "( \t\n*")
							     (& ,(mandatory-whitespace-pattern) ([^] "* \t\n")))
							  (* (| ([^] "*") (& "*" ([^] "/"))))
							  ,(whitespace-pattern)
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

;; digit-range
;;
(define (digit-range)				'(#\0 #\9))


;; (digit-pattern)
;; 
(define (digit-pattern)				`([] ,(digit-range)))

;; alnum-pattern
;;
(define (alnum-pattern)				`([] ,(lowercase-range)
						     ,(uppercase-range)
						     ,(digit-range)))


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
							   (? ([] (,(octal-digit-range)))))))

						  ;; Or an escaped character (not octal)
						  ;;
						  (& "\\" ([^] (,(octal-digit-range))))

						  ;; Or a normal character.
						  ;;
						  (| ([^] ("\\\\'\"")))))




;; !!!
(define (documentation-tag-pattern)
  `(& ,(letters-pattern)
      (* (| "-" ,(alnum-pattern)))))



;;(s cdml-parse)
;; (cdml-parse filename str return nesting-rules :optional containing-tag)
;; 
;;	containing-tag 	- if not #f, parse only a subdocument of this tag type
;; 	filename 	- name of source file
;;
;;	str	 	- unparsed documentation comments
;;			  or a pair:
;;				(already-parsed . remaining-str)
;;
;;	return   	- (return parsed-document remaining-str)
;;
;;			  or if the parsed-document is not a valid subdocument 
;;			  of containing-tag, either:
;;	
;;			  (return () `(,parsed-document . ,remaining-str))
;;
;;			   or
;;
;;			  (return () str)
;;			  
;;			  
;;	can-contain? 	- a procedure:
;;				(can-contain? tag maybe-nested-tag)
;; 
;; If `containing-tag' is not #f and str begins with an illegal tag
;; 
(define-public (cdml-parse filename str return can-contain? :optional containing-tag)
  (if (pair? str)
      ;; We've already parsed the first part of the input string
      ;; so `str' is:
      ;;		(parsed-document . remaining-str)
      ;;
      ;; If `parsed-document' is a valid subdocument of `containing-tag', then
      ;; 
      ;;		(return parsed-document remaining-str)
      ;;
      ;; otherwise:
      ;;
      ;;		(return () `(,parsed-document . ,remaining-str))
      ;;
      (parse-hdml (car str)
		  (lambda (tag parameters . sub-docs)
		    (if (and containing-tag
			     (not (can-contain? containing-tag tag)))
			(return () str)
			(return (car str) (cdr str)))))


      ;; We haven't already parsed the first part of the input string, do so now:
      ;;
      (;; Extract the initial left-parenthesis and tag name from the comment:
       ;;
       (once (regexec-function `(? ,(documentation-tag-pattern))
			       #f
			       #f
			       '0))
       ;; Perform that extraction on the comment stripped of the leading comment decoration (so it
       ;; begins with just the parenthesis:
       ;;
       (regexec (once (regcomp (compile-regexp `(& (^) (? (& ,(leading-comment-decoration-pattern) "("))) first-value)))
		str
		'>
		'REG_NO_SUBEXP_REPORTING)
       
       (lambda (tag)
	 (if (= 0 (string-length tag))
	     ;; Oops, there were no more documentation comments in the string:
	     ;;
	     (return () str)

	     ;; There is a documentation comment:
	     ;;
	     (let ((tag (string->symbol tag))

		   ;; (first-comment-regexp-fn str return)
		   ;; 
		   ;; invokes:
		   ;;
		   ;;		(return first-comment rest-of-str insert-flag)
		   ;;
		   ;; where
		   ;;
		   ;;		insert-flag is 1 for most documentation comments,
		   ;;		  and 2 for documentation comments ending with 
		   ;;		  "insert*/\n"
		   ;;
		   (first-comment-regexp-fn (once (regexec-function `(& "\n"
									,(linear-space-pattern)
									(| (& "insert*/" ,(linear-space-pattern) "\n" (/ 2))
									   (& "*/" ,(linear-space-pattern) (? "\n") (/ 1))))
								    #f
								    #f
								    '( (< 0)
								       >
								       state-label))))
		   ;; (comment-sans-decoration comment-str)
		   ;;
		   ;; returns the documentation comment without leading decoration (e.g. "/***\n * "), 
		   ;; trailing decoration (e.g. " */\n"), or beginning of line decoration (e.g. " * ").
		   ;;
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
			     (sans-leading-decoration		(regexp-subst (once (cached-regexp (compile-regexp (leading-comment-decoration-pattern) first)))
									      sans-beginning-of-line-decoration
									      (lambda (s) ""))))
			sans-leading-decoration))))

	       (if (and containing-tag
			(not (memq tag '(include-documentation)))
			(not (can-contain? containing-tag tag)))

		   ;; If we know now that the tag can not nest in `containing-tag',
		   ;; retrn immediately.
		   ;;
		   (return () str)

		   
		   (first-comment-regexp-fn
		    str
		    (lambda (first-comment rest-of-string insert-tag)
		      (with-input-from-string (comment-sans-decoration first-comment)
			(lambda ()
			  (let* ((header (read))
				 (rest-of-first-comment (fd->string (current-input-port)))
				 (rest-of-first-comment (regexec (once (cached-regexp "[[:([\t ]*\n):]]\\?"))	;; !!! comment-sans-decoration should do this
								 rest-of-first-comment
								 '>)))
			    (cond
			     ;; If this comment is an include directive, process it.  E.g.:
			     ;;
			     ;;		/*(include-documentation "filename.c")
			     ;;		 */
			     ;;
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
								  ;; Include files are parsed to produce a single subdocument
								  ;; which may or may not nest within `containing-tag':
								  ;;
								  (cdml-parse included
									      docstr
									      (lambda (doc ign)
										(parse-hdml doc
											    (lambda (tag parameters . subdocuments)
											      (if (and containing-tag
												       (can-contain? containing-tag tag))
												  (return doc rest-of-string)
												  (return () (cons doc rest-of-string))))))
									      can-contain?)))
			     (#t
			      ;; Not an include directive:  iterate this loop at least once,
			      ;; then once more for each comment ending with "insert*/":
			      ;;
			      (let loop (;; Does the comment end with "insert*/" indicating
					 ;; that the text following is a pre-formatted paragraph?
					 ;;
					 (insert? (= 2 insert-tag))

					 ;; Parse the body of the comment into paragraphs.  Reverse the result.
					 ;;
					 (subdocument-list (reverse (cdml-parse-paragraphs rest-of-first-comment :first-preformatted)))
					 
					 ;; 
					 ;;
					 (rest-of-string rest-of-string))
				(if (not insert?)
				    ;; No pre-formatted text follows the paragraphs just parsed.
				    ;; We now have a document and perhaps some subdocuments
				    ;; for that document (in reverse order, in `subdocument-list').
				    ;;
				    ;; Subsequent comments may contain additional subdocuments.
				    ;; We determine that by examining tags: if a following comment
				    ;; begins with a tag that can nest in this document, then 
				    ;; that comment defines a subdocument of this document.
				    ;;
				    ;; When no more subdocuments are found, return:
				    ;; 
				    (let subdocuments-loop ((subdocument-list subdocument-list)
							     (rest-of-string rest-of-string))
				      (cdml-parse filename
						  rest-of-string
						  (lambda (subdocument rest-of-string)
						    (if (not subdocument)
							(return (cons header (reverse subdocument-list))
								rest-of-string)
							(subdocuments-loop (cons subdocument subdocument-list)
									    rest-of-string)))
						  can-contain?
						  tag))

				    ;; The comment ends with "insert*/".  Find the end of the pre-formatted
				    ;; text:
				    ;;
				    ((once (regexec-function `(& (= :last-newline "\n")
								 ,(linear-space-pattern)
								 "/*"
								 ,(linear-space-pattern)
								 "(end-insert)")
							     #f
							     #f
							     '( (< (@ :last-newline))
								((0 1) >) )))
				     rest-of-string
				     (lambda (insert rest-of-string)
				       ;; Add the preformatted text to the list of paragraphs.
				       ;; Add the commented paragraphs that follow the "/*(end-insert)" 
				       ;; directive.  Find out if the comment that begins with
				       ;; "/*(end-insert)" ends with another "insert*/".  Iterate
				       ;; this loop:
				       ;;
				       (first-comment-regexp-fn
					rest-of-string
					(lambda (first-comment-next-part rest-of-string insert-tag)
					  (loop (= 2 insert-tag)
						(append (reverse (cdml-parse-paragraphs (comment-sans-decoration first-comment-next-part)))
							(cons `((insert) ,insert) subdocument-list))
						rest-of-string)))))))))))))))))))))
				     

;; (cdml-parse-paragraphs str . options)
;; 
;; Parse the body of a comment into paragraphs.
;;
;; If first paragraph begins on the first line, and the option
;; `:first-preformatted' is specified, then treat the first paragraph
;; as pre-formatted text.
;;
;; 

(define (cdml-parse-paragraphs str . options)
  (let* (;; Paragraphs are separated by blank lines:
	 ;;
	 (paragraph-list (separate-fields-discarding-regexp-pt "\n\\([[:([ \t]*\n):]]\\+\\)"
							       1
							       (regexec (once (regcomp "^[[:([ \t]*\n):]]*")) str '>)
							       list))
	 (first-paragraph (and paragraph-list
			       (memq :first-preformatted options)
			       (regexec (once (regcomp "^[ \t]*[^\n \t]")) str #f) ; comment-sans-decoration should remove the *
			       (car paragraph-list)))
	 (normal-paragraphs (remove-if string-null?
				       (if first-paragraph
					   (cdr paragraph-list)
					   paragraph-list))))
    (append (and first-paragraph
		 `(((insert) ,first-paragraph)))
	    (map (lambda (normal-paragraph) (cdml-translate-paragraph 'p normal-paragraph)) normal-paragraphs))))
	

;; (cdml-translate-paragraph tag paragraph)
;; 
;; Translate the body of a paragraph into structured text and build a 
;; document for the paragraph:
;; 
(define (cdml-translate-paragraph tag paragraph)
  (cond
   ;; Trivial translation of empty paragraphs:
   ;;
   ((string-null? paragraph)				`((p) ,paragraph))

   ;; If the first line begins with whitespace, treat it as pre-formatted
   ;; text:
   ;;
   ((char-whitespace? (string-ref paragraph))		`((insert) ,paragraph))

   
   (#t
    ;; If a paragraph begins with a lower-case letter, don't indent that paragraph
    ;; (assume it continues the previous paragraph).
    ;;
    `((,tag ,@(and (char-lower-case? (string-ref paragraph)) '(:noindent)))
	,@(let loop ((remaining paragraph)
		     (so-far ()))
	    (if (string-null? remaining)
		(reverse so-far)
		;; 
		;;	example:		tag:
		;;
		;;	*foo*		-- 	stress
		;;	23xyz		-- 	output
		;;	url:foo		--	hyperlink
		;;	xref:"label"	--	ref
		;;	`foo'		--	output
		;;	{foo}		--	output
		;;	"term"		--	definition
		;;	#t		--	code
		;;	#f		-- 	code
		;;	\foo/		--	in-line topic heading
		;;	|term|		--	index entry for `term'
		;;	|*term|		--	primary index entry for `term'
		;;	
		(regexec (once (regcomp "\\*[a-zA-Z0-9][-0-9a-zA-Z,. ]*[0-9a-zA-Z,. ]\\*\\|\\\\[0-9a-zA-Z][-0-9a-zA-Z ,.:]*[0-9a-zA-Z,.:]/\\|url:\"[^\"]*\"\\|xref:\"[^\"]*\"\\|`[^'`][^']*'\\|{[^}]*}\\|\"[^\"]\"\\|#t\\|#f\\|[\n\t\ ]-\\?[0-9]\\+[\n\t\ .,]\\|#\\\\[a-zA-Z0-9]\\+\\||||\\||\\*|\\||\\*\\?[][_0-9a-zA-Z(){}\\\\$^+-*/ ,.:]*|")) ; !!!
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
				     ((#\*)			`(((stress) ,(make-shared-substring special 1 (1- (string-length special))))
								  ,@so-far))
				     ((#\\)			`(((topic) ,(make-shared-substring special 1 (1- (string-length special))))
								  ,@so-far))
				     ((#\`)			`(((output) ,(make-shared-substring special 1 (1- (string-length special))))
								  ,@so-far))
				     ((#\{)			`(((output) ,(make-shared-substring special 1 (1- (string-length special))))
								  ,@so-far))
				     ((#\")			(let ((term (make-shared-substring special 1 (1- (string-length special)))))
								  `(((definition :term ,term) ,term)
								    ,@so-far)))
				     ((#\x)			(let ((reference (make-shared-substring special 6 (1- (string-length special)))))
								  `(((ref :name ,(normalize-label reference)))
								    ,@so-far)))
				     ((#\u)			(let ((reference (make-shared-substring special 5 (1- (string-length special)))))
								  `(((hyperlink :url ,reference)  ,reference)
								    ,@so-far)))
				     ((#\|)			(if (and (char=? #\* (string-ref special 1)) (< 3 (string-length special)))
								    `(((index :term ,(make-shared-substring special 2 (1- (string-length special))) :primary))
								      ,@so-far)
								    `(((index :term ,(make-shared-substring special 1 (1- (string-length special)))))
								      ,@so-far)))
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



(define ((make-cdml-reader language) file)
  (let* ((str (untabify-string (file->string file)))
	 (docstr (c-source->embedded-documentation str)))
    (cdml-parse file
		docstr
		(lambda (parsed-ouput remaining-input)
		  (if (pair? remaining-input)
		      (throw 'improper-nesting (car remaining-input)))
		  parsed-ouput)
		(let ((table (make-hash-table 127)))
		  (lambda (container contained)
		    (let ((key (cons container contained)))
		      (case (hash-ref table key)
			((yes)		#t)
			((no)		#f)
			(else		(let* ((containable 	(tag-containables language container))
					       (answer 		(if (memq contained containable)
								    'yes
								    'no)))
					  (hash-set! table key answer)
					  (eq? answer 'yes))))))))))

(define r (make-cdml-reader 'pdml))

; (xdml-name-reader 'c-comment (make-cdml-reader old-pdml-nesting-rules))


