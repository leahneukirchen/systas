;;; tag: Tom Lord Tue Dec  4 14:59:32 2001 (doc/xdml-texinfo.scm)
;;;
;;; xdml-texinfo.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc xdml-texinfo)
  :use-module (doc old-xdml)
  :use-module (doc hdml)
  :use-module (data-structures string-fun)
  :use-module (data-structures ratlist)
  :use-module (regexps subst)
  :use-module (regexps cached)
  :use-module (standard string-parsing)
  :use-module (unix output-files)
  :use-module (unix filenames))




(define-public (add-xdml-texinfo-translation)
  (xdml-define-translation 'texinfo xdml-texinfo-pass1 xdml-texinfo-pass2))

(define (xdml-input-file->texinfo-output-file input-file)
  (string-append (filename-sans-extension (filename-nondirectory input-file)) ".texi"))

(define (xdml-texinfo-pass1 input-file document database nesting-rules :optional parser-extension ->texinfo-extension)
  (let ((filename (xdml-input-file->texinfo-output-file input-file)))
    (dynamic-wind
     (lambda ()
       (database 'set! 'current-filename filename))
     (lambda () (xdml->texinfo document database nesting-rules parser-extension ->texinfo-extension))
     (lambda ()
       (database 'remove! 'current-filename)))))

(define (xdml-texinfo-pass2 input-file texinfo-translation database-fn . kw-params)
  (with-overwriting-output-file (string-append (filename-sans-extension input-file) ".texi")
    (lambda ()
      (display texinfo-translation))))



(define-public (xdml->texinfo document database nesting-rules :optional parser-extension ->texinfo-extension)
  (xdml-parse document
	      (lambda (tag parameters sub-documents)
		(->texinfo document tag parameters sub-documents database nesting-rules parser-extension ->texinfo-extension))
	      #f
	      nesting-rules
	      parser-extension))



(define (new-index-name database)
  (let ((old-count (or (database 'ref 'index-number) (char->integer #\a))))
    (database 'remove! 'index-number)
    (database 'set! 'index-number (1+ old-count))
    (string "x" old-count)))

(define (->texinfo document tag parameters sub-documents database nesting-rules parser-extension ->texinfo-extension)

  (define (sub-document->texinfo sd)
    (xdml->texinfo sd database nesting-rules parser-extension ->texinfo-extension))

  (define (all-sub-documents->texinfo)
    (apply lazy-string-append (map sub-document->texinfo sub-documents)))

  (case tag
    ((book)		(let* ((source (or (database 'ref 'current-filename)
					   "  ** unknown file ** "))
			       (rootname (filename-sans-extension (filename-nondirectory source)))
			       (info-file (string-append rootname ".info"))
			       (title (texinfo-section-title (car parameters)))
			       (subtitles (map texinfo-section-title (kw-arg-ref parameters :subtitles)))
			       (authors  (kw-arg-ref parameters :authors))
			       (one-line-summary (kw-arg-ref parameters :one-line-summary))
			       (keywords (kw-arg-ref parameters :keywords))
			       (copyright-years (map ->string (kw-arg-ref parameters :copyright-years)))
			       (copyright (kw-arg-ref parameters :copyright))
			       (publisher (kw-arg-ref parameters :publisher))
			       (contains (map texinfo-section-title (kw-arg-ref parameters :contains)))
			       (on-line-permissions (or (apply string-append-with-separator "\n" (kw-arg-ref parameters :permissions)) ""))
			       (printed-permissions (or (apply string-append-with-separator "\n" (kw-arg-ref parameters :permissions)) "")))

			  #/lazy-string-append
			  /
			  \input texinfo   @c -*-texinfo-*-
			  @c %**start of header
			  @setfilename #,info-file
			  @settitle #,title
			  @setchapternewpage odd
			  @c %**end of header
			  @defindex fu
			  @defindex ma
			  @defindex va
			  @defindex ty
			  @defindex pr
			  @defindex ge
			  #,(delay
			      (let ((indexes (database 'ref 'indexes)))
				(if (not indexes)
				    ""
				    (apply string-append
					   (map (lambda (index)
						  (apply string-append
							 "@defindex " (car index) "\n"
							 (map (lambda (category)
								(let ((code (category->code category)))
								  (string-append "@synindex " code " " (car index) "\n")))
							      (cddr index))))
						indexes)))))#;
			  @ifinfo
			  #,(texinfo-quote one-line-summary)
			  
			  Copyright #,(texinfo-quote (apply string-append-with-separator " " copyright-years)) #,(texinfo-quote copyright)

			  #,(texinfo-quote on-line-permissions)
			  @end ifinfo
			  
			  @c  This title page illustrates only one of the
			  @c  two methods of forming a title page.
			  @titlepage
			  @title #,title
			  #,(apply string-append (map (lambda (x) (string-append "@subtitle " x "\n")) (map texinfo-quote subtitles)))#;
			  @subtitle @i{#,(texinfo-quote one-line-summary)}
			  #,(apply string-append (map (lambda (x) (string-append "@author " x "\n")) (map texinfo-quote authors)))#;
			  
			  @c  The following two commands
			  @c  start the copyright page.
			  @page
			  @vskip 0pt plus 1filll
			  Copyright @copyright{} #,(texinfo-quote (apply string-append-with-separator " " copyright-years)) #,(texinfo-quote copyright)
			  
			  #,(if (not publisher) "" (string-append "Published by " (texinfo-quote publisher) "\n"))
			  
			  #,(texinfo-quote printed-permissions)
			  @end titlepage

			  @ifinfo
			  @node Top
			  @top
			  
			  @center @titlefont{#,(noop title)}
			  #,(apply string-append (map (lambda (s) (string-append "@center " s "\n")) subtitles))

			  #,(texinfo-quote one-line-summary)
			  
			  #,(if (not keywords)
				""
				(apply string-append "Keywords: " (apply string-append-with-separator ", " keywords) "\n"))
			  @end ifinfo
			  
			  @menu
			  #,(apply string-append (map (lambda (c) (if (not c) "" (string-append "* " c "::\n"))) contains))#;
			  @end menu
			  
			  #,(delay
			      (apply string-append
				     (map (lambda (c)
					    (let* ((file (database 'file-of `(texinfo-label ,c)))
						   (file (and file (string-append (filename-sans-extension (filename-nondirectory file)) ".texi"))))
					      (if (or (not file) (string=? file info-file))
						  ""
						  (string-append "@include " file "\n"))))
					  contains)))#;
			  @shortcontents
			  @contents
			  @bye
     			  /#))

    ((h0 h1 h2 h3)	(let* ((section-title (texinfo-section-title (car parameters)))
			       (section-subtitle (and=> (kw-arg-ref parameters :subtitle) texinfo-section-title))
			       (section-includes (kw-arg-ref parameters :includes))
			       (page-needed (or (kw-arg-ref parameters :need) 3200))
			       (force-page (->bool (memq :page parameters)))
			       (node-name section-title)
			       (containing-node (database 'ref 'current-node))
			       (this-is-a-node? (or (not containing-node) (database 'ref `(texinfo-has-menu? ,containing-node))))
			       (index (kw-arg-ref parameters :index))
			       (unnumbered? (memq :unnumbered parameters))
			       (section-type (case tag
					       ((h0)	(cond
							 (index					"centerchap")
							 (unnumbered?				"centerchap")
							 ((memq :appendix parameters)		"appendix")
							 (#t					"chapter")))
					       ((h1)	(cond
							 (unnumbered?				"unnumberedsec")
							 (#t					"section")))
					       ((h2)	(cond
							 (unnumbered?				"unnumberedsubsec")
							 (#t					"subsection")))
					       ((h3)	(cond
							 (unnumbered?				"unnumberedsubsubsec")
							 (#t					"subsubsection"))))))
			  (if index
			      (let ((old-indexes (database 'ref 'indexes)))
				(set! index (cons (new-index-name database)
						  (cons section-title index)))
				(database 'remove! 'indexes)
				(database 'set! 'indexes (cons index old-indexes))))

			  (if containing-node
			      (database 'set!
					`(texinfo-menu ,containing-node)
					(append (database 'ref `(texinfo-menu ,containing-node))
						(list node-name))))

			  (cond
			   (this-is-a-node?	(database 'set! `(texinfo-label ,node-name) node-name))
			   (containing-node	(database 'set! `(texinfo-label ,node-name) containing-node)))

			  (dynamic-wind
			   (lambda ()
			     (set! containing-node (database 'ref 'current-node))
			     (database 'set! 'containing-nodes
				       (cons node-name
					     (and containing-node
						  (database 'ref 'containing-nodes))))
			     (database 'set! 'current-node node-name))

			   (lambda () (let* ((sub-texinfo (all-sub-documents->texinfo)))
					#/lazy-string-append
					/
					#,(if force-page
					      "@page\n"
					      (string-append "@need " (->string page-needed) "\n"))
					#;;;;
					#; The @node line.
					#;
					#,(delay (if (not this-is-a-node?)
						     ""
						     #/string-append
						     /
						     @node #,section-title
						     /#))#;
					#;;;;
					#; The @chapter or @(sub(sub))section line.
					#;
					@#,(val section-type) #,section-title
					#,(if (not section-subtitle)
					      ""
					      (string-append "@strong{" section-subtitle "}\n@sp 2\n"))#;
					#,(if (not section-includes)
					      ""
					      (string-append
					       "@noindent\n"
					       (apply string-append
						      (map (lambda (include)
							     (string-append "@strong{#include <" include ">}@*\n"))
							   section-includes))
					       "@sp 1\n"))#;
					#;;;;
					#; Contents of the chapter or section:
					#;
					#,sub-texinfo

					#,(if (not index)
					      ""
					      #/string-append
					      /
					      @printindex #,(car index)

					      @sp 3
					      /#)
					/#))

			   (lambda ()
			     ;; (set! node-name (database 'ref 'current-node))
			     (database 'set! 'containing-nodes
				       (cdr (database 'ref 'containing-nodes)))
			     (if containing-node
				 (database 'set! 'current-node containing-node)
				 (database 'remove! 'current-node))))))
			    
    

    ((menu)		(let* ((node (database 'ref 'current-node))
			       (menu-key `(texinfo-menu ,node))
			       (has-menu?-key `(texinfo-has-menu? ,node)))
			  (if (not node)
			      (throw 'menu-outside-of-node))
			  (database 'set! has-menu?-key #t)
			  (delay
			    #/string-append
			    /
			    @menu
			    #,(let ((menu (database 'ref menu-key)))
				(apply string-append
				       (map (lambda (menu-item)
					      (string-append "* " (texinfo-section-title menu-item) "::\n"))
					    menu)))#;
			    @end menu

			    /#)))

    ;; Ordinary paragraph:
    ;;
    ((p)			#/lazy-string-append
				/
				
				#,(if (not (memq :noindent parameters))
				      ""
				      "@noindent\n")#;
				#,(all-sub-documents->texinfo)
				/#)

    ;; Pre-formatted, indented, paragraph:
    ;;
    ((insert)		(let* ((lines (separate-fields-after-char #\nl (car sub-documents) list))
			       (lines-grouped-naturally (let loop ((groups ())
								   (line-list lines))
							  (cond
							   ((null? line-list)			(cond
												 ((null? groups)		())
												 ((< 5 (length (car groups)))	(apply-to-args (reverse (car groups))
																  (lambda (a b c d e . rest)
																    (reverse (cons (list e d c b a)
																		   (cons (reverse rest)
																			 (cdr groups)))))))
												 (#t				(reverse groups))))

							   ((blank-line? (car line-list))	(let inner ((blanks (cons (car line-list) ()))
													    (line-list (cdr line-list)))
												  (if (or (not line-list)
													  (not (blank-line? (car line-list))))
												      (loop (cons (reverse blanks) groups)
													    line-list)
												      (inner (cons (car line-list) blanks)
													     (cdr line-list)))))

							   (#t					(let inner ((non-blanks (cons (car line-list) ()))
													    (line-list (cdr line-list)))
												  (if (or (not line-list)
													  (blank-line? (car line-list)))
												      (loop (cons (reverse non-blanks) groups)
													    line-list)
												      (inner (cons (car line-list) non-blanks)
													     (cdr line-list))))))))
			       (lines-groups (apply append
						    (map (lambda (natural-group)
							   (if (> 10 (length natural-group))
							       (list natural-group)
							       (apply-to-args (reverse natural-group)
								 (lambda (e d c b a . rest)
								   (let ((last-group (list a b c d e)))
								     (let loop ((groups ())
										(line-list (reverse rest)))
								       (cond
									((null? line-list)		(reverse (cons last-group groups)))
									((> 5 (length line-list))	(loop (cons line-list groups) ()))
									(#t				(apply-to-args line-list
													  (lambda (a b c d e . rest)
													    (loop (cons (list a b c d e) groups)
														  rest)))))))))))
							 lines-grouped-naturally))))
			  #/lazy-string-append
			  /
			  @example
			  #,(apply string-append
				   (map (lambda (group)
					  #/string-append
					  /
					  @group
					  #,(texinfo-quote (apply string-append group))#;
					  @end group
					  /#)
					lines-groups))#;
			  @end example
			  /#))

    ;; The hierarchy of "section", "sub-section", and "sub-sub-section":
    ;;
    ((paragraphs)			(all-sub-documents->texinfo))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Mark-ups for ordinary text, as in the middle of a paragraph.
    ;;

    ;; The empty document:
    ;;
    (()					"")

    ;; A leaf node in the document tree.
    ;;
    ((string)				(texinfo-quote document))

    ;; A list of other pieces of text:
    ;;
    ((text) 				(all-sub-documents->texinfo))

    ((stress)				#/lazy-string-append
					/
					@emph{#,(all-sub-documents->texinfo)}/#)

    ;; emphar_sized text.
    ;;
    ((emph)				#/lazy-string-append
					/
					@emph{@strong{#,(all-sub-documents->texinfo)}}/#)

    ((topic)				#/lazy-string-append
					/
					@strong{#,(all-sub-documents->texinfo)}/#)

    ((input)				#/lazy-string-append
					/
					@kbd{#,(all-sub-documents->texinfo)}/#)

    ((output)				#/lazy-string-append
					/
					@code{#,(all-sub-documents->texinfo)}/#)

    ;; Indexes
    ;;
    ;; Index items generate index entries and produce no typeset output.
    ;;
    ((index)				(let ((term (kw-arg-ref parameters :term))
					      (categories (or (if (kw-arg-ref parameters :category)
								  (list (kw-arg-ref parameters :category))
								  (kw-arg-ref parameters :categories))
							      '(general))))
					  #/lazy-string-append
					  /
					  #,(if (not categories)
						(string-append "\n@cindex " (texinfo-quote term) "\n")
						(apply string-append
						       (map (lambda (category)
							      #/string-append
							      /
							      @#,(category->code category)index #,(texinfo-quote term)
							      /#)
							    categories)))/#))

    ;; idx is short-hand:
    ;;
    ;;		(idx () "foo") 
    ;;
    ;; is equivalent to:
    ;;
    ;;		((text) ((index :term "foo")) "foo")
    ;;
    ((idx)				(sub-document->texinfo `((text) ((index :term ,(car sub-documents)))
								      ,@sub-documents)))

    ;; Definitions:
    ;;
    ;; Definitions generate index entries and are typeset specially.
    ;;
    ((definition)			(let ((term (kw-arg-ref parameters :term))
					      (categories (kw-arg-ref parameters :categories)))
					  #/lazy-string-append
					  /
					  #,(sub-document->texinfo
					     `((index :term ,term :categories ,categories)))#;
					  @dfn{#,(all-sub-documents->texinfo)}/#))

    ;; dfn is short-hand:
    ;;
    ;;		(dfn () "foo") 
    ;;
    ;; is equivalent to:
    ;;
    ;;		(definition (:term "foo") "foo")
    ;;
    ((dfn)				(sub-document->texinfo
					 (apply xdml 'definition `(:term ,(car sub-documents)) sub-documents)))

    ;; label is a point of possible cross-reference.
    ;; 
    ;; In texinfo, a label applies to the containing node.
    ;;
    ;; Chapters and sections are implicitly labeled with the
    ;; chapter/section name.
    ;;
    ((label)				(let* ((label (kw-arg-ref parameters :name))
					       (node (or (database 'ref 'current-node)
							 " *** unknown node *** ")))
					  (if (database 'ref `(texinfo-label ,label))
					      (throw 'multiply-defined-label label))
					  (database 'set-unique! `(texinfo-label ,label) node)
					  ""))


    ;; (ref :name label)

    ;; In Texinfo, this produces @ref{some-node-name}.
    ;;
    ((ref)				(let ((containing-node (database 'ref 'current-node)))
					  (delay (let* ((label (kw-arg-ref parameters :name))
							(node (or (database 'ref `(texinfo-label ,label))
								  (throw 'undefined-label label))))
						   
						   (if (and (not (string=? label node))
							    (string=? node containing-node))
						       (string-append "@strong{" label "}")
						       #/string-append
						       /
						       #,(if (string=? label node)
							     ""
							     (string-append "@strong{" label "} in "))#;
						       @ref{#,(string node)}#;
						       /#)))))
							   

    ;; Hyperlink is a URL.
    ;;
    ((hyperlink)			#/lazy-string-append
					/
					@url{#,(texinfo-quote (kw-arg-ref parameters :url))} #;
					#,(if (or (not sub-documents)
						  (and (string=? (kw-arg-ref parameters :url)
								 (car sub-documents))
						       (null? (cdr sub-documents))))
					      ""
					      #/lazy-string-append
					      /
					      (#,(char #\")#,(all-sub-documents->texinfo)#,(char #\")) /#)#;
					/#)

    ;; url is shorthand:
    ;; 
    ;; 		(url () "http://555-1212.com")
    ;; 
    ;; stands for:
    ;; 
    ;;		((hyperlink :url "http://555-1212.com")
    ;;		 "http://555-1212.com")
    ;;
    ((url)				(sub-document->texinfo
					 `((hyperlink :url ,(car sub-documents))
					   ,(car sub-documents))))



    ((list)				#/lazy-string-append
					/
					@itemize @bullet
					#,(all-sub-documents->texinfo)
					@end itemize
					/#)

    ((enum)				#/lazy-string-append
					/
					@enumerate
					#,(all-sub-documents->texinfo)
					@end enumerate
					/#)

    ((item)				#/lazy-string-append
					/
					@item
					#,(all-sub-documents->texinfo)/#)

    (else				(if ->texinfo-extension
					    (->texinfo-extension document
							       tag
							       parameters
							       sub-documents
							       database
							       nesting-rules
							       parser-extension
							       sub-document->texinfo)
					    (throw 'xdml-bad-syntax tag parameters sub-documents)))))




(define (mangle->filename str)
  (regexp-subst (once (cached-regexp "[[:([^0-9A-Za-z-]\\|[ \t\n]):]]\\+"))
		(string-downcase! (string str))
		(lambda ign "-")
		:global))

(define-public (texinfo-quote str)
  (regexp-subst (once (regcomp "[@{}]"))
		str
		(lambda (s) (string-append #\@ s))
		:global))



(define-public (texinfo-section-title str)
  (and str (apply join-fields-with "--" (separate-fields-discarding-char #\: str))))

(define (category->code category)
  (case category
    ((function)					"fu")
    ((macro)					"ma")
    ((variable)					"va")
    ((type)					"ty")
    ((program)					"pr")
    ((general)					"ge")
    (else					"ge")))

(define (blank-line? s)
  (->bool (regexec (once (regcomp "^[\t ]*\n\\?$")) s)))
