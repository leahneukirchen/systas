;;; tag: Tom Lord Tue Dec  4 14:59:33 2001 (doc/xdml.scm)
;;;
;;; xdml.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc xdml)
  :use-module (doc hdml)
  :use-module (doc markup)
  :use-module (doc texinfo)
  :use-module (doc html)
  :use-module (data-structures ratlist))



(declare-markup-language 'xdml
			 :synopsis "A simple markup-language for documents strucutred like books."
			 :documentation (value #/quote
					       /
					       XDML is a basic markup language for hierarchically structured
					       documents.  It provides for a book, divided into chapters,
					       sections, and three levels of sub-sectioning.  XDML books may
					       include a title page, bibliographic information page, table of
					       contents, and multiple indexes.  Paragraphs may contain indexed
					       terms, definitions, various kinds of emphar_sized text,
					       cross-references, URLs, pre-formatted text, lists, and
					       enumerations.

					       XDML can currently be rendered into HTML or texinfo.  

					       XDML is not a superset of any other markup language.
					       /#)

			 :parents ()

			 :formatter-entry-points `((html		,(lambda (filename document database format-sub-document)
									   (reset-html-file-gensym-counter database filename)
									   (dynamic-wind
									    (lambda () (database 'set! 'current-filename filename))
									    (lambda () (format-sub-document document))
									    (lambda () (database 'remove! 'current-filename)))))

						   (texinfo		,(lambda (filename document database format-sub-document)
									   (dynamic-wind
									    (lambda () (database 'set! 'current-filename filename))
									    (lambda () (format-sub-document document))
									    (lambda () (database 'remove! 'current-filename)))))))



(define-public xdml-document-type-tags	'(book))
(define-public xdml-level-0-tags	'(h0))
(define-public xdml-level-1-tags	'(h1))
(define-public xdml-level-2-tags	'(h2))
(define-public xdml-level-3-tags	'(h3))

(define-public xdml-block-of-text-leaf-tags	 	`(menu))
(define-public xdml-ordinary-block-of-text-tags 	`(p))
(define-public xdml-block-of-text-string-leaf-tags 	`(insert))
(define-public xdml-recursive-block-of-text-tags 	`(paragraphs))

(define-public xdml-block-of-text-tags			(unionq xdml-block-of-text-leaf-tags             
								xdml-ordinary-block-of-text-tags         
								xdml-block-of-text-string-leaf-tags      
								xdml-recursive-block-of-text-tags))


(define-public xdml-ordinary-structured-text-tags 	'(text
							  definition
							  stress
							  emph
							  topic
							  hyperlink))

(define-public xdml-structured-text-leafs 		'(string output input label index ref list enum))

(define-public xdml-structured-text-tags		(unionq xdml-ordinary-structured-text-tags
								xdml-structured-text-leafs))

(define-public xdml-list-item-tags			'(item))





(define (html-format-book-or-section document tag parameters sub-documents database format-sub-document)
  (apply-to-args parameters
    (lambda (section-name . kw-args)
      (let* ((html-tag (case tag
			 ((book)	"h1")
			 ((h0)		"h2")
			 ((h1)		"h3")
			 ((h2)		"h4")
			 ((h3)		"h5")))
	     (html-attr (case tag
			  ((book)	"align=center")
			  ((h0)		"align=center")
			  ((h1)		"")
			  ((h2)		"")
			  ((h3)		"")))
	     (current-filename		(database 'ref 'current-filename))
	     (file-has-header?		(database 'ref `(html-file-has-header? ,current-filename)))
	     (book?			(eq? tag 'book))
	     (title			(html-quote section-name))
	     (subtitles 		(if book?
					    (map html-quote (kw-arg-ref kw-args :subtitles))
					    (and=> (html-quote (kw-arg-ref kw-args :subtitle))
						   list)))
	     (authors  			(and book? (map html-quote (kw-arg-ref kw-args :authors))))
	     (one-line-summary		(and book? (html-quote (kw-arg-ref kw-args :one-line-summary))))
	     (keywords 			(and book? (map html-quote (kw-arg-ref kw-args :keywords))))
	     (copyright-years		(and book? (map ->string (kw-arg-ref kw-args :copyright-years))))
	     (copyright 		(and book? (html-quote (kw-arg-ref kw-args :copyright))))
	     (publisher 		(and book? (html-quote (kw-arg-ref kw-args :publisher))))
	     (contains 			(and book? (map html-quote (kw-arg-ref kw-args :contains))))
	     (on-line-permissions 	(and book? (apply string-append-with-separator "\n"
							  (map html-quote (kw-arg-ref kw-args :on-line-permissions)))))
	     (section-includes 		(map html-quote (kw-arg-ref kw-args :includes)))
	     (section-label 		(normalize-html-label title))
	     (containing-node		(database 'ref 'current-node))
	     (this-is-a-node? 		(or (not containing-node)
					    (database 'ref `(html-has-menu? ,containing-node))))
	     (index			(kw-arg-ref kw-args :index)))


	(if (eq? tag 'h0)
	    (database 'set-unique!
		      `(html-file-of ,title)
		      current-filename))
	
	(if (not (eq? tag 'book))
	    (if containing-node
		(database 'set!
			  `(html-menu ,containing-node)
			  (append (database 'ref `(html-menu ,containing-node))
				  (list title))))
	    (begin
	      (database 'set-unique!
			'html-book
			title)
	      (database 'set-unique!
			'html-subtitles
			subtitles)
	      (database 'set-unique!
			`(html-contains ,title)
			contains)
	      (for-each (lambda (contained)
			  (database 'set-unique!
				    `(html-contained-in ,contained)
				    title))
			contains)))


	(if (not file-has-header?)
	    (database 'set! `(html-file-has-header? ,current-filename) #t))

	(dynamic-wind
	 (lambda ()
	   (if this-is-a-node?
	       (database 'set! 'current-node title)))

	 (lambda () 
	   #/lazy-string-append
	   /
	   #; HTML header
	   #;
	   #,(if file-has-header?
		 ""
		 (noop
		  #/lazy-string-append
		  /
		  <html>
		  <head>
		  <title>#,title;</title>
		  </head>

		  <body>
		  /#))
	   
	   #; Page header:
	   #;
	   #,(if (not (eq? tag 'h0))
		 ""
		 (delay
		   (let ((book (database 'ref 'html-book))
			 (subtitles (database 'ref 'html-subtitles)))
		     (if (not book)
			 ""
			 #/string-append
			 /
			 <small><i>#,book;#;
			 #,(if (not subtitles)
			       ""
			       #/string-append
			       /
			       #,(char #\:) #,(car subtitles)
			       /#)
			 </i></small>
			 <br><br>
			 /#))))

	   #; Label the section for cross-references:
	   #;
	   #,(format-sub-document `((label :name ,section-name
					   :link ,section-name)))

	   #; Section title and subtitles:
	   #;
	   <#,html-tag; #,html-attr;>#,section-name;#;
	   #,(if (not subtitles)
		 ""
		 (apply string-append
			(map (lambda (st) (string-append "\n<br><small>" st "</small>"))
			     subtitles)))#;
	   <##/#,html-tag;>


	   #; One-line-summary
	   #;
	   #,(if (not one-line-summary)
		 ""
		 (string-append "<i>" one-line-summary "</i><br>"))

	   #; Hyperlinks to `up', `next', and `prev' sections:
	   #;
	   #,(if (not this-is-a-node?)
		 ""
		 (delay
		   (let ((up			(or containing-node
						    (and (eq? tag 'h0)
							 (database 'ref 'html-book)))))
		     (if (not up)
			 ""
			 (let* ((in-menu	(if (database 'ref `(html-has-menu? ,up))
						    (database 'ref `(html-menu ,up))
						    (database 'ref `(html-contains ,up))))
				(here	(member section-name in-menu))
				(next	(and here (cdr here) (cadr here)))
				(rev-menu	(reverse in-menu))
				(rev-here	(member section-name rev-menu))
				(prev	(and rev-here (cdr rev-here) (cadr rev-here))))

			   #/string-append
			   /
			   <small>
			   <b>up: </b>#,(force-lazy-string (format-sub-document `((ref :name ,up))))</br>
			   #,(if (not next)
				 ""
				 #/string-append
				 /
				 <b>next: </b>#,(force-lazy-string (format-sub-document `((ref :name ,next))))</br>
				 /#)
			   #,(if (not prev)
				 ""
				 #/string-append
				 /
				 <b>prev: </b>#,(force-lazy-string (format-sub-document `((ref :name ,prev))))</br>
				 /#)
			   </small>
			   <br>
			   /#)))))

	   #; Author credits
	   #;
	   #,(if (not authors)
		 ""
		 #/lazy-string-append
		 /
		 <hr align=left>
		 <b><i>
		 by<br>
		 #,(apply string-append
			  (map (lambda (author) (string-append author "<br>\n"))
			       authors))
		 </i></b>
		 /#)

	   #; Include file notation (this doesn't belong here!!!):
	   #;
	   #,(if (not section-includes)
		 ""
		 (string-append
		  "<hr align=left>\n"
		  "<pre>\n"
		  (apply string-append
			 (map (lambda (include)
				#/lazy-string-append
				/
				#include &lt;#,include;&gt;
				/#)
			      section-includes))
		  "</pre>\n"
		  "<hr align=left>\n"))

	   #; Sub-documents of the section:
	   #;
	   #,(all-sub-documents->html format-sub-document sub-documents)

	   #; Table of contents
	   #;
	   #,(if (not contains)
		 ""
		 (delay
		   #/string-append
		   /
		   <hr>
		   <hr>
		   <h1 align=left>Contents</h1>
		   <ul>
		   #,(apply string-append
			    (map (lambda (menu-item)
				   (if (not menu-item)
				       "<hr>\n"
				       (string-append "<li>"
						      (force-lazy-string (format-sub-document `((ref :name ,menu-item))))
						      "</li>\n")))
				 contains))#;
		   </ul>
		   <hr>
		   <hr>
		   /#))

	   #; If the section contains an index, insert it here.
	   #; (This doesn't belong here!!!)
	   #;
	   #,(delay
	       (if (not index)
		   ""
		   (let* ((book-title	(kw-arg-ref kw-args :index-for))
			  (contents		(database 'ref `(html-contains ,book-title)))
			  (entries-raw 	(apply append (map (lambda (chapter)
							     (and chapter
								  (let ((file (database 'ref `(html-file-of ,chapter))))
								    (if (not file)
									(throw 'index-could-not-find-file-for chapter))
								    (database 'ref `(html-index-entries ,file)))))
							   contents)))
			  (entries 		()))
		     (for-each (lambda (entry)
				 (if (intersectionq index (index-categories entry))
				     (set! entries (cons entry entries))))
			       entries-raw)
		     (let* ((sorted-entries 		(sort entries
							      (lambda (a b)
								(string<=? (index-term a) (index-term b)))))

			    ;; A sorted list of index term:
			    ;;
			    (terms 			(unique (map index-term sorted-entries)))

			    ;; Index entries, grouped by terms (which are sorted), 
			    ;; and sorted within each group by node name:
			    ;;
			    (collated-entries 	(map (lambda (term)
						       (sort (pick (lambda (entry)
								     (string=? (index-term entry) term))
								   entries)
							     (lambda (a b)
							       (string<=? (index-node a) (index-node b)))))
						     terms))

			    ;; Sorted list of the unique first letters of index terms:
			    ;;
			    (letters 		(unique (map string-ref terms)))

			    ;; A group of terms for each letter (sorted).  Each group
			    ;; contains entries grouped by term (sorted):
			    ;;
			    (alphabetized (map (lambda (letter)
						 (pick (lambda (collated)
							 (char=? letter (string-ref (index-term (car collated)))))
						       collated-entries))
					       letters)))

		       (if (not letters)
			   (begin
			     ;; !!! generate a warning about an empty index:
			     ;;
			     "")

			   #/string-append
			   /
			   <ul>
			   #,(apply string-append
				    (map (lambda (letter collated-for-letter)
					   (apply string-append
						  "<hr>"
						  "<b><big>" (string letter) "</big></b>"
						  "<br>"
						  (map (lambda (entries-for-term)
							 #/string-append
							 /
							 <li>#,(index-term (car entries-for-term))
							 <small>
							 <ul>
							 #,(apply string-append
								  (map (lambda (entry)
									 (force-lazy-string
									  (format-sub-document
									   `((hyperlink :url ,(string-append (index-filename entry)
													     "#"
													     (index-anchor-name entry)))
									     ,(index-node entry)))))
								       entries-for-term))
							 </ul>
							 </small>
							 /#)
						       collated-for-letter)))
					 letters
					 alphabetized))
			   </ul>
			   /#)))))

	   #; Copyright and permissions:
	   #;
	   #,(if (not copyright-years)
		 ""
		 (begin
		   (if (not copyright)
		       (throw 'missing-copyright-holder))
		   #/lazy-string-append
		   /
		   <b>Copyright (C) #;
		   #,(apply string-append-with-separator
			    ", "
			    copyright-years) #;
		   #,copyright;#;
		   </b><br>
		   <br>
		   <pre>
		   #,on-line-permissions
		   </pre>
		   /#))

	   #; Page footer:
	   #;
	   #,(if (not (eq? tag 'h0))
		 ""
		 (delay
		   (let ((book (database 'ref 'html-book))
			 (subtitles (database 'ref 'html-subtitles)))
		     (if (not book)
			 ""
			 #/string-append
			 /
			 <small><i>#,book;#;
			 #,(if (not subtitles)
			       ""
			       #/string-append
			       /
			       #,(char #\:) #,(car subtitles)
			       /#)
			 </i></small>
			 /#))))

	   #; HTML header
	   #;
	   #,(if file-has-header?
		 ""
		 (noop
		  #/lazy-string-append
		  /
		  </body>
		  /#))
	   /#)

	 (lambda ()
	   (if this-is-a-node?
	       (if containing-node
		   (database 'set! 'current-node containing-node)
		   (database 'remove! 'current-node)))))))))


;;; !!! texinfo-section-title used consistently?

(define (texinfo-format-book (document tag parameters sub-documents database format-sub-document))
  (apply-to-args parameters
    (lambda (section-name . kw-args)
      (let* ((source 			(or (database 'ref 'current-filename)
					    "  ** unknown file ** "))
	     (rootname 			(filename-sans-extension (filename-nondirectory source)))
	     (info-file 		(string-append rootname ".info"))
	     (title 			(texinfo-section-title section-name))
	     (subtitles 		(map texinfo-section-title (kw-arg-ref kw-args :subtitles)))
	     (authors  			(map texinfo-quote (kw-arg-ref kw-args :authors)))
	     (one-line-summary		(texinfo-quote (kw-arg-ref kw-args :one-line-summary)))
	     (keywords 			(map texinfo-quote (kw-arg-ref kw-args :keywords)))
	     (copyright-years 		(map texinfo-quote (map ->string (kw-arg-ref kw-args :copyright-years))))
	     (copyright 		(texinfo-quote (kw-arg-ref kw-args :copyright)))
	     (publisher 		(texinfo-quote (kw-arg-ref kw-args :publisher)))
	     (contains 			(kw-arg-ref kw-args :contains))
	     (on-line-permissions 	(texinfo-quote (apply string-append-with-separator "\n" (kw-arg-ref kw-args :on-line-permissions))))
	     (printed-permissions 	(texinfo-quote (apply string-append-with-separator "\n" (kw-arg-ref kw-args :printed-permissions)))))

	#/lazy-string-append
	/
	#; Standard texinfo header:
	#;
	\input texinfo   @c -*-texinfo-*-
	@c %**start of header
	@setfilename #,info-file
	@settitle #,title
	@setchapternewpage odd
	@c %**end of header

	#; Create indexes:
	#;
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
					      (let ((code (texinfo-index-category->code category)))
						(string-append "@synindex " code " " (car index) "\n")))
					    (cddr index))))
			      indexes)))))#;
	@ifinfo
	#; Copyright information for an info file:
	#;
	Copyright #,(apply string-append-with-separator ", " copyright-years) #,copyright

	#,on-line-permissions
	@end ifinfo
	
	#; Title page for a printed document:
	#;
	@titlepage
	@title #,title
	#,(apply string-append (map (lambda (x) (string-append "@subtitle " x "\n")) subtitles))#;
	#,(apply string-append (map (lambda (x) (string-append "@author " x "\n")) authors))#;
	

	#; Copyright page for a printed document:
	#;
	@page
	@vskip 0pt plus 1filll
	Copyright @copyright{} #,(apply string-append-with-separator " " copyright-years) #,copyright
	
	#,(if (not publisher) "" (string-append "Published by " publisher "\n"))
	
	#,printed-permissions
	@end titlepage

	#; The top node (on-line) and table of contents (printed):
	#; 
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
	#,(apply string-append (map (lambda (c) (if (not c) "" (string-append "* " c "::\n"))) (texinfo-section-title contains)))#;
	@end menu
	
	#; @include directives for the texinfo files of each chapter:
	#;
	#,(delay
	    (apply string-append
		   (map (lambda (c)
			  (let* ((file (database 'file-of `(texinfo-label ,c)))
				 (file (and file (string-append (filename-sans-extension (filename-nondirectory file)) ".texi"))))
			    (if (or (not file) (string=? file info-file))
				""
				(string-append "@include " file "\n"))))
			contains)))

	#; Tables of contents: (shortcontens should be optional and available in HTML!!!)
	#;
	@shortcontents
	@contents
	@bye
	/#))))


(define (texinfo-format-section (document tag parameters sub-documents database format-sub-document))
  (apply-to-args parameters
    (lambda (section-name . kw-args)
      (let* ((section-title	(texinfo-section-title section-name))
	     (section-subtitle 	(and=> (kw-arg-ref parameters :subtitle) texinfo-section-title))
	     (section-includes 	(kw-arg-ref parameters :includes))
	     (page-needed 	(or (kw-arg-ref parameters :need) 3200))
	     (node-name 	section-name)
	     (containing-node 	(database 'ref 'current-node))
	     (this-is-a-node? 	(or (not containing-node)
				    (database 'ref `(texinfo-has-menu? ,containing-node))))
	     (index 		(kw-arg-ref parameters :index))
	     (section-type 	(case tag
				  ((h0)	(if index
					    "unnumbered"
					    "chapter"))
				  ((h1)	"section")
				  ((h2)	"subsection")
				  ((h3)	"subsubsection"))))
	(if index
	    (let ((old-indexes (database 'ref 'indexes)))
	      (set! index (cons (new-index-name database)
				(cons section-name index)))
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
	   (database 'set! 'containing-nodes
		     (cons node-name
			   (and containing-node
				(database 'ref 'containing-nodes))))
	   (database 'set! 'current-node node-name))

	 (lambda () (let* ((sub-texinfo (all-sub-documents->texinfo format-sub-document sub-documents)))
		      #/lazy-string-append
		      /
		      #; Ensure enough space on the page to begin a new section:
		      #;
		      @need #,(->string page-needed)

		      #; The @node line.
		      #;
		      #,(delay (if (not this-is-a-node?)
				   ""
				   #/string-append
				   /
				   @node #,section-title
				   /#))#;

		      #; The @chapter or @(sub(sub))section line.
		      #;
		      @#,section-type; #,section-title
		      #,(if (not section-subtitle)
			    ""
			    (string-append "@strong{" section-subtitle "}\n@sp 2\n"))#;
		      #,(if (not section-includes)
			    ""
			    (apply string-append
				   (map (lambda (include)
					  (string-append "@strong{@code{#include <" include ">}}\n@sp 2\n"))
					section-includes)))#;

		      #; Contents of the chapter or section:
		      #;
		      #,sub-texinfo

		      #; If the section contains an index, print it here:
		      #; (This doesn't belong here!!!)
		      #;
		      #,(if (not index)
			    ""
			    #/string-append
			    /
			    @printindex #,(car index)

			    /#)
		      /#))

	 (lambda ()
	   (database 'set! 'containing-nodes
		     (cdr (database 'ref 'containing-nodes)))
	   (if containing-node
	       (database 'set! 'current-node containing-node)
	       (database 'remove! 'current-node))))))))

;;; book
;;; h0
;;; h1
;;; h2
;;; h3


(declare-markup-tag 'xdml 'book
		    :can-contain	(cons 'h0 xdml-block-of-text-tags)
		    :parameter-check	#f ;; !!!
		    :synopsis		"The root of a hierarchical book."
		    :documentation	(noop #/quote
					      /
					      A `book' markup is used to define the root of a hierarchical
					      document.  In HTML, it formats an entry page for the entire
					      document.  In Texinfo, it determines the formatting of the
					      title page, publication information, and tables of contents.
					      /#)
		    :formatters		`((html 	,html-format-book-or-section)

					  (texinfo	,texinfo-format-book)
					  ;; :per-format-check character set in title!!!
					  ))

(declare-markup-tag 'xdml 'h0
		    :can-contain	(cons 'h1 xdml-block-of-text-tags)
		    :parameter-check	#f ;; !!!
		    :synopsis		"A chapter."
		    :documentation	(noop #/quote
					      /
					      An `h0' markup is used to define the root of a chapter.
					      /#)
		    :formatters		`((html 	,html-format-book-or-section)

					  (texinfo	,texinfo-format-section)
					  ;; :per-format-check character set in title!!!
					  ))

(declare-markup-tag 'xdml 'h1
		    :can-contain	(cons 'h2 xdml-block-of-text-tags)
		    :parameter-check	#f ;; !!!
		    :synopsis		"A section."
		    :documentation	(noop #/quote
					      /
					      An `h0' markup is used to define the root of a section.
					      /#)
		    :formatters		`((html 	,html-format-book-or-section)

					  (texinfo	,texinfo-format-section)
					  ;; :per-format-check character set in title!!!
					  ))
(declare-markup-tag 'xdml 'h2
		    :can-contain	(cons 'h3 xdml-block-of-text-tags)
		    :parameter-check	#f ;; !!!
		    :synopsis		"A subsection."
		    :documentation	(noop #/quote
					      /
					      An `h0' markup is used to define the root of a subsection.
					      /#)
		    :formatters		`((html 	,html-format-book-or-section)

					  (texinfo	,texinfo-format-section)
					  ;; :per-format-check character set in title!!!
					  ))

(declare-markup-tag 'xdml 'h3
		    :can-contain	xdml-block-of-text-tags
		    :parameter-check	#f ;; !!!
		    :synopsis		"A sub-subsection."
		    :documentation	(noop #/quote
					      /
					      An `h0' markup is used to define the root of a sub-subsection.
					      /#)
		    :formatters		`((html 	,html-format-book-or-section)

					  (texinfo	,texinfo-format-section)
					  ;; :per-format-check character set in title!!!
					  ))


;;; paragraphs
;;; menu
;;; p
;;; insert

(declare-markup-tag 'xdml 'paragraphs
		    :can-contain	xdml-block-of-text-tags
		    :parameter-check	#f ; !!!
		    :synopsis		"Arbitrary blocks of text."
		    :documentation	(noop #/quote
					      /
					      The `paragraphs' markup is used only to group its sub-documents
					      in documentation source.  It does not effect how those 
					      sub-documents are rendered in formatted output.
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (all-sub-documents->html format-sub-document sub-documents)))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (all-sub-documents->texinfo format-sub-document sub-documents)))))

(declare-markup-tag 'xdml 'menu
		    :can-contain	#f
		    :parameter-check	#f ; !!!
		    :synopsis		"A menu of subsections."
		    :documentation	(noop #/quote
					      /
					      In on-line formats, a menu markup is expanded into a
					      list of hyperlinks to immediately enclosed sections,
					      subsections, or sub-subsections.

					      !!!
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let* ((node (database 'ref 'current-node))
								  (menu-key `(html-menu ,node))
								  (has-menu?-key `(html-has-menu? ,node)))
							     (if (not node)
								 (throw 'menu-outside-of-node))
							     (database 'set! has-menu?-key #t)
							     (delay
							       #/string-append
							       /
							       <ul>
							       #,(let ((menu (database 'ref menu-key)))
								   (apply string-append
									  (map (lambda (menu-item)
										 (string-append "<li>"
												(force-lazy-string (format-sub-document `((ref :name ,menu-item))))
												"</li>\n"))
									       menu)))#;
							       </ul>

							       /#))))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let* ((node (database 'ref 'current-node))
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

							       /#))))))

(declare-markup-tag 'xdml 'p
		    :can-contain	xdml-structured-text-tags
		    :parameter-check	#f ; !!!
		    :synopsis		"A paragraph."
		    :documentation	(noop #/quote
					      /
					      The `paragraph' markup is used to format its sub-documents
					      as a paragraph.

					      !!!
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   <p>#,(all-sub-documents->html format-sub-document sub-documents)</p>/#))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   
							   #,(if (not (memq :noindent parameters))
								 ""
								 "@noindent\n")#;
							   #,(all-sub-documents->texinfo format-sub-document sub-documents)
							   /#))))


(declare-markup-tag 'xdml 'insert
		    :can-contain	'(string)
		    :parameter-check	#f ; !!!
		    :synopsis		"Pre-formatted, fixed-width text."
		    :documentation	(noop #/quote
					      /
					      The `insert' markup is used for pre-formatted, fixed-width
					      text which contains no further markups.

					      !!!
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   <pre>
							   #,(apply string-append sub-documents)
							   </pre>
							   /#))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let* ((lines (separate-fields-after-char #\nl (apply string-append sub-documents) list))
								  (lines-grouped-naturally
								   (let loop ((groups ())
									      (line-list lines))
								     (cond
								      ((null? line-list)
								       (cond
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
								  (lines-groups
								   (apply append
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
							     /#)))))



;;; text
;;; stress
;;; emph
;;; topic
;;; hyperlink
;;; definition

(declare-markup-tag 'xdml 'text
		    :can-contain	xdml-structured-text-tags
		    :parameter-check	#f
		    :synopsis		"Arbitrary text."
		    :documentation	(noop #/quote
					      /
					      The `text' markup is used only to group its sub-documents
					      in documentation source.  It does not effect how those 
					      sub-documents are rendered in formatted output.
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (all-sub-documents->html format-sub-document sub-documents)))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (all-sub-documents->texinfo format-sub-document sub-documents)))))

(declare-markup-tag 'xdml 'stress
		    :can-contain	xdml-structured-text-tags
		    :parameter-check	#f
		    :synopsis		"Heavily emphar_sized text."
		    :documentation	"Stressed text is rendered to stand out from ordinary text."
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   <strong>#,(all-sub-documents->html format-sub-document sub-documents)</strong>/#))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   @emph{@strong{#,(all-sub-documents->texinfo)}}/#))))

(declare-markup-tag 'xdml 'emph
		    :can-contain	xdml-structured-text-tags
		    :parameter-check	#f
		    :synopsis		"Mildly emphar_sized text."
		    :documentation	"Emphar_sized text is rendered to stand out from ordinary text."
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   <em>#,(all-sub-documents->html format-sub-document sub-documents)</em>/#))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   @emph{#,(all-sub-documents->texinfo)}/#))))


(declare-markup-tag 'xdml 'topic
		    :can-contain	xdml-structured-text-tags
		    :parameter-check	#f
		    :synopsis		"Heavily emphar_sized text."
		    :documentation	(noop #/quote
					      /
					      Topic text is rendered to stand out from ordinary text.
					      It is used to emphar_size the topic of a sentence, paragraph
					      or other non-sectioned unit of text.
					      [Should also be added to the concept index.]
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   <strong><u>#,(all-sub-documents->html format-sub-document sub-documents)</u></strong>/#))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   @strong{#,(all-sub-documents->texinfo)}/#))))

(declare-markup-tag 'xdml 'hyperlink
		    :can-contain	xdml-structured-text-tags
		    :parameter-check	(lambda (tag parameters parent-checkers)
					  (let ((url (necessary-kw-arg tag parameters :url)))
					    (if (not (string? url))
						(throw-invalid-tag-parameters tag
									      (memq :url parameters)
									      "Argument to `:url' must be a string."))))
									    
		    :synopsis		"Text that forms a hyperlink."
		    :documentation	(noop #/quote
					      /
					      A hyperlink is arbitrary text associated with a URL.
					      It is rendered as live link in systems that support it,
					      and as the text along with the URL in other systems.

					      Required keyword parameter:

					      	:url string 
					          The target of the hyperlink.
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   <a href="#,(kw-arg-ref parameters :url)">#,(all-sub-documents->html format-sub-document sub-documents)</a>/#))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   @url{#,(texinfo-quote (kw-arg-ref parameters :url))} #;
							   #,(if (not sub-documents)
								 ""
								 #/lazy-string-append
								 /
								 (#,(char #\")#,(all-sub-documents->texinfo format-sub-document sub-documents)#,(char #\")) /#)#;
							   /#))))


(declare-markup-tag 'xdml 'definition
		    :can-contain	xdml-structured-text-tags
		    :parameter-check	#f ; !!!
									    
		    :synopsis		"Text that forms an indexed definition."
		    :documentation	(noop #/quote
					      /
					      A definition is rendered as ordinary text, but is also 
					      indexed by an explicitly stated term.
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let ((term		(kw-arg-ref parameters :term))
								 (categories 	(append (and=> (kw-arg-ref parameters :category) list)
											(kw-arg-ref parameters :categories))))
							     #/lazy-string-append
							     /
							     <em>
							     #,(format-sub-document `((index :term ,term :default-primary :categories ,categories)))
							     #,(all-sub-documents->html format-sub-document sub-documents)
							     </em>
							     /#)))
					  
					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let ((term (kw-arg-ref parameters :term))
								 (categories (kw-arg-ref parameters :categories)))
							     #/lazy-string-append
							     /
							     #,(format-sub-document `((index :term ,term :categories ,categories)))#;
							     @dfn{#,(all-sub-documents->texinfo format-sub-document sub-documents)}/#)))))


;;; string
;;; output
;;; input


(declare-markup-tag 'xdml 'string
		    :can-contain 	()
		    :parameter-check	#f
		    :synopsis		"A literal string containing no sub-documents."
		    :documentation	"Strings are rendered as-is."
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (html-quote document)))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (texinfo-quote document)))))

(declare-markup-tag 'xdml 'output
		    :can-contain 	'(string)
		    :parameter-check	#f
		    :synopsis		"Text representing the output of a computer program."
		    :documentation	"Outputs are rendered in a fixed width font."
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   <code>#,(all-sub-documents->html format-sub-document sub-documents)</code>/#))
					  
					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   @code{#,(all-sub-documents->texinfo format-sub-document sub-documents)}/#))))

(declare-markup-tag 'xdml 'input
		    :can-contain 	'(string)
		    :parameter-check	#f
		    :synopsis		"Text representing the input to a computer program."
		    :documentation	(noop #/quote
					      /
					      Input are rendered in a fixed width font.  When possible,
					      input is rendered in a bold font or otherwise made distinct
					      from text rendered for the tag `output'.
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   <kbd>#,(all-sub-documents->html format-sub-document sub-documents)</kbd>/#))
					  
					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   #/lazy-string-append
							   /
							   @kbd{#,(all-sub-documents->texinfo format-sub-document sub-documents)}/#))))


;;; list
;;; enum

;;; list !!!
;;; enum !!!

;;; item

;;; item !!!

;;; label
;;; ref
;;; index

(declare-markup-tag 'xdml 'label
		    :can-contain	()
		    :parameter-check	(lambda (tag parameters parent-checkers)
					  (let ((name (necessary-kw-arg tag parameters :name))
						(link (optional-kw-arg tag parameters :link)))
					    (if (not (string? name))
						(throw-invalid-tag-parameters tag
									      (memq :name parameters)
									      "Argument to `:name' must be a string."))
					    (if (not (string? link))
						(throw-invalid-tag-parameters tag
									      (memq :link parameters)
									      "Argument to `:link' must be a string."))))
									    
		    :synopsis		"A label is a point of possible cross-reference."
		    :documentation	(noop #/quote
					      /
					      A label is not rendered but is a point of possible 
					      cross-reference (by tags such as `ref').
					      
					      Required keyword parameter:
					      
					      	:name string 

					      A name for the label.   The name should be unique within the
					      entire document.

					      Optional keyword parameter:

					      	:link string

					      Optional text for hyperlinks to this label.
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   
							   (let ((label (kw-arg-ref parameters :name))
								 (link (kw-arg-ref parameters :link))
								 (filename (let ((f (database 'ref 'current-filename)))
									     (or f " ** unknown file ** "))))
							     (if (database 'ref `(html-label ,label))
								 (throw 'multiply-defined-label
									label
									`((html-label ,label) . ,filename)
									(database 'ref `(html-label ,label))))
							     (database 'set-unique! `(html-label ,label) filename)
							     (if link
								 (database 'set-unique! `(html-link ,label) link))
							     #/lazy-string-append
							     /
							     <a name="#,(normalize-html-label label)"></a>/#)))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let* ((label (kw-arg-ref parameters :name))
								  (node (or (database 'ref 'current-node)
									    " *** unknown node *** ")))
							     (if (database 'ref `(texinfo-label ,label))
								 (throw 'multiply-defined-label label))
							     (database 'set-unique! `(texinfo-label ,label) node)
							     "")))))


(declare-markup-tag 'xdml 'ref
		    :can-contain	()
		    :parameter-check	(lambda (tag parameters parent-checkers)
					  (let ((name (necessary-kw-arg tag parameters :name)))
					    (if (not (string? name))
						(throw-invalid-tag-parameters tag
									      (memq :name parameters)
									      "Argument to `:name' must be a string."))))
									    
		    :synopsis		"A simple cross-reference."
		    :documentation	(noop #/quote
					      /
					      A reference to a label is rendered as a live hyperlink,
					      or as a page number reference (perhaps with section name).

					      If rendered as a hyperlink, the text of the link is
					      supplied by the `:link' parameter of the corresponding label.
					      If the label has no `:link', the text is a URL for the label.

					      If rendered as a page reference, the text of the link is suitable
					      for a sentence or phrase beginning with "see".
					      
					      Required keyword parameter:
					      
					      	:name string 
					      	   The name of the label being cross-referenced.
					      /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   
							   (let* ((label (kw-arg-ref parameters :name))
								  (target (lazy-string-append (delay (or (database 'ref 'html-url-prefix) ""))
											      (delay (or (database 'ref `(html-label ,label))
													 (throw 'undefined-label label)))
											      "#"
											      (normalize-html-label label))))
							     #/lazy-string-append
							     /
							     <a href=#,(char #\")#,target;#;
							     #,(char #\")>#,(delay
									      (let ((link (database 'ref `(html-link ,label))))
										(or (and link
											 (format-sub-document `((text) ,link)))
										    target)))#;
							     </a>/#)))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let ((containing-node (database 'ref 'current-node)))
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
									  /#))))))))



(declare-markup-tag 'xdml 'index
		    :can-contain	#f
		    :parameter-check	#f ; !!!
									    
		    :synopsis		"An indexed point in a document."
		    :documentation	(noop  #/quote
					       /
					       An index point produces no visible output in formatted documents 
					       but creates one or more index entries.

					       !!!!
					       /#)
		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let* ((term			(kw-arg-ref parameters :term))
								  (is-primary?	 	(->bool (memq :primary parameters)))
								  (is-default-primary? 	(->bool (memq :default-primary parameters)))
								  (node			(database 'ref 'current-node))
								  (filename 		(let ((f (database 'ref 'current-filename)))
											  (or f " ** unknown file ** ")))
								  (anchor-name 		(html-file-gensym database filename 'index-pt:))
								  (categories 		(append (and=> (kw-arg-ref parameters :category) list)
												(kw-arg-ref parameters :categories)))
								  (key			`(html-index-entries ,filename))
								  (old-list		(database 'ref key)))
							     (database 'set!
								       key
								       (cons (make-index-record term
												is-primary?
												is-default-primary?
												filename
												node
												anchor-name
												categories)
									     old-list))
							     #/lazy-string-append
							     /
							     <a name="#,anchor-name;">
							     /#)))

					  (texinfo	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let ((term (texinfo-quote (kw-arg-ref parameters :term)))
								 (categories (kw-arg-ref parameters :categories)))
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
									       categories)))/#)))))




(define (make-index-record term is-primary? is-default-primary? filename node anchor-name categories)
  (list term is-primary? is-default-primary? filename node anchor-name categories))

(define (index-term l) (list-ref l 0))
(define (index-is-primary? l) (list-ref l 1))
(define (index-is-default-primary? l) (list-ref l 2))
(define (index-filename l) (list-ref l 3))
(define (index-node l) (list-ref l 4))
(define (index-anchor-name l) (list-ref l 5))
(define (index-categories l) (list-ref l 6))

(define (reset-html-file-gensym-counter database filename)
  (let ((n-key		`(html-gensym-n ,filename)))
    (database 'remove! n-key)))

(define (html-file-gensym database filename symbol)
  (let* ((n-key		`(html-gensym-n ,filename))
	 (n		(or (database 'ref n-key)
			    0))
	 (symbol	(symbol-append symbol (->string n))))
    (database 'set! n-key (1+ n))
    symbol))




(define-public (char x)
  (cond
   ((char? x)			(string x))
   ((integer? x)		(integer->char x))
   ((string? x)			(if (not (regexec (once (regcomp "[a-zA-Z]\\+")) x #f))
				    (throw 'not-a-character-name x)
				    (let ((name (string-append "#\\" x))
					  (obj (catch #t
						 (lambda ()
						   (with-input-from-string name read))
						 (lambda ign
						   (throw 'not-a-character-name x)))))
				      (if (not (char? obj))
					  (throw 'not-a-character-name x))
				      obj)))
   (#t				(throw 'not-a-character-name x))))

(define-public (lazy-string-append . args)
  (if (and-map read-only-string? args)
      (apply string args)
      args))


(define-public (force-lazy-string s)
  (cond
   ((null? s)		"")
   ((string? s)		s)
   ((promise? s)	(force s))
   (#t			(apply string (map force-lazy-string s)))))



(define (all-sub-documents->html format-sub-document sub-documents)
    (apply lazy-string-append (map format-sub-document sub-documents)))

(define (all-sub-documents->texinfo  format-sub-document sub-documents)
  (apply lazy-string-append (map format-sub-document sub-documents)))



(define (necessary-kw-arg tag params kw)
  (let ((kw-list	(memq kw params)))
    (if (and kw-list (not (null? (cdr kw-list))))
	(cadr kw-list)
	(throw-invalid-tag-parameters tag
				      ()
				      (string "Missing necessary keyword argument `" (->string kw) "'.")))))

(define (optional-kw-arg tag params kw)
  (let ((kw-list	(memq kw params)))
    (cond
     ((not kw-list)			#f)
     ((not (null? (cdr kw-list)))	(cadr kw-list))
     (#t				(throw-invalid-tag-parameters tag
								      ()
								      (string "Missing argument for keyword `" (->string kw) "'."))))))



(define test
  (let ((db (make-formatter-database))
	(format (markup-language-subdocument-formatter 'xdml 'html))
	(entry (markup-language-formatter-entry-point 'xdml 'html))
	(file "foo.html"))
    (lambda (doc)
      (db 'set-input-file! "foo.c")
      (entry file doc db (lambda (sub-doc) (format sub-doc db))))))

