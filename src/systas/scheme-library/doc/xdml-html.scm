;;; tag: Tom Lord Tue Dec  4 14:59:32 2001 (doc/xdml-html.scm)
;;;
;;; xdml-html.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc xdml-html)
  :use-module (doc old-xdml)
  :use-module (doc hdml)
  :use-module (compatability defrec)
  :use-module (data-structures string-fun)
  :use-module (data-structures ratlist)
  :use-module (regexps subst)
  :use-module (regexps cached)
  :use-module (unix output-files)
  :use-module (unix filenames)
  :use-module (unix file-utils))



(define-public (add-xdml-html-translation)
  (xdml-define-translation 'html xdml-html-pass1 xdml-html-pass2))


(define (xdml-html-pass1 input-file document database nesting-rules :optional parser-extension ->html-extension)
  (let ((filename (xdml-input-file->html-output-file input-file)))
    (reset-html-file-gensym-counter database filename)
    (dynamic-wind
     (lambda ()
       (database 'set! 'current-filename filename))
     (lambda ()
       (xdml-parse document
		   (lambda (tag parameters sub-documents)
		     (xdml->html-one-file document tag parameters sub-documents database nesting-rules parser-extension ->html-extension))
		   #f
		   nesting-rules
		   parser-extension))
     (lambda ()
       (database 'remove! 'current-filename)))))


(define (xdml-html-pass2 input-file html-translation database-fn . kw-params)
  (let ((filename (xdml-input-file->html-output-file input-file)))
    (with-overwriting-output-file filename
      (lambda () (display html-translation)))))


(define (xdml-input-file->html-output-file input-file)
  (string-append (filename-sans-extension (filename-nondirectory input-file)) ".html"))




(define (xdml->html-one-file document tag parameters sub-documents database nesting-rules parser-extension ->html-extension)
  (case tag
    ((book h0 h1 h2 h3)		(let* ((html-file		(or (database 'ref 'current-filename)
								    "  ** unknown file ** "))
				       (title			(html-quote (car parameters)))
				       ;; (indexes			(map (lambda (i) (cons (new-index-name) i)) (kw-arg-ref parameters :indexes)))
				       )

				  (noop
				   #/lazy-string-append
				   /
				   <html>
				   <head>
				   <title>#,(val title)</title>
				   </head>
				   <body>
				   #,(->html document tag parameters sub-documents database nesting-rules parser-extension ->html-extension)
				   </body>
				   /#)))

    (else			(throw 'unsupported-top-level-tag-for-html tag))))



(define-public (xdml->html document database nesting-rules :optional parser-extension ->html-extension)
  (xdml-parse document
	      (lambda (tag parameters sub-documents)
		(->html document tag parameters sub-documents database nesting-rules parser-extension ->html-extension))
	      #f
	      nesting-rules
	      parser-extension))



(define (->html document tag parameters sub-documents database nesting-rules parser-extension ->html-extension)

  (define (sub-document->html sd)
    (xdml->html sd database nesting-rules parser-extension ->html-extension))

  (define (all-sub-documents->html)
    (apply lazy-string-append (map sub-document->html sub-documents)))

  (case tag
    ;; The hierarchy of "book", "chapter", "section", "sub-section", 
    ;; and "sub-sub-section":
    ;;
    ((book h0 h1 h2 h3)	(let* ((section-name (car parameters))
			       (html-tag (case tag
					   ((book)	"h1")
					   ((h0)	"h2")
					   ((h1)	"h3")
					   ((h2)	"h4")
					   ((h3)	"h5")))
			       (html-attr (case tag
					    ((book)	"align=center")
					    ((h0)	"align=center")
					    ((h1)	"align=center")
					    ((h2)	"align=center")
					    ((h3)	"align=center")))
			       (book?			(eq? tag 'book))
			       (title			(html-quote (car parameters)))
			       (subtitles 		(if book?
							    (map html-quote (kw-arg-ref parameters :subtitles))
							    (and=> (html-quote (kw-arg-ref parameters :subtitle))
								   list)))
			       (authors  		(and book? (map html-quote (kw-arg-ref parameters :authors))))
			       (one-line-summary	(and book? (html-quote (kw-arg-ref parameters :one-line-summary))))
			       (keywords 		(and book? (map html-quote (kw-arg-ref parameters :keywords))))
			       (copyright-years		(and book? (map ->string (kw-arg-ref parameters :copyright-years))))
			       (copyright 		(and book? (html-quote (kw-arg-ref parameters :copyright))))
			       (publisher 		(and book? (html-quote (kw-arg-ref parameters :publisher))))
			       (contains 		(and book? (map html-quote (kw-arg-ref parameters :contains))))
			       (home-page-link		(and book? (kw-arg-ref parameters :home-page-link)))
			       (permissions 		(and book? (apply string-append-with-separator
									  "\n"
									  (map html-quote (kw-arg-ref parameters :permissions)))))
			       (section-includes 	(map html-quote (kw-arg-ref parameters :includes)))
			       (section-label 		(normalize-label title))
			       (containing-node		(database 'ref 'current-node))
			       (this-is-a-node? 	(or (not containing-node)
							    (database 'ref `(html-has-menu? ,containing-node))))
			       (index 			(kw-arg-ref parameters :index))
			       )


			  (if (eq? tag 'h0)
			      (database 'set-unique!
					`(html-file-of ,title)
					(database 'ref 'current-filename)))

			  (if home-page-link
			      (database 'set-unique! 'home-page-link home-page-link))
			  
			  (cond

			   (contains			(begin
							  ;; A book with a table of contents.
							  ;;
							  (database 'set-unique!
								    'html-book
								    title)
							  (if subtitles
							      (database 'set-unique!
									'html-subtitles
									subtitles))
							  (database 'set-unique!
								    `(html-contains ,title)
								    contains)
							  (for-each (lambda (contained)
								      (database 'set-unique!
										`(html-contained-in ,contained)
										title))
								    contains)))

			   (containing-node		(begin
							  ;; A (sub-(sub-))section whose parent has a menu.
							  ;;
							  (database 'set!
								    `(html-menu ,containing-node)
								    (append (database 'ref `(html-menu ,containing-node))
									    (list title)))))

			   (#t				#f))

			  (dynamic-wind
			   (lambda ()
			     (if this-is-a-node?
				 (database 'set! 'current-node title)))

			   (lambda () (let* ((sub-html (all-sub-documents->html))
					     (home-link (database 'ref 'home-page-link)))


					#/lazy-string-append
					/
					#,(let ((up (or containing-node
							(and (eq? tag 'h0)
							     (database 'ref 'html-book)))))
					    (if (database 'ref `(html-has-menu? ,up))
						"<hr>\n"
						""))
					#,(sub-document->html (xdml 'label
								    (list :name section-name
									  :link section-name)))

					#,(or (and (eq? tag 'h0) home-link) "")

					<#,(val html-tag) #,(val html-attr)>#,(val section-name)#;
					#,(if (not subtitles)
					      ""
					      (apply string-append
						       (map (lambda (st) (string-append "\n<br><small>" st "</small>"))
							    subtitles)))#;
					<##/#,(val html-tag)>


					#,(if (not one-line-summary)
					      ""
					      (string-append "<i>" one-line-summary "</i><br>"))

					#,(if (not this-is-a-node?)
					      ""
					      (delay
						(let ((up		(or containing-node
									    (and (eq? tag 'h0)
										 (database 'ref 'html-book)))))
						  (if (not up)
						      ""
						      (let* ((in-menu	(if (database 'ref `(html-has-menu? ,up))
									    (database 'ref `(html-menu ,up))
									    (database 'ref `(html-contains ,up))))
							     (here		(member title in-menu))
							     (next		(and here (cdr here) (cadr here)))
							     (rev-menu	(reverse in-menu))
							     (rev-here	(member title rev-menu))
							     (prev		(and rev-here (cdr rev-here) (cadr rev-here))))

							#/string-append
							/
							<small>
							<b>up: </b>#,(force-lazy-string (sub-document->html `((ref :name ,up))))</br>
							#,(if (not next)
							      ""
							      #/string-append
							      /
							      <b>next: </b>#,(force-lazy-string (sub-document->html `((ref :name ,next))))</br>
							      /#)
							#,(if (not prev)
							      ""
							      #/string-append
							      /
							      <b>prev: </b>#,(force-lazy-string (sub-document->html `((ref :name ,prev))))</br>
							      /#)
							</small>
							<br>
							/#)))))

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

					#,(if (not section-includes)
					      ""
					      (string-append
					       "<hr align=left>\n"
					       "<pre>\n"
					       (apply string-append
						      (map (lambda (include)
							     #/lazy-string-append
							     /
							     #include &lt;#,(val include)&gt;
							    /#)
							   section-includes))
					       "</pre>\n"
					       "<hr align=left>\n"))

					#,sub-html

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
								    "</ul>\n<hr>\n<ul>\n"
								    (string-append "<li>"
										   (force-lazy-string (sub-document->html `((ref :name ,menu-item))))
										   "</li>\n")))
							      contains))#;
						</ul>
						<hr>
						<hr>
						/#))

					#,(delay
					    (if (not index)
						""
						(let* ((book-title	(kw-arg-ref parameters :index-for))
						       (contents	(database 'ref `(html-contains ,book-title)))
						       (entries-raw 	(apply append (map (lambda (chapter)
											     (and chapter
												  (let ((file (database 'ref `(html-file-of ,chapter))))
												    (if (not file)
													(throw 'index-could-not-find-file-for chapter))
												    (database 'ref `(html-index-entries ,file)))))
											   contents)))
						       (entries 	()))
						  (for-each (lambda (entry)
							      (if (intersectionq index (index-categories entry))
								  (set! entries (cons entry entries))))
							    entries-raw)
						  (let* ((alpha-entries (sort (pick (lambda (entry)
										      (char-alpha? (string-ref (index-term entry))))
										    entries)
									      (lambda (a b)
										(string-ci<=? (index-term a) (index-term b)))))
							 (other-entries (sort (pick (lambda (entry)
										      (not (char-alpha? (string-ref (index-term entry)))))
										    entries)
									      (lambda (a b)
										(string-ci<=? (index-term a) (index-term b)))))
							 (alpha-terms (unique (map index-term alpha-entries)))
							 (other-terms (unique (map index-term other-entries)))
							 (collated-alpha (map (lambda (term)
										(sort (pick (lambda (entry)
											      (string-ci=? (index-term entry) term))
											    alpha-entries)
										      (lambda (a b)
											(string-ci<=? (index-node a) (index-node b)))))
										alpha-terms))
							 (collated-other (map (lambda (term)
										(sort (pick (lambda (entry)
											      (string-ci=? (index-term entry) term))
											    other-entries)
										      (lambda (a b)
											(string-ci<=? (index-node a) (index-node b)))))
									      other-terms))
							 (letters (unique (map (lambda (s) (char-upcase (string-ref s))) alpha-terms)))
							 (alphabetized (map (lambda (letter)
									      (pick (lambda (collated)
										      (char-ci=? letter (string-ref (index-term (car collated)))))
										    collated-alpha))
									    letters)))
						    (if (and (not collated-alpha) (not collated-other))
							""
							#/string-append
							/
							<ul>
							#,(if (null? collated-other)
							      ""
							      #/string-append
							      /
							      <li> <i>punctuation</i>
							      <ul>
							      #,(apply string-append
								       (map (lambda (entries-for-term)
									      #/string-append
									      /
									      <li>#,(index-term (car entries-for-term))
									      <ul>
									      #,(apply string-append
										       (map (lambda (entry)
											      (string-append
											       "<li>\n"
											       (force-lazy-string
												(sub-document->html
												 `((hyperlink :url ,(string-append (index-filename entry)
																   "#"
																   (index-anchor-name entry)))
												   ,(index-node entry))))))
											    entries-for-term))
									      </ul>
									      /#)
									    collated-other))
							      </ul>
							      /#)
							#,(apply string-append
								 (map (lambda (letter collated-for-letter)
									(string-append
									 "</ul>\n"
									 "<hr>\n"
									 "<ul>\n"
									 "  <li><b><big>" (string letter) "</big></b><br>\n"
									 "\n"
									 "    <ul>\n"
									 (apply string-append
										(map (lambda (entries-for-term)
										       #/string-append
										       /
										       <li>#,(index-term (car entries-for-term))
										       <ul>
										       #,(apply string-append
												(map (lambda (entry)
												       (string-append
													"<li>\n"
													(force-lazy-string
													 (sub-document->html
													  `((hyperlink :url ,(string-append (index-filename entry)
																	    "#"
																	    (index-anchor-name entry)))
													    ,(index-node entry))))))
												     entries-for-term))
										       </ul>
										       /#)
										     collated-for-letter))
									 "    </ul>\n"
									 "  </li>\n"))
								      letters
								      alphabetized))
							</ul>
							/#)))))

					#,(if (not copyright-years)
					      ""
					      (begin
						(if (not copyright)
						    (throw 'missing-copyright-holder))
						#/lazy-string-append
						/
						<b>Copyright (C) #;
						#,(apply string-append-with-separator ", " copyright-years) #;
						#,(val copyright)#;
						</b><br>
						<br>
						<pre>
						#,permissions
						</pre>
						/#))

					#,(if (not (eq? tag 'h0))
					      ""
					      (delay
						(let ((book (database 'ref 'html-book))
						      (subtitles (database 'ref 'html-subtitles)))
						  (if (not book)
						      ""
						      #/string-append
						      /
						      <small><i>#,(val book)#;
						      #,(if (not subtitles)
							    ""
							    #/string-append
							    /
							    #,(char #\:) #,(car subtitles)
							    /#)
						      </i></small><br>
						      /#))))

					#,(or (and (eq? tag 'h0) home-link) "")
					/#))

			   (lambda ()
			     (if this-is-a-node?
				 (if containing-node
				     (database 'set! 'current-node containing-node)
				     (database 'remove! 'current-node)))))))
    


    ((menu)		(let* ((node (database 'ref 'current-node))
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
							     (force-lazy-string (sub-document->html `((ref :name ,menu-item))))
							     "</li>\n"))
					    menu)))#;
			    </ul>

			    /#)))

    ;; Ordinary paragraph:
    ;;
    ((p)		#/lazy-string-append
			/
			<p>#,(all-sub-documents->html)</p>/#)

    ;; Pre-formatted, indented, paragraph:
    ;;
    ((insert)		#/lazy-string-append
			/
			<pre>
			#,(html-quote (car sub-documents))
			</pre>
			/#)


    ((paragraphs)		(all-sub-documents->html))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Mark-ups for ordinary text, as in the middle of a paragraph.
    ;;

    ;; The empty document:
    ;;
    (()					"")

    ;; A leaf node in the document tree.
    ;;
    ((string)				(html-quote document))

    ;; A list of other pieces of text:
    ;;
    ((text)	 			(all-sub-documents->html))

    ;; emphar_sized text.
    ;;
    ((emph)				#/lazy-string-append
					/
					<strong>#,(all-sub-documents->html)</strong>/#)

    ((stress)				#/lazy-string-append
					/
					<em>#,(all-sub-documents->html)</em>/#)

    ((topic)				#/lazy-string-append
					/
					<strong><u>#,(all-sub-documents->html)</u></strong>/#)

    ((input)				#/lazy-string-append
					/
					<kbd>#,(all-sub-documents->html)</kbd>
					/#)

    ((output)				#/lazy-string-append
					/
					<code>#,(all-sub-documents->html)</code>
					/#)


					

    ;; Indexes
    ;;
    ;; Index items generate index entries and produce anchor points.
    ;;
    ((index)				(let* ((term			(kw-arg-ref parameters :term))
					       (is-primary?	 	(->bool (memq :primary parameters)))
					       (is-default-primary? 	(->bool (memq :default-primary parameters)))
					       (node			(database 'ref 'current-node))
					       (filename 		(let ((f (database 'ref 'current-filename)))
									  (or f " ** unknown file ** ")))
					       (anchor-name 		(html-file-gensym database filename 'index-pt:))
					       (categories 		(or (append (and=> (kw-arg-ref parameters :category) list)
										    (kw-arg-ref parameters :categories))
									    '(general)))
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
					  <a name="#,(val anchor-name)"></a>
					  /#))
					  
    ;; idx is short-hand:
    ;;
    ;;		((idx) "foo") 
    ;;
    ;; is equivalent to:
    ;;
    ;;		((text) ((index :term "foo")) "foo")
    ;;
    ((idx)				(sub-document->html `((text) ((index :term ,(car sub-documents)))
								     ,@sub-documents)))
    ;; Definitions:
    ;;
    ;; Definitions generate index entries and are typeset specially.
    ;;
    ((definition)			(let ((term		(kw-arg-ref parameters :term))
					      (categories 	(append (and=> (kw-arg-ref parameters :category) list)
									(kw-arg-ref parameters :categories))))
					  #/lazy-string-append
					  /
					  <em>
					  #,(sub-document->html `((index :term ,term :default-primary :categories ,categories)))
					  #,(all-sub-documents->html)
					  </em>
					  /#))

    ;; dfn is short-hand:
    ;;
    ;;		(dfn () "foo") 
    ;;
    ;; is equivalent to:
    ;;
    ;;		(definition (:term "foo") "foo")
    ;;
    ((dfn)				(sub-document->html (apply xdml 'definition `(:term ,(car sub-documents)) sub-documents)))


    ;; label is a point of possible cross-reference.
    ;; 
    ;; Chapters and secctions are implicitly labeled with the
    ;; chapter/section name.
    ;;
    ((label)				(let ((label (kw-arg-ref parameters :name))
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
					  <a name="#,(normalize-label label)"></a>/#))

    ;; (ref :name label)
    ;; 
    ;; In HTML, a hypertext link to some label.  By default, the text
    ;; of the link consists of the `:link' parameter of the corresponding
    ;; label.
    ;; 
    ;; In printed form, a `ref' prints the text of the hyperlink.
    ;;
    ((ref)				(let* ((label (kw-arg-ref parameters :name))
					       (target (lazy-string-append (delay (or (database 'ref 'html-url-prefix) ""))
									   (delay (or (database 'ref `(html-label ,label))
										      (throw 'undefined-label label)))
									   "#"
									   (normalize-label label))))
					  #/lazy-string-append
					  /
					  <a href=#,(char #\")#,(val target)#;
					  #,(char #\")>#,(delay
							   (let ((link (database 'ref `(html-link ,label))))
							     (or (and link
								      (sub-document->html (xdml 'text () link)))
								 target)))#;
					  </a>/#))

    ;; hyperlink is arbitrary text that should be a live
    ;; link when rendering in html.
    ;;
    ((hyperlink)			#/lazy-string-append
					/
					<a href="#,(kw-arg-ref parameters :url)">#,(all-sub-documents->html)</a>/#)

    ;; url is shorthand:
    ;; 
    ;; 		(url () "http://555-1212.com")
    ;; 
    ;; stands for:
    ;; 
    ;;		((hyperlink :url "http://555-1212.com") "http://555-1212.com")
    ;;
    ((url)				(sub-document->html `((hyperlink :url ,(car sub-documents)) ,(car sub-documents))))


    ((list)				#/lazy-string-append
					/
					<ul>
					#,(all-sub-documents->html)
					</ul>
					/#)

    ((enum)				#/lazy-string-append
					/
					<ol>
					#,(all-sub-documents->html)
					</ol>
					/#)

    ((item)				#/lazy-string-append
					/
					<li>#,(all-sub-documents->html)</li>
					/#)

    (else				(if ->html-extension
					    (->html-extension document
							      tag
							      parameters
							      sub-documents
							      database
							      nesting-rules
							      parser-extension
							      sub-document->html)
					    (throw 'xdml-bad-syntax 'xdml-html tag parameters sub-documents)))))





(define-public (html-quote s)
  (and s
       (let* ((i (apply string-append-with-separator
			"&amp;"
			(separate-fields-discarding-char #\& s list)))
	      (j (apply string-append-with-separator
			"&lt;"
			(separate-fields-discarding-char #\< i list)))
	      (k (apply string-append-with-separator
			"&gt;"
			(separate-fields-discarding-char #\> j list)))
	      (l (apply string-append-with-separator
			"&quot;"
			(separate-fields-discarding-char #\" j list))))
	 l)))

(define-public (normalize-label str)
  (apply string-append-with-separator
	 "_"
	 (separate-fields-discarding-regexp (once (cached-regexp "[\n\t ]\\+"))
					    (sans-surrounding-whitespace str)
					    list)))


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


;;;; ??????????

;; (define (define-html-label database . parameters)
;;   (let ((label (kw-arg-ref parameters :name))
;; 	(link (kw-arg-ref parameters :link))
;; 	(filename (let ((f (database 'ref 'current-filename)))
;; 		     (or f " ** unknown file ** "))))
;;     (if (database 'ref `(html-label ,label))
;; 	(throw 'multiply-defined-label
;; 	       label
;; 	       `((html-label ,label) . ,filename)
;; 	       (database 'ref `(html-label ,label))))
;;     (database 'set-unique! `(html-label ,label) filename)
;;     (if link
;; 	(database 'set-unique! `(html-link ,label) link))))


					     

(define (mangle->filename str)
  (regexp-subst (once (cached-regexp "[[:([^0-9A-Za-z-]\\|[ \t\n]):]]\\+"))
		(string-downcase! (string str))
		(lambda ign "-")
		:global))

