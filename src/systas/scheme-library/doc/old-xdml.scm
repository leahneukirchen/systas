;;; tag: Tom Lord Tue Dec  4 14:59:31 2001 (doc/old-xdml.scm)
;;;
;;; xdml.scm - The extensible document mark-up language.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc old-xdml)
  :use-module (doc hdml)
  :use-module (data-structures ratlist))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h0 "The Extensible Document Mark-up Language."
;;;	:scheme-module (doc xdml))
;;; 
;;; "XDML" -- The Extensible Document Mark-up Language -- is a type of
;;; HDML (hierarchical document mark-up language; see
;;; xref:""Hierarchical Document Mark-up Language").  It defines HDML
;;; tags for:
;;;
;;;	- A conventional fixed-depth hierarchical document structure:
;;; 		book
;;;		h0 (chapter)
;;;		h1 (section)
;;;		h2 (subsection)
;;;		h3 (sub-subsection)
;;;
;;;	- An explicit division of text into paragraphs.
;;; 
;;;	- Generation of tables of contents and indexes.
;;; 
;;; 	- Intra-document cross-references.
;;; 
;;;	- Hyperlink (URL) cross-references.
;;;	
;;;	- A small set of intra-paragraph semantic mark-ups
;;;	  (e.g. "definition", "emphar_sized")
;;;	
;;;	- Enumerations and lists.
;;;	
;;;	- Sub-document menus.
;;;	
;;;	- Enumerations and lists.
;;;	
;;;	- Pre-formatted text.
;;; 
;;; The XDML parser enforces rules for validly nesting these tags and
;;; rules for supplying valid parameters and sub-document lists.
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Tags Defined by XDML")
;;; 
;;; This section lists the mark-up tags of XDML and explains how they
;;; may be nested.  Sub-languages of XDML may add additional tags or
;;; nesting rules.
;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h2 "Hierarchical Book Structure")
;;; 
;;; An XDML document is a hierarchical structure of fixed depth.  The
;;; tags definining the hierarchy are:
;;;
;;;	book		;; A complete document
;;;			;; 
;;;			;; Positional Parameters:
;;;			;;
;;;			;;	"a string" 
;;;			;;		A title for the document.
;;;			;; 
;;;			;; Optional Keyword Parameters:
;;;			;;
;;;			;;	:subtitles ("list" "of" "strings")
;;;			;;		Subtitles for the document.
;;;			;;
;;;			;;	:authors ("list" "of" "strings")
;;;			;;		List of authors.
;;;			;;
;;;			;;	:one-line-summary "a string"
;;;			;;		A one line description of 
;;;			;;		document.
;;;			;;
;;;			;;	:keywords ("list" "of" "strings")
;;;			;;		A list of keywords and
;;;			;;		keyphrases for cataloging
;;;			;;		purposes.
;;;			;;	
;;;			;;	:copyright-years (list of integers)
;;;			;;		Years in which the document is
;;;			;;		copyrighted.
;;;			;;	
;;;			;;	:copyright "a string"
;;;			;;		The name of the copyright 
;;;			;;		owner.
;;;			;;	
;;;			;;	:publisher
;;;			;;		The name of the publisher.
;;;			;;	
;;;			;;	:contains ("list" "of" "(mostly)" "strings")
;;;			;;		The table of contents.
;;;			;;		The list should contains
;;;			;;		chapter titles.  Sometimes
;;;			;;		more than one formatted
;;;			;;		document can be generated from
;;;			;;		a single source document by
;;;			;;		specifying more than one
;;;			;;		`book' tag. In that case, this 
;;;			;;		list determines what chapters 
;;;			;;		are included in a particular
;;;			;;		formatted document.
;;;			;;	
;;;			;;	:indexes (list of index specifiers)
;;;			;;		A list of indexes to generate.
;;;			;;		Each index specifier is:
;;;			;;		  ("Index title" . index-categories)
;;;			;;		where index-categories is a
;;;			;;		list of symbols (see `index')
;;;			;;	
;;;			;;	:on-line-permissions ("list" "of" "strings")
;;;			;;		Copying permissions for
;;;			;;		on-line formatted versions of the
;;;			;;		document.  Each string
;;;			;;		generates one line of output.
;;;			;;	
;;;			;;	:printed-permissions ("list" "of" "strings")
;;;			;;		Copying permissions for
;;;			;;		print formatted versions of the
;;;			;;		document.  Each string
;;;			;;		generates one line of output.
;;;			;;	
;;;
;;;	h0		;; A chapter
;;;			;; 
;;;			;; Positional Parameters:
;;;			;;
;;;			;;	"a string" 
;;;			;;		A title for the chapter.
;;;			;;	
;;;			;; Optional Keyword Parameters:
;;;			;;	
;;;			;;	:subtitle "A string"
;;;			;;		A subtitle for the chapter.
;;;			;;	
;;;			;;	:includes
;;;			;;	:need
;;;			;;
;;;			;; 
;;;	h1		;; A section
;;;			;; 
;;;			;; Positional Parameters:
;;;			;;
;;;			;;	"a string" 
;;;			;;		A title for the section.
;;;			;; 
;;;			;; Optional Keyword Parameters:
;;;			;;	
;;;			;;	:subtitle "A string"
;;;			;;		A subtitle for the section.
;;;			;;
;;;	h2		;; A sub-section
;;;			;; 
;;;			;; Positional Parameters:
;;;			;;
;;;			;;	"a string" 
;;;			;;		A title for the sub-section.
;;;			;; 
;;;			;; Optional Keyword Parameters:
;;;			;;	
;;;			;;	:subtitle "A string"
;;;			;;		A subtitle for the sub-section.
;;;			;;
;;;	h3		;; A sub-sub-section
;;;			;; 
;;;			;; Positional Parameters:
;;;			;;
;;;			;;	"a string" 
;;;			;;		A title for the sub-sub-section.
;;;			;; 
;;;			;; Optional Keyword Parameters:
;;;			;;	
;;;			;;	:subtitle "A string"
;;;			;;		A subtitle for the sub-sub-section.
;;;			;;
;;; 
;;; Each of these tags requires one parameter: a title.  For example:
;;;
;;;	((h0 "The Title of This Book") . <sub-documents>)
;;;


(define-public xdml-document-type-tags	'(book))
(define-public xdml-level-0-tags	'(h0))
(define-public xdml-level-1-tags	'(h1))
(define-public xdml-level-2-tags	'(h2))
(define-public xdml-level-3-tags	'(h3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h2 "Menus")
;;; 
;;; The tag `menu' is used with no parameters or sub-documents in
;;; chapter or section (or sub- or sub-sub-section) to format a
;;; hyper-text menu of sub-documents.
;;;

(define-public xdml-block-of-text-leaf-tags	 	`(menu))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h2 "Paragraph Structure")
;;; 
;;; Chapters and sections (including sub and sub-sub-sections) of
;;; books are divided into paragraphs:
;;;
;;;	p		;; An ordinary paragraph
;;;	insert		;; A pre-formatted fixed-width-font paragraph
;;;	paragraphs	;; A group of paragraphs
;;; 
;;; 
;;; None of these tags requires parameters.
;;; 
;;; The sub-documents of `p' are structured text (defined in a later
;;; section).  
;;; 
;;; The sole sub-document of `insert' must be a string -- it is
;;; ordinarily rendered as-is (perhaps indented) in a fixed-width font
;;; and is usually used to tag input or output from a computer.
;;; 
;;; The sub-documents of `paragraphs' are typically paragraphs (`p')
;;; and inserts (`insert').  In a context where only one sub-document
;;; is permitted, but multiple paragraphs are desired, `paragraphs' is
;;; the tag to use.
;;; 

(define-public xdml-ordinary-block-of-text-tags 	`(p))
(define-public xdml-block-of-text-string-leaf-tags 	`(insert))
(define-public xdml-recursive-block-of-text-tags 	`(paragraphs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h2 "Structured Text Tags")
;;; 
;;; Structured text tags are used inside of paragraphs.
;;;
;;;	()		;; The empty string.
;;;			;;	
;;; 
;;;	string		;; An untagged string may be a paragraph
;;;			;; sub-document 
;;;
;;; 	text		;; An arbitrary grouping of structured
;;;			;; text.
;;; 
;;;	stress		;; Text marked for mild emphasis.
;;;
;;;	emph		;; Text marked for strong emphasis.
;;; 
;;;	topic		;; Text marked as the name of a topic.
;;; 
;;; 	input		;; Text representing pre-formatted computer
;;;			;; input.
;;; 
;;; 	output		;; Text representing pre-formatted computer
;;;			;; output.
;;; 
;;; 	index		;; Index items genreate index entries 
;;;			;; and produce no typeset output.
;;;			;; 
;;;			;; Keyword Parameters:
;;;			;;
;;;			;;	:term "a string"
;;;			;;		The term being defined, for
;;; 			;; 		the purpose of indexing.
;;;			;;	:primary
;;;			;;		This is the primary index point
;;;			;;		for the term.
;;;			;;	:default-primary
;;;			;;		This is the primary index point
;;;			;;		for the term unless another index
;;;			;;		point specifies :primary.  This
;;;			;;		is used in automatically generated
;;;			;;		index entries to express an overridable
;;;			;;		guess that a particular index point
;;;			;;		is primary.
;;;			;;	:categories (list of symbols)
;;;			;;		A list of indexing categories.
;;;			;;		Each category names a separate
;;;			;;		kind of index in which the
;;; 			;; 		term should be included.  An
;;;			;;		actual index contains all of
;;; 			;; 		the terms for one or more 
;;;			;;		categories.
;;;			;;	
;;;			;;		The index categories
;;;			;;		recognized by existing 
;;;			;;		formatters are:
;;;			;;	
;;;			;;			function
;;;			;;			  function names
;;;			;;			macro
;;;			;;			  macro names
;;;			;;			variable
;;;			;;			  variable names
;;;			;;			type
;;;			;;			  type names
;;;			;;			program
;;;			;;			  program names
;;;			;;			general
;;;			;;			  arbitrary index
;;;			;;			  terms
;;;			;;
;;; 
;;; 
;;; 	idx		;; Short-hand:
;;;			;;
;;;			;;	(idx () "foo") 
;;;			;;
;;;			;; is equivalent to:
;;;			;;
;;;			;;	((text) ((index :term "foo")) "foo")
;;;			;;
;;; 
;;; 
;;;	definition	;; The point of definition of an indexed 
;;;			;; term.
;;;			;;
;;;			;; Keyword Parameters:
;;;			;;
;;;			;;	:term "a string"
;;;			;;		The term being defined, for
;;; 			;; 		the purpose of indexing.
;;;			;;	:categories (list of symbols)
;;;			;;		A list of indexing categories.
;;;			;;
;;;			;; Subdocuments:
;;;			;;
;;;			;;	Structured text for the term being 
;;;			;;	defined.
;;;			;;
;;; 
;;; 	dfn		;; Short-hand:
;;;			;;
;;;			;;	(dfn () "foo")
;;;			;;
;;;			;; is equivalent to:
;;;			;;
;;;			;;	(definition (:term "foo") "foo")
;;;			;;
;;; 
;;; 	label		;; A point of possible cross-reference.
;;; 			;;
;;;			;; Keyword Parameters:
;;;			;;
;;;			;;	:name "a string"
;;;			;;		A name for the
;;;			;;		cross-reference.
;;; 			;;	:link "a string"
;;;			;;		Text for hypertext links
;;; 			;;		to this label.
;;; 			;;
;;; 			;;
;;; 
;;; 	ref		;; A cross-reference.
;;; 			;;
;;;			;; Keyword Parameters:
;;;			;;
;;;			;;	:name "a string"
;;;			;;		The name of the
;;;			;;		cross-referenced point
;;; 			;;		and the text to format.
;;;			;;
;;; 
;;; 
;;;	hyperlink	;; A URL, rendered as a live link in HTML
;;;			;; documents.
;;; 			;;
;;;			;; Keyword Parameters:
;;;			;;
;;;			;;	:url "a string"
;;;			;;		The target of the link (a URL).
;;; 			;; 
;;;			;; Subdocuments:
;;;			;;
;;;			;;	A title for the link.
;;;			;; 
;;; 
;;; 	url		;; short-hand:
;;;			;; 
;;;			;; 	(url () "http://555-1212.com")
;;;			;; 
;;;			;; stands for:
;;;			;; 
;;;			;;	((hyperlink :url "http://555-1212.com")
;;;			;;	 "http://555-1212.com")
;;;			;;
;;; 
;;; 	list		;; A non-enumerated list.  Subdocuments should be
;;; 			;; tagged `item'.
;;; 
;;; 	enum		;; An enumerated list.  Subdocuments should be
;;; 			;; tagged `item'.
;;; 
;;; 	item		;; An item within a list.
;;; 

(define-public xdml-ordinary-structured-text-tags '(string
						    text
						    definition
						    stress
						    emph
						    topic
						    hyperlink))

(define-public xdml-structured-text-string-leafs '(output input dfn idx url))
(define-public xdml-structured-text-null-leafs '(label index ref))

(define-public xdml-list-tags		'(list enum))

(define-public xdml-list-item-tags	'(item))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h2 "XDML Tag Nesting Rules")
;;;
;;;
;;;


(define-public (xdml-nesting-rules-product containers . all-contained)
  (let ((contained (apply append all-contained)))
    (map (lambda (container)
	   `(,container ,@ contained))
	 containers)))

(define-public (xdml-combine-nesting-rules . args)
  (define (xdml-combine-nesting-rules-2 r1 r2)
    (let ((keys (unionq (map car r1) (map car r2))))
      (map (lambda (key)
	     (cons key (unionq (assq-ref r1 key) (assq-ref r2 key))))
	   keys)))
  (reduce-init xdml-combine-nesting-rules-2 (car args) (cdr args)))


(define-public xdml-block-of-text-tags 			(append xdml-ordinary-block-of-text-tags
								xdml-recursive-block-of-text-tags
								xdml-block-of-text-string-leaf-tags
								xdml-block-of-text-leaf-tags))
(define-public xdml-nonrecursive-block-of-text-tags	(append xdml-ordinary-block-of-text-tags
								xdml-block-of-text-string-leaf-tags
								xdml-block-of-text-leaf-tags))

(define-public xdml-structured-text-tags (append xdml-ordinary-structured-text-tags
						 xdml-structured-text-string-leafs
						 xdml-list-tags))

(define-public xdml-list-contents-tags (append xdml-structured-text-tags
					       xdml-list-item-tags))

(define-public xdml-section-text-tags (append xdml-block-of-text-tags
					      xdml-recursive-block-of-text-tags
					      xdml-structured-text-tags))

(define-public xdml-nesting-rules	(xdml-combine-nesting-rules
					 (xdml-nesting-rules-product xdml-document-type-tags xdml-level-0-tags)
					 (xdml-nesting-rules-product xdml-level-0-tags xdml-level-1-tags xdml-section-text-tags)
					 (xdml-nesting-rules-product xdml-level-1-tags xdml-level-2-tags xdml-section-text-tags)
					 (xdml-nesting-rules-product xdml-level-2-tags xdml-level-3-tags xdml-section-text-tags)
					 (xdml-nesting-rules-product xdml-level-3-tags xdml-section-text-tags)
					 (xdml-nesting-rules-product xdml-ordinary-block-of-text-tags xdml-structured-text-tags)
					 (xdml-nesting-rules-product xdml-recursive-block-of-text-tags xdml-block-of-text-tags)
					 (xdml-nesting-rules-product xdml-recursive-block-of-text-tags xdml-structured-text-tags)
					 (xdml-nesting-rules-product xdml-block-of-text-string-leaf-tags '(string))
					 (xdml-nesting-rules-product xdml-block-of-text-leaf-tags '())
					 (xdml-nesting-rules-product xdml-ordinary-structured-text-tags xdml-structured-text-tags)
					 (xdml-nesting-rules-product xdml-structured-text-string-leafs '(string))
					 (xdml-nesting-rules-product xdml-structured-text-null-leafs '())
					 (xdml-nesting-rules-product xdml-list-tags xdml-list-contents-tags)
					 (xdml-nesting-rules-product xdml-list-item-tags xdml-structured-text-tags)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "XDML Data Structures")
;;;
;;;
;;;

;;(s xdml)
;; (xdml tag parameters . sub-documents)
;; 
;; Construct an XDML document from the indicated tag, parameter list
;; and sub-documents.
;; 
(define-public (xdml tag parameters . sub-documents)
  (make-xdml tag parameters sub-documents))

;;(s make-xdml)
;; (make-xdml tag parameters sub-documents)
;; 
;; Construct an XDML document from the indicated tag, parameter list
;; and sub-documents.
;; 
(define-public (make-xdml tag parameters sub-documents)
  (let ((answer `((,tag ,@parameters) ,@sub-documents)))
    (xdml-parse answer
		(lambda ign answer))))

;;(s xdml?)
;; (xdml? document)
;; 
;; Return #t if `obj' is a valid XDML document, #f otherwise.  This is
;; a recursive test that examines all sub-documents of `obj'.
;; 
(define-public (xdml? document)
  (xdml-parse document
	      (lambda (tag parameters . sub-documents)
		(and-map xdml? sub-documents))
	      (lambda (document return)
		(and (pair? document)
		     (string? (car document))
		     (xdml? `(structured-text () ,@document))))))
			
(define-public (xdml-parse document :optional return error-return nesting-rules extension)
  (let ((nesting-rules 	(or nesting-rules xdml-nesting-rules))
	(extension	(or extension (lambda (tag parameters sub-documents nesting-rules)
					(throw 'xdml-bad-syntax tag parameters sub-documents)))))
    (if (and (pair? document)
	     (string? (car document)))
	
	(xdml-parse `(structured-text () ,@document) return error-return)

	(parse-hdml document
		    (lambda (tag parameters . sub-documents)
		      (if (not (memq 'string (assq-ref nesting-rules tag)))
			  (set! sub-documents
				(pick (lambda (x)
					(not (and (string? x)
						  (regexec (once (regcomp "^[ \t\n]*$"))
							   x
							   #f))))
				      sub-documents)))
		      (if (or (eq? error-return #t)
			      (xdml-shallow-typecheck? tag parameters sub-documents nesting-rules extension))
			  
			  (return tag parameters sub-documents)

			  (if error-return
			      (error-return document return)
			      (throw 'xdml-syntax-error document))))
		    error-return))))


(define (xdml-shallow-typecheck? tag parameters sub-documents nesting-rules extension?)

  (let ((rule (assq tag nesting-rules)))
    (if (not rule)
	(throw 'xdml-unrecognized-tag tag))
    (let ((containable (cdr rule)))
      (if (not containable)
	  (or (not sub-documents)
	      (throw 'xdml-no-sub-documents-permitted tag parameters sub-documents)))
      (and-map (lambda (sd)
		 (cond
		  ((null? sd)		(memq () containable))
		  ((string? sd)		(memq 'string containable))
		  ((pair? sd)		(memq (car sd) containable))
		  (#t			(throw 'xdml-illegal-sub-document-nesting tag parameters sd))))
	       sub-documents)))
  (case tag
    ((book)			(has-title? parameters))
    ((h0)			(has-title? parameters))
    ((h1)			(has-title? parameters))
    ((h2)			(has-title? parameters))
    ((h3)			(has-title? parameters))

    ((p)			#t)
    ((insert)			#t)
    ((paragraphs)		#t)

    ((menu)			#t)

    ((string)			#t)
    ((text)			#t)
    ((stress)			#t)
    ((emph)			#t)
    ((topic)			#t)
    ((input)			#t)
    ((output)			#t)
    ((index)			(read-only-string? (kw-arg-ref parameters :term)))
    ((idx)			(singleton? sub-documents))
    ((definition)		(read-only-string? (kw-arg-ref parameters :term)))
    ((dfn)			(singleton? sub-documents))

    ((label)			(label-params? parameters))
    ((ref)			(kw-arg-ref parameters :name))

    ((hyperlink)		(hyperlink-params? parameters))
    ((url)			(singleton? sub-documents))

    ((list)			#t)
    ((enum)			#t)
    ((item)			#t)
    (else			(extension? tag parameters sub-documents nesting-rules))))


(define (has-title? parameters)
  (and (pair? parameters)
       (string? (car parameters))))

(define (label-params? parameters)
  (->bool (kw-arg-ref parameters :name)))

(define (singleton? obj)
  (and (pair? obj)
       (null? (cdr obj))))

(define (hyperlink-params? parameters)
  (kw-arg-ref parameters :url))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "XDML Readers")
;;;
;;;
;;;

(define (xdml-read-file file) (with-input-from-file file read))

(define named-xdml-readers `((default . ,xdml-read-file)))

(define-public (xdml-name-reader name reader)
  (set! named-xdml-readers (cons (cons name reader) named-xdml-readers)))

(define-public (xdml-named-reader name)
  (assq-ref named-xdml-readers name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "XDML and Extended String Syntax")
;;; 
;;; 
;;; 


(define-public val first-value)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "XDML Translators")
;;;
;;;
;;;

(define xdml-translation-specs '())

(define-public (xdml-define-translation spec pass1-fn pass2-fn)
  (set! xdml-translation-specs (cons `(,spec ,pass1-fn ,pass2-fn)
				     xdml-translation-specs)))

  
(define-public (xdml-pass1 translation-spec)
  (car (or (assq-ref xdml-translation-specs translation-spec)
	   (throw 'undefined-xdml-translation translation-spec))))

(define-public (xdml-pass2 translation-spec)
  (cadr (or (assq-ref xdml-translation-specs translation-spec)
	    (throw 'undefined-xdml-translation translation-spec))))

