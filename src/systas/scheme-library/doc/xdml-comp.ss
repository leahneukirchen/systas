#!../../=build/systas/systas
;;; xdml-comp.ss - A driver for processing XDML documents.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc xdml-comp)
  :use-module (doc old-pdml)
  :use-module (doc old-xdml)
  :use-module (doc hdml)
  :use-module (doc xdml-html)
  :use-module (doc xdml-texinfo)
  :use-module (doc snarf-c-documentation)
  :use-module (data-structures string-fun)
  :use-module (standard list-lib)
  :use-module (regexps subst)
  :use-module (regexps cached)
  :use-module (unix output-files)
  :use-module (unix options)
  :use-module (unix filenames)
  :use-module (unix file-utils))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h0 "xdml-comp")
;;; 
;;; `xdml-comp' is a driver program that manages the conversion of
;;; documentation source code into an extensible set of output formats
;;; including `HTML' and `texinfo' format, and (in a later release)
;;; `nroff -man' and `Postscript' format.
;;; 
;;; For its input, `xdml-comp' supports an extensible set of markup
;;; language readers -- documentation may be written in any of syntax
;;; for which a reader is provided.
;;; 
;;; Also, `xdml-comp' supports an extensible set of markup languages
;;; (sets of valid markup tags) -- documentation may be written using
;;; any of these tag languages.
;;; 
;;; Thus, when using `xdml-comp', it is necessary to specify:
;;; 
;;; 	1. What markup reader is being used.
;;;	2. What set of markup tags is being used.
;;;	3. What format of output is desired.
;;; 
;;; `xdml-comp' is typically used in several stages to process
;;; multiple documentation input files which define a single document.
;;; Some of these stages produce intermediate files which are further
;;; processed by successive invocations of `xdml-comp'. The formatted
;;; output of `xdml-comp' document is typically stored across multiple
;;; files.
;;; 
;;; 	Typical Stages of Document Processing Using xdml-comp
;;; 
;;; 	1. Each documentation source file is converted from its
;;;        input syntax to XDML s-expression syntax (an intermediate
;;;        representation).
;;; 
;;; 	2. The intermediate files from step 1 are read and scanned for
;;;        all forms of cross-reference.  `xdml-comp' builds an index
;;;        of the files and this index is stored in yet another
;;;        intermediate file.
;;;
;;;	3. Each of the intermediate files from step 1 are read, one at
;;; 	   a time, along with the index file produced in step 2.  For
;;;	   each file from step 1, a formatted output file is produced.
;;; 
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Markup Language Syntaxes and Markup Languages")
;;; 
;;; `xdml-comp' input files may be written in any of an extensible set
;;; set of "markup language syntaxes".  Each syntax is defined by a
;;; "markup language reader" which knows how to read input files in
;;; that syntax.  A markup language syntax describes how you, the
;;; documentation writer, format the documentation as you write it.
;;; The syntax specifies, for example, which parts of what you write
;;; are the text of the documentation, and which parts are markup
;;; directives that indicate how the documentation is structured.
;;; 
;;; Regardless of syntax, `xdml-comp' input files may be written in
;;; any of an extensible set of "markup languages".  A markup language
;;; defines how documentation may be logically structured by
;;; specifying a set of permissible "markup tags" and restrictions on
;;; where and how those tags may be used.
;;; 
;;; The markup language and markup language syntax are two different
;;; things.  The \markup language/ tells how your documentation may be
;;; _logically_structured_.  The \markup language syntax/ tells how
;;; your documentation source files must be _formatted_ in order to
;;; make the logical structure apparent to `xdml-comp'.
;;; 
;;; For example, the _markup_language_ `cdml' (for ``\C/
;;; \D/ocumentation \M/arkup \L/anguage'') specifies that documents
;;; may consist of a hierarchy of:
;;; 
;;; 	books
;;;	  chapters
;;;	    sections
;;;	      sub-sections
;;;	      etc.
;;;
;;; and that any chapter, section or subsection may contain one or
;;; more:
;;; 
;;;	paragraphs
;;;	reference entries for a C function, macro, variable or type
;;; 	pre-formatted examples
;;;	etc.
;;; 
;;; The markup language `cdml' specifies nothing about how those
;;; entities are expressed in the input to `xdml-comp'.
;;;
;;; On the other hand, the _markup_language_reader_ called `c-comment'
;;; specifies that a section is written similarly to this:
;;;
;;;	/*(h1 "<title of section>")
;;;	 * 
;;;	 * <text of section>
;;;	 *
;;;	 */
;;; 
;;; In that example, `h1' is a markup tag that marks a `cdml' section.
;;; The string `"<title of section>"' is a parameter for that markup
;;; tag.  In this case, `<text of section>' would be replaced by
;;; paragraphs, reference entries, etc.
;;; 
;;; Note that the `c-comment' reader knows nothing about specific
;;; mark-up tags such `h1'.  It only knows how tags are formatted in
;;; general.
;;; 
;;; To process a particular documentation input file, both a markup
;;; language, and a markup language reader must be specified.  This is
;;; done using the `--reader' and `--markup-language' options to
;;; `xdml-comp'.
;;;
;;; There is a default reader: it processes `s-expression' syntax (the
;;; syntax in which Scheme data structures are written).  It is
;;; unlikely that you will ever write documentation input files using
;;; that syntax, but it is likely you will process documentation
;;; intermediate files in that syntax.
;;; 
;;; There is a default markup language: `xdml'.  See XREF!!!:"" for
;;; more information about `xdml'.
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Output Formats")
;;; 
;;; `xdml-comp' can format documentation into any of an extensible
;;; set of formats.  Built-in to `xdml' are the output formats:
;;; 
;;; 	HTML	-- suitable for viewing with any Web Browser
;;; 	texinfo	-- which can be converted to Postscript or
;;;		   `info' format by the GNU info tools.
;;; 
;;; Later releases will also support:
;;; 
;;;	Postscript -- direct rendering of documents into postscript
;;;	nroff -man -- rendering of (parts of) programming reference
;;;		      manuals into traditional unix `man' pages.
;;; 
;;; The choice of output format is specified using the
;;; `--output-format' option to `xdml-comp'.  That option may be used
;;; more than once to specify more than one output format.
;;; 
;;; Although the choice of markup language specifies how documents may
;;; be structured, particular output formats may impose further
;;; restrictions on markup languages.  For example, when using the
;;; `texinfo' output format, chapter names must not contain commas,
;;; colons, or apostrophes (due to a limitation in the GNU info
;;; tools).  If only the `HTML' output is desired, that restriction is
;;; not present.
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h0 "xdml-comp usage")
;;;
;;; This chapter explains step-by-step how `xdml-comp' is used.
;;; Each of the command line options to `xdml-comp' is introduced.
;;; These options are summarized in the next chapter.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Invoking xdml-comp")
;;; 
;;; `xdml-comp' is normally invoked by specifying a set of options
;;; and a set of input files.  In some cases, the standard output of
;;; `xdml-comp' should be redirected to a file:
;;; 
;;; 	% xdml-comp [options] input-file ... [> output-file]
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Help and Version Information")
;;;
;;; The option `--help' causes `xdml-comp' to print a message
;;; that describes the recognized command line options:
;;; 
;;;	% xdml-comp --help
;;; 
;;; 
;;; The option `--version' causes `xdml-comp' to print a message
;;; that tells what version of `xdml-comp' is being used.
;;; 
;;;	% xdml-comp --version
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Converting Input Files to XDML s-expression Files")
;;; 
;;; Whatever syntax and markup-language is used to write
;;; documentation, the first stage of `xdml-comp' processing is to
;;; convert the documentation to an intermediate form called "XDML
;;; s-expression notation".  This intermediate form is stored in an
;;; intermediate file.
;;; 
;;; `xdml-comp' is invoked this way to convert input files to
;;; s-expression syntax:
;;; 
;;; 	% xdml-comp --reader <reader-name> 		\
;;;		    [--markup-language <language-name>]	\
;;;		    [--output-format <format> ...]	\
;;;		    --print				\
;;;		    > <output-file>
;;; 
;;; The readers that are built-in to `xdml-comp' are:
;;;
;;;	c-comment	A reader which knows how to extract
;;; 			documentation from specially formatted
;;;			comments in C (and therefore C++) 
;;;			programs. (See XREF!!!:"".)
;;;
;;;	default		A reader that understands s-expression
;;;			syntax.  The output of `xdml-comp',
;;;			when the `--print' option is used, is
;;;			in s-expression syntax.
;;; 
;;; The markup languages that are built-in to `xdml-comp' are:
;;; 
;;; 
;;; 	default 	A simple markup lanuage for books divided
;;; 	(aka xdml)	into chapters, sections, etc.
;;; 
;;; 	cdml		The `xdml' language, augmented with markup
;;;			tags specificly designed for documentation
;;; 			about programs and programming libraries.
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Building an Index File")
;;; 
;;; Many kinds of cross reference are possible in documentation
;;; formatted by `xdml-comp';  indexes, tables of contents, and
;;; hyperlinks (or printed page references) are some examples.
;;; 
;;; To handle such cross references, `xdml-comp' builds a single index
;;; of all cross-referencable entities in all documentation source
;;; files before formatting each individual file.  
;;; 
;;; `xdml-comp' is invoked this way to convert input files to
;;; build an index file:
;;; 
;;; 	% xdml-comp [--reader <reader-name>] 		\
;;;		    [--markup-language <language-name>]	\
;;;		    [--output-format <format> ...]	\
;;;		    --make-index
;;; 		    input-file ...
;;;		    > <output-file>
;;; 
;;; The `input-files' should normally be intermediate files produced
;;; as shown in xref:"Converting Input Files to XDML s-expression
;;; Files".  If that is the case, the `--reader' option should be
;;; omitted; the default reader should be used.
;;; 
;;; The `--make-index' option instructs `xdml-comp' to build an index
;;; of cross-referencable information and to print that index on
;;; standard output.  That output should ordinarily be directed to a
;;; file.
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Producing Final Output")
;;; 
;;; In the final stage of `xdml-comp' processing, each documentation
;;; file is processed to produce a formatted output file.
;;; 
;;; `xdml-comp' is invoked this way to produce formatted output:
;;; 
;;; 	% xdml-comp [--reader <reader-name>] 		\
;;;		    [--markup-language <language-name>]	\
;;;		    [--output-format <format> ...]	\
;;;		    [--use-index index-file]		\
;;;		    --format
;;; 		    input-file
;;; 
;;; The `input-file' should normally be one of intermediate files
;;; produced as shown in xref:"Converting Input Files to XDML
;;; s-expression Files".  If that is the case, the `--reader' option
;;; should be omitted; the default reader should be used.
;;; 
;;; The `--use-index' option instructs `xdml-comp' to resolve
;;; cross-references using an index file produced as shown in
;;; xref:"Building an Index File".
;;; 
;;; The `--format' option instructs `xdml-comp' to produce one or more
;;; formatted output files.  One file is produced for each specified
;;; output format.
;;; 
;;; The formatted output is stored in a file whose name is derived
;;; from the name of the input file by adding (or modifying) a
;;; filename extension.  For example, `HTML' output is stored in files
;;; with names with the extension `.html'.
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h0 "xdml-comp Command Line Option Summary")
;;;
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h0 "xdml-comp Shortcuts")
;;;
;;;

(define xdml-comp-options
  '((version		:char "V"
			:long-name "version"
			:documentation "Display version identification and exit.")
    (help		:char "h"
			:long-name "help"
			:documentation "Display a help message and exit.")

    (reader		:long-name "reader reader-name"
			:requires-argument
			:documentation "Specify an XDML reader.")
    (markup-language	:long-name "markup-language language-name"
			:requires-argument
			:documentation "Specify an XDML sub-language.")
    (output-format	:long-name "output-format format-name"
			:requires-argument
			:documentation "Specify an output format (html, texinfo, ...).")
    (use-index		:char "u index"
			:long-name "use-index index"
			:requires-argument
			:documentation "Add INDEX to the list of indexes searched.")

    (print		:char "p"
			:long-name "print"
			:documentation "Print the document s-expression on stdout.")
    (make-index		:char "m"
			:long-name "make-index"
			:documentation "Print an index of the input files on the standard output.")
    (format		:long-name "format"
			:documentation "Produce translated (formatted) output files.")))



;; (usage port argv0)
;;
;; Print a one-line usage message on port.
;; 
;; `argv0' should be the name by which the program was invoked.
;;
(define (usage port argv0)
  (with-output-to-port port
    (lambda ()
      (apply display*-port
	     port
	     #/list
	     /
	     #,(filename-tail argv0) --help | [options] [files]
	     /#))))


;; (usage-error args)
;;
;; Print a message on the current error port indicating a usage error.
;; The error message includes a `usage' message.
;;
;; `car' of `args' should be the name by which the program was invoked.
;; 
;; `cdr' of `args' should `()' or be the list of unprocessed command
;; line arguments, beginning with the argument that caused the error.
;;
(define (usage-error args)
  (usage (current-error-port) (car args))
  (apply display*-port
	 (current-error-port)
	 #/string
	 /
	 For a complete list options, use #,(char #\")#,(car args)#,(char #\") --help.
	 /#))

;; (version port)
;; 
;; Print a one-line version identifier on `port'.
;; 
(define (version port)
  (display "xdml-comp 1.0.0\n" port))

;; (help args)
;; 
;; Print an extended (multi-line) usage message on `port'.
;; 
;; `argv0' should be the name by which the program was invoked.
;; 
(define (help port argv0)
  (usage (current-output-port) argv0)
  (newline)
  (display (options-help xdml-comp-options)))


;; How input files are read.
;;
(define reader-names (xdml-named-reader 'default))

;; The name of an XDML sub-language to use.
;;
(define markup-language-name ())

;; A list of translations (e.g., 'texinfo and 'html) to invoke:
;;
(define output-format-names ())

;; A list of index files to read:
;;
(define input-indexes ())

;; Display XDML s-expressions on stdout?
;;
(define print-s-expression? #f)

;; Print an index on stdout?
;; 
(define print-index? #f)

;; Produce translated output files.?
;;
(define format? #f)


;; How tags may be nested:
;;
(define nesting-rules xdml-nesting-rules)

;; How XDML constructs are parsed.
;;
(define parser-extension #f)

;; Extensions for html and texinfo translations.
;;
(define ->html-extension #f)
(define ->texinfo-extension #f)



(set! main
      (lambda (args)
	(in '(doc xdml-comp))
	(let ((remaining-args (parse-arguments (or args '("xdml-comp")))))
	  (xdml-comp (cdr remaining-args)))))


(add-xdml-texinfo-translation)
(add-xdml-html-translation)

(define (parse-arguments args)
  (let ((remaining-arguments
	 (let loop ((args args))
	   (call-with-values (lambda () (next-option xdml-comp-options args))
			     (lambda (identifier option argument args)
			       (case identifier
				 ((#f)		args)
				 ((version)		(version)
							(%exit 0))
				 ((help)		(help (current-output-port) (car args))
							(%exit 0))

				 ((reader)		(let ((r (xdml-named-reader (string->symbol argument))))
							  (if (not r)
							      (begin
								(display*-port (current-error-port) "unrecognized reader: \"" argument "\"\n")
								(display*-port (current-error-port) "For a list of readers, try \"--help-languages\"\n")
								(%exit 1)))
							  (set! reader-names r)
							  (loop args)))
				 ((markup-language)	(if markup-language-name
							    (begin
							      (usage-error args)
							      (display*-port (current-error-port)
									     "\"" option "\"" " may be used only once.\n")
							      (%exit 1)))
							(set! markup-language-name argument)
							(loop args))
				 ((output-format)	(set! output-format-names (lset-adjoin eq? output-format-names (string->symbol argument)))
							(loop args))
				 ((use-index)	(set! input-indexes (cons argument input-indexes))
						(loop args))


				 ((print)		(set! print-s-expression? #t)
							(loop args))
				 ((make-index)	(set! print-index? #t)
						(loop args))
				 ((format)		(set! format? #t)
							(loop args))


				 (else		(display*-port (current-error-port) "unrecognized argument: \"" option "\"\n")
						(usage-error args)
						(%exit 1))))))))
    (if markup-language-name
	(begin
	  
	  (let* ((markup-language-module-name 	(map string->symbol (separate-fields-discarding-char #\/ markup-language-name list)))
		 (language-module 		(existing-module markup-language-module-name))
		 (language-name 		(car (reverse markup-language-module-name)))
		 (current-nesting-rules 	nesting-rules)
		 (current-parser-extension 	parser-extension)
		 (current->html-extension 	->html-extension)
		 (current->texinfo-extension 	->texinfo-extension))
	    (set! nesting-rules (xdml-combine-nesting-rules (eval-in-module (symbol-append language-name '-nesting-rules)
									    language-module)
							    current-nesting-rules))
	    (let ((parser (eval-in-module (symbol-append language-name '-parser-extension)
					  language-module)))
	      (set! parser-extension
		    (lambda (tag parameters sub-documents nesting-rule)
		      (parser tag parameters sub-documents nesting-rule current-parser-extension))))

	    (let ((->texinfo (eval-in-module (symbol-append language-name '->texinfo-extension)
					  language-module)))
	      (set! ->texinfo-extension
		    (lambda (document tag parameters sub-documents database nesting-rule parser-extension sub-document->texinfo)
		      (->texinfo document tag parameters sub-documents database nesting-rule parser-extension current->texinfo-extension sub-document->texinfo))))
	    (let ((->html (eval-in-module (symbol-append language-name '->html-extension)
					  language-module)))
	      (set! ->html-extension
		    (lambda (document tag parameters sub-documents database nesting-rule parser-extension sub-document->html)
		      (->html document tag parameters sub-documents database nesting-rule parser-extension current->html-extension sub-document->html)))))))

    remaining-arguments))



(define (xdml-comp input-files)
  (let ((key->file-table 		(make-hash-table 1021))
	(file->bindings-table		(make-hash-table 1021)))

    (for-each (lambda (index)
		(with-input-from-file index
		  (lambda ()
		    (let loop ((x (read)))
		      (and (not (eof-object? x))
			   (let ((filename (car x))
				 (bindings (cdr x)))
			     (and (not (hash-ref file->bindings-table filename))
				  (begin
				    (hash-set! file->bindings-table filename bindings)
				    (for-each (lambda (binding)
						; (pk 'key->file-table (car binding) filename)
						(hash-set! key->file-table (car binding) filename))
					       bindings)))
			     (loop (read))))))))
	      input-indexes)

    (let loop ((input-files input-files)
	       (index ()))
      (if (null? input-files)
	  (if print-index?
	      (for-each (lambda (filename-and-bindings)
			  (write filename-and-bindings)
			  (newline))
			index))
	  
	  (let* ((input-file 		(pk 'input-file (car input-files)))
		 (xdml 			(read-xdml input-file))
		 (document-table	(make-hash-table 1021))
		 (defined-in-table	(make-hash-table 1021))
		 (database-fn		(lambda (op key :optional value)

					  (define (multiply-defined file key value)
					    (display*-port (current-error-port)
							   "ERROR: multiply defined (" (->string key) ")\n"
							   "  defined in " (->string file) " as " (->string value) "\n"
							   "  HEY! defined again in " (->string (hash-ref defined-in-table key))
							   " as " (->string (hash-ref document-table key))"\n")
					    (throw 'multiply-defined key))

					  (if (not input-file)
					      (error "No input file defined in markup database."))
						 
					  (case op
					    ((file-of)		(hash-ref key->file-table key))
					    ((remove!)		(hash-remove! document-table key)
								(hash-remove! defined-in-table key))
					    ((ref)			(cond
									 ((hash-ref document-table key)		=> noop)
									 (#t
									  (let ((file (hash-ref key->file-table key)))
									    ; (pk 'file-for key :file file)
									    (and file
										 (not (string=? file input-file))
										 (let ((bindings (hash-ref file->bindings-table file)))
										   (for-each (lambda (binding)
											       (if (hash-ref document-table (car binding))
												   (multiply-defined
												    file (car binding) (hash-ref document-table (car binding)))
												   (begin
												     (hash-set! document-table (car binding) (cdr binding))
												     (hash-set! defined-in-table (car binding) file))))
											     bindings)
										   (hash-ref document-table key)))))))
					    ((set!)		(hash-set! document-table key value)
								(hash-set! defined-in-table key input-file))
					    ((set-unique!)		(let ((prev-dfn (hash-ref document-table key))
									      (prev-file (hash-ref defined-in-table key)))
									  (if (and prev-dfn
										   (not (and (equal? prev-file input-file)
											     (equal? prev-dfn value))))
									      (multiply-defined input-file key value))
									  (hash-set! document-table key value)
									  (hash-set! defined-in-table key input-file)))
					    ((for-each)		(vector-for-each
								 (lambda (bucket)
								   (map (lambda (binding)
									  (key (car binding) (cdr binding)))
									bucket))
								 document-table))
					    (else			(throw
									 'unrecognized-xdml-database-op op)))))
		 (translations-pass1 (map (lambda (translation-spec)
					    ((xdml-pass1 translation-spec) input-file
									   xdml
									   database-fn
									   nesting-rules
									   parser-extension
									   (case translation-spec
									     ((html)		->html-extension)
									     ((texinfo)		->texinfo-extension)
									     (else		#f))))
					  output-format-names))
		 (new-index	(and print-index?
				     (cons (cons input-file (apply append (vector->list document-table)))
					   index))))
	    (if format?
		(map (lambda (translation-spec translation)
		       (let ((str (force-lazy-string translation)))
			 ((xdml-pass2 translation-spec) input-file str document-table)))
		     output-format-names
		     translations-pass1))
	    (loop (cdr input-files) new-index))))))


(define (read-xdml input-file)
  (let ((answer (reader-names input-file)))
    (if print-s-expression?
	(write answer))
    answer))


