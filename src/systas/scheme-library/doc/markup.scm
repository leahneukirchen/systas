;;; tag: Tom Lord Tue Dec  4 14:59:31 2001 (doc/markup.scm)
;;;
;;; markup.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc markup)
  :use-module (doc hdml)
  :use-module (data-structures ratlist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Serious Bugs (fix NOW!):
;;; 
;;; documentation for tags
;;; per-format checkers
;;; 
;;; Todo:
;;; 
;;; 
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h0 "Extensible Markup Languages")
;;; 
;;; A markup language is a name that is unique among markup languages,
;;; a set of parent languages, a set of tags, formatters for those
;;; tags, rules for nesting those tags, documentation for the
;;; language, and documentation for each tag.
;;; 
;;; Markup languages inherit all properties from their parent
;;; languages according to rules stated in the documentation for each
;;; property.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Declaring Markup Languages")
;;;
;;;
;;;

;;(s declare-markup-language)
;; (declare-markup-language name . kw-params)
;; 
;; Declare `name' (an atom) to be the name of a markup language.
;;
;; `kw-params' is a list of keyword parameters and keyword arguments.
;; It may include:
;; 
;; 	:synopsis "string"	
;;	-- A one line description of the language, formatted 
;; 	   for an 80 column display.
;; 
;; 	:docstring "string"
;;	-- A brief, but multi-line description of the language, 
;;	   formatted for an 80 column display.
;; 
;; 	:parents (list of atoms)
;;	-- A list of other markup languages from which this one
;;	   inherits tags and formatting rules.
;;
;;	:formatter-entry-points
;;	-- an association list keyed on output format names (atoms).
;;	   The binding of each key should be a list:
;;
;;		(formatter-entry-point . kw-arguments)
;; 
;;	   See xref:"Markup Language Formatter Entry Points" for 
;;	   information about formatter entry point procedures.
;; 
;; 
(define-public (declare-markup-language name . kw-params)
  (if (known-markup-language? name)
      (display*-port (current-error-port)
		     "WARNING: markup language "
		     name
		     " declared more than once.\n"))
  (hash-set! known-markup-languages name #t)
  (let ((synopsis 		(kw-arg-ref kw-params :synopsis))
	(docstring 		(kw-arg-ref kw-params :docstring))
	(parent-languages 	(kw-arg-ref kw-params :parents))
	(formatter-entry-points (kw-arg-ref kw-params :formatter-entry-points)))
    (set-markup-language-parents! name parent-languages)
    (set-markup-language-documentation! name synopsis docstring)
    (for-each (lambda (language-entry-point)
		(let ((format (car language-entry-point))
		      (entry	(cadr language-entry-point)))
		  (set-language-formatter-entry-point! name format entry)))
	      formatter-entry-points)))


;; known-markup-languages
;; 
;; A hash table whose keys are the names of markup languages declared
;; using `declare-markup-language'.
;; 
(define known-markup-languages (make-hash-table))


;;(s check-known-markup-language)
;; (check-known-markup-language name)
;; 
;; Throw an exception of type `not-a-known-markup-language' iff `name'
;; (an atom) is _not_ the name of a markup language declared using
;; `declare-markup-language'.
;; 
(define-public (check-known-markup-language name)
  (or (known-markup-language? name)
      (throw 'not-a-known-markup-language name)))


;;(s known-markup-language?)
;; (known-markup-language? name)
;; 
;; Return #t iff `name' (an atom) is the name of a markup language
;; declared using `declare-markup-language'.
;; 
(define-public (known-markup-language? name)
  (hash-ref known-markup-languages name))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Markup Language Heritage")
;;;
;;; Inheritence defined by a list of names of parent languages.
;;; 
;;; "ancestors" means the transitive closure of "parents".  Cycles
;;; not permitted (not currently detected).  Could permit cycles by
;;; using a fixed-point detection technique in
;;; `markup-language-ancestors'.
;;;


;; language-parents-table
;; 
;; A hash table mapping each markup language name to a list of its
;; parent languages.
;; 
(define language-parents-table (make-hash-table))


;;(s set-markup-language-parents!)
;; (set-markup-language-parents! name parents)
;; 
;; Set the list of parents of the markup language named by `name' to
;; the languages named in the list `parents'.
;; 
(define-public (set-markup-language-parents! name parents)
    (hash-set! language-parents-table name parents))

;;(s markup-language-parents)
;; (markup-language-parents name)
;; 
;; Return a list of names of parent languages of the markup language
;; named by `name' (an atom).
;; 
(define-public (markup-language-parents name)
  (hash-ref language-parents-table name))

;;(s markup-language-ancestors)
;; (markup-language-ancestors name)
;; 
;; Return a list of names of all ancestor languages (parent languages,
;; their parent languages, etc.) of the markup language named by
;; `name' (an atom).  This is the transitive closure of
;; `markup-language-parents' but it does not check for cycles.
;; 
(define-public (markup-language-ancestors name)
  (let* ((parents (hash-ref language-parents-table name))
	 (older-generations (map markup-language-parents parents)))
    (apply unionq parents older-generations)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "On-line Markup Language Documentation")
;;; 
;;; Each language has a one-line "synopsis" and multi-line "docstring"
;;; ("documentation string").  The synopsis may be printed by itself,
;;; or followed by the documentation string.  The documentation string
;;; is not meant to be printed by itself.
;;; 
;;; Format documentation for an 80 column display with a fixed-width
;;; character set.
;;; 
;;; Not inherited.
;;; 

;; language-synopsis-table
;; 
;; A hash table mapping each markup language name to a one-line
;; description.
;; 
(define language-synopsis-table (make-hash-table))


;; language-docstring-table
;; 
;; A hash table mapping each markup language name to a multi-line
;; description.
;; 
(define language-docstring-table (make-hash-table))


;; (set-markup-language-documentation! name synopsis docstring)
;; 
;; Record the synopsis and documentation for a markup language.
;; 
(define (set-markup-language-documentation! name synopsis docstring)
  (hash-set! language-synopsis-table name synopsis)
  (hash-set! language-docstring-table name docstring))


;;(s markup-language-synopsis)
;; (markup-language-synopsis name)
;; 
;; Return a one-line description of the markup language named by
;; `name' (an atom).  If no synopsis was explicitly provided,
;; the synopsis is `"No synopsis provided."'.
;; 
(define-public (markup-language-synopsis name)
  (or (hash-ref language-synopsis-table name)
      "No synopsis provided."))


;;(s markup-language-docstring)
;; (markup-language-docstring name)
;; 
;; Return a multi-line description of the markup language named by
;; `name' (an atom).  If no synopsis was explicitly provided, the
;; docstring is `"No documentation provided."'.
;; 
(define-public (markup-language-docstring name)
  (or (hash-ref language-docstring-table name)
      "No documentation provided."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Markup Language Formatter Entry Points")
;;;
;;; A formatter entry point is a procedure which is invoked:
;;; 
;;;	(formatter-entry-point output-filename 
;;;			       document
;;;			       database
;;;			       format-sub-document)
;;;	
;;; Inherited from parent languages.
;;; 
;;; Default is:
;;;
;;;	(lambda (filename document db format-sub-document)
;;;	  (format-sub-document document))
;;; 


;; language-formatter-entry-point-table
;;
;; A hash table mapping each markup language name/format
;; to a procedure which is the entry point for formatting
;; one output file in that language/format.
;;
(define language-formatter-entry-point-table (make-hash-table))
(define (language-formatter-entry-point-table-key language format)
  (cons language format))

;; (set-language-formatter-entry-point! language format procedure)
;; 
;; Set the formatter entry point procedure for language/format.
;; 
(define (set-language-formatter-entry-point! language format procedure)
  (hash-set! language-formatter-entry-point-table
	     (language-formatter-entry-point-table-key language format)
	     procedure))

;;(s markup-language-formatter-entry-point)
;; (markup-language-formatter-entry-point language format)
;; 
;; Return the formatter entry point procedure for language/format.
;; If no entry point was explicitly provided, return a default
;; entry point which simply invokes the `format-sub-document'
;; procedure.
;; 
(define-public (markup-language-formatter-entry-point language format)
  (or (language-formatter-entry-point-without-default language format)
      (lambda (filename document database format-sub-document)
	(format-sub-document document))))

;; (language-formatter-entry-point-without-default language format)
;; 
;; Return the explicitly provided formatter entry point procedure for 
;; language/format (taking inheritence into account).  If no entry
;; point was provided, return #f.
;; 
(define (language-formatter-entry-point-without-default language format)
  (or (hash-ref language-formatter-entry-point-table
		(language-formatter-entry-point-table-key language format))
      (or-map (lambda (parent)
		(language-formatter-entry-point-without-default parent format))
	      (markup-language-parents language))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Declaring Markup Tags")
;;;
;;; A simple interface for setting all of the parameters about
;;; a languge/markup tag at once.
;;;



;;(s declare-markup-tag)
;; (declare-markup-tag language tag . kw-args)
;; 
;; Define a new markup tag for the markup language named by `language'
;; (an atom).  `tag' should be an atom.
;; 
;; Recognized keyword arguments are:
;; 
;; 	:can-contain (tag list)
;;	-- a list of tags that can be sub-documents of this tag.
;; 
;; 	:parameter-check procedure
;;	-- a procedure used to validate arguments to `tag'.
;;	   See xref:"Per-language Tag Parameter Checkers".
;; 
;;	:synopsis
;;	-- a one-line description of the tag.
;; 
;;	:documentation
;;	-- a multi-line description of the tag.
;; 
;;	:formatters
;;	-- an association list keyed on output format names (atoms).
;;	   The binding of each key should be a list:
;;
;;		(formatter-procedure . kw-arguments)
;; 
;;	   See xref:"Markup Language Formatters" for information about
;;	   formatter procedures.
;; 
;; 	  `kw-arguments' (of one element of the list for :formatters)
;;	  may include:
;; 
;; 		:per-format-check
;;		 -- like :parameter-check to `declare-markup-tag'
;;		 and invoked in addition to that procedure when
;; 		 formatting for this specific format.
;; 
;; Attributes tags are inherited in various ways: see the
;; documentation for each attribute.
;; 
;; Tags are inherited.  If TAG is not declared for a language XYZZY,
;; but is declared for one of its ancestor languages, it is the same
;; as:
;; 
;; 		(declare-markup-tag 'XYZZY 'TAG)
;; 
(define-public (declare-markup-tag language tag . kw-args)
  (let* ((containable		(kw-arg-ref kw-args :can-contain))
	 (checker		(kw-arg-ref kw-args :parameter-check))
	 (synopsis		(kw-arg-ref kw-args :synopsis))
	 (documentation		(kw-arg-ref kw-args :documentation))
	 (formatters		(kw-arg-ref kw-args :formatters)))

    (if (tag-known-without-inheritence? language tag)
	(display*-port (current-error-port)
		       "WARNING: markup language tag "
		       tag
		       " declared more than once\n"
		       "  for markup language " language "\n"))
    (assert-tag-known language tag)
    (set-tag-containables! language tag containable)
    (set-tag-checker! language tag checker)
    (for-each (lambda (format-spec)
		(let ((format (car format-spec))
		      (proc 	(cadr format-spec))
		      (per-format-check (kw-arg-ref kw-args :per-format-check)))
		  (set-tag-formatter! language tag format proc)))
	      formatters)))

;; known-tags-table
;; 
;; A table mapping language/tag pairs to #t for declared tags.
;; 
(define known-tags-table (make-hash-table 1021))
(define (known-tags-table-key language tag)
  (cons language tag))


;; (assert-tag-known language tag)
;; 
;; Record that `tag' is a valid markup tag in the markup language `language'.
;; 
(define (assert-tag-known language tag)
  (hash-set! known-tags-table (known-tags-table-key language tag) #t))


;;(s tag-known-without-inheritence?)
;; (tag-known-without-inheritence? language tag)
;; 
;; Return #t if `tag' was explicitly delcared for markup language
;; `language'.
;; 
(define-public (tag-known-without-inheritence? language tag)
  (hash-ref known-tags-table (known-tags-table-key language tag)))

;;(s known-tag?)
;; (known-tag? language tag)
;; 
;; Return #t if `tag' was delcared for markup language `language' or
;; for an ancestor of `language'.
;; 
;; 
(define-public (known-tag? language tag)
  (or (hash-ref known-tags-table (known-tags-table-key language tag))
      (or-map (lambda (ancestor)
		(tag-known-without-inheritence? ancestor tag))
	      (markup-language-ancestors tag))))


;;(s check-tag)
;; (check-tag language tag)
;; 
;; Throw an exception if `tag' was not delcared for markup language 
;; `language' or any ancestor of `language':
;; 
;; 	(throw 'unknown-markup-tag :language language :tag tag)
;; 
(define-public (check-tag language tag)
  (if (not (known-tag? language tag))
      (throw 'unknown-markup-tag :language language :tag tag)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Containable Tags")
;;;
;;; Each language/tag pair may have an explicitly declared set
;;; of containable tags: those tags which can be used in
;;; sub-documents.
;;; 
;;; Inheritence: the valid sub-document tags for a given language/tag
;;; pair is the union of explicitly declared containable tags for that
;;; language and its ancestor languages.
;;;

;; containables-table
;; 
;; A hash table mapping language/tag pairs to explicitly declared
;; lists of valid sub-document tags.
;; 
(define containables-table (make-hash-table 127))
(define (containables-table-key language tag)
  (cons language tag))

;;(s set-tag-containables!)
;; (set-tag-containables! language tag containable-tags)
;; 
;; Explicitly declare the list `containable-tags' to be valid
;; sub-document tags of `tag' in markup language `language'.
;; 
(define-public (set-tag-containables! language tag containable-tags)
  (hash-set! containables-table
	     (containables-table-key language tag)
	     containable-tags))

;;(s tag-containables)
;; (tag-containables language tag)
;; 
;; Return the set of all tags that are valid sub-document tags of
;;; `tag' in markup language `language', taking inheritence into
;;; account.
;; 
(define-public (tag-containables language tag)
  (let* ((per-language 		(hash-ref containables-table
					  (containables-table-key language tag)))
	 (ancestors 		(markup-language-ancestors language))
	 (ancestor-containables (map (lambda (lingua) (tag-containables lingua tag))
				     ancestors)))
    (apply unionq per-language ancestor-containables)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Per-language Tag Parameter Checkers")
;;;
;;; For every tag defined in a markup language there is a procedure
;;; which checks the validity of parameters supplied to that tag and
;;; throws an exception if the parameters are invalid:
;;; 
;;; 	(throw-invalid-tag-parameters tag params message)
;;; 
;;; where `tag' is the tag, `params' is () or the tail of the
;;; parameter list beginning with the first invalid parameter, and
;;; `message' is an 80-column string describing the problem.
;;;
;;; Checker procedures are invoked:
;;; 
;;; 	(check tag parameters inherited-checkers)
;;; 
;;; Note that the procedure returned by `tag-checker' (see below)
;;; is invoked:
;;; 
;;; 	(check tag parameters)
;;; 
;;; 
;;; `inherited-checkers' is an association list whose keys are
;;; are the names of parent languages and whose values are
;;; checker procedures for those languages.  A checker procedure
;;; will ordinarilly want to invoke:
;;; 
;;; 	(for-each (lambda (inherited-checker)
;;; 	  	     ((cdr inherited-checker) tag params))
;;;	   inherited-checkers)
;;; 
;;; for which there is a short-hand:
;;; 
;;; 	(apply-inherited-parameter-checkers
;;;	  inherited-checkers tag params)
;;; 


;; checkers-table
;; 
;; A table mapping language/tag pairs to explicitly supplied 
;; parameter checking procedures.
;; 
(define checkers-table (make-hash-table 127))
(define (checker-table-key language tag)
  (cons language tag))

;;(s set-tag-checker!)
;; (set-tag-checker! language tag checker)
;; 
;; Explicitly declare `checker' to be the parameter checker for `tag'
;; in `langauge'.  See xref:"Per-language Tag Parameter Checking".
;; 
(define-public (set-tag-checker! language tag checker)
  (hash-set! checkers-table
	     (checker-table-key language tag)
	     checker))

;;(s tag-checker)
;; (tag-checker language tag)
;; 
;; Return a parameter checking procedure for `tag' in markup language
;; `language'.  See xref:"Per-language Tag Parameter Checking".
;; 
(define-public (tag-checker language tag)
  (let ((inherited (inherited-tag-checkers language tag))
	(per-language (or (hash-ref checkers-table (checker-table-key language tag))
			  (make-default-tag-checker language tag))))
    (lambda (tag parameters)
      (per-language tag parameters inherited))))


;; (inherited-tag-checkers language tag)
;; 
;; Return an association list mapping the parent markup languages of
;; `language' to parameter checking procedures (as returned by
;; `tag-checker') for markup tag `tag'.
;; 
(define (inherited-tag-checkers language tag)
  (let* ((parents (markup-language-parents language))
	 (parent-checkers (map (lambda (lingua) (cons lingua (tag-checker lingua tag))) parents)))
    parent-checkers))


;; (make-default-tag-checker language tag)
;; 
;; Construct a parameter checking procedure for markup language
;; `language' and tag `tag'.  The procedure invokes the checking
;; procedure for all parent languages of `language'.
;; 
(define (make-default-tag-checker language tag)
  (let ((inherited-checkers (inherited-tag-checkers language tag)))
    (lambda (tag parameters inherited-checkers)
      (and-map (lambda (assq-item) ((cdr assq-item) tag parameters))
	       inherited-checkers))))


;;(s throw-invalid-tag-parameters)
;; (throw-invalid-tag-parameters tag params message)
;; 
;; Throw an exception:
;; 
;; 	(throw 'invalid-tag-parameters
;;	       :tag tag
;;	       :params params
;;	       :message message)
;; 
(define-public (throw-invalid-tag-parameters tag params message)
  (throw 'invalid-tag-parameters :tag tag :params params :message message))


;;(s apply-inherited-parameter-checkers)
;; (apply-inherited-parameter-checkers inherited-checkers tag params)
;; 
;; `inherited-checkers' is an association list whose keys are
;; are the names of parent markup languages and whose values are
;; parameter checker procedures for those languages.  
;; 
;; Such a list is passed to per-language parameter checkers;  see
;; xref:"Per-language Tag Parameter Checkers". 
;; 
;; This procedure applies each parameter checker in-order:
;; 
;; 	(for-each (lambda (inherited-checker)
;; 	  	     ((cdr inherited-checker) tag params))
;;	   inherited-checkers)
;; 
;; 
(define-public (apply-inherited-parameter-checkers inherited-checkers tag params)
  (for-each (lambda (inherited-checker)
	      ((cdr inherited-checker) tag params))
	    inherited-checkers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Markup Language Formatters")
;;;
;;; Output formats are given atomic nams (e.g. `html').
;;; 
;;; For every language/tag/output-format there is a procedure which
;;; attempts to format a document with that tag.
;;;
;;; The formatter procedure is invoked:
;;; 
;;; 		(format document 
;;;			tag
;;;			parameters
;;;			sub-documents
;;;			database
;;;			format-sub-document)
;;; 
;;; Note that for efficiency the parameter list includes both the
;;; complete document (`document') and the parsed components of the
;;; document (`tag', `parameters', and `sub-documents').  If `tag' is
;;; `'string', then `document' is a string.  
;;; 
;;; `document' is an HDML object.  See xref:"Hierarchical Document
;;; Mark-up Language".
;;; 
;;; `tag' is an atom.
;;; 
;;; `parameters' is the list of parameters supplied to the tag.  These
;;; have already checked for validity by the language/tag checker
;;; procedure (see xref:"Per-language Tag Parameter Checkers").
;;; 
;;; `sub-documents' is a list of sub-documents.  These sub-documents
;;; have already been checked for impermissible tags (see
;;; xref:"Containable Tags").
;;; 
;;; `database' is a database procedure (see
;;; xref:"make-formatter-database").
;;; 
;;; `format-sub-document' is a procedure that can be invoked to format
;;; a single sub-document:
;;;
;;;		(format-sub-document sub-document)
;;;
;;; The return type of a formatter procedure is unspecified.  It is
;;; usually specific to a choice of format.
;;; 
;;; Formatters are inherited.  If no formatter is explicity declared
;;; for a language/tag pair, then the list of parent languages is
;;; scanned in order.   The first formatter found is used.  
;;; 
;;; If no formatter is explicity declared or inherited, a default
;;; formatter is used.  The defalut formatter throws an exception:
;;; 
;;; 	(throw 'no-formatter-defined language tag format)
;;; 


;; formatters-table
;; 
;; A table mapping language/tag/format to formatter procedures.
;; 
(define formatters-table (make-hash-table 1021))
(define (formatters-table-key language tag format)
  (list language tag format))


;;(s set-tag-formatter!)
;; (set-tag-formatter! language tag format procedure)
;; 
;; Explicity declare a formatter procedure for `language', `tag', and
;; `format'.  See xref:"Markup Language Formatters".
;; 
(define-public (set-tag-formatter! language tag format procedure)
  (hash-set! formatters-table
	     (formatters-table-key language tag format)
	     procedure))


;;(s tag-formatter)
;; (tag-formatter language tag format)
;; 
;; Return a formatter (explit, inherited, or default) for `language',
;; `tag', and `format'.  See xref:"Markup Language Formatters".
;; 
(define-public (tag-formatter language tag format)
  (or (tag-formatter-but-no-default language tag format)
      (lambda ign
	(throw 'no-formatter-defined :language language :tag tag :format format))))

;;(s tag-formatter-but-no-default)
;; (tag-formatter-but-no-default language tag format)
;; 
;; Return a formatter (explit or inherited) for `language', `tag', and
;; `format'.  If no formatter has been declared, return #f.
;; 
(define-public (tag-formatter-but-no-default language tag format)
  (or (hash-ref formatters-table
		(formatters-table-key language tag format))
      (let ((parents (markup-language-parents language)))
	(or-map (lambda (lingua)
		  (tag-formatter-but-no-default lingua tag format))
		parents))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Formatter Databases")
;;; 
;;; Dynamic, externalisable, namespace of variables (distinct from
;;; Scheme variables) shared by formatters and their
;;; super/sub-document formatters.
;;; 

;;(s make-formatter-database)
;; (make-formatter-database)
;; 
;; Create a database of variables (distinct from Scheme variables)
;; shared by formatters and their super/sub-document formatters.
;; 
;; The database is represented by a procedure that operates in an
;; message-passing manner (see below).
;; 
;; Permanent bindings in this namespace should be to objects which are
;; readable and writable because the contents of the database may be
;; saved in a file.
;; 
;; Every binding made in the database is associated with a filename.
;; When a saved database is reloaded from a file, the saved bindings
;; are not immediately restored.  Instead, when an undefined variable
;; is referenced, if that variable is defined for some filename, then
;; all of the bindings associated with that filename are introduced
;; into the database at once.  If any of the variables bound for that
;; filename already have a binding, an exception of type
;; `'multiply-defined' is thrown.  Thus, a database saved in a file is
;; similar to an object file archive and referencing a variable not
;; bound in a database invokes an opertation that is similar to
;; dynamically linking against an object file archive.
;; 
;; A database procedure created by `make-formatter-database'
;; understands these messages:
;; 
;; 	'set-input-file! filename
;;		Remove all currently bound variables from the database
;;		and make `filename' the filename associated with
;;		subsequent bindings.  Note that the database retains
;;		the removed bindings and will include them in the
;;		output of `'write-index!' (see below).
;; 
;; 	'write-index!
;;		Invoke `'set-input-file! with a filename argument ()
;; 		and write a readable datastructure that contains all
;;		retained bindings.  This output may be stored in a
;;		file and read using `'read-index'.
;; 
;; 	'read-index!
;;		Initialize a database by reading the output of
;;		`'write-index!'.
;; 
;; 	'file-of variable
;;		Return the filename (or #f) associated with the
;;		binding of `variable'.
;; 
;; 	'remove! variable
;;		Remove `variable' from the database.  This does not
;;		remove any previously retained bindings for `variable'.
;;		This does prevent the current binding from being
;;		retained by the next invocation of `'set-input-file!'.
;; 
;; 	'ref variable
;; 		Return the binding of `variable' in the database.
;;		This may invoke the ``dynamic linking'' operation
;;		described above if `variable' is not initially bound.
;;		If the variable as no binding or retained binding,
;;		return #f.
;; 
;; 	'set! variable value
;;		Bind `variable' to `value', associating the binding
;;		with the current filename.
;; 
;; 	'set-unique! variable value
;;		Bind `variable' to `value', associating the binding
;;		with the current filename, but not if the variable 
;;		already has a current or retained binding.  If the
;;		variable is already bound, throw an exception of type
;;		`'multiply-defined'.
;; 
;; 	'for-each procedure
;;		For each current binding (but not retained bindings)
;;		invoke:
;;			(procedure key value)
;; 
;; It is an error to pass any other message to a database procedure.
;;
(define-public (make-formatter-database)
  (let* ((key->file-table 	(make-hash-table 1021))
	 (file->bindings-table	(make-hash-table 1021))
	 (input-file 		())
	 (document-table	())
	 (defined-in-table	())
	 (output-index		()))
    
    (letrec ((self 
	      (lambda (op . parameters)
		(case op
		  ((set-input-file!)	(apply-to-args parameters
					  (lambda (file)
					    (if document-table
						(set! output-index (acons input-file
									  (apply append (vector->list document-table))
									  output-index)))
					    (set! input-file file)
					    (if file
						(begin
						  (set! document-table (make-hash-table 1021))
						  (set! defined-in-table (make-hash-table 1021)))
						(begin
						  (set! document-table ())
						  (set! defined-in-table ()))))))
		  
		  
		  ((write-index!)	(self 'set-input-file! ())
					(write output-index))

		  ((read-index!)	(let ((index (read)))
					  (for-each (lambda (entry)
						      (let ((filename (car entry))
							    (bindings (cdr entry)))
							(if (hash-ref file->bindings-table filename)
							    (throw 'index-contains-multiple-tables-for-file filename))
							(hash-set! file->bindings-table file bindings)
							(for-each (lambda (binding)
								    (let ((key (car binding)))
								      (if (not (hash-ref key->file-table (car binding)))
									  (hash-set! key->file-table (car binding) filename))))
								  bindings)))
						    index)))
		  
		  (else
		   (apply-to-args parameters
		     (lambda (key :optional value)
		       (define (multiply-defined file key value)
			 (display*-port (current-error-port)
					"ERROR: multiply defined (" (->string key) ")\n"
					"  defined in " (->string file) " as " (->string value) "\n"
					"  HEY! defined again in " (->string (hash-ref defined-in-table key))
					" as " (->string (car (hash-ref document-table key)))"\n")
			 (throw 'multiply-defined key))

		       (if (not input-file)
			   (error "No input file defined in markup database."))
		       
		       (case op
			 ((file-of)		(hash-ref key->file-table key))
			 ((remove!)		(hash-remove! document-table key)
						(hash-remove! defined-in-table key))
			 ((ref)			(cond
						 ((pk 'old-value (hash-ref document-table key))		=> car)
						 (#t
						  (let ((file (hash-ref key->file-table key)))
						    (pk 'linking-for key)
						    (and file
							 (not (string=? file input-file))
							 (let ((bindings (hash-ref file->bindings file)))
							   (for-each (lambda (binding)
								       (if (hash-ref document-table (car binding))
									   (multiply-defined file (car binding) (hash-ref document-table (car binding)))
									   (begin
									     (hash-set! document-table (car binding) (cons (cdr binding)))
									     (hash-set! defined-in-table (car binding) file))))
								     bindings)
							   (car (hash-ref document-table key))))))))
			 ((set!)		(hash-set! document-table key (cons value ()))
						(hash-set! defined-in-table key input-file))
			 ((set-unique!)		(let ((prev-dfn (hash-ref document-table key))
						      (prev-file (hash-ref defined-in-table key)))
						  (pk 'su key perv-dfn value)
						  (if (and prev-dfn
							   (not (and (equal? prev-file input-file)
								     (equal? (car prev-dfn) value))))
						      (multiply-defined input-file key value))
						  (hash-set! document-table key (cons value ()))
						  (hash-set! defined-in-table key input-file)))
			 ((for-each)		(vector-for-each
						 (lambda (bucket)
						   (map (lambda (binding)
							  (key (car binding) (cadr binding)))
							bucket))
						 document-table))
			 (else			(throw
					'unrecognized-xdml-database-op op))))))))))
      self)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h1 "Formatters For Entire Markup Languages")
;;; 
;;; 
;;; 


;;(s markup-language-document-formatter)
;; (markup-language-document-formatter language format)
;; 
;; Return a procedure which formats documents in markup language
;;`language' for output format `format'.
;; 
(define-public (markup-language-subdocument-formatter language format)
  (let* ((checker-cache			(make-hash-table 127))
	 (inherited-checkers-cache	(make-hash-table 127))
	 (containables-cache		(make-hash-table 127))
	 (formatter-cache 		(make-hash-table 127))

	 (checker		(lambda (tag)
				  (or (hash-ref checker-cache tag)
				      (let ((checker (tag-checker language tag)))
					(hash-set! checker-cache tag checker)
					checker))))

	 (containables		(lambda (tag)
				  (or (hash-ref containables-cache tag)
				      (let ((containables (tag-containables language tag)))
					(hash-set! containables-cache tag containables)
					containables))))

	 (formatter		(lambda (tag)
				  (or (hash-ref formatter-cache tag)
				      (let ((formatter (tag-formatter language tag format)))
					(hash-set! formatter-cache tag formatter)
					formatter)))))

    (letrec ((self		(lambda (document database)
				  (parse-hdml document
					      (lambda (tag parameters . sub-documents)
						((checker tag) tag parameters)
						(let ((sub-tags		(map hdml-tag sub-documents))
						      (allowed  	(containables tag)))
						  (for-each (lambda (st)
							      (if (not (memq st allowed))
								  (begin
								    (check-tag language st)
								    (throw 'illegally-nested-tags :container tag :contained st))))
							    sub-tags))
						((formatter tag) document tag parameters sub-documents database
								 (lambda (sub-doc)
								   (self sub-doc database))))))))
      self)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h0 "Markup Language Formatters")
;;;
;;;
;;;


;;(s markup-language-formatter)
;; (markup-language-formatter language format database)
;; 
;; Returned procedure is invoked:
;; 
;; 	(format input-filename
;;		output-filename
;;		document
;;		database)
;; 
(define-public (markup-language-formatter language format)
  (let ((entry 			(markup-language-formatter-entry-point language format))
	(format-subdocument 	(markup-language-subdocument-formatter language format)))
    (lambda (input-filename output-filename document database)
      (db 'set-input-file! input-filename)
      (entry output-filename document database (lambda (subdoc) (format-subdocument subdoc database))))))


  