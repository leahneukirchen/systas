;;; tag: Tom Lord Tue Dec  4 14:59:35 2001 (etree/source-hygiene.scm)
;;;
add something to test for duplicate ids

;;; source-hygiene.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (etree source-hygiene)
  :use-module (formatting report)
  :use-module (standard regexps)
  :use-module (standard list-sorting)
  :use-module (standard list-lib)
  :use-module (standard char-set-lib)
  :use-module (unix directories)
  :use-module (unix filenames)
  :use-module (unix file-utils))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Naming Conventions
;;; 
;;; 


;; (find-naming-conventions-file :optional dir)
;; 
;; Find the first occurence of "./=project/naming-conventions.scm" in 
;; the `dir' (or the current directory) or any parent directory.
;; 
;; Return `#f' if no such file is found.
;; 
(define-public (find-naming-conventions-file :optional dir)
  (let* ((dir		(or dir "."))
	 (dir		(filename-as-directory dir))
	 (dir		(if (filename-absolute? dir)
			    dir
			    (clean-filename (form-filename (%% %getcwd) dir)))))
    (let loop ((dir	dir))
      (let ((conventions-file		(form-filename dir "=project/naming-conventions.scm")))
	(cond
	 ((file-exists? conventions-file)		conventions-file)
	 ((string=? dir "/")				#f)
	 (#t						(loop (filename-directory dir))))))))




;; (naming-conventions :optional dir)
;; 
;; Return the project naming conventions that apply to dir.
;; This is the content (as returned from `read') of 
;; 
;; 	(find-naming-conventions-file dir)
;; 
;; or the default conventions, if no naming conventions file is
;; found.
;; 
(define-public (naming-conventions :optional dir)
  (cond
   ((find-naming-conventions-file dir)	=>	file->sexp)
   (#t						default-naming-conventions)))

(define-public default-naming-conventions
  `((unrecognized		(* any))))


;; naming-conventions->procedure conventions
;; 
;; A set of naming conventions is an association list of the form:
;; 
;; 	((category pattern) ...)
;; 
;; where each `category' is a keyword, and each `pattern' a structured regexp.
;; 
;; Form a structured regexp, `sre', by combining the patterns to form:
;; 
;; 	(^$ (| (! category pattern) ...))
;; 
;; and compile a regexp procedure with:
;; 
;; 	(structured-regexp->procedure sre :pick-spec '?)
;; 
(define-public (naming-conventions->procedure conventions)
  (structured-regexp->procedure `(^$ (| ,@(map (lambda (l) (cons '! l)) conventions))) :pick-spec '?))


;; naming-conventions-procedure :optional dir
;; 
;; Simply:
;; 
;; 	(naming-conventions->procedure (naming-conventions dir))
;; 
(define-public (naming-conventions-procedure dir)
  (naming-conventions->procedure (naming-conventions dir)))


;; (categorize-source-tree dir)
;; 
;; Find the naming conventions for `dir'.  Using those conventions,
;; recursively walk `dir'.
;; 
;; For each file or directory found, compute its category based on the
;; naming conventions.  Iff the category is `:source', recursively 
;; search the file (if it is a directory).
;; 
;; Return an alist mapping categories to lists of files visited.
;; 
;; 
(define-public (categorize-source-tree dir)
  (directory-excursion dir
    (lambda ()
      (let* ((conventions	(naming-conventions "."))
	     (categorizor	(naming-conventions->procedure conventions))
	     (raw		(directory-map #f
					       (lambda (f stat depth)
						 (let* ((tail 		(filename-nondirectory f))
							(category	(categorizor tail))
							(is-dir?	(eq? 'S_IFDIR (kw-arg-ref stat :type)))
							(name		(if is-dir?
									    (filename-as-directory f)
									    f)))
						   (cond
						    ((eq? category :source)		(list category name))
						    (category				(throw 'directory-search-skip (list category name)))
						    (#t					(throw 'directory-search-skip (list :unrecognized name))))))))
	     (answer		'()))

	(for-each (lambda (categorized-file)
		    (apply-to-args categorized-file
		      (lambda (category file)
			(set! answer (assq-set! answer category (cons file (assq-ref answer category)))))))
		  raw)

	answer))))


(define-public (source-files-list :optional dir)
  (kw-arg-ref (categorize-source-tree dir) :source))


;; (source-hygiene-report :optional dir)
;; 
;; Return a string which verbosely describes the value
;; returned by `categorize-source-tree'.
;; 
(define-public (source-hygiene-report :optional dir)
  (let ((files		(categorize-source-tree dir))
	(report		(make-report '(source precious junk unrecognized))))

    (with-output-to-report report 'source
      (lambda ()
	(display* "The source tree consists of these files:\n"
		  "\n")
	(for-each (lambda (f) (display* f "\n"))
		  (sort (assq-ref files :source) string<?))
	(newline)))

    (with-output-to-report report 'precious
      (lambda ()
	(display* "These files are categorized `precious'.\n"
		  "That means they are not source files, but\n"
		  "should not be casually deleted.\n"
		  "\n")
	(for-each (lambda (f) (display* f "\n"))
		  (sort (assq-ref files :precious) string<?))
	(newline)))

    (with-output-to-report report 'junk
      (lambda ()
	(display* "These files are categorized `junk'.\n"
		  "That means they are not source files and\n"
		  "can presumably be casually deleted.\n"
		  "\n")
	(for-each (lambda (f) (display* f "\n"))
		  (sort (assq-ref files :junk) string<?))
	(newline)))

    (with-output-to-report report 'unrecognized
      (lambda ()
	(if (assq-ref files :unrecognized)
	    (begin
	      (display* "These files are categorized `unrecognized'.\n"
			"THIS IS AN ERROR -- these files do not follow\n"
			"the naming conventions.\n"
			"\n")
	      (for-each (lambda (f) (display* f "\n"))
			(sort (assq-ref files :unrecognized) string<?))
	      (newline))

	    (display* "There are no unrecognized files here.\n"))))

    (string-append (report-section-text report 'source)
		   #\np "\n"
		   (report-section-text report 'precious)
		   #\np "\n"
		   (report-section-text report 'junk)
		   #\np "\n"
		   (report-section-text report 'unrecognized))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities for Manipulating Lists of Source Files
;;; 
;;; These manipulate source lists returned by `categorize-source-tree'.
;;; 
;;; They presume that directories are syntactically directories (end
;;; in "/") and other files are not.
;;; 


(define (directories-and-files flist)
  (partition (lambda (f) (char=? #\/ (string-ref f -1))) flist))

