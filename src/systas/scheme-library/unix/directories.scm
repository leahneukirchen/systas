;;; directories.scm - filesystem directory utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998, 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (unix directories)
  :use-module (unix filenames)
  :use-module (standard string-parsing))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directory Contents
;;; 

;; (directory-files :optional dirname . kws)
;;
;; Return the list of filenames of files contained in `dirname'
;; or the current directory.
;; 
;; 	:no-.		Omit the file "."
;; 	:no-..		Omit the file ".."
;; 	:no-./..	Omit the files "." and ".."
;; 
(define-public (directory-files :optional dirname . kws)
  (let* ((dir 		(%% %opendir (or dirname ".")))
	 (no-./..?	(memq :no-./.. kws))
	 (no-.?		(or no-./..? (memq :no-. kws)))
	 (no-..?	(or no-./..? (memq :no-.. kws))))

    (let loop ((l '()))
      (let ((next (%readdirname dir)))
	(cond
	 ((errno? next)			(begin
					  (%% %closedir dir)
					  (throw next)))
	 ((not next)			(reverse l))
	 ((and no-.?
	       (string=? "." next))	(loop l))
	 ((and no-..?
	       (string=? ".." next))	(loop l))
	 (#t				(loop (cons next l))))))))


;; (directory-files-in-vicinity :optional dirname . kws)
;;
;; Return the list of filenames of files contained in `dirname'
;; or the current directory.
;; 
;; The filenames returned are created by:
;; 
;; 	(in-vicinity dirname filename)
;; 
;; 	:no-.		Omit the file "."
;; 	:no-..		Omit the file ".."
;; 	:no-./..	Omit the files "." and ".."
;; 
(define-public (directory-files-in-vicinity :optional dirname . kws)
  (map (lambda (f) (in-vicinity dirname f)) (apply directory-files dirname kws)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cd
;;; 

;; (cd dir)
;; 
;; Tilde-expand `dir' and make it the current working directory 
;; (by calling `%chdir').
;; 
(define-public (cd dir)
  (%% %chdir (tilde-expand-filename dir)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directory Excursions
;;; 

;; (directory-excursion dir thunk)
;; 
;; Invoke `(thunk)' in a dynamic context in which the current working
;; directory is `dir'.
;; 
(define-public (directory-excursion dir thunk)
  (let ((origin		(%% %open "." 0 0))
	(target		(%% %open dir 0 0)))
    (dynamic-wind
     (lambda () (%% %fchdir target))
     thunk
     (lambda () (%% %fchdir origin)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating Directories
;;; 

;; (make-directories-to dir)
;; 
;; Create directory `dir' and any parent directories of `dir' which do
;; not already exist.
;; 
;; This procedure throws an exception on error.  If an error occurs, some
;; of the parent directories of `dir' may already have been created.
;; 
;; It is not an error if `dir' already exists.
;; 
(define-public (make-directories-to dir)
  (cond
   ((not dir)						#f)
   ((string=? "/" dir)					(if (not (file-is-directory? dir))
							    (throw 'bogus-root-directory)))
   ((file-exists? dir)					(if (not (file-is-directory? dir))
							    (throw 'ENOTDIR dir)))
   ((not (file-exists? (filename-directory dir)))	(make-directories-to (filename-directory dir))
							(make-directories-to dir))
   (#t							(%% %mkdir dir))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; recursive directory searches
;;; 


;; (directory-map file/files pre post . kws)
;; 
;; `file/files' is a filename or list of filenames.  As a special case,
;; #f is treated as the current working directory.
;; 
;; Walk the filesystem trees rooted at `file/files', returning a list
;; of values.
;; 
;; If `pre' is not #f, it should be a procedure.  For each file (other
;; than the current directory, if it was specified as #f), `pre' is
;; invoked this way:
;; 
;; 	(pre filename stat depth)
;; 
;; where `filename' is a filename `in-vicinity' of one of `files',
;; `stat' is the output of `%lstat' for that filename (unless
;; `:follow-links' is specified -- see below), and `depth' is the
;; depth of `filename' in the traversal (0 for members of `files', 1
;; for the immediate contents of directories in `files', etc.)
;; 
;; `pre' may return a value, or throw one of the exceptions:
;; 
;; 	'directory-search-skip :optional value
;; 	'directory-search-no-value
;; 
;; If a value is returned, or if a value is provided to the `directory-search-skip'
;; exception handler, that value is included in the list of values
;; returned by `directory-map'.
;; 
;; If the exception `directory-search-skip' is thrown, the filesystem
;; search will not descend to the tree rooted at `filename'.  If the
;; exception `directory-search-no-value' is thrown, the subtree will
;; be searched, but no value for this file is added to the list
;; returned from `directory-map'.
;; 
;; After invoking `pre', if a `directory-search-skip' exception is not generated,
;; the subtree rooted at `filename' is recursively searched.
;; 
;; After the subtree is searched (if it is searched), `post' (if it is
;; not #f) is invoked in the same manner as `pre'.  `post' may return
;; a value or throw a `directory-search-no-value' exception.  `post' may not throw a
;; `directory-search-skip' exception.
;; 
;; Any values returned from `pre' and `post' (or provided as an argument to 
;; a `directory-search-skip' exception) are returned from `directory-map' as a list
;; in the order the values were produced.
;; 
;; Note that for a non-directory, `post' is called immediately after `pre'
;; returns, unless `pre' raised a `directory-search-skip' exception.
;; 
;; Permissable keyword arguments to `directory-map' are:
;; 
;; 	:follow-links	If a file is a symlink, use the ultimate target
;; 			of the link instead of the link.  Pass the filename
;; 			of the target and the output of `%stat' for that
;;			file to `pre' and `post'.
;; 
;; 	:assume-depth n
;;			Instead of counting depth in the traversal from 0, 
;;			count from `n'.
;; 
(define-public (directory-map file/files pre :optional post . kws)
  (let* ((files		(if (pair? file/files)
			    file/files
			    (list file/files)))
	 (answer 	(cons () ()))
	 (follow-links?	(->bool (memq :follow-links kws)))
	 (depth		(or (kw-arg-ref kws :assume-depth) 0)))

  (let loop ((to-consider 	files)
	     (pos 		answer))

    (if (not to-consider)
	(cdr answer)
	
	(let* ((f		(car to-consider))
	       (lstat		(%lstat (or f ".")))
	       (f-seen		(if follow-links? (resolve-link f) f))
	       (stat-seen	(if (eq? f f-seen) lstat (%stat f-seen)))
	       (have-stat?	(pair? stat-seen))
	       (type		(and (pair? stat-seen) (kw-arg-ref stat-seen :type)))
	       (is-dir?		(and have-stat? (eq? 'S_IFDIR type))))

	  (let ((pre-value		(if (or (not pre) (not f-seen))
					    '(directory-search-no-value)
					    (catch 'directory-search-skip
					      (lambda () (catch 'directory-search-no-value
							   (lambda () (list 'value (pre f-seen stat-seen depth)))
							   (lambda ign '(directory-search-no-value))))
					      (lambda (tag . opt-value) (cons 'skip opt-value))))))
	    
	    (if (cdr pre-value)
		(begin
		  (set-cdr! pos (cons (cadr pre-value) ()))
		  (set! pos (cdr pos))))

	    (if (eq? (car pre-value) 'skip)

		(loop (cdr to-consider) pos)

		(let* ((contents		(and is-dir?
						     (directory-files-in-vicinity f-seen :no-./..)))
		       (contents-mapped		(and contents (apply directory-map contents pre post :assume-depth (1+ depth) kws)))
		       (post-value		(if (or (not post) (not f-seen))
						    '(directory-search-no-value)
						    (catch 'directory-search-no-value
						      (lambda () (list 'value (post f-seen stat-seen depth)))
						      (lambda ign '(directory-search-no-value))))))
		  (set-cdr! pos contents-mapped)
		  (set! pos (last-pair pos))
		  (if (cdr post-value)
		      (begin
			(set-cdr! pos (cons (cadr post-value) ()))
			(set! pos (cdr pos))))
		  (loop (cdr to-consider) pos)))))))))


;; (directory-for-each file/files pre post . kws)
;; 
;; `file/files' is a filename or list of filenames.  As a special
;; case, #f is treated as the current working directory.
;; 
;; Walk the filesystem trees rooted at `file/files'.  Return an
;; unspecified value.
;; 
;; If `pre' is not #f, it should be a procedure.  For each file (other
;; than the current working directory if it is specified as #f), `pre'
;; is invoked this way:
;; 
;; 	(pre filename stat depth)
;; 
;; where `filename' is a filename `in-vicinity' of one of `files',
;; `stat' is the output of `%lstat' for that filename (unless
;; `:follow-links' is specified -- see below), and `depth' is the
;; depth of `filename' in the traversal (0 for members of `files', 1
;; for the immediate contents of directories in `files', etc.)
;; 
;; The return value of `pre' is ignored, however `pre' throw an
;; exception of type `directory-search-skip'.  If the exception
;; `directory-search-skip' is thrown, the filesystem search will not
;; descend to the tree rooted at `filename'.
;; 
;; After invoking `pre', if a `directory-search-skip' exception is not generated,
;; the subtree rooted at `filename' is recursively searched.
;; 
;; After the subtree is searched (if it is searched), `post' (if it is
;; not #f) is invoked in the same manner as `pre'.  
;; 
;; Note that for a non-directory, `post' is called immediately after `pre'
;; returns, unless `pre' raised a `directory-search-skip' exception.
;; 
;; Permissable keyword arguments to `directory-map' are:
;; 
;; 	:follow-links	If a file is a symlink, use the ultimate target
;; 			of the link instead of the link.  Pass the filename
;; 			of the target and the output of `%stat' for that
;;			file to `pre' and `post'.
;; 
;; 	:assume-depth n
;;			Instead of counting depth in the traversal from 0, 
;;			count from `n'.
;; 
(define-public (directory-for-each file/files pre post . kws)
  (apply directory-map
	 file/files
	 (lambda (file stat depth) (pre file stat depth) (throw 'directory-search-no-value))
	 (lambda (file stat depth) (post file stat depth) (throw 'directory-search-no-value))
	 kws)
  #f)


;; (directory-find file/files predicate? . kws)
;; 
;; `file/files' is a filename or list of filenames or #f (indicating
;; the current directory).
;; 
;; Walk the filesystem trees rooted at `file/files', returning a list
;; of the filenames for which `predicate?' returns true.
;; 
;; For each file (other than the current working directory if it is
;; specified as #f), `predicate?' is invoked this way:
;; 
;; 	(predicate? filename stat depth)
;; 
;; where `filename' is a filename `in-vicinity' of one of `files',
;; `stat' is the output of `%lstat' for that filename (unless
;; `:follow-links' is specified -- see below), and `depth' is the
;; depth of `filename' in the traversal (0 for members of `files', 1
;; for the immediate contents of directories in `files', etc.)
;; 
;; The return value of `predicate?' determines whether or not
;; `filename' is included in the list returned by `directory-find'.
;; Additionally, `predicate?' may throw an exception of type
;; `directory-search-skip'.  If the exception `directory-search-skip'
;; is thrown, the filesystem search will not descend to the tree
;; rooted at `filename'.  If a true argument is provided to a
;; `directory-search-skip' exception, the `filename' is included in
;; the list returned by `directory-find'.
;; 
;; After invoking `predicate?', if a `directory-search-skip' exception
;; is not generated, the subtree rooted at `filename' is recursively
;; searched.
;; 
;; Permissable keyword arguments to `directory-map' are:
;; 
;; 	:follow-links	If a file is a symlink, use the ultimate target
;; 			of the link instead of the link.  Pass the filename
;; 			of the target and the output of `%stat' for that
;;			file to `pre' and `post'.
;; 
;; 	:assume-depth n
;;			Instead of counting depth in the traversal from 0, 
;;			count from `n'.
;; 
;; 	:depth-first	search subtrees rooted at `filename' before 
;;			calling `predicate?' for `filename'.   In this case,
;;			`directory-search-skip' exceptions are not permitted.
;; 
(define-public (directory-find file/files predicate? . kws)
  (let* ((depth-first?		(memq :depth-first kws))
	 (fn			(lambda (file stat depth)
				  (catch 'directory-search-skip

				    (lambda ()
				      (if (catch 'directory-search-no-value
					    (lambda () (predicate? file stat depth))
					    ;; directory-search-no-value exception:
					    ;; 
					    ;; Convert this exception to an error since it has
					    ;; an undesired meaning in `directory-map'.
					    ;; 
					    (lambda ign (error "`directory-search-no-value' exception from `directory-find' predicate" file)))
					  file
					  (throw 'directory-search-no-value)))

				    ;; directory-search-skip exception:
				    ;; 
				    ;; Make sure that this is not a depth-first traversal and
				    ;; that `directory-map' does not receive a value for this
				    ;; filename.
				    ;; 
				    (lambda ign
				      (if depth-first?
					  (error "`directory-search-skip' exception in depth-first `directory-find' traversal" file))
				      (throw 'directory-search-skip))))))


    (apply directory-map
	   file/files
	   (and (not depth-first?) fn)
	   (and depth-first? fn)
	   kws)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; relative cwd
;;; 

;; (relative-cwd root)
;; 
;; If `root' is (syntactically) a subdirectory of the directory
;; returned by `%getcwd', return the relative path from
;; the current directory to `root'.
;; 
;; Note that `root' should already be a fully canonicalized
;; filename.
;; 
(define-public (relative-cwd root)
  (let* ((pwd		(filename-as-directory (%% %getcwd)))
	 (root		(filename-as-directory root)))
    (if (not (string-prefix=? root pwd))
	(error "unable to compute relative cwd" pwd :relative-to root))
    (if (= (string-length root) (string-length pwd))
	"."
	(substring pwd (string-length root)))))

