;;; filenames.scm - syntactic filename manipulation
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998, 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;

(define-module (unix filenames)
  :use-module (unix users)
  :use-module (standard string-parsing)
  :use-module (standard regexps))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntactic Filename Operations
;;; 
;;; These procedures manipulate filenames syntacticly (without
;;; reference to the structure of the filesystem).
;;; 
;;; (This file does contain `tilde-expand-filename' though,
;;; which reads the password file.)
;;; 
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simplifying Filenames
;;; 
;;; `simplifying' means replacing "/./" and "//" with "/".
;;; 
;;; Every procedure in this file that returns a filename
;;; returns a simplified filename.
;;; 



;; (filename-simplify-slashes f)
;; 
;; Return a filename formed by eliminating double separators ("//")
;; and dot components (".")  from `f'.  
;; 
;; This preserves a final "/" if present.
;;

(define-public filename-simplify-slashes

  (let ((slash-synonym		(structured-regexp->procedure `(& "/" (+ (? ".") "/"))
							      :pick-spec '(< >))))

    (lambda (filename)

      (let loop	((filename	filename)
		 (prefixes	'()))

	(cond
	 ((string-null? filename)			(apply join-fields-with #\/ (reverse prefixes)))
	 (#t
	  (slash-synonym filename
			 (lambda (left right)
			   (loop right (cons left prefixes)))
			 (lambda ()
			   (loop "" (cons filename prefixes))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filename Algebra
;;; 
;;; Constructors and destructors for filenames.
;;; 
;;; A `directory-name' is a filename that ends with "/".
;;; 
;;; A `filename' is a filename that does not end with "/".
;;; 


;; (form-filename . components)
;; 
;; Form a filename by concatenating components, separated by "/",
;; but avoid "//" and "/./" in the result.
;; 
;; This is different from `in-vicinity' in two ways:
;; 
;; 	1. No component may be #f.
;;	3. It makes no difference whether any component (considered
;;	   in isolation) is an absolute filename.
;;
(define-public (form-filename . components)
  (filename-simplify-slashes (apply join-fields-with #\/ components)))


;; (directory-as-filename f)
;; 
;; Return a filename equivalent to `f', but clean (`filename-simplify-slashes')
;; and, except in the case of a name for the root directory, not
;; ending with "/".
;;
;; 	a/b/c		=>	a/b/c
;;	a/b/c/		=>	a/b/c
;;	a/./b//c///	=>	a/b/c
;;	/		=>	/
;;	//		=>	/
;;	/.//.//.	=>	/
;; 
;; No special interpretation is honored for filenames beginning with
;; "//".
;; 
;; The returned result may share state with `f'.
;;
(define-public (directory-as-filename d)
  (let ((d 	(filename-simplify-slashes d)))
    (cond
     ((string=? d "/")				"/")
     ((and (< 0 (string-length d))
	   (char=? #\/ (string-ref d -1)))	(make-shared-substring 0 -1))
     (#t					d))))



;; (filename-as-directory f)
;; 
;; Return a filename equivalent to `f', but clean (`filename-simplify-slashes')
;; and certainly ending with "/".  
;;
;; 	a/b/c		=>	a/b/c/
;;	a/b/c/		=>	a/b/c/
;;	a/b/c///	=>	a/b/c///
;;	/.		=>	/
;;	//.		=>	//./
;;			=>	./
;; The returned result may share state with `f'.
;;
(define-public (filename-as-directory f)
  (cond
   ((string-null? f)			"./")
   ((char=? #\/ (string-ref f -1))	(filename-simplify-slashes f))
   (#t					(filename-simplify-slashes (string-append f "/")))))
   

;; (filename-absolute? f)
;; 
;; Return true if f begins with "/", #f otherwise.
;; 
(define-public (filename-absolute? f)
  (and (not (string-null? f))
       (char=? #\/ (string-ref f))))


;; (filename-directory f)
;; 
;; Return a directory name for the parent directory of `f',
;; considering only the syntax of the filename and not the
;; structure of the filesystem.
;; 
;; The filename returned is syntacticly a directory (ends with "/").
;; 
;; If `f' has no directory component, return #f:
;; 
;; 	a/b///c///	=>	a/b/
;; 	abc		=>	#f
;;	abc/		=>	#f
;;	abc/.		=>	#f
;; 	/		=>	/
;; 
(define-public (filename-directory f)
  (let* ((d		(directory-as-filename f))
	 (slash 	(string-rindex d #\/)))
    (and slash (make-shared-substring d 0 (+ slash 1)))))


;; (filename-nondirectory f)
;; (filename-tail f)
;; 
;; Return the last component of filename `f'.
;; 
;;	a/b/c	=>	c
;;	/	=>	/
;;	xyzzy	=>	xyzzy
;; 
(define-public (filename-nondirectory f) (filename-tail f))

(define-public (filename-tail f)
  (let* ((f		(directory-as-filename f))
	 (slash 	(string-rindex f #\/)))
    (if (string=? f "/")
	"/"
	(or (and slash (make-shared-substring f (+ slash 1)))
	    f))))


;; (filename-head f)
;; 
;; Return the first component of filename `f'.
;; 
;; 	/a/b/c	=> /a
;; 	a/b/c	=> a
;;	xyzzy	=> xyzzy
;;	/	=> /
;;		=> 
;; 
(define-public (filename-head f)
  (let* ((f		(directory-as-filename f))
	 (slash 	(string-index f #\/)))
    (cond
     ((not slash)		f)
     ((= 0 slash)		(string-append "/" (filename-head (make-shared-substring f 1))))
     (#t			(make-shared-substring f 0 slash)))))


;; (filename-after-head f)
;; 
;; Return the components of `f' after the first, or #f
;; if `f' has only one component.
;; 
;; 	/a/b/c	=> b/c
;; 	a/b/c	=> b/c
;;	xyzzy	=> #f
;;	/	=> #f
;; 
(define-public (filename-after-head f)
  (let* ((f (directory-as-filename f))
	 (slash (string-index f #\/)))
    (cond
     ((not slash)		#f)
     ((= 0 slash)		(filename-after-head (make-shared-substring f 1)))
     (#t			(make-shared-substring f (1+ slash))))))


;; (filename-extension f)
;; 
;; Return the portion of the last component of `f' that follows the
;; last "." in that component which is not first character of that
;; component, or #f if the last component of `f' does not contain "."
;; which is not its first character.
;; 
;;	a.	=> 
;;	a	=> #f
;; 	x.c/	=> c
;;	a/b/c.c	=> c
;;	/.rc	=> #f
;;	/.rc.x	=> x
;;
(define-public (filename-extension f)
  (let ((f	(filename-tail f)))

    (and=> (string-rindex f #\.)
	   (lambda (pos) (and (< 0 pos) (make-shared-substring f (1+ pos)))))))


;; (filename-sans-extension f)
;; 
;; Return an equivalent to `f' but without the extension (as defined
;; by `filename-extension') or the period preceeding the extension.
;; If `f' has no extension, `f' is returned.
;;
(define-public (filename-sans-extension f)
  (let* ((f 		(directory-as-filename f))
	 (extension	(filename-extension f)))
    (if (not extension)
	f
	(make-shared-substring f 0 (- (1+ (string-length extension)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tilde expansion.
;;; 

;; (tilde-expand-filename)
;;
;; Expand "~" and "~user" if they are the first component in a 
;; filename.
;;
(define-public (tilde-expand-filename f)
  (if (or (string-null? f)
	  (not (char=? #\~ (string-ref f))))
      f
      (let* ((head	(filename-head f))
	     (rest	(filename-after-head f))
	     (user	(if (= 1 (string-length head))
			    (getuid)
			    (substring head 1)))
	     (pwent	(getpw user))
	     (home	(or (kw-arg-ref pwent :dir)
			    head)))

	(string-append (filename-as-directory home) (or rest "")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Canonicalizing Filenames
;;; 
;;; Canonical names are simplified and `as-filename'.
;;; 
;;; Optionally, they're tilde-expanded.
;;; 

;; (canonicalize-filename name :optional tilde-expand?)
;; 
(define-public (canonical-filename name :optional tilde-expand?)

  (directory-as-filename (if tilde-expand?
			     (tilde-expand-filename name)
			     name)))

