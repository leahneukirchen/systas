;;; safe-directory-find-expressions.scm - for safe-directory-find
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (unix safe-directory-find-expressions)
  :use-module (unix safe-directory-find)
  :use-module (unix filenames)
  :use-module (unix directories)
  :use-module (calendar modern)
  :use-module (standard regexps)
  :use-module (standard char-set-lib))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File Types 
;;; 
(define-public pipe 'pipe)
(define-public character 'character)
(define-public directory 'directory)
(define-public block 'block)
(define-public file 'file)
(define-public link 'link)
(define-public socket 'socket)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Expression Formation
;;; 
(define-public lambda0
  (syntax-rules ()
    ((lambda0 expression)		(lambda () expression))))

(define-public sre-lambda1
  (syntax-rules ()
    ((sre-lambda1 sre)			(let ((fn	(structured-regexp->procedure sre)))
					  (lambda (f) (fn f))))))

(define-public begin begin)

(define-public and and)
(define-public or or)
(define-public not not)
(define-public if if)

(define-public = equal?)
(define-public > >)
(define-public < <)
(define-public >= >=)
(define-public <= <=)

(define-public + +)
(define-public - -)
(define-public * *)
(define-public / /)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skip and prune
;;; 

;; (skip)
;; 
;; Do not search the tree rooted at the current file. 
;; Does not return.
;;
(define-public (skip)
  (throw 'directory-search-skip))  

;; (prune)
;; 
;; Do not search the tree rooted at the current file. 
;; Returns #t.  This has no effect during a depth-first
;; search.
;; 
(define-public (prune)
  (find-prune)
  #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List Expressions
;;; 

(define-public member member)
(define-public quote quote)
(define-public quasiquote quasiquote)
(define-public unquote unquote)
(define-public unquote-splicing unquote-splicing)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Time Expressions
;;; 

;; (minutes n)
;; 
;; Convert `n' (expressed in seconds) to minutes.
;; 
(define-public (minutes n) (ceiling (/ n 60)))


;; (days n)
;; 
;; Convert `n' (expressed in seconds) to days.
;; 
(define-public (days n)	(ceiling (/ n seconds-per-day)))


;; (since n)
;; 
;; Return the difference between the time `safe-directory-find' started
;; and `n' (expressed in seconds).
;; 
(define-public (since n) (- n (current-find-start-time)))


;; (until-today n)
;; 
;; Return the difference between 12AM of the day `safe-directory-find' started
;; and `n' (expressed in seconds).
;; 
(define-public (until-today n) (- n (current-find-today-time)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File Sizes
;;; 

;; (kbytes n)
;; 
;; Convert `n' (in bytes) to kilobytes.
;; 
(define-public (kbytes n) (/ n 1024))


;; (mbytes n)
;; 
;; Convert `n' (in bytes) to megabytes
;; 
(define-public (mbytes n) (/ n 1048576))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Users
;;; 

;; (user val)
;; 
;; Convert `val' to an integer user id.
;; 
;; `val' may be:
;; 
;; 	an integer	- which is simply returned
;; 	a string	- which is treated as a user name
;;			  and looked up in the password file
;; 
(define-public user
  (let ((->uid	(lambda (n)
		  (cond
		   ((number? n)		n)
		   ((string? n)		(let ((rec 	(getpwnam n)))
					  (if (not n)
					      (throw 'no-such-user n))
					  (kw-arg-ref rec :uid)))
		   (#t			(throw 'bogus-user-specification n))))))
    (syntax-rules ()
      ((user n)		(once (->uid n))))))



;; (known-user val)
;; 
;; Return #t if `val' identifies a known user, #f otherwise.
;; 
;; `val' may be:
;; 
;; 	an integer	- which is treated as a user id 
;;			  and looked up in the password file
;; 	a string	- which is treated as a user name
;;			  and looked up in the password file
;; 
(define-public known-user
  (let ((known?	(lambda (n)
		  (cond
		   ((number? n)		(and (getpwuid n) #t))
		   ((string? n)		(and (getpwnam n) #t))
		   (#t			#f)))))
    (syntax-rules ()
      ((known-user n)	(once (known? n))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Groups
;;; 

;; (group val)
;; 
;; Convert `val' to an integer group id.
;; 
;; `val' may be:
;; 
;; 	an integer	- which is simply returned
;; 	a string	- which is treated as a group name
;;			  and looked up in the groups file
;; 
(define-public group
  (let ((->gid	(lambda (n)
		  (cond
		   ((number? n)		n)
		   ((string? n)		(let ((rec 	(getgrnam n)))
					  (if (not n)
					      (throw 'no-such-group n))
					  (kw-arg-ref rec :gid)))
		   (#t			(throw 'bogus-group-specification n))))))
    (syntax-rules ()
      ((user n)		(once (->uid n))))))


;; (known-group val)
;; 
;; Return #t if `val' identifies a known group, #f otherwise.
;; 
;; `val' may be:
;; 
;; 	an integer	- which is treated as a group id 
;;			  and looked up in the password file
;; 	a string	- which is treated as a group name
;;			  and looked up in the password file
;; 
(define-public known-group
  (let ((known?	(lambda (n)
		  (cond
		   ((number? n)		(and (getgrgid n) #t))
		   ((string? n)		(and (getgrnam n) #t))
		   (#t			#f)))))
    (syntax-rules ()
      ((known-group n)	(once (known? n))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings and Filenames
;;; 

;; (match regexp string)
;; 
;; Return true if `string' contains a match for `regexp', #f otherwise.
;; 
(define-public match
  (syntax-rules ()
    ((match sre name)	((once (structured-regexp->procedure sre)) name))))


(define-public filename-directory filename-directory)
(define-public filename-tail filename-tail)
(define-public filename-head filename-head)
(define-public filename-after-head filename-after-head)
(define-public filename-extension filename-extension)
(define-public filename-sans-extension filename-sans-extension)
(define-public tilde-expand-filename tilde-expand-filename)

(define-public alpha-sre alpha-sre)
(define-public alnum-sre alnum-sre)
(define-public digit-sre digit-sre)
(define-public whitespace-sre whitespace-sre)
(define-public blank-sre blank-sre)
(define-public non0-digit-sre non0-digit-sre)
(define-public positive-integer-sre positive-integer-sre)
(define-public nonnegative-integer-sre nonnegative-integer-sre)


;; (path)
;; 
;; Return the current filename.
;; 
(define-public (path)
  (current-find-path))


;; (link-path)
;; 
;; Return the target of the current filename if it is a 
;; symbolic link (and throw an exception if it is not).
;; 
(define-public (link-path)
  (%% %readlink (current-find-stat)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Depth of Search
;;; 

;; (depth)
;; 
;; Return the depth of the current file relative to the root
;; of the search.
;; 
(define-public (depth)
  (current-find-depth))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inode Components
;;; 


;; (dev)
;; 
;; The device number of the current file.
;; 
(define-public (dev)
  (kw-arg-ref (current-find-stat) :dev))


;; (ino)
;; 
;; The inode number of the current file.
;; 
(define-public (ino)
  (kw-arg-ref (current-find-stat) :ino))


;; (nlink)
;; 
;; The number of links to the current file.
;; 
(define-public (nlink)
  (kw-arg-ref (current-find-stat) :nlink))


;; (uid)
;; 
;; The owner id of the current file.
;; 
(define-public (uid)
  (kw-arg-ref (current-find-stat) :uid))


;; (gid)
;; 
;; The owner group id of the current file.
;; 
(define-public (gid)
  (kw-arg-ref (current-find-stat) :gid))


;; (size)
;; 
;; The size, in bytes, of the current file.
;; 
(define-public (size)
  (kw-arg-ref (current-find-stat) :size))


;; (atime)
;; 
;; The last-access time of the current file.
;; 
(define-public (atime)
  (kw-arg-ref (current-find-stat) :atime))


;; (mtime)
;; 
;; The last-modification time of the current file.
;; 
(define-public (mtime)
  (kw-arg-ref (current-find-stat) :mtime))


;; (ctime)
;; 
;; The last-status-change time of the current file.
;; 
(define-public (ctime)
  (kw-arg-ref (current-find-stat) :ctime))


;; (file-modified file)
;; 
;; The modification time of `file'.
;; 
(define-public (file-modified file)
  (let ((stat	(%% %stat file)))
    (kw-arg-ref stat :mtime)))


;; (type)
;; 
;; Return the type of the current file.  The result is one of 
;; the symbols:
;; 
;; 	pipe
;;	character
;;	directory
;;	block
;;	file
;;	link
;;	socket
;; 
(define-public (type)
  (let ((stat-type 	(kw-arg-ref (current-find-lstat) :type)))
    (case stat-type
      ((S_IFIFO)	'pipe)
      ((S_IFCHR)	'character)
      ((S_IFDIR)	'directory)
      ((S_IFBLK)	'block)
      ((S_IFREG)	'file)
      ((S_IFLNK)	'link)
      ((S_IFSOCK)	'socket)
      (else		(throw 'unrecognized-file-type stat-type)))))


;; (mode)
;; 
;; Return the symbolic mode of the current file (in the manner of "ls
;; -l").
;; 
(define-public (mode)
  (let ((perms (kw-arg-ref (current-find-stat) :permissions)))
    (string	(if (memq 'S_IRUSR perms)
		    #\r
		    #\-)
		(if (memq 'S_IWUSR perms)
		    #\w
		    #\-)
		(if (memq 'S_IXUSR perms)
		    (if (memq 'S_ISUID perms)
			#\S
			#\x)
		    (if (memq 'S_ISUID perms)
			#\s
			#\-))
		(if (memq 'S_IRGRP perms)
		    #\r
		    #\-)
		(if (memq 'S_IWGRP perms)
		    #\w
		    #\-)
		(if (memq 'S_IXGRP perms)
		    (if (memq 'S_ISGID perms)
			#\S
			#\x)
		    (if (memq 'S_ISGID perms)
			#\s
			#\-))
		(if (memq 'S_IROTH perms)
		    #\r
		    #\-)
		(if (memq 'S_IWOTH perms)
		    #\w
		    #\-)
		(if (memq 'S_IXOTH perms)
		    (if (memq 'S_ISGID perms)
			#\T
			#\x)
		    (if (memq 'S_ISGID perms)
			#\t
			#\-)))))
