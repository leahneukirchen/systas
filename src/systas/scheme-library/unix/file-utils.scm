;;; tag: Tom Lord Tue Dec  4 14:59:29 2001 (unix/file-utils.scm)
;;;
;;; file-utils.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998, 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;


(define-module (unix file-utils))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file-executable?
;;; 

;; (file-executable? f)
;; 
;; Return true if `f' names an executable file (as determined by
;; `%access').
;; 
(define-public (file-executable? f)
  (let ((s (%access? f 'X_OK)))
    (and (not (errno? s))
	 s)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File Type Predicates
;;; 

;; (file-is-type? f type . kws)
;; 
;; Return #t if `f' names of file of type `type'.
;; 
;; `f' may be an integer descriptor number, a file descriptor object
;; (port), a string or a symbol (filename).
;; 
;; `type' is one of:
;; 
;; 	S_IFDIR		- a directory
;; 	S_IFCHR		- a character device
;; 	S_IFBLK		- a block device
;; 	S_IFREG		- a regular file
;; 	S_IFLNK		- a symbolic link
;; 	S_IFSOCK	- a socket
;; 	S_IFIFO		- a pipe (fifo)
;; 
;; If a filename is provided, the file type is checked using `%lstat',
;; unless the `:follow-links' keyword is provided in which case
;; `%stat' is used.
;; 
(define-public (file-is-type? f type . kws)
  (let* ((stat-fn	(cond
			 ((integer? f)			%fstat)
			 ((file-descriptor? f)		%fstat)
			 ((memq :follow-links kws)	%stat)
			 (#t				%lstat)))
	 (s 		(stat-fn f)))
    (and (pair? s)
	 (eq? type (kw-arg-ref s :type)))))


;; (file-is-directory? f . kws)
;; 
;; Return true if `f' is a directory, #f otherwise.
;; 
;; The keyword `:follow-links' means to return true for 
;; symbolic links that point to directories.
;; 
(define-public (file-is-directory? f . kws)
  (apply file-is-type? f 'S_IFDIR kws))


;; (file-is-character-device? f . kws)
;; 
;; Return true if `f' is a character device, #f otherwise.
;; 
;; The keyword `:follow-links' means to return true for 
;; symbolic links that point to character devices.
;; 
(define-public (file-is-character-device? f . kws)
  (apply file-is-type? f 'S_IFCHR kws))


;; (file-is-block-device? f . kws)
;; 
;; Return true if `f' is a block device, #f otherwise.
;; 
;; The keyword `:follow-links' means to return true for 
;; symbolic links that point to block devices.
;; 
(define-public (file-is-block-device? f . kws)
  (apply file-is-type? f 'S_IFBLK kws))


;; (file-is-regular-file? f . kws)
;; 
;; Return true if `f' is a regular file, #f otherwise.
;; 
;; The keyword `:follow-links' means to return true for 
;; symbolic links that point to regular files.
;; 
(define-public (file-is-regular-file? f . kws)
  (apply file-is-type? f 'S_IFREG kws))


;; (file-is-symbolic-link? f . kws)
;; 
;; Return true if `f' is a symbolic link, #f otherwise.
;; 
;; The keyword `:follow-links' means to return true for 
;; symbolic links that point to symbolic links.
;; 
(define-public (file-is-symbolic-link? f . kws)
  (apply file-is-type? f 'S_IFLNK kws))


;; (file-is-socket? f . kws)
;; 
;; Return true if `f' is a socket, #f otherwise.
;; 
;; The keyword `:follow-links' means to return true for 
;; symbolic links that point to sockets.
;; 
(define-public (file-is-socket? f . kws)
  (apply file-is-type? f 'S_IFSOCK kws))


;; (file-is-fifo? f . kws)
;; 
;; Return true if `f' is a fifo, #f otherwise.
;; 
;; The keyword `:follow-links' means to return true for 
;; symbolic links that point to fifos.
;; 
(define-public (file-is-fifo? f . kws)
  (apply file-is-type? f 'S_IFSOCK kws))
    
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converting File Contents to Strings
;;; 

;; (file->string file)
;; 
;; Return the contents of `file' as a string.
;;
(define-public (file->string file)
  (fd->string (%% %open file 'O_RDONLY 0)))


;; (fd->string FD)
;; 
;; Read from FD until EOF is reached.  Return a string of everything
;; read.  Try to do this comparatively efficiently.
;;
(define-public (fd->string fd)
  (let ((chunk-list	(fd->chunk-list fd)))
    (cond
     ((and chunk-list
	   (null? (cdr chunk-list))
	   (not (shared-substring? (car chunk-list))))		(car chunk-list))

     (#t							(apply string-append chunk-list)))))


;; (fd->chunk-list FD)
;;
;; Read from FD until EOF is reached.
;;
;; Return a list of strings and shared substrings that when concatenated,
;; are all the characters read.
;;
(define-public (fd->chunk-list fd)
  (let* ((stat		(%fstat fd))
	 (chunk-hint	(or (and stat
				 (let ((sz (kw-arg-ref stat :size)))
				   (and (< 0 sz)
					sz)))
			    16384))
	 (answer	(cons () ())))

    (let loop ((pos		answer)
	       (chunk-size	chunk-hint))

      (let* ((chunk 	(make-string chunk-size))
	     (amt 	(%% %read fd chunk)))

	(cond
	 ((= amt 0)		(cdr answer))

	 (#t			(set-cdr! pos (cons (if (= amt chunk-size)
							chunk
							(make-shared-substring chunk 0 amt))
						    ()))
				(loop (cdr pos) 16384)))))))


;; (copy-fd from-fd to-fd)
;;
;; Read `from-fd' until EOF is reached, writing the contents to `to-fd'.
;;
;; If an I/O error occurs, signal an error.
;;
(define-public (copy-fd :optional from-fd to-fd)
  (let* ((from-fd	(or from-fd (current-input-port)))
	 (to-fd		(or to-fd (current-output-port)))
	 (stat		(%fstat from-fd))
	 (chunk-size	(min (or (and stat
				      (let ((sz (kw-arg-ref stat :size)))
					(and (< 0 sz)
					     (* 8 sz))))
				 0)
			     16384))
	 (chunk		(make-string chunk-size)))

    (let loop ()
      (let* ((amt 	(%% %read from-fd chunk)))

	(cond
	 ((= amt 0)		#t)

	 (#t			(%% %write to-fd (make-shared-substring chunk 0 amt))
				(loop)))))))



;; (file->sexp file)
;; 
;; Simply:
;; 
;; 	(with-input-from-file file read)
;; 
(define-public (file->sexp file)
  (with-input-from-file file read))


