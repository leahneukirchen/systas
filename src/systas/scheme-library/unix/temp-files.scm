;;; temp-files.scm - temp file utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (unix temp-files)
  :use-module (unix filenames)
  :use-module (standard list-lib))



      

(begin
  (define default-tmp-directory-candidates	'(/usr/tmp /tmp))
  (define-public default-tmp-directory (find file-is-directory? default-tmp-directory-candidates)))


;; (temp-file-channel . keywords => in-port out-port)
;; 
;; Roughly equivalent to (pipe).
;; 
;; Returns two file ports [iport oport] open on a temp file.  Use this
;; when you may have to buffer large quantities between writing and
;; reading. Note that if the consumer gets ahead of the producer, it
;; won't hang waiting for input, it will just return EOF. To play it
;; safe, make sure that the producer runs to completion before
;; starting the consumer.
;; 
;; The temp file is deleted before `temp-file-channel' returns, so as
;; soon as the ports are closed, the file's disk storage is reclaimed.
;; 
;; By default, temp files are created in `default-tmp-directory'
;; (usually "/usr/tmp" or "/tmp").  The keyword argument `:directory' can be
;; used to specify an alternative directory.
;; 
;;  	:directory dir
;;
;; This procedure will not overwrite existing files.  A large number
;; of temporary filenames are tested, each containing the process id
;; and a decimal expression of a random 32 bit integer (produced by
;; `random32' in scheme, which is `random()' in C).  If, in spite of
;; that, no unused filename can be found, an error condition is
;; signaled.
;; 
;; By default, buffers are imposed on both the input and output ports.
;; This can be overriden by use of keywords:
;; 
;; 	:no-input-buffer
;;	:no-output-buffer
;;	:no-buffers		; on either port
;;
;;
(define-public (temp-file-channel . kws)
  (let* ((dir		(kw-arg-ref kws :directory))
	 (prefix		(form-filename (or dir default-tmp-directory) (string-append (->string (getpid)) "."))))
    (let loop ((n	0))
      (if (> n 100)
	  (throw 'no-temporary-filenames-available)
	  (catch 'EEXIST
	    (lambda ()
	      (let ((fname	(string-append prefix (->string (random32)))))
		(without-interrupts
		 (lambda ()
		   (let* ((in-port	(%% %open fname '(O_CREAT O_EXCL O_RDONLY) #o600))
			  (in-stat	(%% %fstat in-port)))
		     (catch #t
		       (lambda ()
			 (with-interrupts
			  (lambda ()
			    (let* ((out-port 	(%% %open fname '(O_CREAT O_WRONLY) #o600))
				   (out-stat	(%% %fstat out-port)))
			      (if (not (and
					(= (getuid)
					   (kw-arg-ref in-stat :uid)
					   (kw-arg-ref out-stat :uid))
					(= (kw-arg-ref in-stat :ino)
					   (kw-arg-ref out-stat :ino))
					(= (kw-arg-ref in-stat :dev)
					   (kw-arg-ref out-stat :dev))))
				  (throw 'EEXIST))
			      (if (not (memq :no-buffers kws))
				  (begin
				    (if (not (memq :no-input-buffer kws))
					(%% %vfdbuf-buffer-fd in-port #f 'O_RDONLY))
				    (if (not (memq :no-output-buffer kws))
					(%% %vfdbuf-buffer-fd out-port #f 'O_WRONLY))))
			      (%% %unlink fname)
			      (values in-port out-port)))))
		       (lambda exception
			 (%unlink fname)
			 (apply throw exception))))))))
	    (lambda ign
	      (loop (1+ n))))))))


;; (temp-file kws)
;; 
;; Create a new temporary file and return two values:
;; 
;; 	filename	; the name of the file
;; 	fd		; an input port open on the file
;; 
;; Keywords:
;; 
;; 	:directory dir	; create the file in `dir' (default is `default-tmp-directory').
;; 	:prefix name	; a prefix for the temporary filename
;; 
(define-public (temp-file . kws)
  (let* ((dir	(kw-arg-ref kws :directory))
	 (prefix		(form-filename (or dir default-tmp-directory) (string-append (or (kw-arg-ref kws :prefix) "") "." (->string (getpid)) "."))))
    (let loop ((n	0))
      (if (> n 100)
	  (throw 'no-temporary-filenames-available)
	  (catch 'EEXIST
	    (lambda ()
	      (let* ((fname	(string-append prefix (->string (random32))))
		     (in-port	(%% %open fname '(O_CREAT O_EXCL O_RDONLY) #o600)))
		(values fname in-port)))
	    (lambda ign
	      (loop (1+ n))))))))


