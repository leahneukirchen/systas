;;; output-files.scm - handling output files cleanly
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (unix output-files)
  :use-module (unix directories)
  :use-module (unix filenames)
  :use-module (unix file-utils)
  :use-module (standard regexps)
  :use-module (standard list-lib))



;; (with-new-output-file file thunk)
;; 
;; Create `file' exclusively and call `thunk' with `file'
;; as the current output port.
;; 
;; If `thunk' exits via an exception, delete `file'.
;; 
(define-public (with-new-output-file file thunk)
  (with-complete-output-files
   (lambda ()
     (let ((port (without-interrupts
		  (lambda ()
		    (let ((fd (%% %open file '(O_WRONLY O_CREAT O_EXCL) #o666)))
		      (set! the-output-file-list (cons file the-output-file-list))
		      fd)))))
       (with-output-to-port port
	 (lambda ()
	   (let ((answer (thunk)))
	     (%close port)
	     answer)))))))


;; (with-overwriting-output-file file thunk)
;; 
;; Truncate `file' and call `thunk' with `file' as the current output
;; port.
;; 
;; If `thunk' exits via an exception, delete `file'.
;; 
(define-public (with-overwriting-output-file file thunk)
  (with-complete-output-files
   (lambda ()
     (let ((port (without-interrupts
		  (lambda ()
		    (let ((fd (%% %open file '(O_WRONLY O_CREAT O_TRUNC) #o666)))
		      (set! the-output-file-list (cons file the-output-file-list))
		      fd)))))
       (with-output-to-port port
	 (lambda ()
	   (let ((answer (thunk)))
	     (%close port)
	     answer)))))))




;; (with-complete-output-files thunk)
;; 
;; Dynamically initialize `the-output-file-list' to () and call
;; `thunk'.  If thunk exits via an exception, unlink all files named
;; on `the-output-file-list', ignoring errors from the "%unlink"
;; system call.
;; 
;; Regardless, restore `the-output-file-list' before returning.
;; 
(define (with-complete-output-files thunk)
  (let* ((saved-output-file-list		())
	 (exch					(lambda ()
						  (let ((tmp the-output-file-list))
						    (set! the-output-file-list saved-output-file-list
							  saved-output-file-list tmp)))))

    (symmetric-wind exch
		    (lambda ()
		      (catch #t
			thunk
			(lambda exception
			  (map %unlink the-output-file-list)
			  (apply throw exception)))))))

(define the-output-file-list ())


     

;; (with-versioning-output-file file thunk)
;; 
;; Back-up `file' with an emacs-style numbered back-up.
;; 
;; Truncate `file' and call `thunk' with `file' as the current output
;; port.
;; 
;; If `thunk' exits via an exception, restore from the back-up file.
;; 
(define-public (with-versioning-output-file file thunk)
  (let ((backup (create-numbered-backup-file file)))
    (catch #t
      (lambda ()
	(with-complete-output-files
	 (lambda ()
	   (let ((port (without-interrupts
			(lambda ()
			  (let ((fd (%% %open file '(O_WRONLY O_CREAT O_TRUNC) #o666)))
			    (set! the-output-file-list (cons file the-output-file-list))
			    fd)))))
	     (with-output-to-port port
	       (lambda ()
		 (let ((answer (thunk)))
		   (%close port)
		   answer)))))))

      (lambda exception
	(if backup
	    (restore-from-numbered-backup file backup)
	    (%% %unlink file))
	(apply throw exception)))))

;; (restore-from-numbered-backup file backup)
;; 
;; Simply `(%% %rename backup file)'.
;; 
(define-public (restore-from-numbered-backup file backup)
  (%% %rename backup file))

;; (create-numbered-backup-file file)
;; 
;; Create a numbered backup of `file'.
;; 
;; A numbered backup for "file" has a form like "file.~3~".
;; 
;; This procedure makes a best-effort at copying the contents
;; of `file' to a newly-created backup file whose number is 
;; one greater than the highest numbered already existing 
;; backup file.
;; 
(define-public (create-numbered-backup-file file)
  (let loop ((n (highest-backup-number file)))
    (catch 'EEXIST
      (lambda ()
	(catch 'ENOENT
	  (lambda ()
	    (with-input-from-file file
	      (lambda ()
		(let ((backup-filename (form-numbered-backup-name file (1+ n))))
		  (with-new-output-file backup-filename
		    (lambda ()
		      (let ((perms	(kw-arg-ref (%% %fstat (current-input-port)) :permission-bits)))
			(%% %fchmod (current-output-port) perms))
		      
		      (copy-fd)
		      backup-filename))))))
	  (lambda ign
	    #f)))
      (lambda ign
	(loop (1+ n))))))

;; (highest-backup-number file)
;; 
;; Find the current highest-numbered backup of `file' and
;; return its number (an integer).
;; 
(define-public (highest-backup-number file)
  (let* ((dir 			(or (filename-directory file) "."))
	 (files			(directory-files dir :no-./..))
	 (backup-name->n	(backup-regexp-proc file))
	 (backups		(filter-map backup-name->n files)))
    (if backups
	(apply max backups)
	0)))
    

;; (form-numbered-backup-name file n)
;; 
;; Form the name of a numered backup file.  Simply:
;; 
;; 	(string-append file ".~" (->string n) "~")
;; 
(define-public (form-numbered-backup-name file n)
  (string-append file ".~" (->string n) "~"))

(define (backup-regexp-proc f)
  (let ((parser (structured-regexp->procedure `(& ,f ".~" (= :digits (* ,(digit-sre))) "~")
					      :pick-spec '(@ :digits))))
    (lambda (file)
      (and=> (parser file) string->number))))

	 
