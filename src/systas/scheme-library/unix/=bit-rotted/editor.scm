;;; editor.scm - invoking an editor subprocess
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2000 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (unix editor)
  :use-module (unix temp-files)
  :use-module (unix file-utils)
  :use-module (unix shell))




;; (editor-program filename . kws)
;; 
;; Return a process form that describes how to invoke the user's
;; preferred editor on the not-yet-existing file `filename'.
;; 
;; This is more involved than you might think.
;; 
;; The editor program to invoke is chosen this way:
;; 
;; 	1. if `kws' does not include the keyword `:no-editor',
;; 	   and the environment variable `EDITOR' is defined,
;; 	   then use the program named by EDITOR.
;;         
;;      2. Otherwise, the editor will be a fork of this process that
;;         runs a simple program that reads successive lines of input
;;         until end-of-file or until a line beginning with `.'.  As
;;         each line is read, it is stored in the file being editted.
;; 
;; If an `EDITOR' program is defined, it is invoked with
;; one argument: the file to be edited.
;; 
;; But wait...there's more!
;; 
;; `kws' can include an argument:
;; 
;; 	:init init-string
;; 	:prompt prompt-string
;; 
;; If an `EDITOR' is being used (rather than `read-until-dot'), the
;; newly created file is initialized to contain `init-string' (if
;; it is provided) or `prompt-string' (if that is provided).
;; 
;; If `read-until-dot' is being used, `prompt-string' is printed before
;; beginning to read input.
;; 
(define-public (editor-program filename . kws)
  (let ((e	(and (not (memq :no-editor kws)) (getenv "EDITOR")))
	(prompt (kw-arg-ref kws :prompt))
	(init	(kw-arg-ref kws :init)))
    (if e
	(let ((fd	(%% %open filename '(O_WRONLY O_CREAT) #o666)))
	  (%% %write fd (or prompt init ""))
	  (%% %close fd)
	  `(,e	,filename))

	(lambda ()
	  (display (or prompt ""))
	  (read-until-dot)))))

;; (call-editor . kws)
;; 
;; Run the users prefered editor (or a keyword-specified process form
;; (see below)) as a foreground
;; 


(define-public (call-editor . kws)
  (let ((dir		(kw-arg-ref kws :directory))
	(editor		(kw-arg-ref kws :editor))
	(prefix		(kw-arg-ref kws :prefix))
	(prompt		(kw-arg-ref kws :prompt))
	(init		(kw-arg-ref kws :init))
	(no-editor?	(and (memq :no-editor? kws) :no-editor)))
    (call-with-values (lambda () (temp-file :directory dir :prefix prefix))
		      (lambda (filename fd)
			(%% %close fd)
			(let* ((editor		(or editor (editor-program filename :prompt prompt :init init no-editor?))))
			  (checking-processes (lambda () (! editor)))
			  filename)))))
    

(define-public (call-editor/string . args)
  (call-with-values (lambda () (apply call-editor args))
		    (lambda (filename)
		      (let ((s (file->string filename)))
			(%% %unlink filename)
			s))))


(define-public (call-editor/sexp . args)
  (call-with-values (lambda () (apply call-editor args))
		    (lambda (filename)
		      (let ((s (file->string filename)))
			(with-input-from-string s read)))))





(define (read-line-by-characters fd)
  (let ((s	" "))
    (let loop ((answer	#f))
      (cond
       ((= 0 (%% %read fd s))				answer)
       ((char=? #\nl (string-ref s))			(string-append (or answer "") s))
       (#t						(loop (string-append (or answer "") s)))))))
    

(define (read-until-dot fd)
  (display "\n\nEnter text followed by a line beginning with \".\"\n\n")
  (let loop ((answer ""))
    (let ((line (read-line-by-characters fd)))
      (cond
       ((not line)				answer)
       ((char=? #\. (string-ref line))		answer)
       (#t					(loop (string-append answer line)))))))


