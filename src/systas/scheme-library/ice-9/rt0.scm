;;; rt0.scm - 	This is the first file loaded by the systas scheme
;;;		interpreter.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998 Tom Lord
;;; This software comes with NO WARRANTY.
;;; The Liberty Software License, version 2:
;;; 	1. This license is valid for all people.
;;;	2. You may use this software without restriction.
;;;



(define argv 		(program-arguments))
(define arg0 		(or (and argv (car argv)) "systas"))

;; start
;; 
;; After this file is loaded, the interpreter calls whatever
;; function is bound to `start'.  By default, that function
;; simply exits.
;;
(define (start) #f)

;; filename-absolute? f
;;
;; Return #t if `f' is an absolute filename, #f otherwise.
;; 
(define (filename-absolute? f)
  (and (<=? 1 (string-length f))
       (char=? #\/ (string-ref f 0))))

;; in-vicinity dir file . files
;;
;; Combine a directory and file name, to form a name for a file,
;; relative to that directory.  
;; 
;; If `dir' is #f or "", return `file' unmodified.
;;
;; If `file' is an absolute file name, return `file'.
;; 
;; If additional files are specified, append each to the result
;; in the same manner.
;;
(define (in-vicinity dir file . files)
  (let ((one-level
	 (cond
	  ((filename-absolute? file)					file)
	  ((not dir)							file)
	  ((string=? "" dir)						file)
	  ((char=? #\/ (string-ref dir -1))				(string-append dir file))
	  (#t								(string-append dir "/" file)))))
    (if (null? files)
	one-level
	(apply in-vicinity one-level files))))
   
;; must-load try-load-fn filename
;;
;; Load a file using `try-load-fn' or exit with an error.
;;
(define (must-load try-load-fn f)
  (or (try-load-fn f)
      (begin
	(display ";;; ERROR " (current-error-port))
	(write `(error-loading-file ,f) (current-error-port) :cycles1)
	(newline (current-error-port))
	(%exit 1))))

;; Load the most basic definitions.  This loads the "load-path" mechanism
;; so that loading additional files is easier.
;;
(@catch #t
  (lambda () (must-load low-level-try-load (in-vicinity ice-9-dir "basic.scm")))
  (lambda error
    (display "ERROR DURING INITIALIZATION\n" (current-error-port))
    (write error (current-error-port))
    (newline (current-error-port))
    (%exit 1)))

;; Dispatch according to the arguments.
;;
;; systas ...other args...
;;	Load "user.scm" (the fancy interactive environment) and enter 
;; 	a read-eval-print loop.  The read-eval-print will interpret
;;	the arguments (see "user.scm").
;;
(@catch #t
    (lambda () (must-load try-load "ice-9/user.scm"))
    (lambda error
      (display "ERROR DURING INITIALIZATION\n" (current-error-port))
      (write error (current-error-port))
      (newline (current-error-port))
      (%exit 1)))

