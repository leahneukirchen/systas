;;; slib.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;; 
;;; Partially derived from "Template.scm" in slib by Aubrey Jaffer
;;; (which is public domain).


(define-module (slib Systas))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compatability Definitions
;;; 

;; (software-type) should be set to the generic operating system type.
;; UNIX, VMS, MACOS, AMIGA and MS-DOS are supported.
;; 
(define (software-type) 'UNIX)


;; (scheme-implementation-type) should return the name of the scheme
;; implementation loading this file.
;; 
(define (scheme-implementation-type) 'systas-scheme)


;; (scheme-implementation-home-page) should return a (string) URI
;; (Uniform Resource Identifier) for this scheme implementation's home
;; page; or false if there isn't one.
;; 
(define (scheme-implementation-home-page) "http://www.regexps.com")

;; (scheme-implementation-version) should return a string describing
;; the version the scheme implementation loading this file.
;; 
(define (scheme-implementation-version) "<version unspecified>")

;; (library-vicinity) should be defined to be the pathname of the
;; directory where files of Scheme library functions reside.
;;
(define (library-vicinity)
  (if development-version?
      (in-vicinity source-root "scheme-library/slib")
      (in-vicinity install-root "share/scheme/slib")))

;; (implementation-vicinity) should be defined to be the pathname of
;; the directory where any auxillary files to your Scheme
;; implementation reside.
;; 
(define (implementation-vicinity) (library-vicinity))

;; Here for backward compatability
;; 
(define (scheme-file-suffix)	".scm")

;; (home-vicinity) should return the vicinity of the user's HOME
;; directory, the directory which typically contains files which
;; customize a computer environment for a user.
;; 
(define home-vicinity
  (let ((home-path (getenv "HOME")))
    (lambda () home-path)))



;; (OUTPUT-PORT-WIDTH <port>)
;;
(define (output-port-width . arg) 79)

;; (OUTPUT-PORT-HEIGHT <port>)
;;
(define (output-port-height . arg) 24)

;; (TMPNAM) makes a temporary file name.
;; 
(define tmpnam (let ((cntr 100))
		 (lambda () (set! cntr (+ 1 cntr))
			 (string-append ",slib_" (number->string cntr)))))


;; (DELETE-FILE <string>)
;;
(define (delete-file f) (%% (%unlink f)))

;; CHAR-CODE-LIMIT is one greater than the largest integer which can
;; be returned by CHAR->INTEGER.
;; 
(define char-code-limit 256)

(define identity noop)

(define slib:eval eval)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *FEATURES* should be set to a list of symbols describing features
;;; of this implementation.  Suggestions for features are:
;;; 

(define *features*
      '(
	source				;can load scheme source files
					;(slib:load-source "filename")

;	compiled			;can load compiled files
					;(slib:load-compiled "filename")

		       ;; Scheme report features

;	rev5-report			;conforms to
;	eval				;R5RS two-argument eval
	values				;R5RS multiple values
	dynamic-wind			;R5RS dynamic-wind
	macro				;R5RS high level macros
	delay				;has DELAY and FORCE
	multiarg-apply			;APPLY can take more than 2 args.
	char-ready?
;	rationalize
	rev4-optional-procedures	;LIST-TAIL, STRING->LIST,
					;LIST->STRING, STRING-COPY,
					;STRING-FILL!, LIST->VECTOR,
					;VECTOR->LIST, and VECTOR-FILL!

	rev4-report			;conforms to

;	ieee-p1178			;conforms to

;	rev3-report			;conforms to

;	rev2-procedures			;SUBSTRING-MOVE-LEFT!,
					;SUBSTRING-MOVE-RIGHT!,
					;SUBSTRING-FILL!,
					;STRING-NULL?, APPEND!, 1+,
					;-1+, <?, <=?, =?, >?, >=?
;	object-hash			;has OBJECT-HASH

	multiarg/and-			;/ and - can take more than 2 args.
	with-file			;has WITH-INPUT-FROM-FILE and
					;WITH-OUTPUT-FROM-FILE
;	transcript			;TRANSCRIPT-ON and TRANSCRIPT-OFF
;	ieee-floating-point		;conforms to IEEE Standard 754-1985
					;IEEE Standard for Binary
					;Floating-Point Arithmetic.
	full-continuation		;can return multiple times

			;; Other common features

;	srfi				;srfi-0, COND-EXPAND finds all srfi-*
;	sicp				;runs code from Structure and
					;Interpretation of Computer
					;Programs by Abelson and Sussman.
;	defmacro			;has Common Lisp DEFMACRO
;	record				;has user defined data structures
	string-port			;has CALL-WITH-INPUT-STRING and
					;CALL-WITH-OUTPUT-STRING
;	sort
;	pretty-print
;	object->string
;	format				;Common-lisp output formatting
;	trace				;has macros: TRACE and UNTRACE
;	compiler			;has (COMPILER)
;	ed				;(ED) is editor
;	system				;posix (system <string>)
	getenv				;posix (getenv <string>)
	program-arguments		;returns list of strings (argv)
;	current-time			;returns time in seconds since 1/1/1970

		  ;; Implementation Specific features

	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cute Systas Module System / Slib Integration Hack
;;; 
;;; slib `require' loads all slib files into this module and makes
;;; all top-level definitions public.
;;; 

(define slib-module (current-module))


(define (slib:load name)
  (module-excursion
   (lambda ()
     (set-current-module slib-module)

     (if (eval '(defined? load-path))
	 (load-with-path name (cons (library-vicinity) (eval 'load-path)))
	 (load-with-path name (list (library-vicinity)))))))

(define slib:load-source slib:load)

(define defmacro:load slib:load)

(define %system-define define)

(define define
  (procedure->memoizing-macro
   (lambda (exp env)
     (if (= (length env) 1)
	 `(define-public ,@(cdr exp))
	 `(%system-define ,@(cdr exp))))))


(slib:load "require.scm")

(define-public require require:require)


