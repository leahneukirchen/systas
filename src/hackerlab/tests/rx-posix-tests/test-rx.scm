;;; test-rx.scm - Rx tests and C code generator
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2000 Free Software Foundation, Inc.
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (test-rx)
  :use-module (standard string-parsing)
  :use-module (unix output-files))



(define (run-tests)
  (do ((n 0 (1+ n))
       (tests rx-tests (cdr tests)))

      ((null? tests) 	#t)

    (apply expand-compile-flags run-a-test n (car tests))))

(define (run-test name)
  (apply expand-compile-flags run-a-test 0 name (assq-ref rx-tests name)))

(define last-compile #f)
(define last-compiled #f)
(define last-exec #f)

(define (expand-compile-flags fn test-number test-name compile compile-error exec exec-result)
  (let ((compile-flags (cadr compile)))
    (cond
     ((eq? 'REG_BOTH compile-flags) 		(fn test-number test-name (list (car compile) #f) compile-error exec exec-result)
						(fn test-number test-name (list (car compile) 'REG_EXTENDED) compile-error exec exec-result))

     ((and (list? compile-flags)
	   (memq 'REG_BOTH compile-flags))	(let ((bflags (delq 'REG_BOTH compile-flags))
						      (eflags (map (lambda (x) (if (eq? x 'REG_BOTH) 'REG_EXTENDED x)) compile-flags)))
						  (fn test-number test-name (list (car compile) bflags) compile-error exec exec-result)
						  (fn test-number test-name (list (car compile) eflags) compile-error exec exec-result)))
     (#t					(fn test-number test-name compile compile-error exec exec-result)))))


(define (run-a-test test-number test-name compile compile-error exec exec-result)
  (display* test-number ": " test-name "\n")
  
  (set! last-compile compile)
  (set! last-compiled 'not-reached)
  (set! last-exec 'not-reached)

  (let ((compiled (apply regcomp compile)))
    (set! last-compiled compiled)
    (if compile-error
	(cond
	 ((compiled-regexp? compiled)		(test-error #f test-name "compilation expected to fail but didn't" compiled))
	 ((not (eq? compiled compile-error))	(test-error #t test-name "compilation expected to fail but didn't" compiled)))
	
	(if (not (compiled-regexp? compiled))
	    (test-error #f test-name "incorrect compilation error (no error expected)" compiled)
	    
	    (begin
	      (set! last-exec exec)
	      (let ((execed (apply regexec compiled (copy-tree exec))))
		(if (not (equal? execed exec-result))
		    (test-error #f test-name "incorrect regexec result" execed)
		    #t))))))
  (force-output))


(define (test-error warning-only? name message bogus-result)
  (force-output)
  (with-output-to-port (current-error-port)
    (lambda ()
      (apply-to-args (assq-ref rx-tests name)
	(lambda (compile compile-error exec exec-result)
	  (display* "### " (if warning-only? "WARNING" "ERROR") " on test " name "\n"
		    "\tdiagnosis: " message "\n"
		    "\tgot: " (->string bogus-result) "\n"
		    "\tregcomp: " (->string compile) "\n"
		    "\tregcomp error: " (->string (or compile-error 'no-error)) "\n"
		    "\tregexec: " (->string exec) "\n"
		    "\tregexec result: " (->string exec-result) "\n")
	  (force-output))))))



(define (generate-test-cases)

  (define (flags-string flags)
    (cond
     ((null? flags)		"0")
     ((symbol? flags)		flags)
     (#t			(apply join-fields-with " | " flags))))

  (define (doit posix-only?)
    (display* "struct rx_test rx_tests[] =\n"
	      "{\n")
    (for-each (lambda (test)
		(apply-to-args (cons (lambda (test-number test-name compile compile-error exec exec-result)
				       (and (or (not posix-only?) (regexec (once (regcomp "^posix" #f)) test-name #f))
					    (display* "  {\n"
						      "    " (->string (symbol->string test-name)) ",\n"
						      "    " (->string (car compile)) ",\n"		; pattern
						      "    " (flags-string (cadr compile)) ",\n"		; cflags
						      "    " (or compile-error 0) ",\n"		; compile error
						      "    " (if exec (->string (car exec)) "\"\"") ",\n"		; string
						      "    " (if (and exec (cddr exec)) (flags-string (caddr exec)) 0) ",\n"	; eflags
						      "    " (if exec-result 1 0) ",\n"		; is_match
						      "    " (if (vector? exec-result) (vector-length exec-result) 0) ",\n") ; n_match
					    (display* "    {")
					    (if (not (vector? exec-result))
						(display* "{-2, -2}, {-2, -2}, {-2, -2}, {-2, -2}, {-2, -2}, {-2, -2}, {-2, -2}, {-2, -2}, {-2, -2}, {-2, -2}, {-2, -2}")
						(begin
						  (vector-for-each (lambda (n) (display* "{" (car n) ", " (cdr n) "}, ")) exec-result)
						  (for-each (lambda (ign) (display* "{-2, -2}, ")) (range (- 10 (vector-length exec-result))))
						  (display* "{-2, -2}")))
					    (display* "}\n"
						      "  },\n")))
				     (cons 0 test))
		  expand-compile-flags))
	      rx-tests)
    (display* "  {0, 0, 0, 0, 0, 0, 0, 0, {{0, 0}}}\n")
    (display* "};\n"))

  (with-overwriting-output-file "test-cases.h"
    (lambda () (doit #f)))
  (with-overwriting-output-file "posix-test-cases.h"
    (lambda () (doit #t))))



(define (generate-tcl-test-cases)

  (define (flags-string flags)
    (cond
     ((null? flags)		"0")
     ((symbol? flags)		flags)
     (#t			(apply join-fields-with " | " flags))))

  (define (doit)
    (for-each (lambda (test)
		(apply-to-args (cons (lambda (test-number test-name compile compile-error exec exec-result)
				       (and (regexec (once (regcomp "^posix" #f)) test-name #f)
					    (cadr compile)
					    (not compile-error)
					    (display* "  test"
						      " " (->string (symbol->string test-name)) ; pattern name
						      " " (->string (car compile)) ; pattern
						      " " (if exec (->string (car exec)) "\"\"") ; string
						      " " (if exec-result 1 0); is_match
						      " " ) 
					    (if (not (vector? exec-result))
						(display* "{-1 -1} {-1 -1} {-1 -1} {-1 -1} {-1 -1} {-1 -1} {-1 -1} {-1 -1} {-1 -1} {-1 -1} {-1 -1}")
						(begin
						  (vector-for-each (lambda (n) (display* "{" (car n) " " (if (<= 0 (cdr n)) (1- (cdr n)) -1) "} ")) exec-result)
						  (for-each (lambda (ign) (display* "{-1 -1} ")) (range (- 10 (vector-length exec-result))))
						  (display* "{-2 -2}")))
					    (display* "  \n")))
				     (cons 0 test))
		  expand-compile-flags))
	      rx-tests)
    (display* "  {0, 0, 0, 0, 0, 0, 0, 0, {{0, 0}}}\n")
    (display* "};\n"))

  (doit))



(define rx-tests
  '(
    ;; Posix Conformance Tests
    ;; 
    ;; The tests on this page are organized with respect to section 2.8
    ;; of Posix.2 ("Regular Expression Notation").
    ;; 
    ;; Every matcher that claims to support Posix should pass these 
    ;; tests (at least).
    ;; 
    ;; 


    ; posix.2 2.8.3.1
    ; 
    ; "BREs matching a single character or collating element"
    ;

    ; ordinary character
    ;
    (posix-2.8.3.1-0 ("a" #f)
		     #f
		     ("a" #(0))
		     #((0 . 1)))


    ; special character preceeded by a backslash
    ; 
    (posix-2.8.3.1-1 ("\\." #f)
		     #f
		     ("." #(0))
		     #((0 . 1)))

    (posix-2.8.3.1-2 ("\\[" #f)
		     #f
		     ("[" #(0))
		     #((0 . 1)))
    (posix-2.8.3.1-3 ("\\\\" #f)
		     #f
		     ("\\" #(0))
		     #((0 . 1)))
    (posix-2.8.3.1-4 ("\\*" #f)
		     #f
		     ("*" #(0))
		     #((0 . 1)))
    (posix-2.8.3.1-5 ("\\^" #f)
		     #f
		     ("^" #(0))
		     #((0 . 1)))
    (posix-2.8.3.1-6 ("\\$" #f)
		     #f
		     ("$" #(0))
		     #((0 . 1)))
    (posix-2.8.3.1-7 ("\\" #f)
		     REG_EESCAPE
		     #f
		     #f)
    (posix-2.8.3.1-8 ("x\\." #f)
		     #f
		     ("x." #(0))
		     #((0 . 2)))

    (posix-2.8.3.1-9 ("x\\[" #f)
		     #f
		     ("x[" #(0))
		     #((0 . 2)))
    (posix-2.8.3.1-10 ("x\\\\" #f)
		      #f
		      ("x\\" #(0))
		      #((0 . 2)))
    (posix-2.8.3.1-11 ("x\\*" #f)
		      #f
		      ("x*" #(0))
		      #((0 . 2)))
    (posix-2.8.3.1-12 ("x\\^" #f)
		      #f
		      ("x^" #(0))
		      #((0 . 2)))
    (posix-2.8.3.1-13 ("x\\$" #f)
		      #f
		      ("x$" #(0))
		      #((0 . 2)))
    (posix-2.8.3.1-14 ("x\\" #f)
		      REG_EESCAPE
		      #f
		      #f)

    ; 2.8.3.1.3
    ; 
    ; "Periods in BREs"
    ; 

    ; match any character except NUL.
    ; NUL is used as end-of-string by regexec.
    ;
    (posix-2.8.3.1.3-0 ("." #f)
		       #f
		       ("a" #(0))
		       #((0 . 1)))
    (posix-2.8.3.1.3-1 ("." #f)
		       #f
		       ("\n" #(0))
		       #((0 . 1)))

    ; 2.8.3.2
    ; 
    ; "RE Bracket Expressions"
    ;

    ; item 1
    ;
    ; An initial ] is interpreted as an ordinary character.
    ; 
    (posix-2.8.3.2-0 ("[]x]" REG_BOTH)
		     #f
		     ("]" #(0))
		     #((0 . 1)))
    (posix-2.8.3.2-1 ("[]x]" REG_BOTH)
		     #f
		     ("x" #(0))
		     #((0 . 1)))
    (posix-2.8.3.2-2 ("[]" REG_BOTH)
		     REG_EBRACK
		     #f
		     #f)


    ; . * [ \ are ordinary
    ;
    (posix-2.8.3.2-3 ("[.]" REG_BOTH)
		     #f
		     ("." #(0))
		     #((0 . 1)))
    (posix-2.8.3.2-4 ("[.]" REG_BOTH)
		     #f
		     ("a" #(0))
		     #f)
    (posix-2.8.3.2-5 ("[*]" REG_BOTH)
		     #f
		     ("*" #(0))
		     #((0 . 1)))
    (posix-2.8.3.2-6 ("[[]" REG_BOTH)
		     #f
		     ("[" #(0))
		     #((0 . 1)))
    (posix-2.8.3.2-7 ("[\\]" REG_BOTH)
		     #f
		     ("\\" #(0))
		     #((0 . 1)))

    ; [. [= and [: are special (tests or correct uses later)
    ;
    (posix-2.8.3.2-8 ("[[.]" REG_BOTH)
		     REG_ECOLLATE
		     #f
		     #f)
    (posix-2.8.3.2-9 ("[[=]" REG_BOTH)
		     REG_ECOLLATE
		     #f
		     #f)
    (posix-2.8.3.2-10 ("[[:]" REG_BOTH)
		      REG_ECTYPE
		      #f
		      #f)

    ; a matching list (more complex tests later)
    ;
    (posix-2.8.3.2-11 ("[abc]" REG_BOTH)
		      #f
		      ("a" #(0))
		      #((0 . 1)))
    (posix-2.8.3.2-12 ("[abc]" REG_BOTH)
		      #f
		      ("b" #(0))
		      #((0 . 1)))
    (posix-2.8.3.2-13 ("[abc]" REG_BOTH)
		      #f
		      ("c" #(0))
		      #((0 . 1)))
    (posix-2.8.3.2-14 ("[abc]" REG_BOTH)
		      #f
		      ("d" #(0))
		      #f)
    (posix-2.8.3.2-15 ("x[abc]" REG_BOTH)
		      #f
		      ("xa" #(0))
		      #((0 . 2)))
    (posix-2.8.3.2-16 ("x[abc]" REG_BOTH)
		      #f
		      ("xb" #(0))
		      #((0 . 2)))
    (posix-2.8.3.2-17 ("x[abc]" REG_BOTH)
		      #f
		      ("xc" #(0))
		      #((0 . 2)))
    (posix-2.8.3.2-18 ("x[abc]" REG_BOTH)
		      #f
		      ("xd" #(0))
		      #f)

    ; a nonmatching list (more complex tests follow)
    ;
    (posix-2.8.3.2-19 ("[^abc]" REG_BOTH)
		      #f
		      ("a" #(0))
		      #f)
    (posix-2.8.3.2-20 ("[^abc]" REG_BOTH)
		      #f
		      ("b" #(0))
		      #f)
    (posix-2.8.3.2-21 ("[^abc]" REG_BOTH)
		      #f
		      ("c" #(0))
		      #f)
    (posix-2.8.3.2-22 ("[^abc]" REG_BOTH)
		      #f
		      ("d" #(0))
		      #((0 . 1)))
    (posix-2.8.3.2-23 ("x[^abc]" REG_BOTH)
		      #f
		      ("xa" #(0))
		      #f)
    (posix-2.8.3.2-24 ("x[^abc]" REG_BOTH)
		      #f
		      ("xb" #(0))
		      #f)
    (posix-2.8.3.2-25 ("x[^abc]" REG_BOTH)
		      #f
		      ("xc" #(0))
		      #f)
    (posix-2.8.3.2-26 ("x[^abc]" REG_BOTH)
		      #f
		      ("xd" #(0))
		      #((0 . 2)))

    
    ; Following the order of documentation in Posix.2,
    ; tests for collating elements go here.
    ; 
    ; But we want to test character ranges first,
    ; sol collating element tests are further
    ; down.
    ; 

    ; [=equiv=] equivalence classes
    ; 
    ; These can not be implemented in a portable way,
    ; at all.  (They can be implemented in a way that
    ; is integrated in a non-portable way with 
    ; `setlocale'.)
    ; 

    ; standard character classes:
    ;
    (posix-2.8.3.2-27 ("[[:alnum:]][[:alnum:]]*" REG_BOTH)
		      #f
		      ("%abc123890XYZ=" #(0))
		      #((1 . 13)))
    (posix-2.8.3.2-28 ("[[:cntrl:]][[:cntrl:]]*" REG_BOTH)
		      #f
		      ("%\n\t\r\f " #(0))
		      #((1 . 5)))
    (posix-2.8.3.2-29 ("[[:lower:]][[:lower:]]*" REG_BOTH)
		      #f
		      ("AbcdE" #(0))
		      #((1 . 4)))
    (posix-2.8.3.2-30 ("[[:lower:]][[:lower:]]*" (REG_BOTH REG_ICASE))
		      #f
		      ("AbcdE" #(0))
		      #((0 . 5)))
    (posix-2.8.3.2-31 ("[[:space:]][[:space:]]*" (REG_BOTH REG_ICASE))
		      #f
		      ("x \t\f\nx" #(0))
		      #((1 . 5)))
    (posix-2.8.3.2-32 ("[[:alpha:]][[:alpha:]]*" REG_BOTH)
		      #f
		      ("%abC123890xyz=" #(0))
		      #((1 . 4)))
    (posix-2.8.3.2-33 ("[[:digit:]][[:digit:]]*" REG_BOTH)
		      #f
		      ("%abC123890xyz=" #(0))
		      #((4 . 10)))
    (posix-2.8.3.2-34 ("[[:print:]][[:print:]]*" REG_BOTH)
		      #f
		      ("\n %abC12\f" #(0))
		      #((1 . 8)))
    (posix-2.8.3.2-35 ("[[:upper:]][[:upper:]]*" REG_BOTH)
		      #f
		      ("\n aBCDEFGHIJKLMNOPQRSTUVWXYz" #(0))
		      #((3 . 27)))
    (posix-2.8.3.2-36 ("[[:upper:]][[:upper:]]*" (REG_BOTH REG_ICASE))
		      #f
		      ("\n aBCDEFGHIJKLMNOPQRSTUVWXYz" #(0))
		      #((2 . 28)))
    (posix-2.8.3.2-37 ("[[:blank:]][[:blank:]]*" (REG_BOTH REG_ICASE))
		      #f
		      ("\na \t b" #(0))
		      #((2 . 5)))
    (posix-2.8.3.2-38 ("[[:graph:]][[:graph:]]*" REG_BOTH)
		      #f
		      ("\n %abC12\f" #(0))
		      #((2 . 8)))
    (posix-2.8.3.2-39 ("[[:punct:]][[:punct:]]*" REG_BOTH)
		      #f
		      ("a~!@#$%^&*()_+=-`[]{};':\"|\\,./?>< " #(0))
		      #((1 . 33)))
    (posix-2.8.3.2-40 ("[[:xdigit:]][[:xdigit:]]*" REG_BOTH)
		      #f
		      ("-0123456789ABCDEFabcdef" #(0))
		      #((1 . 23)))

    ; range expressions
    ; 
    ; These tests presume that the C locale is being used.
    ;
    (posix-2.8.3.2-41 ("[a-z][a-z]*" REG_BOTH)
		      #f
		      ("ABCabcxyzABC" #(0))
		      #((3 . 9)))

    ; out of order ranges are invalid
    ;
    (posix-2.8.3.2-42 ("[a-z][z-a]*" REG_BOTH)
		      REG_ERANGE
		      #f
		      #f)

    ; single character ranges are valid
    ;
    (posix-2.8.3.2-43 ("[a-a][a-a]*" REG_BOTH)
		      #f
		      ("zaaaaab" #(0))
		      #((1 . 6)))
    (posix-2.8.3.2-44 ("[a-a][a-a]*" (REG_BOTH REG_ICASE))
		      #f
		      ("ZAAAAAB" #(0))
		      #((1 . 6)))

    ; - first stands for itself
    ;
    (posix-2.8.3.2-45 ("[--Z][--Z]*" REG_BOTH)
		      #f
		      ("!ABC-./XYZ~" #(0))
		      #((1 . 10)))


    ; - last in a range stands for itself
    ;
    (posix-2.8.3.2-46 ("[*--Z][*--Z]*" REG_BOTH)
		      #f
		      ("!+*,---ABC" #(0))
		      #((1 . 7)))

    ; - last in the expression stands for itself
    (posix-2.8.3.2-47 ("[a-][a-]*" REG_BOTH)
		      #f
		      ("xa-a--a-ay" #(0))
		      #((1 . 9)))


    ; [.col.]	collating elements
    ;
    ; Support for multi-character collating elements 
    ; can not be fully implemented in a portable way.
    ;
    ; These test single character collating elements,
    ; presuming the C locale.  
    ; 
    ; These tests won't detect an implementation that
    ; supports collating elements but is not 
    ; locale-sensative.
    ;
    (posix-2.8.3.2-48 ("[a[.-.]z][a[.-.]z]*" REG_BOTH)
		      #f
		      ("ba-a-a-zw" #((0)))
		      #((1 . 8)))

    (posix-2.8.3.2-49 ("[[.a.]-[.z.]][[.a.]-z]*" REG_BOTH)
		      #f
		      ("ABCabcxyzABC" #(0))
		      #((3 . 9)))
    (posix-2.8.3.2-50 ("[[.a.]-[.a.]][[.a.]-[.a.]]*" REG_BOTH)
		      #f
		      ("zaaaaab" #(0))
		      #((1 . 6)))
    (posix-2.8.3.2-51 ("[a-[.a.]][[.a.]-a]*" (REG_BOTH REG_ICASE))
		      #f
		      ("ZAAAAAB" #(0))
		      #((1 . 6)))

    (posix-2.8.3.2-52 ("[[.-.]-[.Z.]][[.-.]-[.Z.]]*" REG_BOTH)
		      #f
		      ("!ABC-./XYZ~" #(0))
		      #((1 . 10)))


    ; - last in a range stands for itself
    ;
    (posix-2.8.3.2-53 ("[*--Z][*-[.-.]Z]*" REG_BOTH)
		      #f
		      ("!+*,---ZABC" #(0))
		      #((1 . 8)))

    ; complex character sets
    ;
    (posix-2.8.3.2-54 ("[[:digit:]a-z#$%][[:digit:]a-z#$%]*" REG_BOTH)
		      #f
		      ("__abc#lmn012$x%yz789*" #(0))
		      #((2 . 20)))
    (posix-2.8.3.2-55 ("[[:digit:]a-z#$%][[:digit:]a-z#$%]*" (REG_BOTH REG_ICASE))
		      #f
		      ("__abcLMN012x%#$yz789*" #(0))
		      #((2 . 20)))

    ; negated character sets
    ;
    (posix-2.8.3.2-56 ("[^[:digit:]a-z#$%][^[:digit:]a-z#$%]*" REG_BOTH)
		      #f
		      ("abc#lmn012$x%yz789--@*,abc" #(0))
		      #((18 . 23)))
    (posix-2.8.3.2-57 ("[^[:digit:]a-z#$%][^[:digit:]a-z#$%]*" (REG_BOTH REG_ICASE))
		      #f
		      ("abC#LMn012$x%yz789--@*,abc" #(0))
		      #((18 . 23)))

    ; special case of [^- ...]
    (posix-2.8.3.2-58 ("[^-][^-]*" REG_BOTH)
		      #f
		      ("---afd*(&,ml---" #(0))
		      #((3 . 12)))
    (posix-2.8.3.2-59 ("[^--Z][^--Z]*" REG_BOTH)
		      #f
		      ("---AFD*(&,ml---" #(0))
		      #((6 . 12)))
    (posix-2.8.3.2-60 ("[^--Z][^--Z]*" (REG_BOTH REG_ICASE))
		      #f
		      ("---AFD*(&,ml---" #(0))
		      #((6 . 10)))


    ; BREs Matching Multiple Characters
    ;

    ; simple concatenation
    ;
    (posix-2.8.3.3-0 ("abc[def]ghi" #f)
		     #f
		     ("xabcdghiy" #(0))
		     #((1 . 8)))

    ; simple use of BRE subexpressions
    ;
    (posix-2.8.3.3-1 ("abc\\(\\(de\\)\\(fg\\)\\)hi" #f)
		     #f
		     ("xabcdefghiy" #(0))
		     #((1 . 10)))

    ; simple use of *
    ;
    (posix-2.8.3.3-2 ("abc*def" #f)
		     #f
		     ("xabdefy" #(0))
		     #((1 . 6)))
    (posix-2.8.3.3-3 ("abc*def" #f)
		     #f
		     ("xabcdefy" #(0))
		     #((1 . 7)))
    (posix-2.8.3.3-4 ("abc*def" #f)
		     #f
		     ("xabcccccccdefy" #(0))
		     #((1 . 13)))
    (posix-2.8.3.3-5 ("abc\\(def\\)*ghi" #f)
		     #f
		     ("xabcghiy" #(0))
		     #((1 . 7)))
    (posix-2.8.3.3-6 ("abc\\(def\\)*ghi" #f)
		     #f
		     ("xabcdefghi" #(0))
		     #((1 . 10)))
    (posix-2.8.3.3-7 ("abc\\(def\\)*ghi" #f)
		     #f
		     ("xabcdefdefdefghi" #(0))
		     #((1 . 16)))


    ; simple use of {}
    ;
    (posix-2.8.3.3-8 ("abc\\{0,1\\}def" #f)
		     #f
		     ("xabdefy" #(0))
		     #((1 . 6)))
    (posix-2.8.3.3-9 ("abc\\{0,1\\}def" #f)
		     #f
		     ("xabcdefy" #(0))
		     #((1 . 7)))
    (posix-2.8.3.3-10 ("abc\\{0,1\\}def" #f)
		      #f
		      ("xabccdefy" #(0))
		      #f)
    (posix-2.8.3.3-11 ("abc\\{1,3\\}def" #f)
		      #f
		      ("xabdefy" #(0))
		      #f)
    (posix-2.8.3.3-12 ("abc\\{1,3\\}def" #f)
		      #f
		      ("xabcdefy" #(0))
		      #((1 . 7)))
    (posix-2.8.3.3-13 ("abc\\{1,3\\}def" #f)
		      #f
		      ("xabccdefy" #(0))
		      #((1 . 8)))
    (posix-2.8.3.3-14 ("abc\\{1,3\\}def" #f)
		      #f
		      ("xabcccdefy" #(0))
		      #((1 . 9)))
    (posix-2.8.3.3-15 ("abc\\{1,3\\}def" #f)
		      #f
		      ("xabccccdefy" #(0))
		      #f)
    (posix-2.8.3.3-16 ("abc\\{1,\\}def" #f)
		      #f
		      ("xabdefy" #(0))
		      #f)
    (posix-2.8.3.3-17 ("abc\\{1,\\}def" #f)
		      #f
		      ("xabcdefy" #(0))
		      #((1 . 7)))
    (posix-2.8.3.3-18 ("abc\\{1,\\}def" #f)
		      #f
		      ("xabccdefy" #(0))
		      #((1 . 8)))
    (posix-2.8.3.3-19 ("abc\\{1,\\}def" #f)
		      #f
		      ("xabcccdefy" #(0))
		      #((1 . 9)))
    (posix-2.8.3.3-20 ("abc\\{1,\\}def" #f)
		      #f
		      ("xabccccdefy" #(0))
		      #((1 . 10)))
    (posix-2.8.3.3-21 ("abc\\{3\\}def" #f)
		      #f
		      ("xabdefy" #(0))
		      #f)
    (posix-2.8.3.3-22 ("abc\\{3\\}def" #f)
		      #f
		      ("xabcdefy" #(0))
		      #f)
    (posix-2.8.3.3-23 ("abc\\{3\\}def" #f)
		      #f
		      ("xabccdefy" #(0))
		      #f)
    (posix-2.8.3.3-24 ("abc\\{3\\}def" #f)
		      #f
		      ("xabcccdefy" #(0))
		      #((1 . 9)))
    (posix-2.8.3.3-25 ("abc\\{3\\}def" #f)
		      #f
		      ("xabccccdefy" #(0))
		      #f)

    ; syntax errors with {}
    (posix-2.8.3.3-26 ("abc\\{ 1,3\\}def" #f)
		      REG_BADBR
		      #f
		      #f)
    (posix-2.8.3.3-27 ("abc\\{1 ,3\\}def" #f)
		      REG_BADBR
		      #f
		      #f)
    (posix-2.8.3.3-28 ("abc\\{1, 3\\}def" #f)
		      REG_BADBR
		      #f
		      #f)
    (posix-2.8.3.3-29 ("abc\\{1,3 \\}def" #f)
		      REG_BADBR
		      #f
		      #f)

    ; backreferences
    ; 
    (posix-2.8.3.3-30 ("\\(\\(a*\\)\\)*x\\2" #f)
		      #f
		      ("ax" #(0 1 2))
		      #((0 . 2) (1 . 1) (1 . 1)))


    (posix-2.8.3.3-31 ("\\(\\(a*\\)\\)*x\\2" #f)
		      #f
		      ("axa" #(0 1 2))
		      #((0 . 3) (0 . 1) (0 . 1)))

    (posix-2.8.3.3-32 ("\\(\\(abc\\)*\\(abc\\)*\\)\\3" #f)
		      #f
		      ("abcabc" #(0 1 2 3))
		      #((0 . 6) (0 . 3) (-1 . -1) (0 . 3)))

    (posix-2.8.3.3-33 ("\\(\\(abc\\)\\{0,1\\}\\(abc\\)\\{0,1\\}\\)\\3" #f)
		      #f
		      ("abcabc" #(0 1 2 3))
		      #((0 . 6) (0 . 3) (-1 . -1) (0 . 3)))
    (posix-2.8.3.3-34 ("\\(\\(abc\\)\\{0,2\\}\\(abc\\)\\{0,2\\}\\)\\3" #f)
		      #f
		      ("abcabc" #(0 1 2 3))
		      #((0 . 6) (0 . 3) (-1 . -1) (0 . 3)))

    (posix-2.8.3.3-35 ("a\\(b\\)c\\2" #f)
		      REG_ESUBREG
		      #f
		      #f)

    (posix-2.8.3.3-36 ("\\(abc\\)\\1" REG_ICASE)
		      #f
		      ("abcabc" #(0 1))
		      #((0 . 6) (0 . 3)))

    (posix-2.8.3.3-37 ("\\(abc\\)\\1" REG_ICASE)
		      #f
		      ("aBcAbC" #(0 1))
		      #((0 . 6) (0 . 3)))

    ; anchors
    ;
    (posix-2.8.3.5-0 ("^abc" #f)
		     #f
		     ("abcdef" #(0))
		     #((0 . 3)))
    (posix-2.8.3.5-1 ("^abc" #f)
		     #f
		     ("xyzabcdef" #(0))
		     #f)
    (posix-2.8.3.5-2 ("^abc" #f)
		     #f
		     ("\nabcdef" #(0))
		     #f)
    (posix-2.8.3.5-3 ("abc$" #f)
		     #f
		     ("defabc" #(0))
		     #((3 . 6)))
    (posix-2.8.3.5-4 ("abc$" #f)
		     #f
		     ("defabc\n" #(0))
		     #f)
    (posix-2.8.3.5-5 ("^abc$" #f)
		     #f
		     ("abc" #(0))
		     #((0 . 3)))
    (posix-2.8.3.5-6 ("^abc$" #f)
		     #f
		     ("\nabc\n" #(0))
		     #f)

    ; ^ and $ are not special character when not used
    ; as anchors (in BRE).  This refers to 2.8.3.1.2, but the
    ; tests here with the anchor tests.
    ;
    (posix-2.8.3.5-7 ("a\\{0,1\\}^bc" #f)
		     #f
		     ("bc" #(0))
		     #f)
    (posix-2.8.3.5-8 ("a\\{0,1\\}^bc" #f)
		     #f
		     ("^bc" #(0))
		     #((0 . 3)))
    (posix-2.8.3.5-9 ("a\\{0,1\\}^bc" #f)
		     #f
		     ("a^bc" #(0))
		     #((0 . 4)))
    (posix-2.8.3.5-10 ("a^bc" #f)
		      #f
		      ("abc" #(0))
		      #f)
    (posix-2.8.3.5-11 ("a^bc" #f)
		      #f
		      ("a^bc" #(0))
		      #((0 . 4)))

    (posix-2.8.3.5-12 ("ab$c\\{0,1\\}" #f)
		      #f
		      ("ab" #(0))
		      #f)
    (posix-2.8.3.5-13 ("ab$c\\{0,1\\}" #f)
		      #f
		      ("ab$" #(0))
		      #((0 . 3)))
    (posix-2.8.3.5-14 ("ab$c\\{0,1\\}" #f)
		      #f
		      ("ab$c" #(0))
		      #((0 . 4)))

    (posix-2.8.3.5-15 ("ab$c" #f)
		      #f
		      ("abc" #(0))
		      #f)
    (posix-2.8.3.5-16 ("ab$c" #f)
		      #f
		      ("ab\nc" #(0))
		      #f)
    (posix-2.8.3.5-17 ("ab$c" #f)
		      #f
		      ("ab$" #(0))
		      #f)
    (posix-2.8.3.5-18 ("ab$c" #f)
		      #f
		      ("ab$c" #(0))
		      #((0 . 4)))


    ; EREs Matching a Single Character or Collating Element
    ;

    ; ordinary character
    ;
    (posix-2.8.4.1.1-0 ("a" REG_EXTENDED)
		       #f
		       ("a" #(0))
		       #((0 . 1)))

    ; quoted special characters
    ;
    (posix-2.8.4.1.2-0 ("\\." REG_EXTENDED)
		       #f
		       ("a.b" #(0))
		       #((1 . 2)))
    (posix-2.8.4.1.2-1 ("\\[" REG_EXTENDED)
		       #f
		       ("a[b" #(0))
		       #((1 . 2)))
    (posix-2.8.4.1.2-2 ("\\\\" REG_EXTENDED)
		       #f
		       ("a\\b" #(0))
		       #((1 . 2)))
    (posix-2.8.4.1.2-3 ("\\(" REG_EXTENDED)
		       #f
		       ("a(b" #(0))
		       #((1 . 2)))
    ; close paren is special only when matched with
    ; an open paren.
    ;
    (posix-2.8.4.1.2-4 ("\\*" REG_EXTENDED)
		       #f
		       ("a*b" #(0))
		       #((1 . 2)))
    (posix-2.8.4.1.2-5 ("\\+" REG_EXTENDED)
		       #f
		       ("a+b" #(0))
		       #((1 . 2)))
    (posix-2.8.4.1.2-6 ("\\?" REG_EXTENDED)
		       #f
		       ("a?b" #(0))
		       #((1 . 2)))
    (posix-2.8.4.1.2-7 ("\\|" REG_EXTENDED)
		       #f
		       ("a|b" #(0))
		       #((1 . 2)))
    (posix-2.8.4.1.2-8 ("\\^" REG_EXTENDED)
		       #f
		       ("a^b" #(0))
		       #((1 . 2)))
    (posix-2.8.4.1.2-9 ("\\$" REG_EXTENDED)
		       #f
		       ("a$b" #(0))
		       #((1 . 2)))
    

    ; Periods in EREs
    ;
    (posix-2.8.4.1.3-0 ("." REG_EXTENDED)
		       #f
		       ("a" #(0))
		       #((0 . 1)))
    (posix-2.8.4.1.3-1 ("." REG_EXTENDED)
		       #f
		       ("\n" #(0))
		       #((0 . 1)))

    ; Bracket Expressions in EREs
    ; 
    ; Bracket expressions in EREs are the same as bracket
    ; expressions in BREs.  The bracket tests above (posix-2.8.3.2)
    ; test both BREs and EREs.
    ;

    ; EREs Matching Mulitple Characters

    ; simple concatenation
    ;
    (posix-2.8.4.3-0 ("abc[def]ghi" REG_EXTENDED)
		     #f
		     ("xabcdghiy" #(0))
		     #((1 . 8)))

    ; simple use of BRE subexpressions
    ;
    (posix-2.8.4.3-1 ("abc((de)(fg))hi" REG_EXTENDED)
		     #f
		     ("xabcdefghiy" #(0))
		     #((1 . 10)))

    ; simple use of *
    ;
    (posix-2.8.4.3-2 ("abc*def" REG_EXTENDED)
		     #f
		     ("xabdefy" #(0))
		     #((1 . 6)))
    (posix-2.8.4.3-3 ("abc*def" REG_EXTENDED)
		     #f
		     ("xabcdefy" #(0))
		     #((1 . 7)))
    (posix-2.8.4.3-4 ("abc*def" REG_EXTENDED)
		     #f
		     ("xabcccccccdefy" #(0))
		     #((1 . 13)))
    (posix-2.8.4.3-5 ("abc(def)*ghi" REG_EXTENDED)
		     #f
		     ("xabcghiy" #(0))
		     #((1 . 7)))
    (posix-2.8.4.3-6 ("abc(def)*ghi" REG_EXTENDED)
		     #f
		     ("xabcdefghi" #(0))
		     #((1 . 10)))
    (posix-2.8.4.3-7 ("abc(def)*ghi" REG_EXTENDED)
		     #f
		     ("xabcdefdefdefghi" #(0))
		     #((1 . 16)))

    ; simple use of +
    ;
    (posix-2.8.4.3-8 ("abc+def" REG_EXTENDED)
		     #f
		     ("xabdefy" #(0))
		     #f)
    (posix-2.8.4.3-9 ("abc+def" REG_EXTENDED)
		     #f
		     ("xabcdefy" #(0))
		     #((1 . 7)))
    (posix-2.8.4.3-10 ("abc+def" REG_EXTENDED)
		     #f
		     ("xabcccccccdefy" #(0))
		     #((1 . 13)))
    (posix-2.8.4.3-11 ("abc(def)+ghi" REG_EXTENDED)
		     #f
		     ("xabcghiy" #(0))
		     #f)
    (posix-2.8.4.3-12 ("abc(def)+ghi" REG_EXTENDED)
		     #f
		     ("xabcdefghi" #(0))
		     #((1 . 10)))
    (posix-2.8.4.3-13 ("abc(def)+ghi" REG_EXTENDED)
		     #f
		     ("xabcdefdefdefghi" #(0))
		     #((1 . 16)))

    ; simple use of ?
    ;
    (posix-2.8.4.3-14 ("abc?def" REG_EXTENDED)
		     #f
		     ("xabdefy" #(0))
		     #((1 . 6)))
    (posix-2.8.4.3-15 ("abc?def" REG_EXTENDED)
		     #f
		     ("xabcdefy" #(0))
		     #((1 . 7)))
    (posix-2.8.4.3-16 ("abc?def" REG_EXTENDED)
		     #f
		     ("xabcccccccdefy" #(0))
		     #f)
    (posix-2.8.4.3-17 ("abc(def)?ghi" REG_EXTENDED)
		     #f
		     ("xabcghiy" #(0))
		     #((1 . 7)))
    (posix-2.8.4.3-18 ("abc(def)?ghi" REG_EXTENDED)
		     #f
		     ("xabcdefghi" #(0))
		     #((1 . 10)))
    (posix-2.8.4.3-19 ("abc(def)?ghi" REG_EXTENDED)
		     #f
		     ("xabcdefdefdefghi" #(0))
		     #f)




    ; simple use of {}
    ;
    (posix-2.8.4.3-20 ("abc{0,1}def" REG_EXTENDED)
		     #f
		     ("xabdefy" #(0))
		     #((1 . 6)))
    (posix-2.8.4.3-21 ("abc{0,1}def" REG_EXTENDED)
		     #f
		     ("xabcdefy" #(0))
		     #((1 . 7)))
    (posix-2.8.4.3-22 ("abc{0,1}def" REG_EXTENDED)
		      #f
		      ("xabccdefy" #(0))
		      #f)
    (posix-2.8.4.3-23 ("abc{1,3}def" REG_EXTENDED)
		      #f
		      ("xabdefy" #(0))
		      #f)
    (posix-2.8.4.3-24 ("abc{1,3}def" REG_EXTENDED)
		      #f
		      ("xabcdefy" #(0))
		      #((1 . 7)))
    (posix-2.8.4.3-25 ("abc{1,3}def" REG_EXTENDED)
		      #f
		      ("xabccdefy" #(0))
		      #((1 . 8)))
    (posix-2.8.4.3-26 ("abc{1,3}def" REG_EXTENDED)
		      #f
		      ("xabcccdefy" #(0))
		      #((1 . 9)))
    (posix-2.8.4.3-27 ("abc{1,3}def" REG_EXTENDED)
		      #f
		      ("xabccccdefy" #(0))
		      #f)
    (posix-2.8.4.3-28 ("abc{1,}def" REG_EXTENDED)
		      #f
		      ("xabdefy" #(0))
		      #f)
    (posix-2.8.4.3-29 ("abc{1,}def" REG_EXTENDED)
		      #f
		      ("xabcdefy" #(0))
		      #((1 . 7)))
    (posix-2.8.4.3-30 ("abc{1,}def" REG_EXTENDED)
		      #f
		      ("xabccdefy" #(0))
		      #((1 . 8)))
    (posix-2.8.4.3-31 ("abc{1,}def" REG_EXTENDED)
		      #f
		      ("xabcccdefy" #(0))
		      #((1 . 9)))
    (posix-2.8.4.3-32 ("abc{1,}def" REG_EXTENDED)
		      #f
		      ("xabccccdefy" #(0))
		      #((1 . 10)))
    (posix-2.8.4.3-33 ("abc{3}def" REG_EXTENDED)
		      #f
		      ("xabdefy" #(0))
		      #f)
    (posix-2.8.4.3-34 ("abc{3}def" REG_EXTENDED)
		      #f
		      ("xabcdefy" #(0))
		      #f)
    (posix-2.8.4.3-35 ("abc{3}def" REG_EXTENDED)
		      #f
		      ("xabccdefy" #(0))
		      #f)
    (posix-2.8.4.3-36 ("abc{3}def" REG_EXTENDED)
		      #f
		      ("xabcccdefy" #(0))
		      #((1 . 9)))
    (posix-2.8.4.3-37 ("abc{3}def" REG_EXTENDED)
		      #f
		      ("xabccccdefy" #(0))
		      #f)

    ; syntax errors with {}
    ;
    (posix-2.8.4.3-38 ("abc{ 1,3}def" REG_EXTENDED)
		      REG_BADBR
		      #f
		      #f)
    (posix-2.8.4.3-39 ("abc{1 ,3}def" REG_EXTENDED)
		      REG_BADBR
		      #f
		      #f)
    (posix-2.8.4.3-40 ("abc{1, 3}def" REG_EXTENDED)
		      REG_BADBR
		      #f
		      #f)
    (posix-2.8.4.3-41 ("abc{1,3 }def" REG_EXTENDED)
		      REG_BADBR
		      #f
		      #f)


    ; ERE alternation
    ; 

    ; The whole string should be matched.
    ; 
    ; The length of each subexpression, left-to-right, top-to-bottom
    ; should be maximized.
    ;
    ;  The grammar (Posix.2 2.8.5.2) says that the two outermost 
    ; subexpressions are:
    ; 
    ; 		(wee|week)(night|knights)
    ;		s*
    ;
    ; So, compared to "weeknights", 
    ;
    ; 		0 == weeknights
    ; 		1 == wee		-- not week
    ; 		2 == knights		-- not night
    ; 
    (posix-2.8.4.3-42 ("(wee|week)(night|knights)s*" REG_EXTENDED)
		      #f
		      ("weeknights" #(0 1 2))
		      #((0 . 10) (0 . 3) (3 . 10)))

    (posix-2.8.4.3-43 ("(a|aaa)*" REG_EXTENDED)
		      #f
		      ("aaaa" #(0 1))
		      #((0 . 4) (1 . 4)))

    (posix-2.8.4.3-44 ("(a|aaa){0,100}" REG_EXTENDED)
		      #f
		      ("aaaa" #(0 1))
		      #((0 . 4) (1 . 4)))

    ; same thing, checking the "s*" subexpression.
    ;;
    (posix-2.8.4.3-45 ("(wee|week)(night|knights)(s*)" REG_EXTENDED)
		      #f
		      ("weeknights" #(0 1 2 3))
		      #((0 . 10) (0 . 3) (3 . 10) (10 . 10)))


    ; This illustrates semantic differences between Posix and Perl
    ; 
    (posix-2.8.4.3-46 ("(week|wee)(knights|night)" REG_EXTENDED)
		      #f
		      ("weeknights" #(0 1 2))
		      #((0 . 10) (0 . 3) (3 . 10)))


    ; This illustrates that RE+ is not the same as RE RE*
    ; 
    (posix-2.8.4.3-47 ("(aaa|a)+" REG_EXTENDED)
		      #f
		      ("aaaa" #(0 1))
		      #((0 . 4) (1 . 4)))

    ; this is a case that some versions of Rx got wrong
    ;
    (posix-2.8.4.3-48 ("(a*)*x\\1" REG_EXTENDED)
		      #f
		      ("aaaax" #(0 1))
		      #((0 . 5) (4 . 4)))

    ; this is a case that some versions of Rx got wrong
    ;
    (posix-2.8.4.3-49 ("(a*)*x\\1(a*)" REG_EXTENDED)
		      #f
		      ("aaaaxaa" #(0 1 2))
		      #((0 . 7) (2 . 4) (7 . 7)))

    (posix-2.8.4.3-50 ("(a*)*x(\\1a*)" REG_EXTENDED)
		      #f
		      ("aaaaxaa" #(0 1 2))
		      #((0 . 7) (2 . 4) (5 . 7)))

    ; this is a case that some versions of Rx got wrong
    ;
    (posix-2.8.4.3-51 ("(a*)*x(\\1x)*(.*)" REG_EXTENDED)
		      #f
		      ("aaaaxxyy" #(0 1 2 3))
		      #((0 . 8) (4 . 4) (5 . 6) (6 . 8)))

    (posix-2.8.4.3-52 ("(a{0,}){0,}x\\1" REG_EXTENDED)
		      #f
		      ("aaaax" #(0 1))
		      #((0 . 5) (4 . 4)))

    (posix-2.8.4.3-53 ("(a{0,}){0,}x\\1(a{0,})" REG_EXTENDED)
		      #f
		      ("aaaaxaa" #(0 1 2))
		      #((0 . 7) (2 . 4) (7 . 7)))

    (posix-2.8.4.3-54 ("(a{0,}){0,}x(\\1x){0,}(.{0,})" REG_EXTENDED)
		      #f
		      ("aaaaxxyy" #(0 1 2 3))
		      #((0 . 8) (4 . 4) (5 . 6) (6 . 8)))

    ; ^ and $ are always special in an ERE
    ;
    (posix-2.8.4.6-0 ("a{0,1}^bc" REG_EXTENDED)
		     #f
		     ("bc" #(0))
		     #((0 . 2)))
    (posix-2.8.4.6-1 ("a{0,1}^bc" REG_EXTENDED)
		     #f
		     ("^bc" #(0))
		     #f)

    (posix-2.8.4.6-2 ("a{0,1}^bc" REG_EXTENDED)
		     #f
		     ("a^bc" #(0))
		     #f)

    (posix-2.8.4.6-3 ("a^bc" REG_EXTENDED)
		     #f
		     ("abc" #(0))
		     #f)

    (posix-2.8.4.6-4 ("a^bc" REG_EXTENDED)
		     #f
		     ("a^bc" #(0))
		     #f)

    (posix-2.8.4.6-5 ("ab$c{0,1}" REG_EXTENDED)
		     #f
		     ("ab" #(0))
		     #((0 . 2)))

    (posix-2.8.4.6-6 ("ab$c{0,1}" REG_EXTENDED)
		     #f
		     ("ab$" #(0))
		     #f)
    (posix-2.8.4.6-7 ("ab$c{0,1}" REG_EXTENDED)
		     #f
		     ("ab$c" #(0))
		     #f)

    (posix-2.8.4.6-8 ("ab$c" REG_EXTENDED)
		     #f
		     ("abc" #(0))
		     #f)
    (posix-2.8.4.6-9 ("ab$c" REG_EXTENDED)
		     #f
		     ("ab\nc" #(0))
		     #f)
    (posix-2.8.4.6-10 ("ab$c" REG_EXTENDED)
		      #f
		      ("ab$" #(0))
		      #f)
    (posix-2.8.4.6-11 ("ab$c" REG_EXTENDED)
		      #f
		      ("ab$c" #(0))
		      #f)

    ;; Posix Conformance Tests (Part II)
    ;; 
    ;; The tests on this page are organized with respect to section B.5
    ;; of Posix.2 ("C Binding for RE Matching").
    ;; 
    ;; Most of regexp interface is tested above, including the tricky
    ;; question of what values are returned in `pmatch'.
    ;; 
    ;; This section tests these compile flags:
    ;; 
    ;;		REG_ICASE	-- ignore case
    ;; 		REG_NOSUB	-- ignore `pmatch'
    ;; 		REG_NEWLINE	-- treat newline as an end-of-string marker
    ;;
    ;; and these exec flags:
    ;; 
    ;; 		REG_NOTBOL	-- the beginning of the string is not
    ;; 				   matched by ^
    ;; 		REG_NOTEOL	-- the end of the string is not matched
    ;; 				   by $
    ;; 
    ;; Every matcher that claims to support Posix should pass these 
    ;; tests (at least).
    ;; 
    ;;

    ;; Posix REG_ICASE Tests
    ;; 
    ;; These test REG_ICASE.  The tests for bracket expressions 
    ;; above (2.8.3.2) also includes tests of REG_ICASE.
    ;; 
    ;; Every matcher that claims to support Posix should pass these 
    ;; tests (at least).
    ;; 
    
    (posix-case-0 ("xx" (REG_BOTH REG_ICASE))
		  #f
		  ("xX" #f)
		  #t)
    (posix-case-1 ("xX" (REG_BOTH REG_ICASE))
		  #f
		  ("xx" #f)
		  #t)
    (posix-case-2 ("x x" (REG_BOTH REG_ICASE))
		  #f
		  ("x X" #f)
		  #t)
    (posix-case-3 ("x X" (REG_BOTH REG_ICASE))
		  #f
		  ("x x" #f)
		  #t)
    (posix-case-4 ("x X" (REG_BOTH REG_ICASE))
		  #f
		  ("x y x" #f)
		  #f)


    ; REG_NOSUB
    ;
    (posix-nosub-0 ("^*x" REG_NOSUB)
		   #f
		   ("*x" #f #f)
		   #t)
    (posix-nosub-1 ("\\(*x\\)" REG_NOSUB)
		   #f
		   ("*x" #f #f)
		   #t)
    (posix-nosub-3 ("*x" REG_NOSUB)
		   #f
		   ("*x" #f #f)
		   #t)
    (posix-nosub-4 ("a\\{2,5\\}" REG_NOSUB)
		   #f
		   ("a" #f #f)
		   #f)
    (posix-nosub-5 ("a{2,5}" (REG_EXTENDED REG_NOSUB))
		   #f
		   ("a" #f #f)
		   #f)

    (posix-nosub-6 ("a\\{2,5\\}" REG_NOSUB)
		   #f
		   ("aa" #f #f)
		   #t)
    (posix-nosub-7 ("a{2,5}" (REG_EXTENDED REG_NOSUB))
		   #f
		   ("aa" #f #f)
		   #t)

    (posix-nosub-8 ("a\\{2,5\\}" REG_NOSUB)
		   #f
		   ("aaa" #f #f)
		   #t)
    (posix-nosub-9 ("a{2,5}" (REG_EXTENDED REG_NOSUB))
		   #f
		   ("aaa" #f #f)
		   #t)

    (posix-nosub-10 ("a\\{2,5\\}" REG_NOSUB)
		   #f
		   ("aaaa" #f #f)
		   #t)
    (posix-nosub-11 ("a{2,5}" (REG_EXTENDED REG_NOSUB))
		   #f
		   ("aaaa" #f #f)
		   #t)

    (posix-nosub-12 ("a\\{2,5\\}" REG_NOSUB)
		   #f
		   ("aaaaa" #f #f)
		   #t)
    (posix-nosub-13 ("a{2,5}" (REG_EXTENDED REG_NOSUB))
		   #f
		   ("aaaaa" #f #f)
		   #t)

    (posix-nosub-14 ("a\\{2,5\\}" REG_NOSUB)
		   #f
		   ("aaaaaa" #f #f)
		   #t)
    (posix-nosub-15 ("a{2,5}" (REG_EXTENDED REG_NOSUB))
		   #f
		   ("aaaaaa" #f #f)
		   #t)

    (posix-nosub-16 ("(abcd){10,11}" (REG_EXTENDED REG_NOSUB))
		    #f
		    ("abcd" #f #f)
		    #f)

    (posix-nosub-17 ("back-tracking oriented stream-of-solution functions" (REG_EXTENDED REG_NOSUB))
		    #f
		    ("in the spec, and the back-tracking oriented stream-of-solution functions" #f #f)
		    #t)


    ; REG_EOL/REG_BOL
    ;
    (posix-not_ol-0 ("^abc" REG_BOTH)
		     #f
		     ("abcdef" #(0) REG_NOTBOL)
		     #f)
    (posix-not_ol-1 ("^abc" REG_BOTH)
		     #f
		     ("xyz\nabcdef" #(0) REG_NOTBOL)
		     #f)
    (posix-not_ol-2 ("^abc" REG_BOTH)
		     #f
		     ("xyzabcdef" #(0) REG_NOTBOL)
		     #f)
    (posix-not_ol-3 ("^abc" REG_BOTH)
		     #f
		     ("\nabcdef" #(0) REG_NOTBOL)
		     #f)
    (posix-not_ol-4 ("abc$" REG_BOTH)
		     #f
		     ("defabc" #(0) REG_NOTEOL)
		     #f)
    (posix-not_ol-5 ("abc$" REG_BOTH)
		     #f
		     ("defabc\nghi" #(0) REG_NOTEOL)
		     #f)
    (posix-not_ol-6 ("abc$" REG_BOTH)
		     #f
		     ("defabc\n" #(0) REG_NOTEOL)
		     #f)
    (posix-not_ol-7 ("^abc$" REG_BOTH)
		     #f
		     ("abc" #(0) REG_NOTEOL)
		     #f)
    (posix-not_ol-8 ("^abc$" REG_BOTH)
		     #f
		     ("abc" #(0) REG_NOTBOL)
		     #f)
    (posix-not_ol-9 ("^abc$" REG_BOTH)
		     #f
		     ("abc" #(0) (REG_NOTBOL REG_NOTEOL))
		     #f)
    (posix-not_ol-10 ("^abc$" REG_BOTH)
		     #f
		     ("\nabc\n" #(0) REG_NOTBOL)
		     #f)
    (posix-not_ol-11 ("^abc$" REG_BOTH)
		     #f
		     ("\nabc\n" #(0) REG_NOTEOL)
		     #f)
    (posix-not_ol-12 ("^abc$" REG_BOTH)
		     #f
		     ("\nabc\n" #(0) (REG_NOTBOL REG_NOTEOL))
		     #f)

    (posix-complex-not_ol-0 ("^a(b)*c" REG_EXTENDED)
			    #f
			    ("abcdef" #(0 1) REG_NOTBOL)
			    #f)
    (posix-complex-not_ol-1 ("^a(b)*c" REG_EXTENDED)
			    #f
			    ("xyz\nabcdef" #(0 1) REG_NOTBOL)
			    #f)
    (posix-complex-not_ol-2 ("^a(b)*c" REG_EXTENDED)
			    #f
			    ("xyzabcdef" #(0 1) REG_NOTBOL)
			    #f)
    (posix-complex-not_ol-3 ("^a(b)*c" REG_EXTENDED)
			    #f
			    ("\nabcdef" #(0 1) REG_NOTBOL)
			    #f)
    (posix-complex-not_ol-4 ("a(b)*c$" REG_EXTENDED)
			    #f
			    ("defabc" #(0 1) REG_NOTEOL)
			    #f)
    (posix-complex-not_ol-5 ("a(b)*c$" REG_EXTENDED)
			    #f
			    ("defabc\nghi" #(0 1) REG_NOTEOL)
			    #f)
    (posix-complex-not_ol-6 ("a(b)*c$" REG_EXTENDED)
			    #f
			    ("defabc\n" #(0 1) REG_NOTEOL)
			    #f)
    (posix-complex-not_ol-7 ("^a(b)*c$" REG_EXTENDED)
			    #f
			    ("abc" #(0 1) REG_NOTEOL)
			    #f)
    (posix-complex-not_ol-8 ("^a(b)*c$" REG_EXTENDED)
			    #f
			    ("abc" #(0 1) REG_NOTBOL)
			    #f)
    (posix-complex-not_ol-9 ("^a(b)*c$" REG_EXTENDED)
			    #f
			    ("abc" #(0 1) (REG_NOTBOL REG_NOTEOL))
			    #f)
    (posix-complex-not_ol-10 ("^a(b)*c$" REG_EXTENDED)
			     #f
			     ("\nabc\n" #(0 1) REG_NOTBOL)
			     #f)
    (posix-complex-not_ol-11 ("^a(b)*c$" REG_EXTENDED)
			     #f
			     ("\na(b)*c\n" #(0 1) REG_NOTEOL)
			     #f)
    (posix-complex-not_ol-12 ("^a(b)*c$" REG_EXTENDED)
			     #f
			     ("\nabc\n" #(0 1) (REG_NOTBOL REG_NOTEOL))
			     #f)


    ; REG_NEWLINE
    ; 
    (posix-newline-0 ("." REG_BOTH)
		     #f
		     ("\n" #f)
		     #t)
    (posix-newline-1 ("." (REG_BOTH REG_NEWLINE))
		     #f
		     ("\n" #f)
		     #f)

    (posix-newline-2 ("^abc" (REG_BOTH REG_NEWLINE))
		     #f
		     ("xyz\nabcdef\nxyz" #(0))
		     #((4 . 7)))
    (posix-newline-3 ("^abc" (REG_BOTH REG_NEWLINE))
		     #f
		     ("xyz\nxabcdef\nxyz" #(0))
		     #f)
    (posix-newline-4 ("z$\nabc" REG_NEWLINE)
		     #f
		     ("xyz\nabcdef\nxyz" #(0))
		     #f)
    (posix-newline-5 ("z$\nabc" (REG_EXTENDED REG_NEWLINE))
		     #f
		     ("xyz\nabcdef\nxyz" #(0))
		     #((2 . 7)))
    (posix-newline-6 ("^abc" (REG_BOTH REG_NEWLINE))
		     #f
		     ("abc\nabcdef\nxyz" #(0) REG_NOTBOL)
		     #((4 . 7)))

    (posix-newline-7 ("def$" (REG_BOTH REG_NEWLINE))
		     #f
		     ("xyz\nabcdef\nxyz" #(0))
		     #((7 . 10)))
    (posix-newline-8 ("def$" (REG_BOTH REG_NEWLINE))
		     #f
		     ("xyz\nxabcdefx\nxyz" #(0))
		     #f)
    (posix-newline-9 ("def$\nx" REG_NEWLINE)
		     #f
		     ("xyz\nabcdef\nxyz" #(0))
		     #f)
    (posix-newline-10 ("def$\nx" (REG_EXTENDED REG_NEWLINE))
		     #f
		     ("xyz\nabcdef\nxyz" #(0))
		     #((7 . 12)))


    ; REG_NEWLINE tests with more complicated regexps.
    ; 
    (posix-complex-newline-2 ("^a(b)*c" (REG_EXTENDED REG_NEWLINE))
			     #f
			     ("xyz\nabcdef\nxyz" #(0 1))
			     #((4 . 7) (5 . 6)))
    (posix-complex-newline-3 ("^a(b)*c" (REG_EXTENDED REG_NEWLINE))
			     #f
			     ("xyz\nxabcdef\nxyz" #(0 1))
			     #f)
    (posix-complex-newline-4 ("z$\na(b)*c" REG_NEWLINE)
			     #f
			     ("xyz\nabcdef\nxyz" #(0 1))
			     #f)
    (posix-complex-newline-5 ("z$\na(b)*c" (REG_EXTENDED REG_NEWLINE))
			     #f
			     ("xyz\nabcdef\nxyz" #(0 1))
			     #((2 . 7) (5 . 6)))
    (posix-complex-newline-6 ("^a(b)*c" (REG_EXTENDED REG_NEWLINE))
			     #f
			     ("abc\nabcdef\nxyz" #(0 1) REG_NOTBOL)
			     #((4 . 7) (5 . 6)))

    (posix-complex-newline-7 ("d(e)f$" (REG_EXTENDED REG_NEWLINE))
			     #f
			     ("xyz\nabcdef\nxyz" #(0 1))
			     #((7 . 10) (8 . 9)))
    (posix-complex-newline-8 ("d(e)f$" (REG_EXTENDED REG_NEWLINE))
			     #f
			     ("xyz\nxabcdefx\nxyz" #(0 1))
			     #f)
    (posix-complex-newline-9 ("d(e)f$\nx" REG_NEWLINE)
			     #f
			     ("xyz\nabcdef\nxyz" #(0 1))
			     #f)
    (posix-complex-newline-10 ("d(e)f$\nx" (REG_EXTENDED REG_NEWLINE))
			      #f
			      ("xyz\nabcdef\nxyz" #(0 1))
			      #((7 . 12) (8 . 9)))



    ;; Miscellaneous Posix Tests
    ;; 
    ;; The test for some Posix bugs found in older versions of Rx, but
    ;; not caught by the systematic Posix tests above, and for 
    ;; conditions that trigger or defeat various optimizations in
    ;; Rx that might not otherwise be tested by the tests above.
    ;; 
    ;; Every matcher that claims to support Posix should pass these 
    ;; tests (at least).
    ;; 


    ; In BRE, * is not special at the beginning of an expression, of
    ; a parenthesized expression, or following an anchoring "^".
    ;
    (posix-check-0 ("^*x" #f)
		   #f
		   ("*x" #(0))
		   #((0 . 2)))
    (posix-check-1 ("\\(*x\\)" #f)
		   #f
		   ("*x" #(0 1))
		   #((0 . 2) (0 . 2)))
    (posix-check-2 ("*x" #f)
		   #f
		   ("*x" #(0))
		   #((0 . 2)))
    (posix-check-3 ("a\\{2,5\\}" #f)
		   #f
		   ("a" #f)
		   #f)

    (posix-check-4 ("a\\{2,5\\}" #f)
		   #f
		   ("aa" #f)
		   #t)

    (posix-check-5 ("a\\{2,5\\}" #f)
		   #f
		   ("aaa" #f)
		   #t)

    (posix-check-6 ("a\\{2,5\\}" #f)
		   #f
		   ("aaaa" #f)
		   #t)

    (posix-check-7 ("a\\{2,5\\}" #f)
		   #f
		   ("aaaaa" #f)
		   #t)

    (posix-check-8 ("a\\{2,5\\}" #f)
		   #f
		   ("aaaaaa" #(0))
		   #((0 . 5)))

    (posix-check-9 ("(abcd){10,11}" REG_EXTENDED)
		    #f
		    ("abcd" #(0 1))
		    #f)

    ; Arguably, this is a legal regexp on machines with
    ; sizeof(long) > 8.  On all other machines, though,
    ; this checks that numbers in {} expressions are
    ; parsed with some sanity.
    ; 
    (posix-check-10 ("(abcd){9223372036854775808}" REG_EXTENDED)
		    REG_BADBR
		    #f
		    #f)


    (posix-check-11 ("back-tracking oriented stream-of-solution functions" REG_EXTENDED)
		    #f
		    ("in the spec, and the back-tracking oriented stream-of-solution functions" #f)
		    #t)


    ;; These next three tests are about the worst case possible
    ;; for `regexec'.   
    ;; 
     
;;;    (posix-check-12 ("\\([-[:alnum:]][-[:alnum:]][-[:alnum:]]*\\)--.*--\\1" REG_NOSUB)
;;;		    #f
;;;		    ("abcd--abcx--mlkj--oiu--xcvxcv--234--anb--abcq--xyz--abcq--lmno--aqbc--sdlfj--abcq--xy8--yabc--dsfl--weoru--sdlfj" #f)
;;;		    #t)
;;;
    (posix-check-13 ("\\(\\([-[:alnum:]]*\\)\\([-[:alnum:]][-[:alnum:]][-[:alnum:]]*\\)\\)--.*--\\(\\3\\)" #f)
		    #f
		    ("abcq--xyz--cq--abcq--xyz--foo--aqbc--xyz--sdlfj--abcq--sdlfq" #(0 1 2 3 4))
		    #((0 . 53) (0 . 19) (0 . 15) (15 . 19) (49 . 53)))
;;;    (posix-check-13 ("\\(\\([-[:alnum:]]*\\)\\([-[:alnum:]][-[:alnum:]][-[:alnum:]]*\\)\\)--.*--\\(\\3\\)" #f)
;;;		    #f
;;;		    ("abcd--abcx--mlkj--oiu--xcvxcv--234--anb--abcq--xyz--cq--abcq--lmno--aqbc--sdlfj--abcq--xy8--yabc--dsfl--weoru--sdlfq" #(0 1 2 3 4))
;;;		    #((41 . 8) (0 . 0)))
;;;
;;;    (posix-check-14 ("\\([-[:alnum:]][-[:alnum:]][-[:alnum:]]*\\)--.*--\\1" #f)
;;;		    #f
;;;		    ("abcd--abcx--mlkj--oiu--xcvxcv--234--anb--abcq--xyz--abcq--lmno--aqbc--sdlfj--abcq--xy8--yabc--dsfl--weoru--sdlfj" #(0 1))
;;;		    #((41 . 81) (41 . 45)))
;;;




    ;; Rx Tests
    ;; 
    ;; Miscellaneous tests of some Rx-specific extensions 
    ;; and limits.
    ;; 
    ;; 
    (rx-0 ("[[:(abc\\|\\(def\\)):]]*\\1" #f)
	  #f
	  ("defabcdef" #f)
	  #f)

    (rx-1 ("\\(\\(a\\?\\)\\)*xa*" #f)
	  #f
	  ("axa" #(0 1 2))
	  #((0 . 3) (0 . 1) (0 . 1)))

    (rx-2 ("\\(abc\\|abcd\\)\\(d\\|\\)" #f)
	  #f
	  ("abcd" #(0 1 2))
	  #((0 . 4) (0 . 4) (4 . 4)))

    (rx-3 ("(abcd|){2,3}" REG_EXTENDED)
	  #f
	  ("abcd" #(0 1))
	  #((0 . 4) (0 . 4)))

    (rx-4 ("(abcd|){10,11}" REG_EXTENDED)
	  #f
	  ("abcd" #(0 1))
	  #((0 . 4) (0 . 4)))

    (rx-5 ("(abcd){256}" REG_EXTENDED)
	  REG_BADBR
	  #f
	  #f)

    (rx-6 ("(abcd){0,256}" REG_EXTENDED)
	  REG_BADBR
	  #f
	  #f)

    (rx-7 ("a{1,}" REG_EXTENDED)
	  #f
	  ("aaaaa" #(0))
	  #((0 . 5)))

    (rx-8 ("\\(abc$\\)" #f)
	  #f
	  ("abc" 0)
	  "abc")

    (rx-9 ("[[:(abc$):]]" #f)
	  #f
	  ("abc" 0)
	  "abc")

    (rx-10 ("[[:(abc$):]]" #f)
	   #f
	   ("abc$" #f)
	   #f)

    (rx-11 ("(abc|abcd)(d|)" REG_EXTENDED)
	   #f
	   ("abcd" (0 1 2))
	   ("abcd" "abcd" ""))

    (rx-12 ("(ab(c)d|abc)(d|)" REG_EXTENDED)
	   #f
	   ("abcd" (0 1 2 3))
	   ("abcd" "abcd" "c" ""))

    ))


