;;; basic.scm - Essential definitions, always loaded by the
;;;		interpreter.  Don't try running Systas Scheme
;;;		without these definitions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998 Tom Lord
;;; Copyright (C) 1995, 1996 Free Software Foundation, Inc.
;;;
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apply, call-with-current-continuation, and catch
;;;
;;; These turn syntax, @apply and @call-with-current-continuation,
;;; into standard Scheme procedures.
;;;

;; documented in ../../libsystas/scheme.c
;; 
(define apply (lambda (fun . args) (@apply fun (nconc2last args))))

;; documented in ../../libsystas/continuations.c
;;
(define (call-with-current-continuation proc) (@call-with-current-continuation proc))

;; catch tag protected handler
;; 
;; A tail-recursive form of @catch (calls handlers tail-recursively).
;; 
;; documented in ../../libsystas/throw.c
;; 
(define (catch tag protected handler)
  (let ((answer	(@catch tag
			(lambda args (cons 'value (apply protected args)))
			(lambda exception (cons 'exception exception)))))
    (case (car answer)
      ((value)		(cdr answer))
      ((exception)	(apply handler (cdr answer))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple Debugging Tools
;;;


;; peek . any
;;
;; Accept any non-0 number of arguments, write them to the
;; current error port as a comment.  Return the last argument.
;;
;;	(+ 10 (troublesome-fn))
;;	=> (+ 10 (pk 'troublesome-fn-returned (troublesome-fn)))
;;
;; see ../../libsystas/debug.doc
;; 
(define (peek . stuff)
  (force-output)
  (with-output-to-port (current-error-port)
    (lambda ()
      (newline)
      (display ";;; ")
      (write stuff #f :cycles1)
      (newline)
      (force-output)
      (car (last-pair stuff)))))

;; pk . any
;;
;; Shorthand for peek
;;
(define pk peek)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; warnings
;;; 

;; warn . any
;;
;; Accept any non-0 number of arguments, write them to the
;; current error port as a warning message.  Return the last 
;; argument.
;;
(define (warn . stuff)
  (with-output-to-port (current-error-port)
    (lambda ()
      (newline)
      (display ";;; WARNING ")
      (write stuff #f :cycles1)
      (newline)
      (car (last-pair stuff)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; symmetric-wind
;;;

;; doc ../../libsystas/dynwind.c
;; 
(define (symmetric-wind protect thunk)
  (dynamic-wind protect thunk protect))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; apply-to-args
;;;
;;; apply-to-args is functionally redunant with apply and, worse,
;;; is less general than apply since it only takes two arguments.
;;;
;;; On the other hand, apply-to-args is a syntacticly convenient way to 
;;; perform binding in many circumstances when the "let" family of
;;; of forms don't cut it.  E.g.:
;;;
;;;	(apply-to-args (return-3d-mouse-coords)
;;;	  (lambda (x y z) 
;;;		...))
;;;

;;; apply-to-args args fn
;;;
;;; Apply `fn' to `args'.
;;;
(define (apply-to-args args fn) (apply fn args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; read
;;;
;;; This expands the type of the built-in read function so that 
;;; in addition to reading from ports, it can read from any string-like
;;; object.
;;;

;; builtin-read ?port?
;;
;; Read an object from a port.
;;
(define builtin-read read)

;; read ?object?
;;
;; Read an object from a port or a string.
;;
;; doc in ../../libsystas/read-print.c
;; 
(define (read . opts)
  (cond
   ((null? opts)      			(builtin-read))
   ((read-only-string? (car opts))	(with-input-from-string (car opts)
					  (lambda () 
					    (apply builtin-read (current-input-port) (cdr opts)))))
   (#t					(apply builtin-read opts))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; display*
;;;

;; documented in ../../libsystas/read-print.c
;;
(define (display* . args) (for-each display args))
(define (display*-port port . args)
  (if port
      (with-output-to-port port (lambda () (apply display* args)))
      (apply display* args)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Convenience Names
;;;

;; documented in ../../libsystas/numbers.c
;; 
(define < <?)
(define <= <=?)
(define = =?)
(define > >?)
(define >= >=?)


(define value noop)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash Tables
;;;

;; make-hash-table ?n?
;;
;; Make a hash table (a vector) with `n' or a default number of 
;; buckets.
;;
;; doc in ../../libsystas/hashtab.c
;; 
(define (make-hash-table . opt-k)
  (make-vector (if (null? opt-k) 63 (car opt-k)) '()))

;; hash-table-for-each proc table
;; 
;; For each `key' and `value' pair in `table', invoke:
;; 
;; 	(proc key value)
;; 
(define (hash-table-for-each proc table)
  (vector-for-each (lambda (chain)
		     (for-each (lambda (entry)
				 (proc (car entry) (cdr entry)))
			       chain))
		   table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Booleans
;;;

;; and=> value procedure
;;
;; If `value' is not #f, call `procedure', passing `value'
;; as the sole argument.
;;
;; doc in ../../libsystas/scheme.c
;; 
(define (and=> value thunk) (and value (thunk value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Integer Math
;;;
;;; doc for this section in ../../libsystas/numbers.c
;;; 

;; 1+ n
;; 
;; Return `n' + 1.
;;
(define (1+ n) (+ n 1))

;; 1- n
;; 
;; Return `n' - 1.
;;
(define (1- n) (+ n -1))

;; integer? x
;;
;; Test whether `x' is a number with the fractional and imaginary
;; parts equal to 0.
;;
(define (integer? x) (and (number? x) (= x (inexact->exact (real-part x)))))

;; ipow-by-squaring x k acc proc
;;
(define (ipow-by-squaring x k acc proc)
  (cond ((zero? k) acc)
	((= 1 k) (proc acc x))
	(else (ipow-by-squaring (proc x x)
				(quotient k 2)
				(if (even? k) acc (proc acc x))
				proc))))

(define (ipow x k)
  (ipow-by-squaring x k 1 *))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transcendental Functions
;;;
;;; Derived from "Transcen.scm", Complex trancendental functions for SCM.
;;; Copyright (C) 1992, 1993 Jerry D. Hedden.
;;; See the file `COPYING' for terms applying to this program.
;;;
;;; doc for this section in ../../libsystas/numbers.c
;;; 

;; exp z
;;
;; Return e raised to the power `z'.
;;
(define (exp z)
  (if (real? z) ($exp z)
      (make-polar ($exp (real-part z)) (imag-part z))))

;; expt z1 z2
;;
;; Return `z1' raised to the `z2' power.
;;
(define (expt z1 z2)
  (cond ((exact? z2)
	 (integer-expt z1 z2))
	((and (real? z2) (real? z1) (>= z1 0))
	 ($expt z1 z2))
	(else
	 (exp (* z2 (log z1))))))

;; log z
;;
;; Return the natural logarithm of `z'.
;;
(define (log z)
  (if (and (real? z) (>= z 0))
      ($log z)
      (make-rectangular ($log (magnitude z)) (angle z))))

;; sqrt z
;;
;; Return the square root of `z'.
;;
(define (sqrt z)
  (if (real? z)
      (if (negative? z) (make-rectangular 0 ($sqrt (- z)))
	  ($sqrt z))
      (make-polar ($sqrt (magnitude z)) (/ (angle z) 2))))

;; sinh z
;;
;; Return the hyperbolic sine of `z'.
;;
(define (sinh z)
  (if (real? z) ($sinh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sinh x) ($cos y))
			  (* ($cosh x) ($sin y))))))
;; cosh z
;;
;; Return the hyperbolic cosine of `z'.
;;
(define (cosh z)
  (if (real? z) ($cosh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cosh x) ($cos y))
			  (* ($sinh x) ($sin y))))))
;; tanh z
;;
;; Return the hyperbolic tangent of `z'.
;;
(define (tanh z)
  (if (real? z) ($tanh z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cosh x) ($cos y))))
	(make-rectangular (/ ($sinh x) w) (/ ($sin y) w)))))

;; asinh z
;;
;; Return the inverse hyperbolic sine of `z'.
;;
(define (asinh z)
  (if (real? z) ($asinh z)
      (log (+ z (sqrt (+ (* z z) 1))))))

;; acosh z
;;
;; Return the inverse hyperbolic cosine of `z'.
;;
(define (acosh z)
  (if (and (real? z) (>= z 1))
      ($acosh z)
      (log (+ z (sqrt (- (* z z) 1))))))

;; atanh z
;;
;; Return the inverse hyperbolic tangent of `z'.
;;
(define (atanh z)
  (if (and (real? z) (> z -1) (< z 1))
      ($atanh z)
      (/ (log (/ (+ 1 z) (- 1 z))) 2)))

;; sin z
;;
;; Return the sine of `z'.
;;
(define (sin z)
  (if (real? z) ($sin z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sin x) ($cosh y))
			  (* ($cos x) ($sinh y))))))
;; cos z
;;
;; Return the cosine of `z'.
;;
(define (cos z)
  (if (real? z) ($cos z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cos x) ($cosh y))
			  (- (* ($sin x) ($sinh y)))))))
;; tan z
;;
;; Return the sine of `z'.
;;
(define (tan z)
  (if (real? z) ($tan z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cos x) ($cosh y))))
	(make-rectangular (/ ($sin x) w) (/ ($sinh y) w)))))

;; asin z
;;
;; Return the inverse sine of `z'.
;;
(define (asin z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($asin z)
      (* -i (asinh (* +i z)))))

;; acos z
;;
;; Return the inverse cosine of `z'.
;;
(define (acos z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($acos z)
      (+ (/ (angle -1) 2) (* +i (asinh (* +i z))))))

;; atan z . y
;;
;; Return the inverse tangent of `z'.
;;
(define (atan z . y)
  (if (null? y)
      (if (real? z) ($atan z)
	  (/ (log (/ (- +i z) (+ +i z))) +2i))
      ($atan2 z (car y))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Vectors
;;;
;;; (vector-copy, vector-map and vector-for-each stolen from fps-1.0)
;;;

;; doc in ../../libsystas/vectors.c
;; 
(define (vector-copy v . opt-len)
  (let* ((v-len (vector-length v))
	 (len   (if (null? opt-len)
		    v-len
		    (if (> v-len (car opt-len))
			v-len 
			(car opt-len))))
	 (dup-v (make-vector len #f)))
    (let lp ((n 0))
      (if (>= n v-len)
	  dup-v
	  (begin (vector-set! dup-v n (vector-ref v n))
		 (lp (+ n 1)))))))

;; doc in ../../libsystas/vectors.c
;; 
(define (vector-map proc vect)
  (let* ((len (vector-length vect))
	 (new-vect (make-vector len)))
    (let loop ((n (- len 1)))
      (if (< n 0)
	  new-vect
	  (begin
	    (vector-set! new-vect n (proc (vector-ref vect n)))
	    (loop (- n 1)))))))

;; doc in ../../libsystas/vectors.c
;; 
(define (vector-for-each proc vec)
  (let ((len (vector-length vec)))
    (let loop ((n 0))
      (if (< n len)
	  (begin
	    (proc (vector-ref vec n))
	    (loop (1+ n)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic Port Code
;;; 
;;; Specificly, the parts of the low-level port code that are 
;;; written in Scheme rather than C.
;;;
;;; doc for this section in ../../libsystas/ports.c
;;;

;; call-with-input-string str proc
;;
;; Call `thunk' with a single argument: a read-only string port
;; that returns the characters of `str'.
;; 
(define (call-with-input-string str thunk)
  (let ((p (%make-string-port 'O_RDONLY str)))
    (if (not (port? p))
	(error "unable to create string port"))
    (let ((a (thunk p)))
      (close-port p)
      a)))


;; call-with-output-string str proc
;;
;; Call `thunk' with a single argument: a write-only string port.
;; Return the string of characters written to that port.
;; 
(define (call-with-output-string thunk)
  (let ((p (%make-string-port 'O_WRONLY)))
    (if (not (port? p))
	(error "unable to create string port"))
    (thunk p)
    (let ((s (string-port->string p)))
      (close-port p)
      s)))

;; open-input-file filename
;;
;; Open a file for reading and return a port.
;; Signal an error if the `filename' can not be opened.
;;
(define (open-input-file str)
  (open-file str 'O_RDONLY 0))

;; open-output-file filename
;;
;; Open a file for writing and return a port.
;; Signal an error if the `filename' can not be opened.
;;
(define (open-output-file str)
  (open-file str '(O_WRONLY O_CREAT O_EXCL) #o666))

;; open-io-file filename
;;
;; Open a file for reading and writing, and return a port.
;; Signal an error if the `filename' can not be opened.
;;
(define (open-io-file str) (open-file str 'O_RDWR 0))

;; close-input-port port
;;
;; Close a port created by open-input-file
;;
(define close-input-port close-port)

;; close-output-port port
;;
;; Close a port created by open-output-file
;;
(define close-output-port close-port)

;; close-io-port port
;;
;; Close a port created by open-io-file
;;
(define close-io-port close-port)

;; call-with-input-file filename procedure
;;
;; Open `filename' for reading and pass a port for
;; the open file to `procedure'.  Close the port
;; if `procedure' returns normally.  Return the value
;; from `procedure'.
;;
(define (call-with-input-file str proc)
  (let* ((file (open-input-file str))
	 (ans (proc file)))
    (close-input-port file)
    ans))

;; call-with-output-file filename procedure
;;
;; Open `filename' for writing and pass a port for
;; the open file to `procedure'.  Close the port
;; if `procedure' returns normally.  Return the value
;; from `procedure'.
;;
(define (call-with-output-file str proc)
  (let* ((file (open-output-file str))
	 (ans (proc file)))
    (close-output-port file)
    ans))

;; with-input-from-port port thunk
;;
;; Evaluate `thunk' in the dynamic context of `port'
;; as the current input port.
;;
(define (with-input-from-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-input-port port)))))
    (dynamic-wind swaports thunk swaports)))

;; with-output-to-port port thunk
;;
;; Evaluate `thunk' in the dynamic context of `port'
;; as the current output port.
;;
(define (with-output-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-output-port port)))))
    (dynamic-wind swaports thunk swaports)))

;; with-error-to-port port thunk
;;
;; Evaluate `thunk' in the dynamic context of `port'
;; as the current error port.
;;
(define (with-error-to-port port thunk)
  (let* ((swaports (lambda () (set! port (set-current-error-port port)))))
    (dynamic-wind swaports thunk swaports)))

;; with-input-from-file filename thunk
;;
;; Evaluate `thunk' in the dynamic context of a port
;; opened for `filename' as the current input port.
;;
(define (with-input-from-file file thunk)
  (let* ((nport (open-input-file file))
	 (ans (with-input-from-port nport thunk)))
    (close-port nport)
    ans))

;; with-output-from-file filename thunk
;;
;; Evaluate `thunk' in the dynamic context of a port
;; opened for `filename' as the current output port.
;;
(define (with-output-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-output-to-port nport thunk)))
    (close-port nport)
    ans))

;; with-error-from-file filename thunk
;;
;; Evaluate `thunk' in the dynamic context of a port
;; opened for `filename' as the current error port.
;;
(define (with-error-to-file file thunk)
  (let* ((nport (open-output-file file))
	 (ans (with-error-to-port nport thunk)))
    (close-port nport)
    ans))

;; with-input-from-string string thunk
;;
;; Evaluate `thunk' in the dynamic context of a port
;; that reads from `string' as the current input port.
;;
(define (with-input-from-string string thunk)
  (call-with-input-string string
   (lambda (p) (with-input-from-port p thunk))))

;; with-output-to-string string thunk
;;
;; Evaluate `thunk' in the dynamic context of a port
;; that writes to a newly created string as the 
;; current output port.  When `thunk' returns,
;; return the string.
;;
(define (with-output-to-string thunk)
  (call-with-output-string
   (lambda (p) (with-output-to-port p thunk))))

;; with-error-to-string string thunk
;;
;; Evaluate `thunk' in the dynamic context of a port
;; that writes to a newly created string as the 
;; current error port.  When `thunk' returns,
;; return the string.
;;
(define (with-error-to-string thunk)
  (call-with-output-string
   (lambda (p) (with-error-to-port p thunk))))

;; ->string object
;;
;; Return a string containing `object' as printed by `write'.
;;
(define (->string object . kws)
  (with-output-to-string (lambda () (apply write object #f kws))))

;; display->string object
;;
;; Return a string containing `object' as printed by `display'.
;;
(define (display->string object)
  (with-output-to-string (lambda () (display object))))

;; the-eof-object
;;
;; The unique object returned when reading from a port
;; that has reached the end-of-file.
;;
(define the-eof-object (call-with-input-string "" (lambda (p) (read-char p))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Symbols
;;;
;;; doc for this section in ../../libsystas/symbols.c
;;;

;; symbol-append . args
;;
;; Equivalent to `(string->symbol (apply string-append args))'
;;
(define (symbol-append . args)
  (string->symbol (apply string-append args)))

;; list->symbol . args
;;
;; Equivalent to `(string->symbol (apply list->string args))'
;;
(define (list->symbol . args)
  (string->symbol (apply list->string args)))

;; symbol . args
;;
;; Equivalent to `(string->symbol (apply string args))'
;;
(define (symbol . args)
  (string->symbol (apply string args)))

;; hash-table-symbol-append hash-table . args
;;
;; Like `(apply symbol-append args)' except that the
;; symbol is constructed in the specified `hash-table'.
;;
(define (hash-table-symbol-append ob . args)
  (string->hash-table-symbol ob (apply string-append args)))

;; hash-table-gensym hash-table . ?name-parts?
;;
;; Construct a new symbol in the specified `hash-table'.
;;
;; If any `name-parts' are specified, concatenate them 
;; using `string-append' to form the root of the name of
;; the new symbol.
;;
(define hash-table-gensym
  (let ((n -1))
    (lambda (hash-table . opt)
      (if (null? opt)
	  (set! opt '(%%gensym)))
      (let loop ((proposed-name (apply string-append opt)))
	(if (string->hash-table-symbol hash-table proposed-name #t)
	    (loop (apply string-append
			 (append opt (begin (set! n (1+ n)) (list (number->string n))))))
	    (string->hash-table-symbol hash-table proposed-name))))))

;; gensym . ?name-parts?
;;
;; Construct a new, ordinary symbol.
;;
;; If any `name-parts' are specified, concatenate them 
;; using `string-append' to form the root of the name of
;; the new symbol.
;;
(define (gensym . args) (apply hash-table-gensym #t args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;
;;; section docs in ../../libsystas/strings.c
;;; 

(define (string-copy str)
  (string str))

(define (has-any-suffix? str)
  (let ((suffix-index (string-rindex str #\.))
	(slash-index (string-rindex str #\/)))
    (and suffix-index
	 (or (not slash-index) (> suffix-index slash-index)))))

(define (string-for-each proc str)
  (let ((len (string-length str)))
    (let loop ((n 0))
      (if (< n len)
	  (begin (proc (string-ref str n))
		 (loop (1+ n)))))))

(define (string-downcase s)
  (string-downcase! (string s)))

(define (string-upcase s)
  (string-upcase! (string s)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists
;;;
;;; section docs in ../../libsystas/list.c
;;;

;; make-list n init
;;
;; Return a new  `n' element list, all elements `eq?' to `init'.
;;
(define (make-list n :optional init)
  (let loop ((answer '())
	     (n n))
    (if (<= n 0)
	answer
	(loop (cons init answer) (- n 1)))))

;; range x :optional y
;;
;; Return a new `x' element list of the integers `0 .. x-1'.
;; With two arguments, return a list `x..y-1'.
;;
(define (range x :optional y)
  (if (and y (< y x))
      '()
      (let loop ((x (if y x 0))
		 (y (or y x))
		 (answer ()))
	(if (= x y)
	    answer
	    (loop x (1- y) (cons (1- y) answer))))))

;; reverse-range x :optional y
;;
;; Return a new `x' element list of the integers `x-1 .. 0'.
;; With two arguments, return a list `y-1..x'.
;;
(define (reverse-range x :optional y)
  (let loop ((x (if y x 0))
	     (y (or y x))
	     (answer ()))
    (if (= x y)
	answer
	(loop (1+ x) y (cons (1+ x) answer)))))


;; filter pred list -> list 
;; 
;; Return all the elements of list that satisfy predicate pred. The
;; list is not disordered -- elements that appear in the result list
;; occur in the same order as they occur in the argument list. The
;; returned list may share a common tail with the argument list. The
;; dynamic order in which the various applications of pred are made is
;; not specified.
;; 
;; 	(filter even? '(0 7 8 8 43 -4)) => (0 8 8 -4)
;; 
(define (filter pred l)
  (let ((answer (cons '() '())))
    (let loop ((s answer)
	       (l l))
      (cond
       ((null? l) 	(cdr answer))
       ((pred (car l))	(set-cdr! s (cons (car l) ()))
			(loop (cdr s) (cdr l)))
       (else		(loop s (cdr l)))))))


;; list-elt-indexq l k
;;
;; Return the index (0 based) of the first element in list `l'
;; that matches (`eq?') the value `k'.
;;
(define (list-elt-indexq l k)
  (let loop ((n 0)
	     (l l))
    (and (not (null? l))
	 (if (eq? (car l) k)
	     n
	     (loop (+ n 1) (cdr l))))))

;; list-elt-indexv l k
;;
;; Return the index (0 based) of the first element in list `l'
;; that matches (`eqv?') the value `k'.
;;
(define (list-elt-indexv l k)
  (let loop ((n 0)
	     (l l))
    (and (not (null? l))
	 (if (eqv? (car l) k)
	     n
	     (loop (+ n 1) (cdr l))))))

;; list-elt-index l k
;;
;; Return the index (0 based) of the first element in list `l'
;; that matches (`equal?') the value `k'.
;;
(define (list-elt-index l k)
  (let loop ((n 0)
	     (l l))
    (and (not (null? l))
	 (if (equal? (car l) k)
	     n
	     (loop (+ n 1) (cdr l))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Based on the interface to 
;;;
;;; 	"queue.scm"  Queues/Stacks for Scheme 
;;;  	Written by Andrew Wilcox (awilcox@astro.psu.edu) on April 1, 1992.
;;;
;;;
;;;
;;; A list is just a bunch of cons pairs that satisfy some
;;; constraints, right?  Association lists are the same.  Hash tables
;;; are just vectors and association lists.  You can print them, read
;;; them, write them as constants, pun them off as other data
;;; structures etc. This is good.  This is lisp.  These structures are
;;; fast and compact and easy to manipulate arbitrarily because of
;;; their simple, regular structure and non-disjointedness
;;; (associations being lists and so forth).
;;;
;;; So I figured, queues should be the same -- just a "subtype" of cons-pair 
;;; structures in general.  And it _almost_ works out.
;;;
;;; A queue is a cons pair:
;;;		( <the-q> . <last-pair> )
;;;
;;; <the-q> is a list of things in the q.   New elements go at the end of 
;;; that list.
;;;
;;; <last-pair> is #f if the q is empty, and otherwise is the last pair 
;;; of <the-q>.
;;;
;;; q's print nicely, but alas, they do not read well because the 
;;; eq?-ness of <last-pair> and (last-pair <the-q>) is lost by read.   
;;; The procedure
;;; 
;;;		(sync-q! q)
;;;
;;; recomputes and resets the <last-pair> component of a queue.
;;;

;; sync-q! obj
;;
;; Convert `obj' to a queue.  
;;
;; `obj' should have been created by (something equivalent to):
;;
;;	(define s (with-output-to-string (lambda () (write a-queue))))
;;	(define obj (with-input-from-string s read))
;;
;; Until `sync-q!' is called, the `obj' returned from `read'
;; is not a proper queue.
;;
(define (sync-q! obj)
  (set-cdr! obj (and (not (null? (car obj)))
		     (last-pair (car obj)))))

;; make-q
;;
;;  Return a new, empty queue.
;;
(define (make-q) (cons '() #f))


;; q? obj
;;
;;   Return true if obj is a queue.
;;
;;   An object is a queue if it is equal? to '(() . #f) or
;;   if it is a pair `P' with:
;;
;;		(list? (car P))
;; and 		(eq? (cdr P) (last-pair P)).
;;
(define (q? obj) (and (pair? obj)
		      (or (and (null? (car obj))
			       (not (cdr obj)))
			  (and
			   (list? (car obj))
			   (eq? (cdr obj) (last-pair (car obj)))))))

;; q-empty? obj
;;  
;; Return #t if `obj' is an empty queue, #f otherwise.
;;
(define (q-empty? obj)
  (and (pair? obj) (null? (car obj)) (not (cdr obj))))


;; q-empty-check q
;;
;; Throw an exception of type 'q-empty if Q is an empty queue.
;;
(define (q-empty-check q) (if (q-empty? q) (throw 'q-empty q)))

;; q-front q
;;
;; Return the first element of the queue `q'.
;;
(define (q-front q) (q-empty-check q) (caar q))

;; q-rear q
;;
;; Return the last element of the queue `q'.
(define (q-rear q) (q-empty-check q) (cadr q))

;; q-remove! q obj
;;
;; Remove all occurences of `obj' from the queue `q'.
;;
(define (q-remove! q obj)
  (set-car! q (delq! obj (car q)))
  (set-cdr! q (and (car q) (last-pair (car q)))))

;; q-contains? q obj
;;
;; Return #t if the queue `q' contains `obj', #f otherwise.
;;
(define (q-contains? q obj)
  (->bool (memq obj (car q))))

;; q-push! q obj
;;
;; Add `obj' to the front of queue `q'.
;;
(define (q-push! q d)
  (let ((h (cons d (car q))))
    (set-car! q h)
    (if (null? (cdr q))
	(set-cdr! q h))
    d))

;; enq! q obj
;;
;; Add `obj' to the rear of queue `q'.
;;
(define (enq! q d)
  (let ((h (cons d '())))
    (if (not (null? (cdr q)))
       	(set-cdr! (cdr q) h)
	(set-car! q h))
    (set-cdr! q h)
    d))

;; q-pop! q
;;
;; Remove the front element of queue `q' and return it.
;;
(define (q-pop! q)
  (q-empty-check q)
  (let ((it (caar q))
	(next (cdar q)))
    (if (null? next)
	(set-cdr! q #f))
    (set-car! q next)
    it))

;; deq! q
;;
;; Remove the front element of queue `q' and return it.
;;
(define deq! q-pop!)

;; clear-q! q
;;
;; Clear the queue `q' of all members.
;;
(define (clear-q! q)
  (set-car! q '())
  (set-cdr! q #f)
  q)

;; q-length q
;;
;; Return the number of enqueued elements.
;;
(define (q-length q) (length (car q)))

;; q-members q
;;
;; Return a list of the enqueued elements.
;;
(define (q-members q) (append (car q) ()))

;; q-rotate q
;;
;; Make the front element of the queue `q' the last element,
;; moving all other elements forward one position.
;;
(define (q-rotate q)
  (q-empty-check q)
  (enq! q (deq! q)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regular expressions
;;; 
;;; doc in ../../libsystas/rgx.c
;;; 

(define (regexec regexp str :optional match-pick eflags return error-return)
  (let ((result (low-level-regexec regexp str match-pick eflags)))
    (cond
     ((eq? 'EINTR result)	(regexec regexp str match-pick eflags return error-return))
     ((not result)		(and error-return (error-return)))
     ((not return)		result)
     ((pair? result)		(apply return result))
     (#t			(return result)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; and-map, or-map, and map-in-order
;;;
;;; (and-map fn lst) is like (and (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;;
;;; (or-map fn lst) is like (or (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;;
;;; (map-in-order fn lst) is like (map fn lst) but definately in order of lst.
;;;
;;; docs in ../../libsystas/list.c

;; and-map f l
;;
;; Apply `f' to successive elements of `l' until the end of list or `f' 
;; returns #f.
;;
;; If returning early, return #f.  Otherwise, return the last value returned
;; by `f'.  If `f' has never been called because `l' is empty, return #t.
;; 
(define (and-map f lst)
  (let loop ((result #t)
	     (l lst))
    (and result
	 (or (and (null? l)
		  result)
	     (loop (f (car l)) (cdr l))))))

;; or-map f l
;;
;; Apply `f' to successive elements of `l' until end of list or until `f'
;; returns a value other than #f.
;;
;; If returning early, return the return value of the last call to `f'.
;; Otherwise, return #f.
;;
(define (or-map f lst)
  (let loop ((result #f)
	     (l lst))
    (or result
	(and (not (null? l))
	     (loop (f (car l)) (cdr l))))))

;; map-in-order
;;
;; Like map, but guaranteed to process the list in order.
;;
(define (map-in-order . args) (apply map args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files and Filenames
;;;
;;; A small collection of file handling functions, suitable
;;; for bootstrapping the interpreter.
;;;
;;; doc in ../../libsystas/filesys.c

;; file-exists? filename
;;
;; Return #t if the named file exits.
;;
;; This function returns #f if the file exists as a symbolic
;; link, but the link points to a non-existent file.
;;
(define (file-exists? f)
  (let ((s (%stat f)))
    (not (errno? s))))


;; file-is-directory? filename
;;
;; Return #t if `filename' is the name of an existing 
;; directory.
;;
(define (file-is-directory? f)
  (let ((s (%stat f)))
    (and (not (errno? s))
	 (eq? 'S_IFDIR (kw-arg-ref s :type)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interrupts
;;; 
;;; doc in ../../libsystas/async.c

(define (without-interrupts thunk)
  (dynamic-wind mask-interrupts
		thunk
		unmask-interrupts))

(define (with-interrupts thunk)
  (if (not (interrupts-masked?))
      (thunk)
      ;; We can't write:
      ;;
      ;; 	(dynamic-wind unmask-interrupts thunk mask-interrupts)
      ;;
      ;; because `unmask-interrupts' can be exited by a exception 
      ;; thrown from an interrupt handler.  That would cause
      ;; `with-interrupts' to exit with interrupts unmasked.
      ;; 
      (dynamic-wind (lambda () #f)
		    (lambda ()
		      (dynamic-wind unmask-interrupts
				    thunk
				    (lambda () #f)))
		    mask-interrupts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Error Handling
;;;
;;; With the exception of "fatal", errors are signaled 
;;; by throwing exceptions.
;;;
;;; An error condition is named by a symbol.  If there is
;;; an exception handler for that symbol, the error is caught.
;;;
;;; If there is no exception handler, the symbol's
;;; "throw-handler-default" property is checked.  If that
;;; property is set, that value is called as a procedure and 
;;; given a chance to return from the throw.
;;;
;;; As a last resort, the exception is converted to an 
;;; exception of type "error" (which has a default handler)
;;; and re-thrown.
;;;
;;; docs in ../../libsystas/error.c
;;;

;; fatal . args
;; 
;; Display on the current error port an error message containing 
;; the arguments and exit the process with status equal to 1.
;;
(define (fatal . args)
  (display ";;; FATAL ERROR\n" (current-error-port))
  (for-each (lambda (x)
	      (display ";;; " (current-error-port))
	      (write x (current-error-port) :cycles1)
	      (newline (current-error-port)))
	    args)
  (%exit 1))

;; error . args
;;
;; Signal a generic error, passing `args' to the error handler.
;; (Does not return.)
;;
(define (error . args)
  (apply throw 'error args))

;; bad-throw is the hook that is called upon a throw to a an unhandled
;; key.  If the key has a default handler it is applied to the throw
;; (see default-exception-handler).
;;
(define (bad-throw key . args)
  (let ((default (default-exception-handler key)))
    (or (and default (apply default key args))
	(throw 'error 'unhandled-exception key args))))

(define default-exception-handler-table (make-hash-table 509))

;; default-exception-handler obj
;;
;; Return the default exception handler for `obj'.
;;
(define (default-exception-handler s) (hashq-ref default-exception-handler-table s))

;; set-default-exception-handler obj value
;;
;; Set the default exception handler for `obj'.
;;
(define (set-default-exception-handler s v)
  (hashq-set! default-exception-handler-table s v)
  v)

;; establish a default handler for exceptions of type `error'
;;
(begin
  (define (default-error-handler tag . args)
    (force-output (current-output-port))
    (with-output-to-port (current-error-port)
      (lambda ()
	(display #\nl)
	(display ";;; ")
	(display "ERROR: ")
	(for-each (lambda (x) (write x #f :cycles1) (display " ")) args)
	(newline)
	(force-output)
	(abort))))
  (set-default-exception-handler 'error default-error-handler))

;; The default handler for errors thrown by built-in
;; functions.  The action is to print a message and abort.
;;
(begin
  (define (handle-parameter-error ignored desc proc . args)
    (let* ((msg (if (symbol? proc)
		    (string-append "in " proc ", " desc)
		    desc))
	   (rest (if (and proc (not (symbol? proc)))
		     (cons proc args)
		     args)))
      (force-output (current-output-port))
      (with-output-to-port (current-error-port)
	(lambda ()
	  (display #\nl)
	  (display ";;; ")
	  (display "ERROR: ")
	  (display msg)
	  (and (pair? args)
	       (begin
		 (display " ")
		 (write args #f :cycles1)))
	  (newline)
	  (force-output)
	  (abort)))))
  (set-default-exception-handler 'parameter-error handle-parameter-error))

;; The default handler for errors thrown by eval
;;
;; The action is to print a message and the errant expression
;; and abort.
;;
(begin
  (define (handle-eval-error ignored desc proc arg expression)
    (let* ((msg (if (symbol? proc)
		    (string-append "in " proc ", " desc)
		    desc)))
      (force-output (current-output-port))
      (with-output-to-port (current-error-port)
	(lambda ()
	  (display #\nl)
	  (display ";;; ")
	  (display "ERROR: ")
	  (display msg)
	  (newline)
	  (display ";;;        procedure: ")
	  (write proc #f :cycles1)
	  (newline)
	  (display ";;;        arg: ")
	  (write arg #f :cycles1)
	  (newline)
	  (display ";;;        expression: ")
	  (write expression #f :cycles1)
	  (newline)
	  (force-output)
	  (abort)))))
  (set-default-exception-handler 'eval-error handle-eval-error))

(begin
  (define (handle-signal ignored signal-name)
    (let* ((msg (string-append "unhandled signal: " (string-upcase! (string signal-name)))))
      (force-output (current-output-port))
      (with-output-to-port (current-error-port)
	(lambda ()
	  (display #\nl)
	  (display ";;; ")
	  (display "ERROR: ")
	  (display msg)
	  (newline)
	  (force-output)
	  (abort)))))
  (set-default-exception-handler 'signal handle-signal))

;; By default, signals are handled by a function named
;; after the particular signal.
;;
(define (sigchld) ())
(define (sighup) (%exit 1))
(define (siggc) (run-gc-hooks))
(define (sigalrm) ())
(define (sigio) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GC hooks
;;; 

(define gc-hooks ())
(define (run-gc-hooks)
  (for-each (lambda (hook)
	      (catch #t
		hook
		(lambda data
		  (catch #t
		    (lambda ()
		      (display ";;; error in GC hook\n;;; " (current-error-port))
		      (write data (current-error-port))
		      (newline (current-error-port)))
		    (lambda ign #f)))))
	    gc-hooks))
(define (add-gc-hook proc)
  (if (not (memq proc gc-hooks))
      (set! gc-hooks (cons proc gc-hooks))))

(define (remove-gc-hook proc)
  (set! gc-hooks (delq proc gc-hooks)))

(define (brief-gc-message) (display "\n;;; gc\n" (current-error-port)))
(define (verbose-gc-message)
  (display "\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;;; gc\n" (current-error-port))
  (for-each (lambda (stat)
	      (display*-port (current-error-port) ";;; " (car stat) " " (cdr stat) "\n"))
	    (gc-stats))
  (display ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n" (current-error-port)))
	    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; System Calls
;;;
;;; Any procedure is considered a "system call" if it indicates
;;; errors by returning an "errno" object.  These functions 
;;; are for doing two things:
;;;
;;; 	1. Checking the return value from a system call
;;;	   and converting errors to exceptions.
;;;
;;;	2. Checking the return value from a system call
;;;	   and retrying interrupted system calls.
;;;
;;; docs in ../../libsystas/system.c


;; %e fn . args
;; %signal-errors fn . args
;;
;; Apply `fn' to `args'.  If `fn' returns an errno object,
;; throw a exception whose type is the name of the answer.
;;
(define (%e fn . args) (apply %signal-errors fn args))
(define (%signal-errors fn . args)
  (let ((answer (apply fn args)))
    (if (errno? answer)
	(throw answer (cons fn args))
	answer)))

;; %i fn . args
;; %retry-interrupted-calls fn . args
;;
;; Apply `fn' to `args'.  If `fn' returns an errno object,
;; and the indicated error is EINTR, retry the call to "fn".
;;
(define (%i fn . args) (apply %retry-interrupted-calls fn args))
(define (%retry-interrupted-calls fn . args)
  (let ((answer (apply fn args)))
    (if (and (errno? answer)
	     (eq? 'EINTR (errno->integer answer)))
	(apply %retry-interrupted-calls fn args)
	answer)))

;; %% fn . args
;; %high-level-system-call fn . args
;;
;; Apply `fn' to `args'.  If `fn' returns an errno object,
;; and the indicated error is EINTR, retry the call to "fn".
;;
;; If `fn' returns an errno object for some other error,
;; throw an exception whose type is the name of the error
;;
(define (%% fn . args) (apply %high-level-system-call fn args))

(define (%high-level-system-call fn . args)
  (let ((answer (apply %i fn args)))
    (if (errno? answer)
	(throw answer (cons fn args))
	answer)))


(define (errno-exceptions-return thunk)
  (catch #t
    thunk
    (lambda (tag . args) (if (errno? tag) tag (apply throw tag args)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load Paths
;;;
;;; doc in ../../libsystas/load.c
;;; 

;; parse-path path-string
;;
;; Convert a string of the form:
;;
;;		"elt1:elt2:elt3..."
;;
;; to a list of the form:
;;
;;		("elt1" "elt2" "elt3" ...)
;;
(define (parse-path path-string)
  (cond ((string? path-string)
	 (let loop ((curdir "") (env path-string) (path '()))
	   (cond ((= (string-length env) 0) 
		  (if (> (string-length curdir) 0)
		      (append path (list curdir))
		      path))
		 ((char=? (string-ref env 0) #\:)
		  (loop "" 
			(substring env 1 (string-length env))
			(append path (list curdir))))
		 (#t
		  (loop (string-append curdir (substring env 0 1))
			(substring env 1 (string-length env))
			path)))))
	(#t '())))


;; load-path
;;
;; A list of directories to search for scheme source code.
;;
;; The list consists of elements from the environment variable
;; "SCHEME_LOAD_PATH" followed by the built-in load path.
;;
(define load-path
  (let ((env 	(getenv "SCHEME_LOAD_PATH")))
    (or (and env (parse-path env))
	(append (if development-version?
		    (list (in-vicinity build-directory "../scheme-library")
			  source-root
			  (in-vicinity source-root "scheme-library"))
		    ())
		(list (in-vicinity install-root "share/scheme"))
		(list "./" "/")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; try-load
;;;
;;; doc in ../../libsystas/load.c
;;; 


;; try-load filename
;;
;; Try to load the named file from the list of directories
;; bound to `load-path'.   If `load-path' is not defined,
;; try to load the named file from the current directory or,
;; if `filename' is absolute, from the root.
;;
;; Return #f if the file could not be found.
;;
(define (try-load name)
  (if (and (not (filename-absolute? name))
	   (eval '(defined? load-path)))
      (try-load-with-path name (eval 'load-path))
      (low-level-try-load name #f read-sharp)))



;; try-load-with-path filename path
;;
;; Try to load the named file from the indicated list of
;; directories.   
;;
;; Return #f if the file could not be found.
;;
(define (try-load-with-path filename path)
  (or-map (lambda (d)
	    (let ((f (in-vicinity d filename)))
	      (and (not (file-is-directory? f))
		   (low-level-try-load f #f read-sharp))))
	  path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load
;;;
;;; The default way to load source files.
;;;
;;; doc in ../../libsystas/load.c

;; load-verbosely
;;
;; If not #f, then print informative messages on the
;; current output port while loading files.
;;
(define load-verbosely #t)

;; load filename
;;
;; Try to load a file by searching the load path.
;; Both `filename' and `filename' with `(scheme-file-suffix)'
;; appended are tried.
;;
;; The load path is the list of directories bound to `load-path'
;; and defaults to the current directory (for relative file names)
;; or the root directory (for absolute file names).
;;
;; If the file can not be found, an error is signalled.
;;
;; `load-verbosely' controls the printed output
;; of this procedure.
;;
(define (load name)
  (if (eval '(defined? load-path))
      (load-with-path name (eval 'load-path))
      (load-with-path name '())))

;; load-with-path filename path
;;
;; Like `load', but searches the specified path instead of the
;; default path.
;;
(define load-indent -2)
(define scheme-suffixes '(".scm" ".ss"))
(define (load-with-path name path)
  (let ((indent load-indent))
    (dynamic-wind
     (lambda () (set! load-indent (modulo (+ indent 2) 16)))

     (lambda ()
       (if load-verbosely
	   (with-output-to-port (current-error-port)
	     (lambda ()
	       (display ";;; ")
	       (display (make-string load-indent #\space))
	       (display "loading ")
	       (display name)
	       (display "...\n")
	       (force-output))))
       (if (not (or-map (lambda (d)
			  (let ((f (in-vicinity d name)))
			    (if (or (and (not (file-is-directory? f))
					 (low-level-try-load f #f read-sharp))
				    (and (not (has-any-suffix? f))
					 (or-map (lambda (suffix)
						   (low-level-try-load (string-append f suffix)
								       #f
								       read-sharp))
						 scheme-suffixes)))
				(begin
				  (if load-verbosely
				      (with-output-to-port (current-error-port)
					(lambda () 
					  (display ";;; ")
					  (display (make-string load-indent #\ ))
					  (display "...loaded ")
					  (display f)
					  (display ".\n")
					  (force-output))))
				  #t)
				#f)))
			path))
	   (begin
	     (if load-verbosely
		 (with-output-to-port (current-error-port)
		   (lambda ()
		     (display ";;; ")
		     (display (make-string load-indent #\ ))
		     (display "...COULD NOT LOAD ")
		     (display name)
		     (display " from ")
		     (write path #f :cycles1)
		     (newline)
		     (force-output))))
	     (throw 'could-not-load name path))))
     (lambda () (set! load-indent indent))))
  #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reader Extensions
;;;

;; read-sharp
;;
;; The procedure `read' accepts an optional parameter which
;; is used to read objects whose print representation begins
;; with "#".  The repl (in "user.scm") and loader (in this file)
;; pass the value of this variable for that parameter.
;;
(define read-sharp (lambda args (apply default-read-sharp args)))

;;; default-read-sharp first-character case-insensitive? port
;;;
;;; Reader code for various "#c" forms.
;;; 
;;; Assume that "#" and `first-character' have already
;;; been read.  Read the remaining part of the next object
;;; on the input stream `port'.
;;;
(define (default-read-sharp port case-insensitive? c)
  (case (pk 'read-sharp c)
    ;; Treat #! as a comment marker, in keeping with a unix convention.
    ((#\!) 		(let skip ((c (peek-char port)))
			  (if (or (eof-object? c) (eq? #\newline c))
			      (read port case-insensitive? default-read-sharp)
			      (skip (read-char port)))))
    (else		(error "unknown # object" c))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros
;;;
;;; A simple, easy-to-understand but non-hygenic form of macros.
;;;
;;; These macros are expanded once-per-use, on-demand, at run-time.
;;; The expansions are memoized.
;;;

;; macro-table
;;
;; A doubly weak hash table whose keys are macros,
;; and whose values are procedures that transform 
;; source forms in the manner of those macros.
;;
(define macro-table (make-doubly-weak-hash-table 523))

;; known-macro? obj
;;
;; Return #t if `obj' is a macro with a known transformer.
;; A "macro transformer" is a procedure that accepts a source
;; form (the macro, applied to arguments) and returns an
;; the a source form which is the result of expanding the
;; macro.
;;
(define (known-macro? m)  (->bool (hashq-ref macro-table m)))
(define (assert-macro! macro transformer)
  (hashq-set! macro-table macro transformer)
  macro)
(define (macro-transformer macro) (hashq-ref macro-table macro))

;; procedure->expandable-memoizing-macro
;;
;; Given a macro transformer (see "known-macro?"), return a memoizing
;; macro based on that transformer.  The resulting macro can be expanded
;; using macroexpand-1 and macroexpand.
;;
(define procedure->expandable-memoizing-macro
  (lambda (xform)
    (let ((a (procedure->memoizing-macro xform)))
      (assert-macro! a xform)
      a)))


;; make-memoizing-macro transformer
;;
;; Given a macro transformer (see "known-macro?"), return a memoizing
;; macro based on that transformer.
;;
(define make-memoizing-macro
  (lambda (f)
    (procedure->expandable-memoizing-macro (lambda (exp env) (copy-tree (apply f (cdr exp)))))))


;; define-memoizing-macro (name . params) . body
;;
;; Define a memoizing macro (one which is expanded once, on-demand, 
;; at run-time) which does its work by passing the source form to 
;; be expanded to the procedure:
;;
;;		(lambda (name . params) . body)
;;
(define define-memoizing-macro
  (let ((defmacro-transformer
	  (lambda (name-parms . body)
	    (let ((name (car name-parms))
		  (parms (cdr name-parms)))
	      `(define ,name (make-memoizing-macro (lambda ,parms ,@body)))))))
    (make-memoizing-macro defmacro-transformer)))


;; macroexpand-1 exp
;;
;; If the source form `exp' is a macro invocation (within the 
;; currently active top-level), expand that macro and return
;; the new source form.  Otherwise, return `exp'.
;;
(define (macroexpand-1 e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (eval `(defined? ,a)) (eval a))))
		(if (known-macro? val)
		    ((macro-transformer val) e ())
		    e)))
   (#t e)))

;; macroexpand exp
;;
;; If the source form `exp' is a macro invocation (within the 
;; currently active top-level), expand that macro and recursively
;; apply `macroexpand' to the result.  Otherwise, return `exp'.
;;
(define (macroexpand e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (eval `(defined? ,a)) (eval a))))
		(if (known-macro? val)
		    (macroexpand ((macro-transformer val) e ()))
		    e)))
   (#t e)))

;; macroexpand-recursive exp
;;
;; If the source form `exp' is a macro invocation (within the 
;; currently active top-level), expand that macro and recursively
;; apply `macroexpand-recursive' to the result.  
;;
;; Otherwise, apply `macroexpand-recursive' to all elements of `exp'
;; and return the result.
;;
(define (macroexpand-recursive e)
  (cond
   ((pair? e) (let* ((a (car e))
		     (val (and (symbol? a) (eval `(defined? ,a)) (eval a))))
		(if (known-macro? val)
		    (macroexpand-recursive ((macro-transformer val) e ()))
		    (map macroexpand-recursive e))))
   (#t e)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax-rules
;;; 

;;macro: syntax-rules literals clauses
;;
;; Return a hygenic macro.
;; 
;; `literals' is a list of identifiers, `clauses' a list of patterns
;; and expansion templates.  Each clause is of the form:
;;
;;	(pattern template)
;; 
;; A template is an atomic, self-evaluating value, a symbol which is a
;; literal (an element of the list `literals'), the symbol `...', a
;; symbol which not a literal or `...' or a tree structure (cons
;; pairs) with atoms, literals, or symbols as leaves.
;;
;; A pattern is a tree structure whose leaf nodes are all symbols.
;; It must be of the form:
;;
;;	(macro-name . rest-of-pattern)
;;
;; `macro-name' must be a symbol and must be the same for all of the patterns.
;;
;; An application of the resulting macro is expanded by comparing the
;; applying expression to the patterns and replacing the expression with
;; the expansion of the template of the first matching pattern.
;; 
;; Here is a simple example, defining `iff' ("if and only if"):
;;
;;	(define iff
;;	  (syntax-rules ()
;;	    ((iff test result)		(if test result))
;;	    ((iff test result else)	(if test (or result else)))))
;;
;; With that definition, these expressions are equivalent:
;;
;;	(iff <test> <result>) is equivalent to:
;;	(if <test> <result>)
;;
;;	(iff <test> <result> <else>) is equivalent to:
;;	(if <test> (or <result> <else>))
;;
;;
;; * Pattern Matching and Template Expansion
;;
;; 
;;
(define syntax-rules
  (procedure->expandable-memoizing-macro
   (lambda (exp macro-environment)
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; exp			-- a form: 	(syntax-rules <literals> . <clauses>)
     ;; macro-environment	-- the environment binding variables free in <clauses>
     ;;
     ;; Syntax checking:
     ;;
     (if (not (and (list? exp)
		   (< 2 (length exp))
		   (list? (cadr exp))
		   (and-map symbol? (cadr exp))))
	 (throw 'bad-syntax-rules-syntax exp))
     (let ((first-clause (caddr exp)))
       (if (not (and (list? first-clause)
		     (pair? first-clause)
		     (pair? (car first-clause))
		     (symbol? (caar first-clause))))
	   (throw 'bad-syntax-rules-syntax exp)

	   (let ((macro-name (caar first-clause)))
	     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	     ;; exp			-- a form: 	(syntax-rules <literals> . <clauses>)
	     ;; macro-environment	-- the environment binding variables free in <clauses>
	     ;; macro-name 		-- the name of the macro as it appears in <clauses>
	     ;;
	     ;; The result of expanding `syntax-rules' is:
	     ;;
	     ;;		(procedure->memoizing-macro 
	     ;;		  (lambda (expression expression-env)
	     ;;		    <transformer>)
	     ;;
	     ;; <transformer> is generated by `transform-clauses' and rewrites
	     ;; input forms according to the `syntax-rules' specification.
	     ;;
	     `(,procedure->expandable-memoizing-macro
	       (,lambda (expression expression-env)
			(,let ((expression (,cdr expression)))
			      ,(transform-clauses macro-name (cadr exp) (cddr exp) macro-environment 'expression 'expression-env))))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; false* - a unique value used as `false' by the syntax-rules compiler
;;
(define (false*) false*)

;; and*=> exp then else
;;
;; Evaluate `exp' and save its value (`V').
;; If the value is not `false*', then return `(then V)'
;; otherwise, return  `(else)'.
;;
(define-memoizing-macro (and*=> exp then else)
  `((,lambda (exp-value then else)
	     (,if (,not (,eq? ,false* exp-value))
		  (then exp-value)
		  (else)))
    ,exp
    ,then
    ,else))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compiling `define-syntax' Clauses
;;

;; transform-clauses macro-name literals clauses pattern-environment exp-var
;;
;; Compile `clauses' -- define syntax clauses.
;;
;; `macro-name' 		-- the name of the macro as used in the clauses.
;; `literals'			-- the list of literal symbols passed to `define-syntax'
;; `pattern-environment'	-- the environment that binds free variables in expansions
;; `exp-var'			-- the name of the variable holding an expression
;;				   being expanded by these clauses.
;; `exp-env-var'		-- the name of the variable holding an environment of a caller 
;;				   of the macro defined by these clauses.
;;
(define (transform-clauses macro-name literals clauses pattern-environment exp-var exp-env-var)
  (cond
   ((null? clauses)		`(throw 'bad-syntax (,cons ',macro-name ,exp-var)))

   (#t				(if (not (and (pair? clauses)
					      (list? (car clauses))
					      (= 2 (length (car clauses)))
					      (pair? (caar clauses))
					      (eq? macro-name (caaar clauses))))
				    (throw 'bad-syntax-rules-syntax clauses))
				(transform-clause literals
						  (cdaar clauses)
						  (cadar clauses)
						  (transform-clauses macro-name literals (cdr clauses) pattern-environment exp-var exp-env-var)
						  pattern-environment
						  exp-var
						  exp-env-var))))


;; transform-clause literals pattern template otherwise pattern-environment exp-var exp-env-var
;;
;; Compile one define syntax clause consisting of `pattern' and `template'.
;;
;; `literals'			-- the list of literal symbols passed to `define-syntax'
;; `pattern'			-- the pattern of this clause
;; `template'			-- the template expansion of this clause
;; `otherwise'			-- the expression to evaluate if this clause doesn't match
;; `pattern-environment'	-- the environment that binds free variables in expansions
;; `exp-var'			-- the name of the variable holding an expression
;;				   being expanded by these clauses.
;; `exp-env-var'		-- the name of the variable holding an environment of a caller 
;;				   of the macro defined by these clauses.
;;
;;
(define (transform-clause literals pattern template otherwise pattern-environment exp-var exp-env-var)
  `(,and*=> ,(transform-pattern literals pattern pattern-environment exp-var exp-env-var)
	    (,lambda (bindings)
		     (,expand ',literals bindings ',template ',pattern-environment))
	    (lambda () ,otherwise)))


;; transform-pattern literals pattern pattern-environment exp-var exp-env-var
;;
;; Compile a pattern from a `syntax-rules' clause.
;; 
;; The compiled pattern is code that compares the value of `exp-var'
;; to `pattern'.  It return a list (possibly empty) of pattern
;; variable bindings if the match succeeds, and `false*' otherwise.
;; 
;; `literals'			-- the list of literal symbols passed to `define-syntax'
;; `pattern'			-- the pattern of this clause
;; `pattern-environment'	-- the environment that binds free variables in expansions
;; `exp-var'			-- the name of the variable holding an expression
;;				   being expanded by these clauses.
;; `exp-env-var'		-- the name of the variable holding an environment of a caller 
;;				   of the macro defined by these clauses.
;;
(define (transform-pattern literals pattern pattern-environment exp-var exp-env-var)

  (define pattern-variable
    (let ((variables-encountered ()))
      (lambda (v)
	(if (memq v variables-encountered)
	    (throw 'pattern-variable-used-more-than-once v pattern))
	(set! variables-encountered (cons v variables-encountered))
	v)))

  (define (transform-pattern literals pattern pattern-environment exp-var exp-env-var return)
    (cond
     ((null? pattern)			(return `(,if (,null? ,exp-var)
						      ()
						      ,false*)
						()))

     ((memq pattern literals)		(return `(,if (,eq? (,denoted-variable ,exp-var ,exp-env-var)
							    #',(denoted-variable pattern pattern-environment))
						      ()
						      ,false*)
						()))

     ((eq? '... pattern)		(throw 'bad-pattern-specification pattern))

     ((symbol? pattern)			(let ((pv (pattern-variable pattern)))
					  (return `(,list (,list ',pv ,exp-var))
						  (list pv))))
     
     ((and (pair? pattern)
	   (pair? (cdr pattern))
	   (null? (cddr pattern))
	   (eq? '... (cadr pattern)))	(transform-pattern literals (car pattern) pattern-environment exp-var exp-env-var
							   (lambda (sub-pattern-match-exp sub-pattern-variables)
							     (return
							      `(,let ((subs (,map (,lambda (,exp-var) ,sub-pattern-match-exp) ,exp-var)))
								     (,if (,memq ,false* subs)
									  ,false*
									  (,list 			; a list of pattern variable bindings
									   (,list '...			; containing one binding; for '...
										  ;; The binding for '... is:
										  ;;
										  ;; ( (<sub-pattern-variables> <one-pass-bindings> ...) ...)
										  ;;
										  ;; <sub-pattern-variables> is a list of variables bound
										  ;; by a repeated sub-pattern.   Each <one-pass-bindings>
										  ;; is a set of bindings for those variables.
										  ;;
										  (,list (,list ',sub-pattern-variables subs))))))
							      ()))))
     

     ((pair? pattern)			(transform-pattern
					 literals (car pattern) pattern-environment exp-var exp-env-var
					 (lambda (first-match-exp first-pattern-variables)
					   (transform-pattern
					    literals (cdr pattern) pattern-environment exp-var exp-env-var
					    (lambda (rest-match-exp rest-pattern-variables)
					      (return `(,if (,not (,pair? ,exp-var))
							    ,false*
							    (,let ((first (,let ((,exp-var (,car ,exp-var)))
										,first-match-exp))
								   (rest (,let ((,exp-var (,cdr ,exp-var)))
									       ,rest-match-exp)))
								  (,if (,or (,eq? ,false* first)
									    (,eq? ,false* rest))
								       ,false*
								       (,let ((first-regular (,filter (lambda (x) (not (eq? '... (car x)))) first))
									      (rest-regular (,filter (lambda (x) (not (eq? '... (car x)))) rest))
									      (first-... (,assq '... first))
									      (rest-... (,assq '... rest)))
									     ;; The cumulative pattern variable environment
									     ;; contains all of the regular bindings from both
									     ;; the CAR and CDR of the pattern...
									     ;;
									     (,append first-regular
										      rest-regular

										      ;; ...followed by a binding for '...
										      ;;
										      (,if (,not (,or first-... rest-...))
											  '()
											  (,let ((first-sub-env (,if first-... (,cadr first-...) '()))
												 (rest-sub-env (,if rest-... (,cadr rest-...) '())))
												(,list (,list '... (,append first-sub-env rest-sub-env))))))))))
						      (append first-pattern-variables rest-pattern-variables)))))))


     (#t				(throw 'bad-pattern-specification pattern))))

  (transform-pattern literals pattern pattern-environment exp-var exp-env-var first-value))


(define (template-free-variables literals template)
  (cond
   ((eq? '... template)		'())
   ((symbol? template)		(if (memq template literals)
				    '()
				    (list template)))
   ((pair? template)		(append (template-free-variables literals (car template))
					(template-free-variables literals (cdr template))))
   (#t				'())))


(define (expand literals bindings template macro-environment)
  (cond
   ((null? template)				template)

   ((assq-ref bindings template) =>		(lambda (x) (copy-tree (car x))))

   ((symbol? template)				(if (memq template literals)
						    template
						    (denoted-variable template macro-environment)))
   ((and (pair? template)
	 (pair? (cdr template))
	 (null? (cddr template))
	 (eq? '... (cadr template)))		(let* ((template-variables (template-free-variables literals template))
						       (other-bindings (filter (lambda (x) (not (eq? '... (car x)))) bindings))
						       (repeated-bindings (car (assq-ref bindings '...)))
						       (mapped-repeated-bindings (map cadr
										      (filter (lambda (repeated-subpattern-bindings)
												(or-map (lambda (x)
													  (memq x template-variables))
													(car repeated-subpattern-bindings)))
											      repeated-bindings))))
						  (and (pair? mapped-repeated-bindings)
						       (apply map
							      (lambda one-layer-...-bindings
								(expand literals
									(append other-bindings (apply append one-layer-...-bindings))
									(car template)
									macro-environment))
							      mapped-repeated-bindings))))
   ((pair? template)				(cons (expand literals bindings (car template) macro-environment)
						      (expand literals bindings (cdr template) macro-environment)))

   (#t						template)))


(define define-syntax define)
(define let-syntax let)
(define letrec-syntax letrec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Once
;;;

(define once
  (procedure->memoizing-macro
   (lambda (once-exp once-env)
     (if (not (= 2 (length once-exp)))
	 (throw 'bad-once-syntax once-exp))
     (let* ((value-exp (cadr once-exp))
	    (value (eval-environment! value-exp once-env)))
       `(quote ,value)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; While
;;;
;;; with implicit local bindings for `continue' and `break'.
;;;
;;; This implementation uses the throw-tag `break'.
;;;
(define-memoizing-macro (while cond . body)
  `(letrec ((break (lambda ()
		     (letrec ((continue 	(lambda (break)
						  (if (let ((continue (lambda () (break)))
							    (break (lambda (v) (break v))))
							(and ,cond
							     (or (begin ,@body)
								 #t)))
						      (continue break)
						      #f))))
		       (catch #f
			 (lambda (break)
			   (continue (lambda args (apply throw break args))))
			 (lambda (tag . val)
			   (if val
			       (car val)
			       (break))))))))
     (break)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Records
;;;
;;; doc in ../../libsystas/records.doc
;;;

(begin
  ;; make-record-type type-name field-names
  ;;
  ;; Returns a "record-type descriptor", a value which represents a new
  ;; data type disjoint from all others.  
  ;;
  ;; `type-name' must be a symbol (a name for the new type).
  ;;
  ;; `field-names' must be a list of symbols naming the "fields" of a
  ;; record of the new type.  
  ;;
  (define make-record-type #f)

  ;; record-type? obj
  ;;
  ;; Returns #t if `obj' is a record type descriptor and #f otherwise.
  ;;
  (define record-type? #f)

  ;; record-type-name rtd
  ;;
  ;; Returns the symbolic name associated with the record type represented 
  ;; by `rtd'.
  ;;
  (define record-type-name #f)

  ;; record-type-fields rtd
  ;;
  ;; Returns a list of the symbols naming the fields in members of the
  ;; record type represented by `rtd'.  
  ;;
  (define record-type-fields #f)

  ;; record-type-printer rtd
  ;;
  ;; Return the print function for the record type represented by `rtd'.  
  ;; Return #f if no print function is defined.
  ;;
  (define record-type-printer #f)

  ;; record-constructor rtd [field-names]
  ;;
  ;; Returns a procedure for constructing new members of the record type
  ;; represented by `rtd'.  The returned procedure accepts exactly as
  ;; many arguments as there are symbols in the list `field-names'.
  ;; These values are used, in order, as the initial values of
  ;; those fields in a new record, which is returned by the constructor
  ;; procedure.  The values of any fields not named in that list are
  ;; unspecified.  The `field-names' argument defaults to the list of
  ;; field names in the call to `make-record-type' that created the
  ;; type represented by `rtd'.
  ;;
  (define record-constructor #f)

  ;; record? obj
  ;;
  ;; Returns #t if `obj' is a record of any type and #f otherwise.
  ;;
  (define record? #f)

  ;; record-predicate rtd
  ;;
  ;; Returns a procedure for testing membership in the record type represented
  ;; by `rtd'.  The returned procedure accepts exactly one argument and
  ;; returns #t if the argument is a member of the indicated record type;
  ;; it returns a #f otherwise.
  ;;
  (define record-predicate #f)

  ;; record-type-descriptor record
  ;;
  ;; Returns a record-type descriptor which represents the type of the
  ;; given record.
  ;;
  (define record-type-descriptor #f)

  ;; record-accessor rtd field-name
  ;;
  ;; Returns a procedure for reading the value of a particular field of
  ;; a member of the type represented by `rtd'.  The returned procedure
  ;; accepts exactly one argument which must be a record of the
  ;; appropriate type.  It returns the current value of the field named
  ;; by that symbol.
  ;;
  (define record-accessor #f)

  ;; record-modifier rtd field-name
  ;;
  ;; Returns a procedure for writing the value of a particular field of
  ;; a member of the type represented by `rtd'.  The returned procedure
  ;; accepts exactly two arguments: first, a record of the appropriate
  ;; type, and second, an arbitrary value.   It sets the field
  ;; named by the symbol to the given value.  The returned value of the 
  ;; modifier procedure is unspecified.  
  ;;
  (define record-modifier #f)

  (let* (;; Construct the structure type of record type descriptors:
	 ;;
	 (the-record-secret (string->hash-table-symbol #f "the-record-secret"))
	 (rtd-structure-type (cons #f the-record-secret))

	 ;; Construct the record type descriptor of record type descriptors.
	 ;;
	 (record-type-fields-list '(name fields printer))
	 (rtd-record-type-fields (vector 'record-type-descriptor
					 record-type-fields-list
					 #f))
	 (rtd-record-type (let ((answer (make-structure rtd-structure-type rtd-record-type-fields)))
			    (set-car! rtd-structure-type answer)
			    answer))

	 ;; For every record type, there is a corresponding structure type.
	 ;;
	 (record-structure-type-memo (let ((table (make-doubly-weak-hash-table 61)))
				       (hashq-set! table rtd-record-type rtd-structure-type)
				       table))
	 (record-type-structure-type (lambda (rtd)
				       (or (hashq-ref record-structure-type-memo rtd)
					   (let ((answer (cons rtd the-record-secret)))
					     (hashq-set! record-structure-type-memo rtd answer)
					     answer))))

	 (-make-record-type (lambda (name fields . opt-printer)
			      (let* ((printer (and (pair? opt-printer) (car opt-printer)))
				     (rtd (make-structure rtd-structure-type
							  (vector name fields printer))))
				(set-structure-print-function (record-type-structure-type rtd)
							      printer)
				rtd)))

	 (-record? (lambda (obj)
		     (and (structure? obj)
			  (->bool (structure-data the-record-secret obj)))))
	 (-record-type? (lambda (obj)
			  (and (-record? obj)
			       (eq? rtd-record-type (structure-public-type obj)))))
	 (record-type-fields-index (list-elt-indexq record-type-fields-list 'fields))
	 (-record-type-fields (lambda (obj)
				(if (-record-type? obj)
				    (vector-ref (structure-data the-record-secret obj)
						record-type-fields-index)
				    (error 'not-a-record-type obj))))
	 (-record-constructor (lambda (rtd . opt)
				(let ((n-fields-constructor (length (if (pair? opt) (car opt) (-record-type-fields rtd))))
				      (n-fields (length (-record-type-fields rtd)))
				      (structure-type (record-type-structure-type rtd)))
				  (lambda fields
				    (if (not (= n-fields-constructor (length fields)))
					(apply throw 'WNA (record-type-name rtd) fields)
					(make-structure structure-type (make-vector n-fields fields #t)))))))
	 (-record-predicate (lambda (rtd)
			      (lambda (obj)
				(and (-record? obj)
				     (eq? rtd (structure-public-type obj))))))
	 (-record-type-descriptor (lambda (obj)
				    (if (-record? obj)
					(structure-public-type obj)
					(error 'not-a-record obj))))
	 (-record-accessor (lambda (rtd field-name)
			     (let* ((pos (list-elt-indexq (-record-type-fields rtd) field-name)))
			       (if (not pos)
				   (error 'no-such-field field-name))
			       (lambda (obj)
				 (if (and (-record? obj)
					  (eq? rtd (structure-public-type obj)))
				     (vector-ref (structure-data the-record-secret obj) pos)
				     (error 'wrong-record-type obj))))))
	 (-record-modifier (lambda (rtd field-name)
			     (let* ((pos (list-elt-indexq (-record-type-fields rtd) field-name)))
			       (if (not pos)
				   (error 'no-such-field field-name))
			       (lambda (obj value)
				 (if (and (-record? obj)
					  (eq? rtd (structure-public-type obj)))
				     (vector-set! (structure-data the-record-secret obj) pos value)
				     (error 'wrong-record-type obj))))))
	 (-record-type-name (-record-accessor rtd-record-type 'name))
	 (-record-type-printer (-record-accessor rtd-record-type 'printer)))

    (set! make-record-type -make-record-type)
    (set! record-type? -record-type?)
    (set! record-type-name -record-type-name)
    (set! record-type-fields -record-type-fields)
    (set! record-type-printer -record-type-printer)
    (set! record-constructor -record-constructor)
    (set! record? -record?)
    (set! record-predicate -record-predicate)
    (set! record-type-descriptor -record-type-descriptor)
    (set! record-accessor -record-accessor)
    (set! record-modifier -record-modifier)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multiple Values
;;; 
;;; doc in ../../libsystas/values.doc
;;; 

(define (%print-values values port writing?)
  (display "#<values" port)
  (for-each (lambda (v) (display " ") (write v port))  (values-list values))
  (display ">" port))

(define values-type (make-record-type 'multiple-values
				      '(list-of-values)
				      %print-values))
				      
(define make-values (record-constructor values-type))
(define values-list (record-accessor values-type 'list-of-values))
(define values?     (record-predicate values-type))

(define (values . args)
  (cond
   ((null? args)	(make-values '()))
   ((null? (cdr args))	(car args))
   (#t			(make-values args))))


(define (call-with-values producer consumer)
  (let ((v (producer)))
    (cond
     ((values? v)	(apply consumer (values-list v)))
     (#t		(consumer v)))))


