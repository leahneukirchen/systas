;;; tag: Tom Lord Tue Dec  4 14:59:40 2001 (systas-tests/unit-boolean.scm)
;;;
(display "**************** booleans tests ****************\n")


(load "tests/test.scm")

(define false #f)
(if (eq? false '())
    (display*-port (current-error-port)
		   ";;;;;;;;;;;;;;;; WARNING ;;;;;;;;;;;;;;;;\n"
		   ";;\n"
		   ";;		#f and () are the same\n"
		   ";;		which is not consistent\n"
		   ";;		with R5RS\n"
		   ";;\n"))

(pk 'not-tests)
(test (not false) #t)
(test (not #t) false)
(test (not '()) (eq? false '()))
(test (not 'a) false)

(pk 'boolean?-tests)
(test (boolean? false) #t)
(test (boolean? #t) #t)
(test (boolean? '()) (eq? false '()))
(test (boolean? 'a) false)

(pk '->bool-tests)
(test (->bool false) false)
(test (->bool '()) (not (eq? false '())))
(test (->bool #t) #t)
(test (->bool 'a) #t)
