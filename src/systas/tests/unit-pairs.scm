;;; tag: Tom Lord Tue Dec  4 14:59:37 2001 (systas-tests/unit-pairs.scm)
;;;
(display "**************** cons pair tests ****************\n")

(load "tests/test.scm")

(pk 'cons-tests)
(test (cons 'a 'b) '(a . b))
(test (cons 'a ()) '(a))
(test (cons 'a '(b c)) '(a b c))


(pk 'cons*-tests)
(test (cons* 'a 'b 'c 'd 'e) '(a b c d . e))
(test (cons* 'a 'b 'c 'd 'e ()) '(a b c d e))


(pk 'pair?-tests)
(test (pair? '(a . b)) #t)
(test (pair? '(a b)) #t)
(test (pair? ()) #f)
(test (pair? #(a b c)) #f)
(test (pair? 'a) #f)


(pk 'set-car!-tests)
(define x (cons 'a 'b))
(test (set-car! x 'z) '(z . b))
(test (eq? x (set-car! x 'y)) #t)


(pk 'set-cdr!-tests)
(define x (cons 'a 'b))
(test (set-cdr! x 'z) '(a . z))
(test (eq? x (set-cdr! x 'y)) #t)


(pk 'CxR-tests)
(define car-answer 'car-answer)
(define cdr-answer 'cdr-answer)
(define test-val 	(cons car-answer cdr-answer))

(define test-val-a 	(cons test-val #(error)))
(define test-val-d	(cons #(error) test-val))

(define test-val-aa 	(cons test-val-a #(error)))
(define test-val-ad 	(cons #(error) test-val-a))
(define test-val-da 	(cons test-val-d #(error)))
(define test-val-dd 	(cons #(error) test-val-d))

(define test-val-aaa	(cons test-val-aa #(error)))
(define test-val-ada	(cons test-val-ad #(error)))
(define test-val-daa	(cons test-val-da #(error)))
(define test-val-dda	(cons test-val-dd #(error)))
(define test-val-aad	(cons #(error) test-val-aa))
(define test-val-add	(cons #(error) test-val-ad))
(define test-val-dad	(cons #(error) test-val-da))
(define test-val-ddd	(cons #(error) test-val-dd))

(test (car test-val) 'car-answer)
(test (cdr test-val) 'cdr-answer)

(test (caar test-val-a) 'car-answer)
(test (cdar test-val-a) 'cdr-answer)
(test (cadr test-val-d) 'car-answer)
(test (cddr test-val-d) 'cdr-answer)

(test (caaar test-val-aa) 'car-answer)
(test (cdaar test-val-aa) 'cdr-answer)
(test (cadar test-val-da) 'car-answer)
(test (cddar test-val-da) 'cdr-answer)
(test (caadr test-val-ad) 'car-answer)
(test (cdadr test-val-ad) 'cdr-answer)
(test (caddr test-val-dd) 'car-answer)
(test (cdddr test-val-dd) 'cdr-answer)

(test (caaaar test-val-aaa) 'car-answer)
(test (cdaaar test-val-aaa) 'cdr-answer)
(test (cadaar test-val-daa) 'car-answer)
(test (cddaar test-val-daa) 'cdr-answer)
(test (caadar test-val-ada) 'car-answer)
(test (cdadar test-val-ada) 'cdr-answer)
(test (caddar test-val-dda) 'car-answer)
(test (cdddar test-val-dda) 'cdr-answer)

(test (caaadr test-val-aad) 'car-answer)
(test (cdaadr test-val-aad) 'cdr-answer)
(test (cadadr test-val-dad) 'car-answer)
(test (cddadr test-val-dad) 'cdr-answer)
(test (caaddr test-val-add) 'car-answer)
(test (cdaddr test-val-add) 'cdr-answer)
(test (cadddr test-val-ddd) 'car-answer)
(test (cddddr test-val-ddd) 'cdr-answer)

