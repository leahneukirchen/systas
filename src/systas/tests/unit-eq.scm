;;; tag: Tom Lord Tue Dec  4 14:59:39 2001 (systas-tests/unit-eq.scm)
;;;
(display "**************** equality tests ****************\n")

(load "tests/test.scm")

(pk 'eq?-tests)
(test (eq? 'a 'a) #t)
(test (eq? 1 1) #t)
(test (eq? 1.0 1.0) #f)
(test (eq? "foo" "foo") #f)
(test (eq? '(a b c) '(a b c)) #f)
(test (eq? '(a b . c) '(a b . c)) #f)
(test (let ((x '(a b c))) (eq? x x)) #t)

(pk 'eqv?-tests)
(test (eqv? 'a 'a) #t)
(test (eqv? 1 1) #t)
(test (eqv? 1.0 1.0) #t)
(test (eqv? "foo" "foo") #f)
(test (eqv? '(a b c) '(a b c)) #f)
(test (eqv? '(a b . c) '(a b . c)) #f)
(test (let ((x '(a b c))) (eqv? x x)) #t)


(pk 'equal?-tests)
(test (equal? 'a 'a) #t)
(test (equal? 1 1) #t)
(test (equal? 1.0 1.0) #t)
(test (equal? "foo" "foo") #t)
(test (equal? '(a b c) '(a b c)) #t)
(test (equal? '(a b . c) '(a b . c)) #t)
(test (let ((x '(a b c))) (equal? x x)) #t)
