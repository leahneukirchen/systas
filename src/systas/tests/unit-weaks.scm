;;; tag: Tom Lord Tue Dec  4 14:59:42 2001 (systas-tests/unit-weaks.scm)
;;;
(display "**************** weak vector/hash table tests ****************\n")

(load "tests/test.scm")

(pk 'weak-vector?-tests/make-*-weak-*-tests)

(test (weak-vector? #(a b c)) #f)
(test (weak-vector? (make-weak-vector 10)) #t)
(test (weak-vector? (make-weak-key-hash-table 10)) #f)
(test (weak-vector? (make-weak-value-hash-table 10)) #f)
(test (weak-vector? (make-doubly-weak-hash-table 10)) #f)

(pk 'weak-key-hash-table?-tests/make-*-weak-*-tests)
(test (weak-key-hash-table? #(a b c)) #f)
(test (weak-key-hash-table? (make-weak-vector 10)) #f)
(test (weak-key-hash-table? (make-weak-key-hash-table 10)) #t)
(test (weak-key-hash-table? (make-weak-value-hash-table 10)) #f)
(test (weak-key-hash-table? (make-doubly-weak-hash-table 10)) #f)


(pk 'weak-value-hash-table?-tests/make-*-weak-*-tests)
(test (weak-value-hash-table? #(a b c)) #f)
(test (weak-value-hash-table? (make-weak-vector 10)) #f)
(test (weak-value-hash-table? (make-weak-key-hash-table 10)) #f)
(test (weak-value-hash-table? (make-weak-value-hash-table 10)) #t)
(test (weak-value-hash-table? (make-doubly-weak-hash-table 10)) #f)


(pk 'doubly-weak-hash-table?-tests/make-*-weak-*-tests)
(test (doubly-weak-hash-table? #(a b c)) #f)
(test (doubly-weak-hash-table? (make-weak-vector 10)) #f)
(test (doubly-weak-hash-table? (make-weak-key-hash-table 10)) #f)
(test (doubly-weak-hash-table? (make-weak-value-hash-table 10)) #f)
(test (doubly-weak-hash-table? (make-doubly-weak-hash-table 10)) #t)

; (pk 'weak-vector-gc-tests)
; (define (x) '(x))
; (define (y) '(y))
; (define (z) '(z))
; (define (a) (list 'a))
; (define (b) (list 'b))
; (define (c) (list 'c))
; 
; (define w (weak-vector (x) (y) (z) (a) (b) (c)))
; (gc)
; (test (vector->list w) '((x) (y) (z) #f #f #f))
; 
; (pk 'weak-key-hash-table-gc-tests)
; (define w (make-weak-key-hash-table 13))
; (hash-set! w (x) 'x-val)
; (hash-set! w (y) 'y-val)
; (hash-set! w (z) 'z-val)
; (hash-set! w (a) 'a-val)
; (hash-set! w (b) 'b-val)
; (hash-set! w (c) 'c-val)
; (gc)
; (test (hash-ref w (x)) 'x-val)
; (test (hash-ref w (y)) 'y-val)
; (test (hash-ref w (z)) 'z-val)
; (test (hash-ref w (a)) #f)
; (test (hash-ref w (b)) #f)
; (test (hash-ref w (c)) #f)
; 
; (define w (make-weak-key-hash-table 1))
; (hash-set! w (x) 'x-val)
; (hash-set! w (y) 'y-val)
; (hash-set! w (z) 'z-val)
; (hash-set! w (a) 'a-val)
; (hash-set! w (b) 'b-val)
; (hash-set! w (c) 'c-val)
; (gc)
; (test (hash-ref w (x)) 'x-val)
; (test (hash-ref w (y)) 'y-val)
; (test (hash-ref w (z)) 'z-val)
; (test (hash-ref w (a)) #f)
; (test (hash-ref w (b)) #f)
; (test (hash-ref w (c)) #f)
; 
; 
; (pk 'weak-value-hash-table-gc-tests)
; (define w (make-weak-value-hash-table 13))
; (hash-set! w 'x-key (x))
; (hash-set! w 'y-key (y))
; (hash-set! w 'z-key (z))
; (hash-set! w 'a-key (a))
; (hash-set! w 'b-key (b))
; (hash-set! w 'c-key (c))
; (gc)
; (test (hash-ref w 'x-key) (x))
; (test (hash-ref w 'y-key) (y))
; (test (hash-ref w 'z-key) (z))
; (test (hash-ref w 'a-key) #f)
; (test (hash-ref w 'b-key) #f)
; (test (hash-ref w 'c-key) #f)
; 
; (define w (make-weak-value-hash-table 1))
; (hash-set! w 'x-key (x))
; (hash-set! w 'y-key (y))
; (hash-set! w 'z-key (z))
; (hash-set! w 'a-key (a))
; (hash-set! w 'b-key (b))
; (hash-set! w 'c-key (c))
; (gc)
; (test (hash-ref w 'x-key) (x))
; (test (hash-ref w 'y-key) (y))
; (test (hash-ref w 'z-key) (z))
; (test (hash-ref w 'a-key) #f)
; (test (hash-ref w 'b-key) #f)
; (test (hash-ref w 'c-key) #f)
; 
; 
; (pk 'doubly-weak-hash-table-gc-tests)
; 
; (define w (make-doubly-weak-hash-table 1))
; (hash-set! w (x) (x))
; (hash-set! w (y) (y))
; (hash-set! w (z) (z))
; (hash-set! w (a) (a))
; (hash-set! w (b) (b))
; (hash-set! w (c) (c))
; (gc)
; (test (hash-ref w (x)) (x))
; (test (hash-ref w (y)) (y))
; (test (hash-ref w (z)) (z))
; (test (hash-ref w (a)) #f)
; (test (hash-ref w (b)) #f)
; (test (hash-ref w (c)) #f)


(pk 'list->weak-vector-tests/equal?-weak-vector-tests)
(test (equal? #(a b c) (list->weak-vector '(a b c))) #f)
(define w (make-weak-vector 3))
(vector-set! w 0 'a)
(vector-set! w 1 'b)
(vector-set! w 2 'c)
(test (equal? w (list->weak-vector '(a b c))) #t)
