;;; tag: Tom Lord Tue Dec  4 14:59:41 2001 (systas-tests/unit-alist.scm)
;;;
(display "**************** association list tests ****************\n")

(load "tests/test.scm")

(pk 'acons-tests)
(test (acons 'a 'b ()) '((a . b)))
(test (acons 'a 'b '((a . b))) '((a . b) (a . b)))

(pk 'assq-tests)
(test (assq 'a '((c . d) (a . b))) '(a . b))
(test (assq 'x '((c . d) (a . b))) #f)
(test (assq '1.5 '((c . d) (1.5 . b))) #f)
(test (assq "x" '((c . d) ("x" . b))) #f)
(test (assq 'q '((c . d) (a . b) #(x y z))) #f)

(pk 'assv-tests)
(test (assv 'a '((c . d) (a . b))) '(a . b))
(test (assv 'x '((c . d) (a . b))) #f)
(test (assv '1.5 '((c . d) (1.5 . b))) '(1.5 . b))
(test (assv "x" '((c . d) ("x" . b))) #f)
(test (assv 'q '((c . d) (a . b) #(x y z))) #f)

(pk 'assoc-tests)
(test (assoc 'a '((c . d) (a . b))) '(a . b))
(test (assoc 'x '((c . d) (a . b))) #f)
(test (assoc '1.5 '((c . d) (1.5 . b))) '(1.5 . b))
(test (assoc "x" '((c . d) ("x" . b))) '("x" . b))
(test (assoc "X" '((c . d) ("x" . b))) #f)
(test (assoc 'q '((c . d) (a . b) #(x y z))) #f)

(define (case-cmp? a b)
  (string=? (string-downcase! (string a)) (string-downcase! (string b))))

(test (assoc "x" '((c . d) ("X" . b)) case-cmp?) '("X" . b))
(test (assoc "x" '((c . d) ("Y" . b)) case-cmp?) #f)

(pk 'assq-ref-tests)
(test (assq-ref '((c . d) (a . b)) 'a) 'b)
(test (assq-ref '((c . d) (a . b)) 'x) #f)
(test (assq-ref '((c . d) (1.5 . b)) '1.5) #f)
(test (assq-ref '((c . d) ("x" . b)) "x") #f)

(pk 'assv-ref-tests)
(test (assv-ref '((c . d) (a . b)) 'a) 'b)
(test (assv-ref '((c . d) (a . b)) 'x) #f)
(test (assv-ref '((c . d) (1.5 . b)) '1.5) 'b)
(test (assv-ref '((c . d) ("x" . b)) "x") #f)

(pk 'assoc-ref-tests)
(test (assoc-ref '((c . d) (a . b)) 'a) 'b)
(test (assoc-ref '((c . d) (a . b)) 'x) #f)
(test (assoc-ref '((c . d) (1.5 . b)) '1.5) 'b)
(test (assoc-ref '((c . d) ("x" . b)) "x") 'b)
(test (assoc-ref '((c . d) ("x" . b)) "X") #f)

(test (assoc-ref '((c . d) ("x" . b)) "X" case-cmp?) 'b)
(test (assoc-ref '((c . d) ("Y" . b)) "x" case-cmp?) #f)

(pk 'assq-set!-tests)
(test (assq-set! '((c . d) (a . b)) 'a 'z) '((c . d) (a . z)))
(test (assq-set! '((c . d) (a . b)) 'x 'z) '((x . z) (c . d) (a . b)))
(test (assq-set! '((c . d) (1.5 . b)) '1.5 'z) '((1.5 . z) (c . d) (1.5 . b)))
(test (assq-set! '((c . d) ("x" . b)) "x" 'z) '(("x" . z) (c . d) ("x" . b)))

(pk 'assv-set!-tests)
(test (assv-set! '((c . d) (a . b)) 'a 'z) '((c . d) (a . z)))
(test (assv-set! '((c . d) (a . b)) 'x 'z) '((x . z) (c . d) (a . b)))
(test (assv-set! '((c . d) (1.5 . b)) '1.5 'z) '((c . d) (1.5 . z)))
(test (assv-set! '((c . d) ("x" . b)) "x" 'z) '(("x" . z) (c . d) ("x" . b)))

(pk 'assoc-set!-tests)
(test (assoc-set! '((c . d) (a . b)) 'a 'z) '((c . d) (a . z)))
(test (assoc-set! '((c . d) (a . b)) 'x 'z) '((x . z) (c . d) (a . b)))
(test (assoc-set! '((c . d) (1.5 . b)) '1.5 'z) '((c . d) (1.5 . z)))
(test (assoc-set! '((c . d) ("x" . b)) "x" 'z) '((c . d) ("x" . z)))
(test (assoc-set! '((c . d) ("X" . b)) "x" 'z) '(("x" . z) (c . d) ("X" . b)))

(test (assoc-set! '((c . d) ("x" . b)) "x" 'z case-cmp?) '((c . d) ("x" . z)))
(test (assoc-set! '((c . d) ("X" . b)) "x" 'z case-cmp?) '((c . d) ("X" . z)))


(pk 'assq-remove!-tests)
(test (assq-remove! '((c . d) (a . z)) 'a) '((c . d)))
(test (assq-remove! '((x . z) (c . d) (a . b)) 'x) '((c . d) (a . b)))
(test (assq-remove! '((c . d) (a . b)) 'x) '((c . d) (a . b)))
(test (assq-remove! '((1.5 . z) (c . d) (1.5 . b)) '1.5) '((1.5 . z) (c . d) (1.5 . b)))
(test (assq-remove! '(("x" . z) (c . d) ("x" . b)) "x") '(("x" . z) (c . d) ("x" . b)))

(pk 'assv-remove!-tests)
(test (assv-remove! '((c . d) (a . z)) 'a) '((c . d)))
(test (assv-remove! '((x . z) (c . d) (a . b)) 'x) '((c . d) (a . b)))
(test (assv-remove! '((c . d) (a . b)) 'x) '((c . d) (a . b)))
(test (assv-remove! '((1.5 . z) (c . d) (1.5 . b)) '1.5) '((c . d) (1.5 . b)))
(test (assv-remove! '(("x" . z) (c . d) ("x" . b)) "x") '(("x" . z) (c . d) ("x" . b)))

(pk 'assoc-remove!-tests)
(test (assoc-remove! '((c . d) (a . z)) 'a) '((c . d)))
(test (assoc-remove! '((x . z) (c . d) (a . b)) 'x) '((c . d) (a . b)))
(test (assoc-remove! '((c . d) (a . b)) 'x) '((c . d) (a . b)))
(test (assoc-remove! '((1.5 . z) (c . d) (1.5 . b)) '1.5) '((c . d) (1.5 . b)))
(test (assoc-remove! '(("x" . z) (c . d) ("x" . b)) "x") '((c . d) ("x" . b)))

(test (assoc-remove! '((c . d) ("x" . z)) "x" case-cmp?) '((c . d)))
(test (assoc-remove! '((c . d) ("X" . z)) "x" case-cmp?) '((c . d)))
