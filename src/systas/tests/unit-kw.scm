;;; tag: Tom Lord Tue Dec  4 14:59:38 2001 (systas-tests/unit-kw.scm)
;;;
(display "**************** keyword tests ****************\n")

(load "tests/test.scm")

(pk 'keyword?-tests)
(test (keyword? :foo) #t)
(test (keyword? 'foo) #f)
(test (keyword? (symbol->keyword 'foo)) #t)
(test (keyword? (keyword->symbol :foo)) #f)


(pk 'kw-arg-ref-tests)
(test (kw-arg-ref '(:foo 1) :foo) 1)
(test (kw-arg-ref '(:bar :foo 2) :foo) 2)
(test (kw-arg-ref '(:bar :foo 3 :baz) :foo) 3)
(test (kw-arg-ref '(:bar :baz) :foo) #f)
(error-test (kw-arg-ref '(:bar :foo :baz) :foo))
(error-test (kw-arg-ref '(:bar :foo) :foo))


(pk 'kw-arg-set!-tests)
(test (kw-arg-set! '(:foo 1) :foo 10) '(:foo 10))
(test (kw-arg-set! '(:bar :foo 2) :foo 100) '(:bar :foo 100))
(test (kw-arg-set! '(:bar :foo 3 :baz) :foo 69) '(:bar :foo 69 :baz))
(test (kw-arg-set! '(:bar :baz) :foo 'abc) '(:foo abc :bar :baz))
(test (kw-arg-set! '(:bar :baz :foo) :foo 'abc) '(:bar :baz :foo abc))

(pk 'symbol->keyword-tests)
(test (symbol->keyword 'foo) :foo)
(test (symbol->keyword 'bar) :bar)
(error-test (symbol->keyword "foo"))

(pk 'keyword->symbol-tests)
(test (keyword->symbol :foo) 'foo)
(test (eq? 'foo (keyword->symbol :foo) (keyword->symbol :foo)) #t)
(test (keyword->symbol :bar) 'bar)
(error-test (keyword->symbol "foo"))
(error-test (keyword->symbol 'foo))

