;;; tag: Tom Lord Tue Dec  4 14:59:37 2001 (systas-tests/unit-symbols.scm)
;;;
(display "**************** symbols tests ****************\n")

(load "tests/test.scm")

(pk 'FIX-THIS (set! interactive-mode #t))


(pk 'symbol?-tests)
(test (symbol? 'abc) #t)
(test (symbol? (make-shared-substring 'abc 1 2)) #f)
(test (symbol? "abc") #f)


(pk 'symbol->string-tests)
(test (symbol->string 'abc) "abc")


(pk 'string->symbol-tests)
(test (string->symbol "abc") 'abc)

(pk 'string->hash-table-symbol-tests)
(test (symbol? (string->hash-table-symbol #f "foo")) #t)
(test (string=? (string->hash-table-symbol #f "foo") 'foo) #t)

(define x (make-hash-table))
(test (symbol? (string->hash-table-symbol x "foo")) #t)
(test (string=? (string->hash-table-symbol x "foo") 'foo) #t)
(test (string->hash-table-symbol x "foo") (string->hash-table-symbol x "foo"))

(test (string->hash-table-symbol #t "foo") 'foo)

(pk '************************FIX-THESE-TESTS-BEFORE-INTERFACE-CHANGE************************)

(pk 'intern-symbol-tests)

(pk 'unintern-symbol-tests)

(pk 'symbol-interned?-tests)


