;;; tag: Tom Lord Tue Dec  4 14:59:37 2001 (systas-tests/unit-strings.scm)
;;;
(display "**************** strings tests ****************\n")

(load "tests/test.scm")

(pk 'string?-tests)
(test (string? "foo") #t)
(test (string? 'foo) #f)
(test (string? (make-shared-substring "abcd")) #t)
(test (string? (make-shared-substring 'abcd)) #f)

(pk 'string-tests)
(test (string "abc" #\d 101) "abcde")

(pk 'make-string-tests)
(test (string-length (make-string 10)) 10)
(test (string? (make-string 10)) #t)
(test (make-string 5 #\a) "aaaaa")

(pk 'substring-tests)
(test (substring "abcd") "abcd")
(test (substring "abcd" 1) "bcd")
(test (substring "abcd" 1 3) "bc")
(test (substring "abcd" 1 -1) "bc")

(pk 'shared-substring?-tests)
(test (shared-substring? "sdlfkj") #f)
(test (shared-substring? (make-shared-substring "abcd")) #t)


(pk 'make-shared-substring-tests)
(test (make-shared-substring "abcd") "abcd")
(test (make-shared-substring "abcd" 1) "bcd")
(test (make-shared-substring "abcd" 1 3) "bc")
(test (make-shared-substring "abcd" 1 -1) "bc")
(test (let* ((x "abcd")
	     (y (make-shared-substring x 1 -1)))
	(string-set! y 1 #\q)
	x)
      "abqd")
	     

(pk 'shared-substring-parts-tests)

(let* ((x "abcd")
       (y (make-shared-substring x 1 3))
       (z (shared-substring-parts y)))
  (test (list x 1 3) z))


(pk 'read-only-string?-tests)
(test (read-only-string? "foo") #t)
(test (read-only-string? 'foo) #t)
(test (read-only-string? (make-shared-substring "abcd")) #t)
(test (read-only-string? (make-shared-substring 'abcd)) #t)


(pk 'writable-string?-tests)
(test (writable-string? "foo") #t)
(test (writable-string? 'foo) #f)
(test (writable-string? (make-shared-substring "abcd")) #t)
(test (writable-string? (make-shared-substring 'abcd)) #f)


(pk 'basic-string?-tests)
(test (basic-string? "foo") #t)
(test (basic-string? 'foo) #f)
(test (basic-string? (make-shared-substring "abcd")) #f)
(test (basic-string? (make-shared-substring 'abcd)) #f)


(pk 'string-length-tests)
(test (string-length "abcd") 4)
(test (string-length 'abcd) 4)
(test (string-length (make-shared-substring "abcd")) 4)
(test (string-length (make-shared-substring 'abcd)) 4)


(pk 'string-null?-tests)
(test (string-null? "") #t)
(test (string-null? "sldkjf") #f)
(test (string-null? '#s"") #t)
(error-test (string-null? 4312))

(pk 'string-ref-tests)
(test (string-ref "abc" 0) #\a)
(test (string-ref "abc" 2) #\c)
(test (string-ref "abc" -1) #\c)
(error-test (string-ref "abc" 3))
(test (string-ref 'abc 0) #\a)
(test (string-ref 'abc) #\a)

(pk 'string-set!-tests)
(test (string-set! "abc" 0 #\z) "zbc")
(test (string-set! "abc" 2 #\z) "abz")
(test (string-set! "abc" -1 #\z) "abz")
(error-test (string-set! "abc" 3 #\z))
(error-test (string-set! 'abc 0 #\z))


(pk 'string-index-tests)
(test (string-index "abc" #\b) 1)
(test (string-index "abc" #\z) #f)
(test (string-index 'abc #\b) 1)
(test (string-index 'abc #\z) #f)
(test (string-index 'abcbc #\b) 1)
(test (string-index "abcbc" #\b 2) 3)
(test (string-index "abcbc" #\b 2 5) 3)
(test (string-index "abcbc" #\b 2 3) #f)


(pk 'string-rindex-tests)
(test (string-rindex "abc" #\b) 1)
(test (string-rindex "abc" #\z) #f)
(test (string-rindex 'abc #\b) 1)
(test (string-rindex 'abc #\z) #f)
(test (string-rindex 'abcbc #\b) 3)
(test (string-rindex "abcbc" #\b 2) 3)
(test (string-rindex "abcbc" #\b 2 5) 3)
(test (string-rindex "abcbc" #\b 1 3) 1)
(test (string-rindex "abcbc" #\b 2 3) #f)


(pk 'substring-move-left!-tests)
(test (substring-move-left! "abcdefg" 1 4 "lmnopqr" 2) "lmbcdqr")
(let ((x "abcdefg"))
  (test (substring-move-left! x 2 5 x 1) "acdeefg"))
(let ((x "abcdefg"))
  (test (substring-move-left! x 1 4 x 2) "abbbbfg"))

(test (substring-move-left! "abcdefg" -6 -3 "lmnopqr" -5) "lmbcdqr")
(let ((x "abcdefg"))
  (test (substring-move-left! x -5 -2 x -6) "acdeefg"))
(let ((x "abcdefg"))
  (test (substring-move-left! x -6 -3 x -5) "abbbbfg"))

(let ((x 'abcdefg))
  (error-test (substring-move-left! x -6 -3 x -5)))


(pk 'substring-move-right!-tests)
(test (substring-move-right! "abcdefg" 1 4 "lmnopqr" 2) "lmbcdqr")
(let ((x "abcdefg"))
  (test (substring-move-right! x 2 5 x 1) "aeeeefg"))
(let ((x "abcdefg"))
  (test (substring-move-right! x 1 4 x 2) "abbcdfg"))

(test (substring-move-right! "abcdefg" -6 -3 "lmnopqr" -5) "lmbcdqr")
(let ((x "abcdefg"))
  (test (substring-move-right! x -5 -2 x -6) "aeeeefg"))
(let ((x "abcdefg"))
  (test (substring-move-right! x -6 -3 x -5) "abbcdfg"))

(let ((x 'abcdefg))
  (error-test (substring-move-right! x -6 -3 x -5)))


(pk 'substring-fill!-tests)
(test (substring-fill! "abcdefg" 2 5 #\x) "abxxxfg")
(test (substring-fill! "abcdefg" -5 -2 #\x) "abxxxfg")
(error-test (substring-fill! 'abcdefg -5 -2 #\x))


(pk 'string-fill!-tests)
(test (string-fill! "abcdefg" #\x) "xxxxxxx")
(error-test (string-fill! 'abcdefg #\x))

(pk 'string=?-tests)
(test (string=? "abc" 'abc) #t)
(test (string=? "ABC" 'abc) #f)


(pk 'string<?-tests)
(test (string<? "abc" 'abc) #f)
(test (string<? "aac" 'abc) #t)
(test (string<? "abc" 'abcd) #t)
(test (string<? "abc" 'ABCD) #f)
(test (string<? "ABC" 'abc) #t)


(pk 'string<=?-tests)
(test (string<=? "abc" 'abc) #t)
(test (string<=? "aac" 'abc) #t)
(test (string<=? "abc" 'abcd) #t)
(test (string<=? "abc" 'ABCD) #f)
(test (string<=? "ABC" 'abc) #t)


(pk 'string>?-tests)
(test (string>? 'abc "abc") #f)
(test (string>? 'abc "aac") #t)
(test (string>? 'abcd "abc") #t)
(test (string>? 'ABCD "abc") #f)
(test (string>? 'abc "ABC") #t)


(pk 'string>=?-tests)
(test (string>=? 'abc "abc") #t)
(test (string>=? 'abc "aac") #t)
(test (string>=? 'abcd "abc") #t)
(test (string>=? 'ABCD "abc") #f)
(test (string>=? 'abc "ABC") #t)


(pk 'string-ci=?-tests)
(test (string-ci=? "abc" 'abc) #t)
(test (string-ci=? "ABC" 'abc) #t)


(pk 'string-ci<?-tests)
(test (string-ci<? "abc" 'abc) #f)
(test (string-ci<? "aac" 'abc) #t)
(test (string-ci<? "abc" 'abcd) #t)
(test (string-ci<? "abc" 'ABCD) #t)
(test (string-ci<? "ABC" 'abc) #f)


(pk 'string-ci<=?-tests)
(test (string-ci<=? "abc" 'abc) #t)
(test (string-ci<=? 'aac "abc") #t)
(test (string-ci<=? "abc" 'abcd) #t)
(test (string-ci<=? "abc" 'ABCD) #t)
(test (string-ci<=? "ABC" 'abc) #t)


(pk 'string-ci>?-tests)
(test (string-ci>? 'abc "abc") #f)
(test (string-ci>? 'abc "aac") #t)
(test (string-ci>? 'abcd "abc") #t)
(test (string-ci>? 'ABCD "abc") #t)
(test (string-ci>? 'abc "ABC") #f)


(pk 'string-ci>=?-tests)
(test (string-ci>=? 'abc "abc") #t)
(test (string-ci>=? 'abc "aac") #t)
(test (string-ci>=? 'abcd "abc") #t)
(test (string-ci>=? 'ABCD "abc") #t)
(test (string-ci>=? 'abc "ABC") #t)


(pk 'string-upcase!-tests)
(test (string-upcase! "abc") "ABC")
(test (string-upcase! "aBC") "ABC")
(test (string-upcase! "@abc{") "@ABC{")


(pk 'string-downcase!-tests)
(test (string-downcase! "ABC") "abc")
(test (string-downcase! "aBC") "abc")
(test (string-downcase! "@ABC{") "@abc{")


(pk 'string->list-tests)
(test (string->list "abc") '(#\a #\b #\c))


(pk 'list->string-tests)
(test (list->string ()) "")
(test (list->string '(#\a #\b #\c)) "abc")
(test (list->string `(#\a #\b #\c "def" ghi ,(char->integer #\j))) "abcdefghij")


(pk 'string-append-tests)
(test (string-append) "")
(test (string-append #\a #\b #\c) "abc")
(test (string-append #\a #\b #\c "def" 'ghi (char->integer #\j)) "abcdefghij")
