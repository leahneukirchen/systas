;;; tag: Tom Lord Tue Dec  4 14:59:38 2001 (systas-tests/unit-list.scm)
;;;
(display "**************** list tests ****************\n")

(load "tests/test.scm")

(pk 'null?-tests)
(test (null? ()) #t)
(test (null? 'a) #f)


(pk 'list?-tests)
(test (list? ()) #t)
(test (list? '(a b c)) #t)
(test (list? 'a) #f)
(test (list? '(a b . c)) #f)


(pk 'list-tests)
(test (list) ())
(test (list 'a 'b 'c) '(a b c))


(pk 'list-append-tests)
(test (list-append) ())
(test (list-append '() '(a b) '(c d) '() '(e f)) '(a b c d e f))
(test (let ((x '(a b c)))
	(and (eq? x (list-append x))
	     (eq? x (cddr (list-append '(x y) x)))))
      #t)


(pk 'append-tests)
(test (append) ())
(test (append '() '(a b) '(c d) '() '(e f)) '(a b c d e f))
(test (let ((x '(a b c)))
	(and (eq? x (append x))
	     (eq? x (cddr (append '(x y) x)))))
      #t)


(pk 'list-append!-tests)
(test (list-append!) ())
(test (list-append! '() '(a b) '(c d) '() '(e f)) '(a b c d e f))
(test (let ((x '(a b c)))
	(and (eq? x (list-append! x))
	     (eq? x (list-append! x  '(x y)))))
      #t)


(pk 'append!-tests)
(test (append!) ())
(test (append! '() '(a b) '(c d) '() '(e f)) '(a b c d e f))
(test (let ((x '(a b c)))
	(and (eq? x (append! x))
	     (eq? x (append! x  '(x y)))))
      #t)


(pk 'list-copy-tests)
(test (list-copy ()) ())
(test (list-copy '(a b . c)) '(a b . c))
(test (list-copy '(a b c)) '(a b c))
(test (let ((x '(a b . c)))
	(not (eq? x (list-copy x))))
      #t)


(pk 'copy-tree-tests)
(test (copy-tree ()) ())
(test (copy-tree '(a b . c)) '(a b . c))
(test (copy-tree '((a b) (c (d e g #(x y (m n . q) () "foo"))))) '((a b) (c (d e g #(x y (m n . q) () "foo")))))
(test (let ((x '((a b) (c (d e g #(x y (m n . q) () "foo"))))))
	(not (eq? x (copy-tree x))))
      #t)


(begin
  (define circular (list 'a 'b 'c))
  (set-cdr! (last-pair circular) circular))


(pk 'list-length-tests)
(test (list-length ()) 0)
(error-test (list-length 'a))
(error-test (list-length '(a . b)))
(test (list-length '(a b c)) 3)
(test (list-length '(a)) 1)
(error-test (list-length circular))


(pk 'list-length+-tests)
(test (list-length+ ()) 0)
(test (list-length+ 'a) 0)
(test (list-length+ '(a . b)) 1)
(test (list-length+ '(a b c)) 3)
(test (list-length+ '(a)) 1)
(test (list-length+ circular) #f)


(pk 'soft-list-length-tests)
(test (soft-list-length ()) 0)
(test (soft-list-length 'a) #f)
(test (soft-list-length '(a . b)) #f)
(test (soft-list-length '(a b c)) 3)
(test (soft-list-length '(a)) 1)
(test (soft-list-length circular) #f)


(pk 'list-reverse-tests)
(test (list-reverse '()) ())
(test (list-reverse '(a b c)) '(c b a))
(test (let* ((x '(a b c))
	     (y (list-reverse x)))
	(equal? x '(a b c)))
      #t)
(error-test (list-reverse '(a b . c)))
(test (list-reverse '(a b c) 'tail) '(c b a . tail))


(pk 'list-reverse!-tests)
(test (list-reverse! '()) ())
(test (list-reverse! '(a b c)) '(c b a))
(test (let* ((x '(a b c))
	     (y (list-reverse! x)))
	(equal? x '(a b c)))
      #f)
(error-test (list-reverse! '(a b . c)))
(test (list-reverse! '(a b c) 'tail) '(c b a . tail))


(pk 'reverse!-tests)
(test (reverse! '()) ())
(test (reverse! '(a b c)) '(c b a))
(test (let* ((x '(a b c))
	     (y (reverse! x)))
	(equal? x '(a b c)))
      #f)
(error-test (reverse! '(a b . c)))
(test (reverse! '(a b c) 'tail) '(c b a . tail))


(pk 'list-head-tests)
(test (list-head () 0) ())
(test (list-head '(a b c) 2) '(a b))
(test (list-head '(a b c d e f g h) 4) '(a b c d))


(pk 'list-tail-tests)
(test (list-tail () 0) ())
(test (list-tail '(a b c d) 2) '(c d))
(test (list-tail '(a b . x) 2) 'x)


(pk 'last-pair-tests)
(error-test (last-pair ()))
(test (last-pair '(a b c d)) '(d))
(test (let ((x '(a b c d)))
	(set-car! (last-pair x) 'z)
	x)
      '(a b c z))


(pk 'list-ref-tests)
(error-test (list-ref '(a b c) -1))
(test (list-ref '(a b c) 0) 'a)
(test (list-ref '(a b c) 1) 'b)
(test (list-ref '(a b c) 2) 'c)
(error-test (list-ref '(a b c) 3))


(pk 'list-set!-tests)
(error-test (list-set! '(a b c) -1 'z))
(test (list-set! '(a b c) 0 'z) '(z b c))
(test (list-set! '(a b c) 1 'z) '(a z c))
(test (list-set! '(a b c) 2 'z) '(a b z))
(test (let ((x '(a b c)))
	(eq? x (list-set! x 2 'z)))
      #t)
(error-test (list-set! '(a b c) 3 'z))


(pk 'list-cdr-ref-tests)
(error-test (list-cdr-ref 'a 0))
(test (list-cdr-ref '(a b c) 0) '(b c))
(test (list-cdr-ref '(a b c) 1) '(c))
(test (list-cdr-ref '(a b c) 2) '())
(error-test (list-cdr-ref '(a b c) 3))


(pk 'list-cdr-set!-tests)
(error-test (list-cdr-set! 'a 0 'z))
(test (list-cdr-set! '(a b c) 0 'z) '(a . z))
(test (list-cdr-set! '(a b c) 1 'z) '(a b . z))
(test (list-cdr-set! '(a b c) 2 'z) '(a b c . z))
(error-test (list-cdr-set! '(a b c) 3 'z))


(pk 'memq-tests)
(test (memq 'a '(a b c)) '(a b c))
(test (memq 'b '(a b c)) '(b c))
(test (memq 'c '(a b c)) '(c))
(test (memq 'q '(a b c)) #f)
(test (memq 'q ()) #f)
(test (memq 'q '(a b c . d)) #f)

(test (memq '1.2 '(1.2 2.4 3.6)) #f)
(test (memq '2.4 '(1.2 2.4 3.6)) #f)
(test (memq '3.6 '(1.2 2.4 3.6)) #f)
(test (memq '4.8 '(1.2 2.4 3.6)) #f)
(test (memq '4.8 ()) #f)
(test (memq '4.8 '(1.2 2.4 3.6 . d)) #f)

(test (memq "a" '("a" "b" "c")) #f)
(test (memq "b" '("a" "b" "c")) #f)
(test (memq "c" '("a" "b" "c")) #f)
(test (memq "d" '("a" "b" "c")) #f)
(test (memq "d" ()) #f)
(test (memq "d" '("a" "b" "c" . "d")) #f)


(pk 'memv-tests)
(test (memv 'a '(a b c)) '(a b c))
(test (memv 'b '(a b c)) '(b c))
(test (memv 'c '(a b c)) '(c))
(test (memv 'q '(a b c)) #f)
(test (memv 'q ()) #f)
(test (memv 'q '(a b c . d)) #f)

(test (memv '1.2 '(1.2 2.4 3.6)) '(1.2 2.4 3.6))
(test (memv '2.4 '(1.2 2.4 3.6)) '(2.4 3.6))
(test (memv '3.6 '(1.2 2.4 3.6)) '(3.6))
(test (memv '4.8 '(1.2 2.4 3.6)) #f)
(test (memv '4.8 ()) #f)
(test (memv '4.8 '(1.2 2.4 3.6 . d)) #f)

(test (memv "a" '("a" "b" "c")) #f)
(test (memv "b" '("a" "b" "c")) #f)
(test (memv "c" '("a" "b" "c")) #f)
(test (memv "d" '("a" "b" "c")) #f)
(test (memv "d" ()) #f)
(test (memv "d" '("a" "b" "c" . "d")) #f)

(pk 'member-tests)
(test (member 'a '(a b c)) '(a b c))
(test (member 'b '(a b c)) '(b c))
(test (member 'c '(a b c)) '(c))
(test (member 'q '(a b c)) #f)
(test (member 'q ()) #f)
(test (member 'q '(a b c . d)) #f)

(test (member '1.2 '(1.2 2.4 3.6)) '(1.2 2.4 3.6))
(test (member '2.4 '(1.2 2.4 3.6)) '(2.4 3.6))
(test (member '3.6 '(1.2 2.4 3.6)) '(3.6))
(test (member '4.8 '(1.2 2.4 3.6)) #f)
(test (member '4.8 ()) #f)
(test (member '4.8 '(1.2 2.4 3.6 . d)) #f)

(test (member "a" '("a" "b" "c")) '("a" "b" "c"))
(test (member "b" '("a" "b" "c")) '("b" "c"))
(test (member "c" '("a" "b" "c")) '("c"))
(test (member "d" '("a" "b" "c")) #f)
(test (member "d" ()) #f)
(test (member "d" '("a" "b" "c" . "d")) #f)

(test (member "A" '("a" "b" "c") string-ci=?) '("a" "b" "c"))
(test (member "B" '("a" "b" "c") string-ci=?) '("b" "c"))
(test (member "C" '("a" "b" "c") string-ci=?) '("c"))
(test (member "D" '("a" "b" "c") string-ci=?) #f)
(test (member "D" () string-ci=?) #f)
(test (member "D" '("a" "b" "c" . "d") string-ci=?) #f)


(pk 'delq!-tests)
(test (delq! 'x '(a x b c x d e)) '(a b c d e))
(test (delq! 'x '(a b c d e)) '(a b c d e))
(test (delq! 'x ()) ())
(test (delq! 'x '(a x b c x d e . f)) '(a b c d e . f))
(test (delq! 'x '(a x b c x d x . f)) '(a b c d . f))
(test (delq! 'x '(x a b c x)) '(a b c))
(test (let ((x '(a x b x c)))
	(eq? x (delq! 'x x)))
      #t)


(test (delq! '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e)) '(1.2 10.9 2.4 3.6 10.9 4.8 e))
(test (delq! '10.9 '(1.2 2.4 3.6 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delq! '10.9 ()) ())
(test (delq! '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e . f)) '(1.2 10.9 2.4 3.6 10.9 4.8 e . f))
(test (delq! '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 10.9 . f)) '(1.2 10.9 2.4 3.6 10.9 4.8 10.9 . f))
(test (delq! '10.9 '(10.9 1.2 2.4 3.6 10.9)) '(10.9 1.2 2.4 3.6 10.9))
(test (let ((x '(1.2 10.9 2.4 10.9 3.6)))
	(eq? x (delq! '10.9 x)))
      #t)

(test (delq! "x" '("a" "x" "b" "c" "x" "d" "e")) '("a" "x" "b" "c" "x" "d" "e"))
(test (delq! "x" '("a" "b" "c" "d" "e")) '("a" "b" "c" "d" "e"))
(test (delq! "x" ()) ())
(test (delq! "x" '("a" "x" "b" "c" "x" "d" "e" . "f")) '("a" "x" "b" "c" "x" "d" "e" . "f"))
(test (delq! "x" '("a" "x" "b" "c" "x" "d" "x" . "f")) '("a" "x" "b" "c" "x" "d" "x" . "f"))
(test (delq! "x" '("x" "a" "b" "c" "x")) '("x" "a" "b" "c" "x"))
(test (let ((x '("a" "x" "b" "x" "c")))
	(eq? x (delq! "x" x)))
      #t)


(pk 'delq-tests)
(test (delq 'x '(a x b c x d e)) '(a b c d e))
(test (delq 'x '(a b c d e)) '(a b c d e))
(test (delq 'x ()) ())
(test (delq 'x '(a x b c x d e . f)) '(a b c d e . f))
(test (delq 'x '(a x b c x d x . f)) '(a b c d . f))
(test (delq 'x '(x a b c x)) '(a b c))
(test (let ((x '(a x b x c)))
	(eq? x (delq 'x x)))
      #f)


(test (delq '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e)) '(1.2 10.9 2.4 3.6 10.9 4.8 e))
(test (delq '10.9 '(1.2 2.4 3.6 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delq '10.9 ()) ())
(test (delq '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e . f)) '(1.2 10.9 2.4 3.6 10.9 4.8 e . f))
(test (delq '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 10.9 . f)) '(1.2 10.9 2.4 3.6 10.9 4.8 10.9 . f))
(test (delq '10.9 '(10.9 1.2 2.4 3.6 10.9)) '(10.9 1.2 2.4 3.6 10.9))
(test (let ((x '(1.2 10.9 2.4 10.9 3.6)))
	(eq? x (delq '10.9 x)))
      #f)


(test (delq "x" '("a" "x" "b" "c" "x" "d" "e")) '("a" "x" "b" "c" "x" "d" "e"))
(test (delq "x" '("a" "b" "c" "d" "e")) '("a" "b" "c" "d" "e"))
(test (delq "x" ()) ())
(test (delq "x" '("a" "x" "b" "c" "x" "d" "e" . "f")) '("a" "x" "b" "c" "x" "d" "e" . "f"))
(test (delq "x" '("a" "x" "b" "c" "x" "d" "x" . "f")) '("a" "x" "b" "c" "x" "d" "x" . "f"))
(test (delq "x" '("x" "a" "b" "c" "x")) '("x" "a" "b" "c" "x"))
(test (let ((x '("a" "x" "b" "x" "c")))
	(eq? x (delq "x" x)))
      #f)


(pk 'delv!-tests)
(test (delv! 'x '(a x b c x d e)) '(a b c d e))
(test (delv! 'x '(a b c d e)) '(a b c d e))
(test (delv! 'x ()) ())
(test (delv! 'x '(a x b c x d e . f)) '(a b c d e . f))
(test (delv! 'x '(a x b c x d x . f)) '(a b c d . f))
(test (delv! 'x '(x a b c x)) '(a b c))
(test (let ((x '(a x b x c)))
	(eq? x (delv! 'x x)))
      #t)


(test (delv! '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delv! '10.9 '(1.2 2.4 3.6 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delv! '10.9 ()) ())
(test (delv! '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e . f)) '(1.2 2.4 3.6 4.8 e . f))
(test (delv! '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 10.9 . f)) '(1.2 2.4 3.6 4.8 . f))
(test (delv! '10.9 '(10.9 1.2 2.4 3.6 10.9)) '(1.2 2.4 3.6))
(test (let ((x '(1.2 10.9 2.4 10.9 3.6)))
	(eq? x (delv! '10.9 x)))
      #t)

(test (delv! "x" '("a" "x" "b" "c" "x" "d" "e")) '("a" "x" "b" "c" "x" "d" "e"))
(test (delv! "x" '("a" "b" "c" "d" "e")) '("a" "b" "c" "d" "e"))
(test (delv! "x" ()) ())
(test (delv! "x" '("a" "x" "b" "c" "x" "d" "e" . "f")) '("a" "x" "b" "c" "x" "d" "e" . "f"))
(test (delv! "x" '("a" "x" "b" "c" "x" "d" "x" . "f")) '("a" "x" "b" "c" "x" "d" "x" . "f"))
(test (delv! "x" '("x" "a" "b" "c" "x")) '("x" "a" "b" "c" "x"))
(test (let ((x '("a" "x" "b" "x" "c")))
	(eq? x (delv! "x" x)))
      #t)

(pk 'delv-tests)
(test (delv 'x '(a x b c x d e)) '(a b c d e))
(test (delv 'x '(a b c d e)) '(a b c d e))
(test (delv 'x ()) ())
(test (delv 'x '(a x b c x d e . f)) '(a b c d e . f))
(test (delv 'x '(a x b c x d x . f)) '(a b c d . f))
(test (delv 'x '(x a b c x)) '(a b c))
(test (let ((x '(a x b x c)))
	(eq? x (delv 'x x)))
      #f)


(test (delv '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delv '10.9 '(1.2 2.4 3.6 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delv '10.9 ()) ())
(test (delv '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e . f)) '(1.2 2.4 3.6 4.8 e . f))
(test (delv '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 10.9 . f)) '(1.2 2.4 3.6 4.8 . f))
(test (delv '10.9 '(10.9 1.2 2.4 3.6 10.9)) '(1.2 2.4 3.6))
(test (let ((x '(1.2 10.9 2.4 10.9 3.6)))
	(eq? x (delv '10.9 x)))
      #f)

(test (delv "x" '("a" "x" "b" "c" "x" "d" "e")) '("a" "x" "b" "c" "x" "d" "e"))
(test (delv "x" '("a" "b" "c" "d" "e")) '("a" "b" "c" "d" "e"))
(test (delv "x" ()) ())
(test (delv "x" '("a" "x" "b" "c" "x" "d" "e" . "f")) '("a" "x" "b" "c" "x" "d" "e" . "f"))
(test (delv "x" '("a" "x" "b" "c" "x" "d" "x" . "f")) '("a" "x" "b" "c" "x" "d" "x" . "f"))
(test (delv "x" '("x" "a" "b" "c" "x")) '("x" "a" "b" "c" "x"))
(test (let ((x '("a" "x" "b" "x" "c")))
	(eq? x (delv "x" x)))
      #f)

(pk 'delete!-tests)
(test (delete! 'x '(a x b c x d e)) '(a b c d e))
(test (delete! 'x '(a b c d e)) '(a b c d e))
(test (delete! 'x ()) ())
(test (delete! 'x '(a x b c x d e . f)) '(a b c d e . f))
(test (delete! 'x '(a x b c x d x . f)) '(a b c d . f))
(test (delete! 'x '(x a b c x)) '(a b c))
(test (let ((x '(a x b x c)))
	(eq? x (delete! 'x x)))
      #t)


(test (delete! '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delete! '10.9 '(1.2 2.4 3.6 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delete! '10.9 ()) ())
(test (delete! '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e . f)) '(1.2 2.4 3.6 4.8 e . f))
(test (delete! '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 10.9 . f)) '(1.2 2.4 3.6 4.8 . f))
(test (delete! '10.9 '(10.9 1.2 2.4 3.6 10.9)) '(1.2 2.4 3.6))
(test (let ((x '(1.2 10.9 2.4 10.9 3.6)))
	(eq? x (delete! '10.9 x)))
      #t)


(test (delete! "x" '("a" "x" "b" "c" "x" "d" "e")) '("a" "b" "c" "d" "e"))
(test (delete! "x" '("a" "b" "c" "d" "e")) '("a" "b" "c" "d" "e"))
(test (delete! "x" ()) ())
(test (delete! "x" '("a" "x" "b" "c" "x" "d" "e" . "f")) '("a" "b" "c" "d" "e" . "f"))
(test (delete! "x" '("a" "x" "b" "c" "x" "d" "x" . "f")) '("a" "b" "c" "d" . "f"))
(test (delete! "x" '("x" "a" "b" "c" "x")) '("a" "b" "c"))
(test (let ((x '("a" "x" "b" "x" "c")))
	(eq? x (delete! "x" x)))
      #t)


(pk 'delete-tests)
(test (delete 'x '(a x b c x d e)) '(a b c d e))
(test (delete 'x '(a b c d e)) '(a b c d e))
(test (delete 'x ()) ())
(test (delete 'x '(a x b c x d e . f)) '(a b c d e . f))
(test (delete 'x '(a x b c x d x . f)) '(a b c d . f))
(test (delete 'x '(x a b c x)) '(a b c))
(test (let ((x '(a x b x c)))
	(eq? x (delete 'x x)))
      #f)


(test (delete '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delete '10.9 '(1.2 2.4 3.6 4.8 e)) '(1.2 2.4 3.6 4.8 e))
(test (delete '10.9 ()) ())
(test (delete '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 e . f)) '(1.2 2.4 3.6 4.8 e . f))
(test (delete '10.9 '(1.2 10.9 2.4 3.6 10.9 4.8 10.9 . f)) '(1.2 2.4 3.6 4.8 . f))
(test (delete '10.9 '(10.9 1.2 2.4 3.6 10.9)) '(1.2 2.4 3.6))
(test (let ((x '(1.2 10.9 2.4 10.9 3.6)))
	(eq? x (delete '10.9 x)))
      #f)

(test (delete "x" '("a" "x" "b" "c" "x" "d" "e")) '("a" "b" "c" "d" "e"))
(test (delete "x" '("a" "b" "c" "d" "e")) '("a" "b" "c" "d" "e"))
(test (delete "x" ()) ())
(test (delete "x" '("a" "x" "b" "c" "x" "d" "e" . "f")) '("a" "b" "c" "d" "e" . "f"))
(test (delete "x" '("a" "x" "b" "c" "x" "d" "x" . "f")) '("a" "b" "c" "d" . "f"))
(test (delete "x" '("x" "a" "b" "c" "x")) '("a" "b" "c"))
(test (let ((x '("a" "x" "b" "x" "c")))
	(eq? x (delete "x" x)))
      #f)



(test (delete "X" '("a" "x" "b" "c" "x" "d" "e") string-ci=?) '("a" "b" "c" "d" "e"))
(test (delete "X" '("a" "b" "c" "d" "e") string-ci=?) '("a" "b" "c" "d" "e"))
(test (delete "X" () string-ci=?) ())
(test (delete "X" '("a" "x" "b" "c" "x" "d" "e" . "f") string-ci=?) '("a" "b" "c" "d" "e" . "f"))
(test (delete "X" '("a" "x" "b" "c" "x" "d" "x" . "f") string-ci=?) '("a" "b" "c" "d" . "f"))
(test (delete "X" '("x" "a" "b" "c" "x") string-ci=?) '("a" "b" "c"))
(test (let ((x '("a" "x" "b" "x" "c")))
	(eq? x (delete "X" x string-ci=?)))
      #f)


(pk 'map-tests)
(test (map list '(a b c))  '((a) (b) (c)))
(test (map list '(a b c) '(1 2 3 4) '(q r s)) '((a 1 q) (b 2 r) (c 3 s)))
(test (map list '(a b c) () '(1 2 3 4) '(q r s)) ())

(pk 'for-each-tests)
(test (let ((x ()))
	(for-each (lambda args (set! x (append x (list args))))
		  '(a b c))
	x)
      '((a) (b) (c)))
(test (let ((x ()))
	(for-each (lambda args (set! x (append x (list args))))
		  '(a b c) '(1 2 3 4) '(q r s))
	x)
      '((a 1 q) (b 2 r) (c 3 s)))
(test (let ((x ()))
	(for-each (lambda args (set! x (append x (list args))))
		  '(a b c) () '(1 2 3 4) '(q r s))
	x)
      ())
