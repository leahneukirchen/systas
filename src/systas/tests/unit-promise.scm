;;; tag: Tom Lord Tue Dec  4 14:59:37 2001 (systas-tests/unit-promise.scm)
;;;
(display "**************** promise tests ****************\n")

(load "tests/test.scm")


(pk 'promise?/make-promise-tests)
(test (promise? (make-promise (lambda () 123))) #t)
(test (promise? (lambda () 123)) #f)
(test (promise? (delay 123)) #t)

(pk 'force-tests)
(test (force (make-promise (lambda () 123))) 123)
(test (let* ((x 123)
	     (y (make-promise (lambda () (set! x (+ 1 x)) x))))
	(list (force y) (force y) x))
      '(124 124 124))

(pk 'delay-tests)
(test (force (delay 123)) 123)
(test (let* ((x 123)
	     (y (delay (begin (set! x (+ 1 x)) x))))
	(list (force y) (force y) x))
      '(124 124 124))
