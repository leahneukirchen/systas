;;; test-macros.scm - help for writing test cases
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (test test-macros))




(define-module (test test-macros))

(define-public-memoizing-macro (test exp)
  `(let ((exp-gives	,exp))
     (or exp-gives
	 (error "test failed" :exp ',exp))))

(define-public-memoizing-macro (test-equal exp val)
  `(let ((exp-gives	,exp)
	 (val-gives	,val))
     (or (equal? exp-gives val-gives)
	 (error "test-equal failed" :exp ',exp :val ,val :exp-gives exp-gives :val-gives val-gives))))

(define-public-memoizing-macro (test-exception exp exception)
  `(or (catch #t
	 (lambda ()
	   (catch ,exception
	     (lambda () ,exp #f)
	     (lambda ign #t)))
     
	 (lambda got-exception
	   (error "test-exception got wrong exception" :exp ',exp :expected-exception ',exception :got-exception got-exception)))
       
       (error "test-exception got no exception" :exp ',exp :expected-exception ',exception)))
