;;; tag: Tom Lord Tue Dec  4 14:59:41 2001 (systas-tests/test.scm)
;;;
(set! interactive-mode #f)

(define test-error error)

(define (test-expected-error . args)
  (display*-port (current-error-port) ";;; ")
  (for-each (lambda (arg)
	      (display*-port (current-error-port) arg " "))
	    args)
  (newline (current-error-port)))


(define-syntax generalized-test
  (syntax-rules ()
    ((generalized-test exp expect =)		(let ((got exp)
						      (want expect))
						  (if (not (= got want))
						      (test-error "FAILED: "
								  :expression 'exp
								  :expected want
								  :got got))))))

(define-syntax generalized-failed-test
  (syntax-rules ()
    ((generalized-test exp expect =)		(let ((got exp)
						      (want expect))
						  (if (not (= got want))
						      (test-expected-error "FAILED (expected): "
									   :expression 'exp
									   :expected want
									   :got got)
						      (test-error "unexpeted pass: "
								  :expression 'exp
								  :expected want
								  :got got))))))


(define-syntax test
  (syntax-rules ()
    ((test exp expect)				(generalized-test exp expect equal?))))


(define-syntax testv
  (syntax-rules ()
    ((test exp expect)				(generalized-test exp expect eqv?))))


(define-syntax testq
  (syntax-rules ()
    ((test exp expect)				(generalized-test exp expect eq?))))


(define-syntax failed-test
  (syntax-rules ()
    ((test exp expect)				(generalized-failed-test exp expect equal?))))


(define-syntax failed-testv
  (syntax-rules ()
    ((test exp expect)				(generalized-failed-test exp expect eqv?))))


(define-syntax failed-testq
  (syntax-rules ()
    ((test exp expect)				(generalized-failed-test exp expect eq?))))

(define (error-value) #f)

(define-syntax error-test
  (syntax-rules ()
    ((error-test exp)				(testq (catch #t (lambda () exp) (lambda ign error-value)) error-value))))




