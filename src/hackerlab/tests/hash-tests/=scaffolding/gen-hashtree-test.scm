;;; tag: Tom Lord Tue Dec  4 14:59:27 2001 (=scaffolding/gen-hashtree-test.scm)
;;;
;;; gen-hashtree-test.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2000 Free Software Foundation, Inc.
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (=scaffolding gen-hashtree-test))



(define (gen n a b)

  (define (toss?) (zero? (logand (random32) 1)))
  (define (roll n) (modulo (random32) n))

  (define table (make-hash-table))
  (define seq-table (make-hash-table))
  (define n-items 0)

  (define (script n)
    (let loop ((script	"")
	       (answers	"")
	       (n		n))
      (if (= 0 (modulo n 100))
	  (pk 'n n))
      (if (<= n 0)
	  (values script answers)
	  (call-with-values (lambda () (add-operation script answers))
	    (lambda (script answers)
	      (loop script answers (1- n)))))))


  (define (add-operation script answers)
    (if (or (= n-items 0) (toss?))
	(let ((new-key 		(roll 2147483647))
	      (new-value	(roll 2147483647)))
	  (hashq-set! table new-key new-value)
	  (hashq-set! seq-table n-items new-key)
	  (set! n-items (1+ n-items))
	  (values (string-append script "s " (->string new-key) " " (->string new-value) "\n")
		  answers))

	(if (toss?)
	    (let ((old-key 	(hashq-ref seq-table (roll n-items))))
	      (hashq-remove! table old-key)
	      (values (string-append script "d " (->string old-key) "\n")
		      answers))

	    (let* ((key 		(hashq-ref seq-table (roll n-items)))
		   (value		(hashq-ref table key)))
	      (values (string-append script "f " (->string key) "\n")
		      (if value
			  (string-append answers (->string key) " => " (->string value) "\n")
			  (string-append answers (->string key) " unbound\n")))))))


  (call-with-values (lambda () (script n))
    (lambda (script answers)
      (with-output-to-file a (lambda () (display script) (display "q\n")))
      (with-output-to-file b (lambda () (display answers))))))

