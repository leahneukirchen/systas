;;"tree.scm" Implementation of COMMON LISP tree functions for Scheme
;;; Author: Aubrey Jaffer
;;;
;;; This code is in the public domain.

;; Deep copy of the tree -- new one has all new pairs.  (Called
;; tree-copy in Dybvig.)
(define (tree:copy-tree tree)
  (if (pair? tree)
      (cons (tree:copy-tree (car tree))
	    (tree:copy-tree (cdr tree)))
      tree))

;; Substitute occurrences of old equal? to new in tree.
;; Similar to tree walks in SICP without the internal define.
(define (tree:subst new old tree . equ?)
  (set! equ? (if (null? equ?) equal? (car equ?)))
  (letrec ((walk (lambda (tree)
		   (cond ((equ? old tree) new)
			 ((pair? tree)
			  (cons (walk (car tree))
				(walk (cdr tree))))
			 (else tree)))))
    (walk tree)))

;; The next 2 aren't in CL.  (Names from Dybvig)

(define (tree:substq new old tree)
  (tree:subst new old tree eq?))

(define (tree:substv new old tree)
  (tree:subst new old tree eqv?))

(define copy-tree tree:copy-tree)
(define subst tree:subst)
(define substq tree:substq)
(define substv tree:substv)
