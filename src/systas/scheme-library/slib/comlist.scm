;;"comlist.scm" Implementation of COMMON LISP list functions for Scheme
; Copyright (C) 1991, 1993, 1995, 2001 Aubrey Jaffer.
; Copyright (C) 2000 Colin Walters
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;; Some of these functions may be already defined in your Scheme.
;;; Comment out those definitions for functions which are already defined.

;;;; LIST FUNCTIONS FROM COMMON LISP

;;; Some tail-recursive optimizations made by
;;; Colin Walters <walters@cis.ohio-state.edu>
;;; AGJ restored order July 2001.

;;;From: hugh@ear.mit.edu (Hugh Secker-Walker)
(define (comlist:make-list k . init)
  (set! init (if (pair? init) (car init)))
  (do ((k k (+ -1 k))
       (result '() (cons init result)))
      ((<= k 0) result)))

(define (comlist:copy-list lst) (append lst '()))

(define (comlist:adjoin obj lst) (if (memv obj lst) lst (cons obj lst)))

(define (comlist:union lst1 lst2)
  (define ans (if (null? lst1) lst2 lst1))
  (cond ((null? lst2) lst1)
	(else (for-each (lambda (elt) (set! ans (comlist:adjoin elt ans)))
			lst2)
	      ans)))

(define (comlist:intersection lst1 lst2)
  (if (null? lst2)
      lst2
      (let build-intersection ((lst1 lst1)
			       (result '()))
	(cond ((null? lst1) (reverse result))
	      ((memv (car lst1) lst2)
	       (build-intersection (cdr lst1) (cons (car lst1) result)))
	      (else
	       (build-intersection (cdr lst1) result))))))

(define (comlist:set-difference lst1 lst2)
  (if (null? lst2)
      lst1
      (let build-difference ((lst1 lst1)
			     (result '()))
	(cond ((null? lst1) (reverse result))
	      ((memv (car lst1) lst2) (build-difference (cdr lst1) result))
	      (else (build-difference (cdr lst1) (cons (car lst1) result)))))))

(define (comlist:position obj lst)
  (letrec ((pos (lambda (n lst)
		  (cond ((null? lst) #f)
			((eqv? obj (car lst)) n)
			(else (pos (+ 1 n) (cdr lst)))))))
    (pos 0 lst)))

(define (comlist:reduce-init pred? init lst)
  (if (null? lst)
      init
      (comlist:reduce-init pred? (pred? init (car lst)) (cdr lst))))

(define (comlist:reduce pred? lst)
  (cond ((null? lst) lst)
	((null? (cdr lst)) (car lst))
	(else (comlist:reduce-init pred? (car lst) (cdr lst)))))

(define (comlist:some pred lst . rest)
  (cond ((null? rest)
	 (let mapf ((lst lst))
	   (and (not (null? lst))
		(or (pred (car lst)) (mapf (cdr lst))))))
	(else (let mapf ((lst lst) (rest rest))
		(and (not (null? lst))
		     (or (apply pred (car lst) (map car rest))
			 (mapf (cdr lst) (map cdr rest))))))))

(define (comlist:every pred lst . rest)
  (cond ((null? rest)
	 (let mapf ((lst lst))
	   (or (null? lst)
	       (and (pred (car lst)) (mapf (cdr lst))))))
	(else (let mapf ((lst lst) (rest rest))
		(or (null? lst)
		    (and (apply pred (car lst) (map car rest))
			 (mapf (cdr lst) (map cdr rest))))))))

(define (comlist:notany pred . ls) (not (apply comlist:some pred ls)))

(define (comlist:notevery pred . ls) (not (apply comlist:every pred ls)))

(define (comlist:list-of?? predicate . bound)
  (define (errout) (apply slib:error 'list-of?? predicate bound))
  (case (length bound)
    ((0)
     (lambda (obj)
       (and (list? obj)
	    (every predicate obj))))
    ((1)
     (set! bound (car bound))
     (cond ((negative? bound)
	    (set! bound (- bound))
	    (lambda (obj)
	      (and (list? obj)
		   (<= bound (length obj))
		   (every predicate obj))))
	   (else
	    (lambda (obj)
	      (and (list? obj)
		   (<= (length obj) bound)
		   (every predicate obj))))))
    ((2)
     (let ((low (car bound))
	   (high (cadr bound)))
       (cond ((or (negative? low) (negative? high)) (errout))
	     ((< high low)
	      (set! high (car bound))
	      (set! low (cadr bound))))
       (lambda (obj)
	 (and (list? obj)
	      (<= low (length obj) high)
	      (every predicate obj)))))
    (else (errout))))

(define (comlist:find-if pred? lst)
  (cond ((null? lst) #f)
	((pred? (car lst)) (car lst))
	(else (comlist:find-if pred? (cdr lst)))))

(define (comlist:member-if pred? lst)
  (cond ((null? lst) #f)
	((pred? (car lst)) lst)
	(else (comlist:member-if pred? (cdr lst)))))

(define (comlist:remove pred? lst)
  (define head (list '*head*))
  (let remove ((lst lst)
	       (tail head))
    (cond ((null? lst))
	  ((eqv? pred? (car lst)) (remove (cdr lst) tail))
	  (else
	   (set-cdr! tail (list (car lst)))
	   (remove (cdr lst) (cdr tail)))))
  (cdr head))

(define (comlist:remove-if pred? lst)
  (let remove-if ((lst lst)
		  (result '()))
    (cond ((null? lst) (reverse result))
	  ((pred? (car lst)) (remove-if (cdr lst) result))
	  (else (remove-if (cdr lst) (cons (car lst) result))))))

(define (comlist:remove-if-not pred? lst)
  (let remove-if-not ((lst lst)
		      (result '()))
    (cond ((null? lst) (reverse result))
	  ((pred? (car lst)) (remove-if-not (cdr lst) (cons (car lst) result)))
	  (else (remove-if-not (cdr lst) result)))))

(define comlist:nconc
  (if (provided? 'rev2-procedures) append!
      (lambda args
	(cond ((null? args) '())
	      ((null? (cdr args)) (car args))
	      ((null? (car args)) (apply comlist:nconc (cdr args)))
	      (else
	       (set-cdr! (last-pair (car args))
			 (apply comlist:nconc (cdr args)))
	       (car args))))))

;;;From: hugh@ear.mit.edu (Hugh Secker-Walker)
(define (comlist:nreverse rev-it)
;;; Reverse order of elements of LIST by mutating cdrs.
  (cond ((null? rev-it) rev-it)
	((not (list? rev-it))
	 (slib:error "nreverse: Not a list in arg1" rev-it))
	(else (do ((reved '() rev-it)
		   (rev-cdr (cdr rev-it) (cdr rev-cdr))
		   (rev-it rev-it rev-cdr))
		  ((begin (set-cdr! rev-it reved) (null? rev-cdr)) rev-it)))))

(define (comlist:last lst n)
  (comlist:nthcdr (- (length lst) n) lst))

(define (comlist:butlast lst n)
  (letrec
      ((len (- (length lst) n))
       (bl (lambda (lst n)
	     (let build-until-zero ((lst lst)
				    (n n)
				    (result '()))
	       (cond ((null? lst) (reverse result))
		     ((positive? n)
		      (build-until-zero
		       (cdr lst) (- n 1) (cons (car lst) result)))
		     (else (reverse result)))))))
    (bl lst (if (negative? n)
		(slib:error "negative argument to butlast" n)
		len))))

(define (comlist:nthcdr n lst)
  (if (zero? n) lst (comlist:nthcdr (+ -1 n) (cdr lst))))

(define (comlist:butnthcdr n lst)
  (letrec
      ((bl (lambda (lst n)
	     (let build-until-zero ((lst lst)
				    (n n)
				    (result '()))
	       (cond ((null? lst) (reverse result))
		     ((positive? n)
		      (build-until-zero
		       (cdr lst) (- n 1) (cons (car lst) result)))
		     (else (reverse result)))))))
    (bl lst (if (negative? n)
		(slib:error "negative argument to butnthcdr" n)
		n))))

;;;; CONDITIONALS

(define (comlist:and? . args)
  (cond ((null? args) #t)
	((car args) (apply comlist:and? (cdr args)))
	(else #f)))

(define (comlist:or? . args)
  (cond ((null? args) #f)
	((car args) #t)
	(else (apply comlist:or? (cdr args)))))

;;; Checks to see if a list has any duplicate MEMBERs.
(define (comlist:has-duplicates? lst)
  (cond ((null? lst) #f)
	((member (car lst) (cdr lst)) #t)
	(else (comlist:has-duplicates? (cdr lst)))))

;;; remove duplicates of MEMBERs of a list
(define (comlist:remove-duplicates lst)
  (letrec ((rem-dup
	    (lambda (lst nlst)
	      (cond ((null? lst) (reverse nlst))
		    ((member (car lst) nlst) (rem-dup (cdr lst) nlst))
		    (else (rem-dup (cdr lst) (cons (car lst) nlst)))))))
    (rem-dup lst '())))

(define (comlist:list* obj1 . obj2)
  (define (list*1 obj)
    (if (null? (cdr obj))
	(car obj)
	(cons (car obj) (list*1 (cdr obj)))))
  (if (null? obj2)
      obj1
      (cons obj1 (list*1 obj2))))

(define (comlist:atom? obj)
  (not (pair? obj)))

(define (comlist:delete obj lst)
  (let delete ((lst lst))
    (cond ((null? lst) '())
	  ((equal? obj (car lst)) (delete (cdr lst)))
	  (else
	   (set-cdr! lst (delete (cdr lst)))
	   lst))))

(define (comlist:delete-if pred lst)
  (let delete-if ((lst lst))
    (cond ((null? lst) '())
	  ((pred (car lst)) (delete-if (cdr lst)))
	  (else
	   (set-cdr! lst (delete-if (cdr lst)))
	   lst))))

(define (comlist:delete-if-not pred lst)
  (let delete-if ((lst lst))
    (cond ((null? lst) '())
	  ((not (pred (car lst))) (delete-if (cdr lst)))
	  (else
	   (set-cdr! lst (delete-if (cdr lst)))
	   lst))))

;;; exports

(define make-list comlist:make-list)
(define copy-list comlist:copy-list)
(define adjoin comlist:adjoin)
(define union comlist:union)
(define intersection comlist:intersection)
(define set-difference comlist:set-difference)
(define position comlist:position)
(define reduce-init comlist:reduce-init)
(define reduce comlist:reduce) ; reduce is also in collect.scm
(define some comlist:some)
(define every comlist:every)
(define notevery comlist:notevery)
(define notany comlist:notany)
(define find-if comlist:find-if)
(define member-if comlist:member-if)
(define remove comlist:remove)
(define remove-if comlist:remove-if)
(define remove-if-not comlist:remove-if-not)
(define nconc comlist:nconc)
(define nreverse comlist:nreverse)
(define last comlist:last)
(define butlast comlist:butlast)
(define nthcdr comlist:nthcdr)
(define butnthcdr comlist:butnthcdr)
(define and? comlist:and?)
(define or? comlist:or?)
(define has-duplicates? comlist:has-duplicates?)
(define remove-duplicates comlist:remove-duplicates)

(define delete-if-not comlist:delete-if-not)
(define delete-if comlist:delete-if)
(define delete comlist:delete)
(define comlist:atom comlist:atom?)
(define atom comlist:atom?)
(define atom? comlist:atom?)
(define list* comlist:list*)
(define list-of?? comlist:list-of??)
