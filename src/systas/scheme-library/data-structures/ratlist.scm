;;; ratlist.scm - A rational collection of list functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Portions Copyright (C) 1998 Tom Lord
;;; Portions Copyright (C) 1997 Free Software Foundation
;;; Portions by Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;; Portions Copyright (C) 1991, 1993, 1995 Aubrey Jaffer.
;;; 
;;; See the file =copying-conditions.gpl-slib for copying permissions.
;;;



(define-module (data-structures ratlist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists as Sets (In General)
;;; 

(define-memoizing-macro (define-ratlist-functions equality? membership . names)
  (let ((adjoin (kw-arg-ref names :adjoin))
	(union (kw-arg-ref names :union))
	(intersection (kw-arg-ref names :intersection))
	(set-difference (kw-arg-ref names :set-difference))
	(subset? (kw-arg-ref names :subset?))
	(closure (kw-arg-ref names :closure))
	(unique (kw-arg-ref names :unique))
	(delete! (kw-arg-ref names :delete!))
	)
	
  `(begin
     (define-public (,adjoin e l) (if (,membership e l) l (cons e l)))


     (define-public (,union . sets) (,unique (apply append sets)))

     (define-public (,intersection . sets)
       (let ((first 	(if sets (car sets) ()))
	     (rest 	(and sets (cdr sets))))
	 (pick (lambda (elt)
		 (and-map (lambda (set) (,membership elt set)) rest))
	       first)))

     (define-public (,set-difference first . rest)
       (pick (lambda (elt)
	       (and-map (lambda (set) (not (,membership elt set))) rest))
	     first))

     (define-public (,subset? . sets)
       (define (subset-2 a b)
	 (and-map (lambda (elt) (,membership elt a)) b))

       (cond
	((null? sets)		#t)
	((null? (cdr sets))	#t)
	(#t			(and (subset-2 (car sets) (cadr sets))
				     (apply ,subset? (cdr sets))))))


     (define-public (,closure first next)
       (let loop ((answer (list first))
		  (new (next first)))
	 (cond
	  ((null? new) 				answer)
	  ((,membership (car new) answer)	(loop answer (cdr new)))
	  (else					(loop (cons (car new) answer)
						      (append (next (car new)) (cdr new)))))))

     (define-public (,unique l)
       (if (null? l)
	   ()
	   (cons (car l) (,delete! (car l) (,unique (cdr l))))))
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists as Sets (Specific Functions)
;;; 

(define-ratlist-functions eq? memq
  :adjoin adjoinq
  :union unionq
  :intersection intersectionq
  :set-difference set-differq
  :subset? subsetq?
  :closure closureq
  :unique uniq
  :delete! delq!)

(define-ratlist-functions eqv? memv
  :adjoin adjoinv
  :union unionv
  :intersection intersectionv
  :set-difference set-differv
  :subset? subsetv
  :closure closurev
  :unique uniqv
  :delete! delv!)

(define-ratlist-functions equal? member
  :adjoin adjoin
  :union union
  :intersection intersection
  :set-difference set-difference
  :subset? subset?
  :closure closure
  :unique unique
  :delete! delete!)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; List Cross Products
;;;

(define-public (product . args)
  (cond
   ((null? args)		())
   ((null? (cdr args))		(car args))
   ((null? (cddr args))		(prod (car args) (cadr args)))
   (#t				(cons-prod (car args) (apply product (cdr args))))))

(define-public (prod a b)
  (apply append!
	 (map (lambda (an-a) (map (lambda (a-b) (list an-a a-b)) b)) a)))


(define-public (cons-prod a b)
  (apply append!
	 (map (lambda (an-a) (map (lambda (a-b) (cons an-a a-b)) b)) a)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subset Procedures
;;;
;;; These procedures extract elements from lists on the basis
;;; of predicate procedures.
;;;

;; pick p l
;;
;; Apply P to each element of L, returning a list of elts
;; for which P returns a non-#f value.
;;
(define-public (pick p l)
  (let ((answer (cons () ())))
    (let loop ((s answer)
	       (l l))
      (cond
       ((null? l) 	(cdr answer))
       ((p (car l))	(set-cdr! s (cons (car l) ()))
			(loop (cdr s) (cdr l)))
       (else		(loop s (cdr l)))))))


;; Use a predicate to choose an element from a list.
;;
(define-public (choose pred l . rest)
  (let mapf ((l l)
	     (rest rest))
    (and (not (null? l))
	 (if (apply pred (car l) (map car rest))
	     (car l)
	     (mapf (cdr l) (map cdr rest))))))


;; Use a predicate to remove elements from a list.
;;
(define-public (remove-if p l) (pick (lambda (x) (not (p x))) l))

;; pick-mappings p l
;;
;; Apply P to each element of L, returning a list of the 
;; non-#f return values of P.
;;
(define-public (pick-mappings p l)
  (let ((answer (cons () ())))
    (let loop ((s answer)
	       (l l))
      (cond
       ((null? l) 	(cdr answer))
       ((p (car l)) =>	(lambda (mapping)
			  (set-cdr! s (cons mapping ()))
			  (loop (cdr s) (cdr l))))
       (else		(loop s (cdr l)))))))


;; Use a predicate to choose the mapping of an element from a list.
;;
(define-public (choose-mapping pred l . rest)
  (let mapf ((l l)
	     (rest rest))
    (and (not (null? l))
	 (or (apply pred (car l) (map car rest))
	     (mapf (cdr l) (map cdr rest))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; 
 

;; Use a predicate to choose every satisfying element from 
;; a list.
;;
(define-public (every pred l . rest)
  (let mapf ((l l)
	     (rest rest)
	     (answer ()))
    (if (null? l)
	(reverse answer)
	(if (apply pred (car l) (map car rest))
	    (mapf (cdr l) (map cdr rest) (cons (car l) answer))
	    (mapf (cdr l) (map cdr rest) answer)))))

(define-public (every pred l . rest)
  (cond ((null? rest)
	 (let mapf ((l l))
	   (or (null? l)
	       (and (pred (car l)) (mapf (cdr l))))))
	(else (let mapf ((l l) (rest rest))
		(or (null? l)
		    (and (apply pred (car l) (map car rest))
			 (mapf (cdr l) (map cdr rest))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reduce
;;;

(define-public (reduce-init f init l)
  (if (null? l)
      init
      (reduce-init f (f init (car l)) (cdr l))))

;; (define-public (reduce f l)
;;   (and l (reduce-init f (car l) (cdr l))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Merging and Sorting
;;;

;; (sorted? sequence less?)
;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;; such that for all 1 <= i <= m,
;;	(not (less? (list-ref list i) (list-ref list (- i 1)))).

(define-public (sorted? seq less?)
    (cond
	((null? seq)
	    #t)
	((vector? seq)
	    (let ((n (vector-length seq)))
		(if (<= n 1)
		    #t
		    (do ((i 1 (+ i 1)))
			((or (= i n)
			     (less? (vector-ref seq (- i 1))
			     	    (vector-ref seq i)))
			    (= i n)) )) ))
	(else
	    (let loop ((last (car seq)) (next (cdr seq)))
		(or (null? next)
		    (and (not (less? (car next) last))
			 (loop (car next) (cdr next)) )) )) ))


;; (merge a b less?)
;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;; and returns a new list in which the elements of a and b have been stably
;; interleaved so that (sorted? (merge a b less?) less?).
;; Note:  this does _not_ accept vectors.  See below.
;;
(define-public (merge a b less?)
    (cond
	((null? a) b)
	((null? b) a)
	(else (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
	    ;; The loop handles the merging of non-empty lists.  It has
	    ;; been written this way to save testing and car/cdring.
	    (if (less? y x)
		(if (null? b)
		    (cons y (cons x a))
		    (cons y (loop x a (car b) (cdr b)) ))
		;; x <= y
		(if (null? a)
		    (cons x (cons y b))
		    (cons x (loop (car a) (cdr a) y b)) )) )) ))


;; (merge! a b less?)
;; takes two sorted lists a and b and smashes their cdr fields to form a
;; single sorted list including the elements of both.
;; Note:  this does _not_ accept vectors.
;;
(define-public (merge! a b less?)
    (define (loop r a b)
	(if (less? (car b) (car a))
	    (begin
		(set-cdr! r b)
		(if (null? (cdr b))
		    (set-cdr! b a)
		    (loop b a (cdr b)) ))
	    ;; (car a) <= (car b)
	    (begin
		(set-cdr! r a)
		(if (null? (cdr a))
		    (set-cdr! a b)
		    (loop a (cdr a) b)) )) )
    (cond
	((null? a) b)
	((null? b) a)
	((less? (car b) (car a))
	    (if (null? (cdr b))
		(set-cdr! b a)
		(loop b a (cdr b)))
	    b)
	(else ; (car a) <= (car b)
	    (if (null? (cdr a))
		(set-cdr! a b)
		(loop a (cdr a) b))
	    a)))



;; (sort! sequence less?)
;; sorts the list or vector sequence destructively.  It uses a version
;; of merge-sort invented, to the best of my knowledge, by David H. D.
;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;; adapted it to work destructively in Scheme.
;;
(define-public (sort! seq less?)
    (define (step n)
	(cond
	    ((> n 2)
		(let* ((j (quotient n 2))
		       (a (step j))
		       (k (- n j))
		       (b (step k)))
		    (merge! a b less?)))
	    ((= n 2)
		(let ((x (car seq))
		      (y (cadr seq))
		      (p seq))
		    (set! seq (cddr seq))
		    (if (less? y x) (begin
			(set-car! p y)
			(set-car! (cdr p) x)))
		    (set-cdr! (cdr p) '())
		    p))
	    ((= n 1)
		(let ((p seq))
		    (set! seq (cdr seq))
		    (set-cdr! p '())
		    p))
	    (else
		'()) ))
    (if (vector? seq)
	(let ((n (vector-length seq))
	      (vec seq))
	  (set! seq (vector->list seq))
	  (do ((p (step n) (cdr p))
	       (i 0 (+ i 1)))
	      ((null? p) vec)
	    (vector-set! vec i (car p)) ))
	;; otherwise, assume it is a list
	(step (length seq)) ))

;; (sort sequence less?)
;; sorts a vector or list non-destructively.  It does this by sorting a
;; copy of the sequence.  My understanding is that the Standard says
;; that the result of append is always "newly allocated" except for
;; sharing structure with "the last argument", so (append x '()) ought
;; to be a standard way of copying a list x.
;;
(define-public (sort seq less?)
    (if (vector? seq)
	(list->vector (sort! (vector->list seq) less?))
	(sort! (append seq '()) less?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Orderings of Lists
;;; 

(define-public (binary-list<? <? =? a b)
  (cond
   ((null? a)			(not (null? b)))
   ((null? b)			#f)
   ((<? (car a) (car b))	#t)
   ((=? (car a) (car b))	(binary-list<? <? =? (cdr a) (cdr b)))
   (#t				#f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ordered Mappings
;;;

(define-public map-in-order map)
(define-public for-each-in-order for-each)

