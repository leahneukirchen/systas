;;; "srfi-1.scm" SRFI-1 list-processing library		-*-scheme-*-
; Copyright 2001 Aubrey Jaffer
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

;			   Some pieces from:
;;;
;;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;;; this code as long as you do not remove this copyright notice or
;;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.
;;;     -Olin

;;@code{(require 'srfi-1)}
;;@ftindex srfi-1
;;
;;@noindent
;;Implements the @dfn{SRFI-1} @dfn{list-processing library} as described
;;at @url{http://srfi.schemers.org/srfi-1/srfi-1.html}

(require 'common-list-functions)
(require 'rev2-procedures)		;for append!
(require 'values)

;;@subheading Constructors

;;@body @code{(define (xcons d a) (cons a d))}.
(define (xcons d a) (cons a d))

;;@body Returns a list of length @1.  Element @var{i} is @code{(@2
;;@var{i})} for 0 <= @var{i} < @1.
(define (list-tabulate len proc)
  (do ((i (- len 1) (- i 1))
       (ans '() (cons (proc i) ans)))
      ((< i 0) ans)))

;;@args obj1 obj2
(define cons* comlist:list*)

;;@args count start step
;;@args count start
;;@args count
;;Returns a list of @1 numbers: (@2, @2+@3, @dots{},  @2+(@1-1)*@3).
(define (iota count . args)
  (let ((start (if (null? args) 0 (car args)))
	(step (if (or (null? args) (null? (cdr args))) 1 (cadr args))))
    (list-tabulate count (lambda (idx) (+ start (* step idx))))))

;;@body
;;Returns a circular list of @1, @2, @dots{}.
(define (circular-list obj1 . obj2)
  (let ((ans (cons obj1 obj2)))
    (set-cdr! (last-pair ans) ans)
    ans))

;;@subheading Predicates

;;@args obj
(define proper-list? list?)

;;@body
(define (circular-list? x)
  (let lp ((x x) (lag x))
    (and (pair? x)
	 (let ((x (cdr x)))
	   (and (pair? x)
		(let ((x   (cdr x))
		      (lag (cdr lag)))
		  (or (eq? x lag) (lp x lag))))))))

;;@body
(define (dotted-list? obj)
  (not (or (proper-list? obj) (circular-list? obj))))

;;@args obj
(define null-list? null?)

;;@body
(define (not-pair? obj) (not (pair? obj)))

;;@body
(define (list= =pred . lists)
  (or (null? lists)			; special case
      (let lp1 ((list-a (car lists)) (others (cdr lists)))
	(or (null? others)
	    (let ((list-b (car others))
		  (others (cdr others)))
	      (if (eq? list-a list-b)	; EQ? => LIST=
		  (lp1 list-b others)
		  (let lp2 ((list-a list-a) (list-b list-b))
		    (if (null-list? list-a)
			(and (null-list? list-b)
			     (lp1 list-b others))
			(and (not (null-list? list-b))
			     (=pred (car list-a) (car list-b))
			     (lp2 (cdr list-a) (cdr list-b)))))))))))

;;@subheading Selectors

;;@args pair
(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)
(define (fifth   obj) (car    (cddddr obj)))
(define (sixth   obj) (cadr   (cddddr obj)))
(define (seventh obj) (caddr  (cddddr obj)))
(define (eighth  obj) (cadddr (cddddr obj)))
(define (ninth   obj) (car  (cddddr (cddddr obj))))
(define (tenth   obj) (cadr (cddddr (cddddr obj))))

;;@body
(define (car+cdr pair) (values (car pair) (cdr pair)))

;;@body
(define (take lst k) (comlist:butnthcdr k lst))
(define take! take)
(define (drop lst k) (comlist:nthcdr k lst))

;;@args lst k
(define take-right comlist:butlast)
(define drop-right comlist:last)
(define drop-right! drop-right)

;;@body
(define (split-at lst k) (values (take lst k) (drop lst k)))
(define split-at! split-at)

;;@args lst
;;(car (last-pair lst))
(define (last lst . k)
  (if (null? k)
      (car (last-pair lst))
      (apply comlist:last lst k)))

;;@subheading Miscellaneous

;;@body
(define (length+ obj) (and (list? obj) (length obj)))

;;Append and append! are provided by R4RS and rev2-procedures.

;;@body
(define (concatenate  lists) (reduce-right append  '() lists))
(define (concatenate! lists) (reduce-right append! '() lists))

;;Reverse is provided by R4RS.
;;@args lst
(define reverse! comlist:nreverse)

;;@body
(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
	(lp (cdr rev-head) (cons (car rev-head) tail)))))
(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
	(let ((next-rev (cdr rev-head)))
	  (set-cdr! rev-head tail)
	  (lp next-rev rev-head)))))

;;@body
(define (zip list1 . list2) (apply map list list1 list2))

;;@body
(define (unzip1 lst) (map car lst))
(define (unzip2 lst) (values (map car lst) (map cadr lst)))
(define (unzip3 lst) (values (map car lst) (map cadr lst) (map caddr lst)))
(define (unzip4 lst) (values (map car lst) (map cadr lst) (map caddr lst)
			     (map cadddr lst)))
(define (unzip5 lst) (values (map car lst) (map cadr lst) (map caddr lst)
			     (map cadddr lst) (map fifth lst)))

;;@body
(define (count pred list1 . list2)
  (cond ((null? list2)
	 (let mapf ((l list1) (count 0))
	   (if (null? l)
	       count (mapf (cdr l)
			   (+ count (if (pred (car l)) 1 0))))))
	(else (let mapf ((l list1) (rest list2) (count 0))
		(if (null? l)
		    count
		    (mapf (cdr l)
			  (map cdr rest)
			  (+ count (if (apply pred (car l) (map car rest))
				       1 0))))))))

;;@subheading Fold and Unfold

;;@subheading Filtering and Partitioning

;;@subheading Searching

;;@args pred list
(define find comlist:find-if)

;;@args pred list
(define find-tail comlist:member-if)


;;@args obj list pred
;;@args obj list
;;
;;@0 returns the first sublist of @2 whose car is @1, where the sublists
;;of @2 are the non-empty lists returned by @t{(list-tail @2 @var{k})}
;;for @var{k} less than the length of @2.  If @1 does not occur in @2,
;;then @t{#f} (not the empty list) is returned.  The procedure @3 is
;;used for testing equality.  If @3 is not provided, @samp{equal?} is
;;used.
(define member
  (let ((old-member member))
    (lambda (obj list . pred)
      (if (null? pred)
	  (old-member obj list)
	  (let ((pred (car pred)))
	    (find-tail (lambda (ob) (pred ob obj)) list))))))

;;@subheading Deleting

;;@subheading Association lists

;;@args obj alist pred
;;@args obj alist
;;
;;@2 (for ``association list'') must be a list of pairs.  These
;;procedures find the first pair in @2 whose car field is @1, and
;;returns that pair.  If no pair in @2 has @1 as its car, then @t{#f}
;;(not the empty list) is returned.  The procedure @3 is used for
;;testing equality.  If @3 is not provided, @samp{equal?} is used.
(define assoc
  (let ((old-assoc assoc))
    (lambda (obj alist . pred)
      (if (null? pred)
	  (old-assoc obj alist)
	  (let ((pred (car pred)))
	    (find (lambda (pair) (pred obj (car pair))) alist))))))

;;@subheading Set operations
