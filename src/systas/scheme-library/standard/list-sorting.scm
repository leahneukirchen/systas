;;; tag: Tom Lord Tue Dec  4 14:59:30 2001 (standard/list-sorting.scm)
;;;
;;; list-sorting.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;
;;; Derived (trivially) from a public domain work by Richard A. O'Keefe,
;;; which is in turn based on Prolog code by D.H.D.Warren.
;;;



(define-module (standard list-sorting))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Merging and Sorting
;;;

;; (sorted? sequence less?)
;; 
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
;; 
;; Takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
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
;; 
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
;; 
;; Sorts the list or vector sequence destructively.  It uses a version
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
;; 
;; Sorts a vector or list non-destructively.  It does this by sorting a
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

;; (binary-list<? <? =? a b)
;; 
;; Compare lists `a' and `b' elementwise, using `<?' and `=?'.
;; Return `#t' if `a' is lexically less than `b', `#f' otherwise.
;; 
(define-public (binary-list<? <? =? a b)
  (cond
   ((null? a)			(not (null? b)))
   ((null? b)			#f)
   ((<? (car a) (car b))	#t)
   ((=? (car a) (car b))	(binary-list<? <? =? (cdr a) (cdr b)))
   (#t				#f)))

