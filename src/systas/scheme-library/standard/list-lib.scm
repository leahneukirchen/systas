;;; list-lib.scm - SRFI-1 ("List Library" by Olin Shivers)
;;;		   plus extensions
;;;		   "***" marks extensions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (standard list-lib))



(define-public cons cons)
(define-public list list)

;; xcons d a -> pair 
;; 
;; 	(lambda (d a) (cons a d))
;; 
;; Of utility only as a value to be conveniently passed to
;; higher-order procedures.
;; 
;;	(xcons '(b c) 'a) => (a b c)
;; 
;; The name stands for "eXchanged CONS." 
;; 
(define-public (xcons d a) (cons a d))


(define-public cons* cons*)
(define-public make-list make-list)

;; list-tabulate n init-proc -> list 
;; 
;; Returns an n-element list. Element i of the list, where 0 <= i < n,
;; is produced by (init-proc i). No guarantee is made about the
;; dynamic order in which init-proc is applied to these indices.
;; 
;; 	(list-tabulate 4 values) => (0 1 2 3)
;; 
(define-public (list-tabulate n proc)
  (map proc (range n)))


(define-public list-copy list-copy)

;; circular-list elt1 elt2 ... -> list 
;; 
;; Constructs a circular list of the elements. 
;; 
;; 	(circular-list 'z 'q) => (z q z q z q ...)
;; 
(define-public (circular-list first . rest)
  (let ((l	(cons first rest)))
    (set-cdr! (last-pair l) l)
    l))

;; iota count [start step] -> list 
;; 
;; Returns a list containing the elements 
;; 
;;	(start start+step ... start+(count-1)*step)
;; 
;; The start and step parameters default to 0 and 1,
;; respectively. This procedure takes its name from the
;; APL primitive. 
;; 
;;	(iota 5) => (0 1 2 3 4)
;;	(iota 5 0 -0.1) => (0 -0.1 -0.2 -0.3 -0.4)
;; 
(define-public (iota count :optional start step)
  (let ((start		(or start 0))
	(step		(or step 1)))
    (list-tabulate count (lambda (n) (+ start (* step n))))))


(define-public pair? pair?)
(define-public null? null?)

;; proper-list? x -> boolean 
;; 
;; Returns true iff x is a proper list -- a finite, nil-terminated list. 
;; 
;; More carefully: The empty list is a proper list. A pair whose cdr
;; is a proper list is also a proper list:
;; 
;;	<proper-list> ::= ()                            (Empty proper list)
;;		          | (cons <x> <proper-list>)    (Proper-list pair)
;; 
;; Note that this definition rules out circular lists. This function is required to detect this case and return
;; false. 
;; 
;; Nil-terminated lists are called "proper" lists by R5RS and Common
;; Lisp. The opposite of proper is improper.
;; 
;; R5RS binds this function to the variable list?. 
;; 
;; 	(not (proper-list? x)) = (or (dotted-list? x) (circular-list? x))
;; 
(define-public proper-list? list?)


;; circular-list? x -> boolean 
;; 
;; True if x is a circular list. A circular list is a value such that
;; for every n >= 0, cdrn(x) is a pair.
;; 
;; Terminology: The opposite of circular is finite. 
;; 
;;	(not (circular-list? x)) = (or (proper-list? x) (dotted-list? x))
;; 
(define-public (circular-list? x)
  (and (pair? x)
       (not (length+ x))))


;; dotted-list? x -> boolean 
;; 
;; True if x is a finite, non-nil-terminated list. That is, there
;; exists an n >= 0 such that cdrn(x) is neither a pair nor (). This
;; includes non-pair, non-() values (e.g. symbols, numbers), which are
;; considered to be dotted lists of length 0.
;; 
;;	(not (dotted-list? x)) = (or (proper-list? x) (circular-list? x))
;; 
(define-public (dotted-list? x)
  (not (or (proper-list? x)
	   (circular-list? x))))


;; not-pair? x -> boolean 
;; 
;;	(lambda (x) (not (pair? x)))
;; 
;; Provided as a procedure as it can be useful as the termination
;; condition for list-processing procedures that wish to handle all
;; finite lists, both proper and dotted.
;; 
(define-public (not-pair? x) (not pair? x))

;; null-list? list -> boolean 
;; 
;; List is a proper or circular list. This procedure returns true if
;; the argument is the empty list (), and false otherwise. It is an
;; error to pass this procedure a value which is not a proper or
;; circular list. This procedure is recommended as the termination
;; condition for list-processing procedures that are not defined on
;; dotted lists.
;; 
(define-public (null-list? x)	(null? x))


;; list= elt= list1 ... -> boolean 
;; 
;; Determines list equality, given an element-equality
;; procedure. Proper list A equals proper list B if they are of the
;; same length, and their corresponding elements are equal, as
;; determined by elt=. If the element-comparison procedure's first
;; argument is from listi, then its second argument is from listi+1,
;; i.e.  it is always called as (elt= a b) for a an element of list A,
;; and b an element of list B.
;; 
;; In the n-ary case, every listi is compared to listi+1 (as opposed,
;; for example, to comparing list1 to every listi, for i>1). If there
;; are no list arguments at all, list= simply returns true.
;; 
;; It is an error to apply list= to anything except proper
;; lists. While implementations may choose to extend it to circular
;; lists, note that it cannot reasonably be extended to dotted lists,
;; as it provides no way to specify an equality procedure for
;; comparing the list terminators.
;; 
;; Note that the dynamic order in which the elt= procedure is applied
;; to pairs of elements is not specified.  For example, if list= is
;; applied to three lists, A, B, and C, it may first completely
;; compare A to B, then compare B to C, or it may compare the first
;; elements of A and B, then the first elements of B and C, then the
;; second elements of A and B, and so forth.
;; 
;; The equality procedure must be consistent with eq?. That is, it
;; must be the case that
;; 
;; 	(eq? x y) => (elt= x y). 
;; 
;; Note that this implies that two lists which are eq? are always
;; list=, as well; implementations may exploit this fact to
;; "short-cut" the element-by-element comparisons.
;; 
;; 	(list= eq?) => #t       ; Trivial cases
;; 	(list= eq? '(a)) => #t
;; 
(define-public (list= elt= . lists)
  (cond
   ((null? lists)		#t)
   (#t
    (let ((list1 (car lists)))
      (let loop-others ((others	(cdr lists)))
	(cond
	 ((null? others)		#t)
	 (#t
	  (let loop-list1 ((l1 list1)
			   (l2 (car others)))
	    (cond
	     ((null? l1)		(and (null? l2)
					     (loop-others (cdr others))))
	     ((null? l2)		#f)
	     ((elt= (car l1) (car l2))	(loop-list1 (cdr l1) (cdr l2)))
	     (#t			#f))))))))))


(define-public car car)
(define-public cdr cdr)

(define-public caar caar)
(define-public cadr cadr)
(define-public cdar cdar)
(define-public cddr cddr)

(define-public caaar caaar)
(define-public caadr caadr)
(define-public cadar cadar)
(define-public caddr caddr)
(define-public cdaar cdaar)
(define-public cdadr cdadr)
(define-public cddar cddar)
(define-public cdddr cdddr)

(define-public caaaar caaaar)
(define-public caaadr caaadr)
(define-public caadar caadar)
(define-public caaddr caaddr)
(define-public cadaar cadaar)
(define-public cadadr cadadr)
(define-public caddar caddar)
(define-public cadddr cadddr)
(define-public cdaaar cdaaar)
(define-public cdaadr cdaadr)
(define-public cdadar cdadar)
(define-public cdaddr cdaddr)
(define-public cddaar cddaar)
(define-public cddadr cddadr)
(define-public cdddar cdddar)
(define-public cddddr cddddr)

(define-public set-car! set-car!)
(define-public set-cdr! set-cdr!)

(define-public list-ref list-ref)

(define-public (first l) (car l))
(define-public (second l) (cadr l))
(define-public (third l) (caddr l))
(define-public (fourth l) (cadddr l))
(define-public (fifth l) (list-ref l 4))
(define-public (sixth l) (list-ref l 5))
(define-public (seventh l) (list-ref l 6))
(define-public (eighth l) (list-ref l 7))
(define-public (ninth l) (list-ref l 8))
(define-public (tenth l) (list-ref l 9))



;; car+cdr pair -> [x y] 
;; 
;; The fundamental pair deconstructor: 
;; 
;;	(lambda (p) (values (car p) (cdr p)))
;; 
(define-public (car+cdr: x ret) (ret (car x) (cdr x)))
(define-public (car+cdr x) (car+cdr: x values))

;; take x i -> list 
;; 
;; take returns the first i elements of list x.
;; 
;; 	(take '(a b c d e)  2) => (a b)
;; 
;; x may be any value -- a proper, circular, or dotted list: 
;; 
;; 	(take '(1 2 3 . d) 2) => (1 2)
;; 	(take '(1 2 3 . d) 3) => (1 2 3)
;; 
;; For a legal i, take and drop partition a list in a manner which
;; can be inverted with append:
;; 
;; 	(append (take x i) (drop x i)) = x
;; 
(define-public (take x i) (list-head x i))


;; drop x i -> object 
;; 
;; drop returns all but the first i elements of list x. 
;; 
;; 	(drop '(a b c d e)  2) => (c d e)
;; 
;; x may be any value -- a proper, circular, or dotted list: 
;; 
;; 	(drop '(1 2 3 . d) 2) => (3 . d)
;; 	(drop '(1 2 3 . d) 3) => d
;; 
;; For a legal i, take and drop partition a list in a manner which
;; can be inverted with append:
;; 
;; 	(append (take x i) (drop x i)) = x
;; 
;; drop is exactly equivalent to performing i cdr operations on x; the
;; returned value shares a common tail with x. 
;; 
(define-public (drop x i) (list-tail x i))


;; take-right flist i -> object 
;; 
;; take-right returns the last i elements of flist.
;; 
;; 	(take-right '(a b c d e) 2) => (d e)
;; 
;; The returned list may share a common tail with the argument list.
;; 
;; flist may be any finite list, either proper or dotted: 
;; 
;; 	(take-right '(1 2 3 . d) 2) => (2 3 . d)
;; 	(take-right '(1 2 3 . d) 0) => d
;; 
;; For a legal i, take-right and drop-right partition the list in a
;; manner which can be inverted with append:
;; 
;; 	(append (take flist i) (drop flist i)) = flist
;; 
;; take-right's return value is guaranteed to share a common tail with
;; flist. If the argument is a list of non-zero length, drop-right is
;; guaranteed to return a freshly-allocated list, even in the case
;; where nothing is dropped, e.g. (drop-right lis 0).
;; 
(define-public (take-right flist i)
  (drop flist (- (length+ flist) i)))


;; drop-right flist i -> list 
;; 
;; drop-right returns all but the last i elements of flist. 
;; 
;; 	(drop-right '(a b c d e) 2) => (a b c)
;; 
;; The returned list may share a common tail with the argument list. 
;; 
;; flist may be any finite list, either proper or dotted: 
;; 
;; 	(drop-right '(1 2 3 . d) 2) => (1)
;; 	(drop-right '(1 2 3 . d) 0) => (1 2 3)
;; 
;; For a legal i, take-right and drop-right partition the list in a
;; manner which can be inverted with append:
;; 
;; 	(append (take flist i) (drop flist i)) = flist
;; 
;; take-right's return value is guaranteed to share a common tail with
;; flist. If the argument is a list of non-zero length, drop-right is
;; guaranteed to return a freshly-allocated list, even in the case
;; where nothing is dropped, e.g. (drop-right lis 0).
;; 
(define-public (drop-right flist i)
  (take flist (- (length+ flist) i)))

;; take! x i -> list 
;; 
;; take! is a destructive variants of take.
;; 
;; If x is circular, take! will return a shorter-than-expected list: 
;; 
;; 	(take! (circular-list 1 3 5) 8) => (1 3)
;; 
(define-public (take! x i)
  (if (> i 0)
      (let ((p 	(list-tail x (1- i))))
	(set-cdr! p ())))
  x)

;; drop-right! flist i -> list 
;; 
;; drop-right! is a destructive variant of drop-right.
;; 
(define-public (drop-right! flist i)
  (take! flist (- (length+ flist) i)))


;; split-at  x i -> [list object] 
;; 
;; split-at splits the list x at index i, returning a list of the
;; first i elements, and the remaining tail. It is equivalent to
;; 
;; 	(values (take x i) (drop x i))
;; 
;; So,
;; 
;; 	(split-at '(a b c d e f g h) 3)
;;	 => #<values (a b c) (d e f g h)>
;; 
(define-public (split-at: x i ret)
  (ret (take x i) (drop x i)))

(define-public (split-at x i)
  (split-at: x i values))


;; split-at! x i -> [list object] 
;; 
;; split-at! desctructively splits the list x at index i, returning a
;; list of the first i elements, and the remaining tail. 
;; 
(define-public (split-at!: x i ret)
  (let ((d (drop x i)))
    (ret (take! x i) d)))

(define-public (split-at! x i)
  (split-at!: x i values))


;; last pair -> object 
;; 
;; last returns the last element of the non-empty, finite list
;; pair. 
;; 
;; 	(last '(a b c)) => c
;; 
(define-public (last l) (car (last-pair l)))

(define-public last-pair last-pair)
(define-public length list-length)
(define-public length+ list-length+)
(define-public append append)
(define-public append! append!)

;; concatenate  list-of-lists -> value 
;; 
;; Append the elements of the together. That is, concatenate returns
;; 
;; 	(apply append list-of-lists)
;; 
(define-public (concatenate lol) (apply append lol))

;; concatenate! list-of-lists -> value 
;; 
;; 	(apply append! list-of-lists)
;; 
(define-public (concatenate! lol) (apply append! lol))

(define-public reverse reverse)
(define-public reverse! reverse!)

;; append-reverse  rev-head tail -> list 
;; 
;; append-reverse returns 
;; 
;;	(append (reverse rev-head) tail)
;; 
;; It is provided because it is a common operation -- a common
;; list-processing style calls for this exact operation to transfer
;; values accumulated in reverse order onto the front of another list,
;; and because the implementation is significantly more efficient than
;; the simple composition it replaces. (But note that this pattern of
;; iterative computation followed by a reverse can frequently be
;; rewritten as a recursion.)
;;
(define-public (append-reverse rev-head tail) (append (reverse rev-head) tail))


;; append-reverse!  rev-head tail -> list 
;; 
;; append-reverse! returns 
;; 
;;	(append! (reverse! rev-head) tail)
;; 
;; It is provided because it is a common operation -- a common
;; list-processing style calls for this exact operation to transfer
;; values accumulated in reverse order onto the front of another list,
;; and because the implementation is significantly more efficient than
;; the simple composition it replaces. (But note that this pattern of
;; iterative computation followed by a reverse can frequently be
;; rewritten as a recursion.)
;;
(define-public (append-reverse! rev-head tail) (append! (reverse! rev-head) tail))


;; zip clist1 clist2 ... -> list 
;; 
;; 	(lambda lists (apply map list lists))
;; 
;; If zip is passed n lists, it returns a list as long as the shortest
;; of these lists, each element of which is an n-element list
;; comprised of the corresponding elements from the parameter lists.
;; 
;; 	(zip '(one two three) 
;; 	     '(1 2 3)
;; 	     '(odd even odd even odd even odd even))
;; 	=> ((one 1 odd) (two 2 even) (three 3 odd))
;; 
;; 	(zip '(1 2 3))
;;	 => ((1) (2) (3))
;; 
;; At least one of the argument lists must be finite: 
;; 
;; 	(zip '(3 1 4 1) (circular-list #f #t)) 
;; 	=> ((3 #f) (1 #t) (4 #f) (1 #t))
;; 
(define-public (zip . lists) (apply map list lists))


;; unzip1 list -> list 
;; unzip2 list -> [list list] 
;; unzip3 list -> [list list list] 
;; unzip4 list -> [list list list list] 
;; 
;; unzip1 takes a list of lists, where every list must contain at
;; least one element, and returns a list containing the initial
;; element of each such list. That is, it returns 
;; 
;; 	(map car lists)
;; 
;; unzip2 takes a list of lists, where every list must contain at
;; least two elements, and returns two values: a list of the first
;; elements, and a list of the second elements. unzip3 does the same
;; for the first three elements of the lists, and so forth.
;; 
;; 	(unzip2 '((1 one) (2 two) (3 three)))
;;	 => #<values (1 2 3) (one two three)>
;; 
(define-public (unzip2: lst ret) (ret (map car lst) (map cadr lst)))
(define-public (unzip3: lst ret) (ret (map car lst) (map cadr lst) (map cadr lst) (map caddr lst)))
(define-public (unzip4: lst ret) (ret (map car lst) (map cadr lst) (map cadr lst) (map caddr lst) (map cadddr lst)))
(define-public (unzip5: lst ret) (ret (map car lst) (map cadr lst) (map cadr lst) (map caddr lst) (map cadddr lst) (map (lambda (x) (car (cddddr x))) lst)))

(define-public (unzip1 lst) (map car lst))
(define-public (unzip2 lst) (unzip2: lst values))
(define-public (unzip3 lst) (unzip3: lst values))
(define-public (unzip4 lst) (unzip4: lst values))
(define-public (unzip5 lst) (unzip5: lst values))

;; count pred clist1 clist2 -> integer 
;; 
;; pred is a procedure taking as many arguments as there are lists and
;; returning a single value. It is applied element-wise to the
;; elements of the lists, and a count is tallied of the number of
;; elements that produce a true value. This count is returned. count
;; is "iterative" in that it is guaranteed to apply pred to the list
;; elements in a left-to-right order. The counting stops when the
;; shortest list expires.
;; 
;; 	(count even? '(3 1 4 1 5 9 2 5 6)) => 3
;; 	(count < '(1 2 4 8) '(2 4 6 8 10 12 14 16)) => 3
;; 
;; At least one of the argument lists must be finite: 
;; 
;; 	(count < '(3 1 4 1) (circular-list 1 10)) => 2
;; 

(define-public (count pred . lists)
  (apply xfold
	 (lambda (total . values) (if (apply pred values) (1+ total) total))
	 0
	 lists))

;; map proc clist1 clist2 ... -> list 
;; 
;; [R5RS+] proc is a procedure taking as many arguments as there are
;; list arguments and returning a single value. map applies proc
;; element-wise to the elements of the lists and returns a list of the
;; results, in order.  The dynamic order in which proc is applied to
;; the elements of the lists is unspecified.
;; 
;; 	(map cadr '((a b) (d e) (g h))) =>  (b e h)
;; 
;; 	(map (lambda (n) (expt n n))
;; 	     '(1 2 3 4 5))
;; 	=>  (1 4 27 256 3125)
;; 
;; 	(map + '(1 2 3) '(4 5 6)) =>  (5 7 9)
;; 
;; 	(let ((count 0))
;; 	  (map (lambda (ignored)
;; 		 (set! count (+ count 1))
;; 		 count)
;; 	       '(a b)))
;;	=>  (1 2) or (2 1)
;; 
;; This procedure is extended from its R5RS specification to allow the
;; arguments to be of unequal length; it terminates when the shortest
;; list runs out.
;; 
;; At least one of the argument lists must be finite: 
;; 
;; 	(map + '(3 1 4 1) (circular-list 1 0)) => (4 1 5 1)
;; 
(define-public map map)

;; for-each proc clist1 clist2 ... -> unspecified 
;; 
;; [R5RS+] The arguments to for-each are like the arguments to map,
;; but for-each calls proc for its side effects rather than for its
;; values. Unlike map, for-each is guaranteed to call proc on the
;; elements of the lists in order from the first element(s) to the
;; last, and the value returned by for-each is unspecified.
;; 
;; This procedure is extended from its R5RS specification to allow the
;; arguments to be of unequal length; it terminates when the shortest
;; list runs out.
;; 
;; At least one of the argument lists must be finite. 
;; 
(define-public for-each for-each)


;; fold kons knil clist1 clist2 ... -> value 
;; The fundamental list iterator. 
;; 
;; First, consider the single list-parameter case. If clist1 = (e1 e2
;; ... en), then this procedure returns
;; 
;; 	(kons en ... (kons e2 (kons e1 knil)) ... ) 
;; 
;; That is, it obeys the (tail) recursion 
;; 
;; 	(fold kons knil lis) = (fold kons (kons (car lis) knil) (cdr lis))
;; 	(fold kons knil '()) = knil
;; 
;; Examples: 
;; 
;; 	(fold + 0 lis)                  ; Add up the elements of LIS.
;; 	
;; 	(fold cons '() lis)             ; Reverse LIS.
;; 	
;; 	(fold cons tail rev-head)       ; See APPEND-REVERSE.
;; 	
;; 	;; How many symbols in LIS?
;; 	(fold (lambda (x count) (if (symbol? x) (+ count 1) count))
;; 	       0
;; 	       lis)
;; 	
;; 	;; Length of the longest string in LIS:
;; 	(fold (lambda (s max-len) (max max-len (string-length s)))
;; 	      0
;; 	      lis)
;; 
;; If n list arguments are provided, then the kons function must take
;; n+1 parameters: one element from each list, and the "seed" or fold
;; state, which is initially knil. The fold operation terminates when
;; the shortest list runs out of values:
;; 
;; 	(fold cons* '() '(a b c) '(1 2 3 4 5)) => (c 3 b 2 a 1)
;; 
;; At least one of the list arguments must be finite. 
;;
(define-public (fold kons knil . lists)
  (if (or-map null? lists)
      knil
      (apply fold kons (apply kons (append! (map car lists) (cons knil ()))) (map cdr lists))))
(define-public fold-iterative fold)


;; xfold xkons xknil clist1 clist2 ... -> value 
;; 
;; The fundamental list iterator, with alternate argument orders. 
;; 
;; First, consider the single list-parameter case. If clist1 = (e1 e2
;; ... en), then this procedure returns
;; 
;; 	(xkons (xkons ... (xkons (xkons xknil e1) e2)) en) 
;; 
;; That is, it obeys the (tail) recursion 
;; 
;; 	(xfold xkons xknil lis) = (xfold xkons (xkons xknil (car lis)) (cdr lis))
;; 	(xfold xkons xknil '()) = knil
;; 
;; Examples: 
;; 
;; 	(xfold + 0 lis)                  ; Add up the elements of LIS.
;; 	
;; 	(xfold xcons '() lis)             ; Reverse LIS.
;; 	
;; 	(xfold xcons tail rev-head)       ; See APPEND-REVERSE.
;; 	
;; 	;; How many symbols in LIS?
;; 	(xfold (lambda (count x) (if (symbol? x) (+ count 1) count))
;; 	       0
;; 	       lis)
;; 	
;; 	;; Length of the longest string in LIS:
;; 	(xfold (lambda (max-len s) (max max-len (string-length s)))
;; 	       0
;; 	       lis)
;; 
;; If n list arguments are provided, then the xkons function must take
;; n+1 parameters: the "seed" or fold state, which is initially knil,
;; and one element from each list. The xfold operation terminates
;; when the shortest list runs out of values.
;;
(define-public (xfold xkons xknil . lists)
  (if (or-map null? lists)
      xknil
      (apply xfold xkons (apply xkons xknil (map car lists)) (map cdr lists))))
(define-public xfold-iterative xfold)


;; fold-right kons knil clist1 clist2 ... -> value 
;; 
;; The fundamental list recursion operator. 
;; 
;; First, consider the single list-parameter case. If clist1 = (e1 e2
;; ... en), then this procedure returns
;; 
;; 	(kons e1 (kons e2 ... (kons en knil))) 
;; 
;; That is, it obeys the recursion 
;; 
;; 	(fold-right kons knil lis) = (kons (car lis) (fold-right kons knil (cdr lis)))
;; 	(fold-right kons knil '()) = knil
;; 
;; Examples: 
;; 
;; 	(fold-right cons '() lis)               ; Copy LIS.
;; 
;; 	;; Filter the even numbers out of LIS.
;; 	(fold-right (lambda (x l) (if (even? x) (cons x l) l)) '() lis))
;; 
;; If n list arguments are provided, then the kons function must take
;; n+1 parameters: one element from each list, and the "seed" or fold
;; state, which is initially knil. The fold operation terminates when
;; the shortest list runs out of values:
;; 
;; 	(fold-right cons* '() '(a b c) '(1 2 3 4 5)) => (a 1 b 2 c 3)
;; 
;; At least one of the list arguments must be finite. 
;; 
(define-public (fold-right kons knil . lists)
  (if (or-map null? lists)
      knil
      (apply kons (append! (map car lists) (apply fold-right kons knil (map cdr lists))))))
(define-public fold-recursive fold-right)

;; xfold-right kcons knil . lists
;; 
;; Like `fold-right', except that the accumulated value is the first
;; argument to `kcons' instead of the last.
;; 
(define-public (xfold-right kons knil . lists)
  (if (or-map null? lists)
      knil
      (apply kons (apply fold-right kons knil (map cdr lists)) (map car lists))))
(define-public xfold-recursive xfold-right)


;; reduce f ridentity list -> value 
;; 
;; reduce is a variant of fold. 
;; 
;; ridentity should be a "right identity" of the procedure f -- that
;; is, for any value x acceptable to f,
;; 
;; (f x ridentity) = x
;; 
;; reduce has the following definition: 
;; 
;; 	If list = (), return ridentity;
;; 
;; 	Otherwise, return (fold f (car list) (cdr list)). 
;;	  ...in other words, we compute (fold f ridentity list). 
;; 
;; Note that ridentity is used only in the empty-list case. You
;; typically use reduce when applying f is expensive and you'd like to
;; avoid the extra application incurred when fold applies f to the
;; head of list and the identity value, redundantly producing the same
;; value passed in to f. For example, if f involves searching a file
;; directory or performing a database query, this can be
;; significant. In general, however, fold is useful in many contexts
;; where reduce is not (consider the examples given in the fold
;; definition -- only one of the five folds uses a function with a
;; right identity. The other four may not be performed with reduce).
;; 
;; Note: MIT Scheme and Haskell flip F's arg order for their reduce
;; and fold functions.  SLIB uses the name `reduce' for a different
;; function and the name `reduce-init' for what we call `reduce'.
;; 
;; 	;; Take the max of a list of non-negative integers.
;;
;;	 (reduce max 0 nums) ; == (apply max 0 nums)
;;
(define-public (reduce f ridentity list)
  (cond
   ((null? list)	ridentity)
   (#t			(fold f (car list) (cdr list)))))
(define-public reduce-iterative reduce)


;; xreduce f lidentity list -> value 
;; 
;; xreduce is a variant of xfold. 
;; 
;; lidentity should be a "left identity" of the procedure f -- that
;; is, for any value x acceptable to f,
;; 
;; (f lidentity x) = x
;; 
;; xreduce has the following definition: 
;; 
;; 	If list = (), return lidentity;
;; 
;; 	Otherwise, return (xfold f (car list) (cdr list)). 
;;	  ...in other words, we compute (fold f lidentity list). 
;; 
;; Note that lidentity is used only in the empty-list case. You
;; typically use reduce when applying f is expensive and you'd like to
;; avoid the extra application incurred when fold applies f to the
;; head of list and the identity value, redundantly producing the same
;; value passed in to f. For example, if f involves searching a file
;; directory or performing a database query, this can be
;; significant. In general, however, fold is useful in many contexts
;; where reduce is not (consider the examples given in the fold
;; definition -- only one of the five folds uses a function with a
;; right identity. The other four may not be performed with reduce).
;; 
;; Note: MIT Scheme and Haskell flip F's arg order for their reduce
;; and fold functions.  SLIB uses the name `reduce' for a different
;; function and the name `reduce-init' for what we call `reduce'.
;; 
;; 	;; Take the max of a list of non-negative integers.
;;
;;	 (reduce max 0 nums) ; == (apply max 0 nums)
;;
(define-public (xreduce f lidentity list)
  (cond
   ((null? list)	lidentity)
   (#t			(xfold f (car list) (cdr list)))))
(define-public xreduce-iterative xreduce)


;; reduce-right f ridentity list -> value 
;; 
;; reduce-right is the fold-right variant of reduce. It obeys the
;; following definition:
;; 
;;        (reduce-right f ridentity '()) = ridentity
;;        (reduce-right f ridentity '(e1)) = e1 ;; which should be (f e1 ridentity)
;;        (reduce-right f ridentity '(e1 e2 ...)) =
;;            (f e1 (reduce f ridentity (e2 ...)))
;; 
;; ...in other words, we compute (fold-right f ridentity list). 
;; 
;;        ;; Append a bunch of lists together.
;;        ;; I.e., (apply append list-of-lists)
;;        (reduce-right append '() list-of-lists)
;; 
(define-public (reduce-right f ridentity list)
  (cond
   ((null? list)	ridentity)
   ((null? (cdr list))	(car list))
   (#t			(fold-right f ridentity list))))
(define-public reduce-right-recursive reduce-right)

(define-public (xreduce-right f lidentity list)
  (cond
   ((null? list)	lidentity)
   ((null? (cdr list))	(car list))
   (#t			(xfold-right f lidentity list))))
(define-public xreduce-right-recursive xreduce-right)



;; pair-fold kons knil clist1 clist2 ... -> value 
;; 
;; Analogous to fold, but kons is applied to successive sublists of
;; the lists, rather than successive elements -- that is, kons is
;; applied to the pairs making up the lists, giving this (tail)
;; recursion:
;; 
;;	(pair-fold kons knil lis) = (let ((tail (cdr lis)))
;;				      (pair-fold kons (kons lis knil) tail))
;; 
;;	(pair-fold kons knil '()) = knil
;; 
;; For finite lists, the kons function may reliably apply set-cdr! to
;; the pairs it is given without altering the sequence of execution.
;; 
;; Example: 
;; 
;;	;;; Destructively reverse a list.
;;	(pair-fold (lambda (pair tail) (set-cdr! pair tail) pair) '() lis)
;; 
;; At least one of the list arguments must be finite. 
;; 
(define-public (pair-fold kons knil . lists)
  (if (or-map null? lists)
      knil
      (let ((cdrs	(map cdr lists)))
	(apply pair-fold kons (apply kons (append lists (cons knil ()))) cdrs))))
(define-public pair-fold-iterative pair-fold)

;; xpair-fold kcons knil . lists
;; 
;; Like `pair-fold', except that the accumulated value is the first
;; argument to `kcons' instead of the last.
;; 
(define-public (xpair-fold kons knil . lists)
  (if (or-map null? lists)
      knil
      (let ((cdrs	(map cdr lists)))
	(apply pair-fold kons (apply kons knil lists) cdrs))))
(define-public xpair-fold-iterative xpair-fold)



;; pair-fold-right kons knil clist1 clist2 ... -> value 
;; 
;; Holds the same relationship with fold-right that pair-fold holds
;; with fold. Obeys the recursion
;; 
;;        (pair-fold-right kons knil lis) = 
;;            (kons lis (pair-fold-right kons knil (cdr lis)))
;;        (pair-fold-right kons knil '()) = knil
;; 
;; Example: 
;; 
;;        (pair-fold-right cons '() '(a b c)) => ((a b c) (b c) (c))
;; 
;; At least one of the list arguments must be finite. 
;; 
(define-public (pair-fold-right kons knil . lists)
  (if (or-map null? lists)
      knil
      (let ((cdrs	(map cdr lists)))
	(apply kons (append lists (pair-fold-right kons knil cdrs))))))
(define-public pair-fold-recursive pair-fold-right)

(define-public (xpair-fold-right kons knil . lists)
  (if (or-map null? lists)
      knil
      (let ((cdrs	(map cdr lists)))
	(apply kons (xpair-fold-right kons knil cdrs) lists))))
(define-public xpair-fold-recursive xpair-fold-right)



;; unfold p f g seed [tail-gen] -> list 
;; 
;; unfold is best described by its basic recursion: 
;; 
;; 	(unfold p f g seed) = 
;; 	(if (p seed) 
;;	    (tail-gen seed)
;; 	    (cons (f seed)
;; 		  (unfold p f g (g seed))))
;;
;; `p' determines when to stop unfolding.  
;; 		
;; `f' maps each seed value to the corresponding list element. 
;; 
;; `g' maps each seed value to next seed value. 
;; 
;; `seed ' is the "state" value for the unfold. 
;; 
;; `tail-gen' creates the tail of the list; defaults to (lambda (x) '()) 
;; 
;; In other words, we use `g' to generate a sequence of seed values
;; seed, g(seed), g2(seed), g3(seed), ...  These seed values are
;; mapped to list elements by f, producing the elements of the result
;; list in a left-to-right order. P says when to stop.
;; 
;; unfold is the fundamental recursive list constructor, just as
;; fold-right is the fundamental recursive list consumer. While unfold
;; may seem a bit abstract to novice functional programmers, it can be
;; used in a number of ways:
;; 
;; List of squares: 1^2 ... 10^2
;; 
;;              (unfold (lambda (x) (> x 10))
;;                      (lambda (x) (* x x))
;;                      (lambda (x) (+ x 1))
;;                      1)
;;                              
;;              (unfold null-list? car cdr lis) ; Copy a proper list.
;; 
;;              ;; Read current input port into a list of values.
;;              (unfold eof-object? values (lambda (x) (read)) (read))
;; 
;;              ;; Copy a possibly non-proper list:
;;              (unfold not-pair? car cdr lis values)
;; 
;;              ;; Append HEAD onto TAIL:
;;              (unfold null-list? car cdr head (lambda (x) tail))
;; 
;; Interested functional programmers may enjoy noting that fold-right
;; and unfold are in some sense inverses. That is, given operations
;; knull?, kar, kdr, kons, and knil satisfying 
;;
;;	(kons (kar x) (kdr x)) = x
;; 
;; and 
;; 
;;	(knull? knil) = #t
;; 
;; then 
;; 
;;	(fold-right kons knil (unfold knull? kar kdr x)) = x 
;; 
;; and
;; 	(unfold knull? kar kdr (fold-right kons knil x)) = x.  
;; 
;; This is sometimes called an "anamorphism;" when an explicit
;; tail-gen procedure is supplied, it is called an "apomorphism."
;; 
(define-public (unfold p f g seed . tail-gen)
  (if (p seed) 
      ((or tail-gen (lambda (x) ())) seed)
      (cons (f seed) (unfold p f g (g seed)))))
(define-public unfold-recursive unfold)


;; unfold-right p f g seed [tail] -> list 
;; 
;; unfold-right constructs a list with the following loop: 
;; 
;;      (let lp ((seed seed) (lis tail))
;;        (if (p seed) lis
;;            (lp (g seed)
;;                (cons (f seed) lis))))
;; 
;;  p 
;;      Determines when to stop unfolding. 
;;  f 
;;      Maps each seed value to the corresponding list element. 
;;  g 
;;      Maps each seed value to next seed value. 
;;  seed 
;;      The "state" value for the unfold. 
;;  tail 
;;      list terminator; defaults to '(). 
;; 
;; In other words, we use g to generate a sequence of seed values 
;; 
;; 	seed, g(seed), g2(seed), g3(seed), ... 
;; 
;; These seed values are mapped to list elements by f, producing the
;; elements of the result list in a right-to-left order. P says when
;; to stop.
;; 
;; unfold-right is the fundamental iterative list constructor, just as
;; fold is the fundamental iterative list consumer. While unfold-right
;; may seem a bit abstract to novice functional programmers, it can be
;; used in a number of ways:
;; 
;;        ;; List of squares: 1^2 ... 10^2
;;        (unfold-right zero? 
;;                      (lambda (x) (* x x))
;;                      (lambda (x) (- x 1))
;;                      10)
;; 
;;        ;; Reverse a proper list.
;;        (unfold-right null-list? car cdr lis)
;; 
;;        ;; Read current input port into a list of values.
;;        (unfold-right eof-object? values (lambda (x) (read)) (read))
;; 
;;        ;; (append-reverse rev-head tail)
;;        (unfold-right null-list? car cdr rev-head tail)
;; 
;; Interested functional programmers may enjoy noting that fold and
;; unfold-right are in some sense inverses. That is, given operations
;; knull?, kar, kdr, kons, and knil satisfying
;; 
;;        (kons (kar x) (kdr x)) = x and (knull? knil) = #t 
;; 
;; then 
;; 
;;        (fold kons knil (unfold-right knull? kar kdr x)) = x 
;; 
;; and 
;;        (unfold-right knull? kar kdr (fold kons knil x)) = x. 
;; 
;; This combinator presumably has some pretentious mathematical name;
;; interested readers are invited to communicate it to the author.
;; 
(define-public (unfold-right pred fn inc seed :optional tail)
  (let lp ((seed seed)
	   (lis tail))
    (if (p seed)
	lis
	(lp (g seed)
	    (cons (f seed) lis)))))
(define-public unfold-iterative unfold-right)

;; append-map  f clist1 clist2 ... -> value 
;; append-map! f clist1 clist2 ... -> value 
;; 
;; Equivalent to 
;; 
;; 	(apply append (map f clist1 clist2 ...)) 
;; 
;; and 
;; 
;; 	(apply append! (map f clist1 clist2 ...)) 
;; 
;; Map f over the elements of the lists, just as in the map
;; function. However, the results of the applications are appended
;; together to make the final result. append-map uses append to append
;; the results together; append-map! uses append!.
;; 
;; The dynamic order in which the various applications of f are made
;; is not specified.
;; 
;; Example: 
;; 
;; 	(append-map! (lambda (x) (list x (- x))) '(1 3 8))
;; 	=> (1 -1 3 -3 8 -8)
;; 
;; At least one of the list arguments must be finite. 
;; 

(define-public (append-map f . lists) (apply append (apply map f lists)))
(define-public (append-map! f . lists) (apply append (apply map! f lists)))


;; map! f list1 clist2 ... -> list 
;; 
;; Linear-update variant of map -- map! is allowed, but not required,
;; to alter the cons cells of list1 to construct the result list.
;; 
;; The dynamic order in which the various applications of f are made
;; is not specified. In the n-ary case, clist2, clist3, ... must have
;; at least as many elements as list1.
;; 
(define-public (map! f l1 . lists)
  (let loop ((pos 	l1)
	     (lists	lists))
    (if (null? pos)
	l1
	(begin
	  (set-car! pos (apply f (car pos) (map car lists)))
	  (loop (cdr pos) (map cdr lists))))))


;; map-in-order f clist1 clist2 ... -> list 
;; 
;; A variant of the map procedure that guarantees to apply f across
;; the elements of the listi arguments in a left-to-right order. This
;; is useful for mapping procedures that both have side effects and
;; return useful values.
;; 
;; At least one of the list arguments must be finite. 
;; 
(define-public map-in-order map-in-order)


;; pair-for-each f clist1 clist2 ... -> unspecific 
;; 
;; Like for-each, but f is applied to successive sublists of the
;; argument lists. That is, f is applied to the cons cells of the
;; lists, rather than the lists' elements. These applications occur in
;; left-to-right order.
;; 
;; The f procedure may reliably apply set-cdr! to the pairs it is
;; given without altering the sequence of execution.
;; 
;; 	(pair-for-each (lambda (pair) (display pair) (newline)) '(a b c)) ==>
;; 	(a b c)
;; 	(b c)
;; 	(c)
;; 
;; At least one of the list arguments must be finite. 
;; 
(define-public (pair-for-each f . lists)
  (if (not (or-map null? lists))
      (let ((cdrs (map cdr lists)))
	(apply f lists)
	(apply pair-for-each f cdrs))))

;; filter-map f clist1 clist2 ... -> list 
;; 
;; Like map, but only true values are saved. 
;; 
;; 	(filter-map (lambda (x) (and (number? x) (* x x))) 
;;		    '(a 1 b 3 c 7))
;; 	=> (1 9 49)
;; 
;; The dynamic order in which the various applications of f are made
;; is not specified.
;; 
;; At least one of the list arguments must be finite. 
;; 
(define-public (filter-map f . lists)
  (filter (negate null?) (apply map f lists)))


;; filter pred list -> list 
;; 
;; Return all the elements of list that satisfy predicate pred. The
;; list is not disordered -- elements that appear in the result list
;; occur in the same order as they occur in the argument list. The
;; returned list may share a common tail with the argument list. The
;; dynamic order in which the various applications of pred are made is
;; not specified.
;; 
;; 	(filter even? '(0 7 8 8 43 -4)) => (0 8 8 -4)
;; 
(define-public filter filter)


;; remove pred list -> list 
;; 
;; Returns list without the elements that satisfy predicate pred:
;; 
;; 	(lambda (pred list) (filter (lambda (x) (not (pred x))) list))
;; 
;; The list is not disordered -- elements that appear in the result
;; list occur in the same order as they occur in the argument
;; list. The returned list may share a common tail with the argument
;; list. The dynamic order in which the various applications of pred
;; are made is not specified.
;; 
;; 	(remove even? '(0 7 8 8 43 -4)) => (7 43)
;; 
(define-public (remove pred lst)
  (filter (negate pred) lst))


;; partition pred list -> [list list] 
;; 
;; Partitions the elements of list with predicate pred, and returns
;; two values: the list of in-elements and the list of
;; out-elements. The list is not disordered -- elements occur in the
;; result lists in the same order as they occur in the argument
;; list. The dynamic order in which the various applications of pred
;; are made is not specified. One of the returned lists may share a
;; common tail with the argument list.
;; 
;; 	(partition symbol? '(one 2 3 four five 6)) => 
;; 	  #<values (one four five) (2 3 6)>
;;

(define-public (partition: predicate lst return)
  (let ((in 	(cons () ()))
	(out	(cons () ())))
    (let loop ((in-pos 	in)
	       (out-pos	out)
	       (lst	lst))
      (cond
       ((null? lst)	 	(return (cdr in) (cdr out)))
       ((predicate (car lst))	(set-cdr! in-pos (cons (car lst) ()))
				(loop (cdr in-pos) out-pos (cdr lst)))
       (else			(set-cdr! out-pos (cons (car lst) ()))
				(loop in-pos (cdr out-pos) (cdr lst)))))))
(define-public (partition predicate lst)
  (partition: predicate lst values))


;; filter!    pred list -> list 
;; remove!    pred list -> list 
;; partition! pred list -> [list list] 
;; 
;; Destructive variants of filter, partition and remove. These
;; procedures alter the cons cells in the argument list to construct
;; the result lists.
;; 

(define-public (filter! pred l)
  (let ((answer (cons () l)))
    (let loop ((pos answer))
      (cond
       ((null? (cdr pos))	(cdr answer))
       ((pred (cadr pos))	(loop (cdr pos)))
       (else			(set-cdr! pos (cddr pos))
				(loop pos))))))
(define-public (remove! pred l)
  (filter! (negate pred) l))

(define-public (partition!: predicate lst return)
  (let ((in 	(cons () ()))
	(out	(cons () ())))
    (let loop ((in-pos 	in)
	       (out-pos	out)
	       (lst	lst))
      (if (null? lst)
	  (return (cdr in) (cdr out))

	  (let ((rest (cdr lst)))
	    (set-cdr! lst ())
	    (cond
	     ((predicate (car lst))	(set-cdr! in-pos lst)
					(loop (cdr in-pos) out-pos rest))
	     (else			(set-cdr! out-pos lst)
					(loop in-pos (cdr out-pos) rest))))))))

(define-public (partition! predicate lst)
  (partition!: predicate lst values))



;; member x list [=] -> list 
;; memq x list -> list 
;; memv x list -> list 
;; 
;; [R5RS+] These procedures return the first sublist of list whose car
;; is x, where the sublists of list are the non-empty lists returned
;; by (drop list i) for i less than the length of list. If x does not
;; occur in list, then #f is returned. memq uses eq? to compare x with
;; the elements of list, while memv uses eqv?, and member uses equal?.
;; 
;;     (memq 'a '(a b c))			=>  (a b c)
;;     (memq 'b '(a b c))			=>  (b c)
;;     (memq 'a '(b c d))          		=>  #f
;;     (memq (list 'a) '(b (a) c)) 		=>  #f
;;     (member (list 'a) '(b (a) c))        	=>  ((a) c)
;;     (memq 101 '(100 101 102))   		=>  *unspecified*
;;     (memv 101 '(100 101 102))   		=>  (101 102)
;; 
;; member is extended from its R5RS definition to allow the client to
;; pass in an optional equality procedure = used to compare keys.
;; 
;; The comparison procedure is used to compare the elements ei of list
;; to the key x in this way:
;; 
;; 	(= x ei) ; list is (E1 ... En) 
;; 
;; That is, the first argument is always x, and the second argument is
;; one of the list elements. Thus one can reliably find the first
;; element of list that is greater than five with (member 5 list <)
;; 
;; Note that fully general list searching may be performed with the
;; find-tail and find procedures, e.g.
;; 
;; 	(find-tail even? list) ; Find the first elt with an even key.
;; 

(define-public member member)
(define-public memq memq)
(define-public memv memv)

;; ***
;; member-split: x list = return => (return head tail)
;; member-split x list [=] => head tail
;; 
;; Return two sublists of `list', a `head', which does not begin contain `x',
;; and a tail, which is () or else begins with `x'.
;; 
;; Elements are compared using `=', which defaults to `eq?':
;; 
;; 	(= x elt)
;; 
;; The `tail', if not (), shares state with `list'.  The `head' never
;; shares state with `list'.
;; 
(define-public (member-split: x lst = return)
  (let ((= 	(or = eq?))
	(answer	(cons () ())))
    (let loop ((pos answer)
	       (lst lst))
      (cond
       ((null? lst)		(return (cdr answer) ()))
       ((= x (car lst))		(return (cdr answer) lst))
       (#t			(set-cdr! pos (cons (car lst) ()))
				(loop (cdr pos) (cdr lst)))))))
(define-public (member-split x lst :optional =)
  (member-split: x lst = values))

;; ***
;; member-split!: x list = return => (return head tail)
;; member-split! x list [=] => head tail
;; 
;; 
(define-public (member-split!: x lst = return)
  (let ((= 	(or = eq?))
	(answer	(cons () ())))
    (let loop ((pos answer)
	       (lst lst))
      (cond
       ((null? lst)		(return (cdr answer) ()))
       ((= x (car lst))		(return (cdr answer) lst))
       (#t			(let ((rest (cdr lst)))
				  (set-cdr! pos lst)
				  (set-cdr! lst ())
				  (loop (cdr pos) rest)))))))
(define-public (member-split! x lst :optional =)
  (member-split!: x lst = values))

;; find pred clist -> value 
;; 
;; Return the first element of clist that satisfies predicate pred;
;; false if no element does.
;; 
;; 	(find even? '(3 1 4 1 5 9)) => 4
;; 
;; Note that find has an ambiguity in its lookup semantics -- if find
;; returns #f, you cannot tell (in general) if it found a #f element
;; that satisfied pred, or if it did not find any element at all. In
;; many situations, this ambiguity cannot arise -- either the list
;; being searched is known not to contain any #f elements, or the list
;; is guaranteed to have an element satisfying pred. However, in cases
;; where this ambiguity can arise, you should use find-tail instead of
;; find -- find-tail has no such ambiguity:
;; 
;; 	(cond ((find-tail pred lis) => (lambda (pair) ...)) ; Handle (CAR PAIR)
;; 	       (else ...)) ; Search failed.
;; 
(define-public (find pred lst)
  (and lst
       (if (pred (car lst))
	   (car lst)
	   (find pred (cdr lst)))))


;; find-tail pred clist -> pair or false 
;; 
;; Return the first pair of clist whose car satisfies pred. If no pair
;; does, return false.
;; 
;; find-tail can be viewed as a general-predicate variant of the
;; member function.
;; 
;; Examples: 
;; 
;; 	(find-tail even? '(3 1 37 -8 -5 0 0)) => (-8 -5 0 0)
;; 	(find-tail even? '(3 1 37 -5)) => #f
;; 
;; 	;; MEMBER X LIS:
;; 	(find-tail (lambda (elt) (equal? x elt)) lis)
;; 
;; In the circular-list case, this procedure "rotates" the list.
;; 
;; Find-tail is essentially drop-while, where the sense of the
;; predicate is inverted: Find-tail searches until it finds an element
;; satisfying the predicate; drop-while searches until it finds an
;; element that doesn't satisfy the predicate.
;; 
(define-public (find-tail pred lst)
  (and lst
       (if (pred (car lst))
	   lst
	   (find-tail pred (cdr lst)))))

;; any pred clist1 clist2 ... -> value 
;; 
;; Applies the predicate across the lists, returning true if the
;; predicate returns true on any application.
;; 
;; If there are n list arguments clist1 ... clistn, then pred must be
;; a procedure taking n arguments and returning a boolean result.
;; 
;; any applies pred to the first elements of the clisti parameters. If
;; this application returns a true value, any immediately returns that
;; value. Otherwise, it iterates, applying pred to the second elements
;; of the clisti parameters, then the third, and so forth. The
;; iteration stops when a true value is produced or one of the lists
;; runs out of values; in the latter case, any returns #f. The
;; application of pred to the last element of the lists is a tail
;; call.
;; 
;; Note the difference between find and any -- find returns the
;; element that satisfied the predicate; any returns the true value
;; that the predicate produced.
;; 
;; Like every, any's name does not end with a question mark -- this is
;; to indicate that it does not return a simple boolean (#t or #f),
;; but a general value.
;; 
;; 	(any integer? '(a 3 b 2.7))   => #t
;; 	(any integer? '(a 3.1 b 2.7)) => #f
;; 	(any < '(3 1 4 1 5) '(2 7 1 8 2)) => #t
;; 
(define-public (any pred . lists)
  (and (not (or-map null? lists))
       (or (apply pred (map car lists))
	   (apply any pred (map cdr lists)))))


;; every pred clist1 clist2 ... -> value 
;; 
;; Applies the predicate across the lists, returning true if the
;; predicate returns true on every application.
;; 
;; If there are n list arguments clist1 ... clistn, then pred must be
;; a procedure taking n arguments and returning a boolean result.
;; 
;; every applies pred to the first elements of the clisti
;; parameters. If this application returns false, every immediately
;; returns false. Otherwise, it iterates, applying pred to the second
;; elements of the clisti parameters, then the third, and so
;; forth. The iteration stops when a false value is produced or one of
;; the lists runs out of values. In the latter case, every returns the
;; true value produced by its final application of pred. The
;; application of pred to the last element of the lists is a tail
;; call.
;; 
;; If one of the clisti has no elements, every simply returns #t. 
;; 
;; Like any, every's name does not end with a question mark -- this is
;; to indicate that it does not return a simple boolean (#t or #f),
;; but a general value.
;; 
(define-public (every pred . lists)
  (let loop ((lists lists)
	     (answer #t))
    (if (or-map null? lists)
	answer
	(let ((res (apply pred (map car lists))))
	  (and res (loop (map cdr lists) res))))))


;; list-index pred clist1 clist2 ... -> integer or false 
;; 
;; Return the index of the leftmost element that satisfies pred.
;; 
;; If there are n list arguments clist1 ... clistn, then pred must be
;; a function taking n arguments and returning a boolean result.
;; 
;; list-index applies pred to the first elements of the clisti
;; parameters. If this application returns true, list-index
;; immediately returns zero. Otherwise, it iterates, applying pred to
;; the second elements of the clisti parameters, then the third, and
;; so forth. When it finds a tuple of list elements that cause pred to
;; return true, it stops and returns the zero-based index of that
;; position in the lists.
;; 
;; The iteration stops when one of the lists runs out of values; in
;; this case, list-index returns #f.
;; 
;; 	(list-index even? '(3 1 4 1 5 9)) => 2
;; 	(list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) => 1
;; 	(list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2)) => #f
;;
(define-public (list-index pred . lists)
  (let loop ((n 	0)
	     (lists 	lists))
    (and (not (or-map null? lists))
	 (if (apply pred (map car lists))
	     n
	     (loop (1+ n) (map cdr lists))))))


;; span   pred clist -> [list clist] 
;; span!  pred list  -> [list list] 
;; break  pred clist -> [list clist] 
;; break! pred list  -> [list list] 
;; 
;; Span splits the list into the longest initial prefix whose elements
;; all satisfy pred, and the remaining tail.  Break inverts the sense
;; of the predicate: the tail commences with the first element of the
;; input list that satisfies the predicate.
;; 
;; In other words: span finds the intial span of elements satisfying
;; pred, and break breaks the list at the first element satisfying
;; pred.
;; 
;; Span is equivalent to 
;; 
;; 	(values (take-while pred clist) 
;; 	(drop-while pred clist))
;; 
;; Span! and break! are the side-effecting variants. 
;; 
;; 	(span even? '(2 18 3 10 22 9)) => (2 18) (3 10 22 9)
;; 
;; 	(break even? '(3 1 4 1 5 9)) => (3 1) (4 1 5 9)
;; 

(define-public (span: pred clist return)
  (let ((answer		(cons () ())))
    (let loop ((pos	answer)
	       (clist	clist))
      (cond
       ((null? clist)			(return (cdr answer) ()))
       ((not (pred (car clist)))	(return (cdr answer) clist))
       (#t				(set-cdr! pos (cons (car answer) ()))
					(loop (cdr pos) (cdr clist)))))))
(define-public (span pred clist) (span: pred clist values))

(define-public (span!: pred clist return)
  (let ((answer		(cons () ())))
    (let loop ((pos	answer)
	       (clist	clist))
      (cond
       ((null? clist)			(return (cdr answer) ()))
       ((not (pred (car clist)))	(return (cdr answer) clist))
       (#t				(set-cdr! pos clist)
					(let ((rest (cdr clist)))
					  (set-cdr! clist ())
					  (loop (cdr pos) rest)))))))
(define-public (span! pred clist) (span!: pred clist values))

(define-public (break: pred clist return) (span: (negate pred) clist return))
(define-public (break pred clist) (span (negate pred) clist))

(define-public (break!: pred clist return) (span!: (negate pred) clist return))
(define-public (break! pred clist) (span! (negate pred) clist))


;; take-while  pred clist -> list 
;; take-while! pred clist -> list 
;; 
;; Returns the longest initial prefix of clist whose elements all
;; satisfy the predicate pred.
;; 
;; Take-while! is the side-effecting variant. It alters the argument
;; list to produce the result.
;; 
;; 	(take-while even? '(2 18 3 10 22 9)) => (2 18)
;; 
(define-public (take-while pred clist)
  (let ((answer		(cons () ())))
    (let loop ((pos answer)
	       (clist clist))
      (cond
       ((null? clist)		(cdr answer))
       ((pred (car clist))	(set-cdr! pos (cons (car clist) ()))
				(loop (cdr pos) (cdr clist)))
       (#t			(cdr answer))))))

(define-public (take-while! pred clist)
  (let ((answer		(cons () ())))
    (let loop ((pos answer)
	       (clist clist))
      (cond
       ((null? clist)		(cdr answer))
       ((pred (car clist))	(let ((rest (cdr clist)))
				  (set-cdr! clist ())
				  (set-cdr! pos clist)
				  (loop (cdr pos) rest)))
       (#t			(cdr answer))))))


;; drop-while pred clist -> list 
;; 
;; Drops the longest initial prefix of clist whose elements all
;; satisfy the predicate pred, and returns the rest of the list.
;; 
;; 	(drop-while even? '(2 18 3 10 22 9)) => (3 10 22 9)
;; 
;; The circular-list case may be viewed as "rotating" the list.
;;
;; 
(define-public (drop-while pred clist)
  (and clist
       (cond
	((pred (car clist))	(drop-while pred (cdr clist)))
	(#t			clist))))




;; delete  x list [=] -> list 
;; delete! x list [=] -> list 
;; 
;; delete uses the comparison procedure =, which defaults to equal?,
;; to find all elements of list that are equal to x, and deletes them
;; from list. The dynamic order in which the various applications of =
;; are made is not specified.
;; 
;; The list is not disordered -- elements that appear in the result
;; list occur in the same order as they occur in the argument
;; list. The result may share a common tail with the argument list.
;; 
;; Note that fully general element deletion can be performed with the
;; remove and remove! procedures, e.g.:
;; 
;; 	;; Delete all the even elements from LIS:
;; 	(remove even? lis)
;; 
;; The comparison procedure is used in this way: (= x ei). That is, x
;; is always the first argument, and a list element is always the
;; second argument. The comparison procedure will be used to compare
;; each element of list exactly once; the order in which it is applied
;; to the various ei is not specified. Thus, one can reliably remove
;; all the numbers greater than five from a list with (delete 5 list
;; <)
;; 
;; delete! is the side-effecting variant of delete. 
;; 

(define-public delete delete)
(define-public delete! delete!)



;; ***
;; delete-first x list [=]
;; 
;; Delete the first occurence of `x' from `list', using `=' to
;; compare elements:
;; 
;;	(= x elt)
;; 
;; The default `=' procedure is `eq?'.
;; 
(define-public (delete-first x lst :optional =)
  (member-split: x lst =
		 (lambda (head tail)
		   (append head (and tail (cdr tail))))))

;; ***
;; delete-first! x list [=]
;; 
(define-public (delete-first! x lst :optional =)
  (member-split!: x lst =
		  (lambda (head tail)
		    (append! head (and tail (cdr tail))))))
    


;; delete-duplicates  list [=] -> list 
;; delete-duplicates! list [=] -> list 
;; 
;; delete-duplicates removes duplicate elements from the list
;; argument. If there are multiple equal elements in the argument
;; list, the result list only contains the first or leftmost of these
;; elements in the result. The order of these surviving elements is
;; the same as in the original list -- delete-duplicates does not
;; disorder the list (hence it is useful for "cleaning up" association
;; lists).
;; 
;; The = parameter is used to compare the elements of the list; it
;; defaults to equal?. If x comes before y in list, then the
;; comparison is performed (= x y). The comparison procedure will be
;; used to compare each pair of elements in list no more than once;
;; the order in which it is applied to the various pairs is not
;; specified.
;; 
;; Implementations of delete-duplicates are allowed to share common
;; tails between argument and result lists -- for example, if the list
;; argument contains only unique elements, it may simply return
;; exactly this list.
;; 
;; Be aware that, in general, delete-duplicates runs in time O(n2) for
;; n-element lists. Uniquifying long lists can be accomplished in O(n
;; lg n) time by sorting the list to bring equal elements together,
;; then using a linear-time algorithm to remove equal
;; elements. Alternatively, one can use algorithms based on
;; element-marking, with linear-time results.
;; 
;; delete-duplicates! is the side-effecting variant of
;; delete-duplicates.
;; 
;; 	(delete-duplicates '(a b a c a b c z)) => (a b c z)
;; 
;; 	;; Clean up an alist:
;; 	(delete-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
;; 			   (lambda (x y) (eq? (car x) (car y))))
;; 	=> ((a . 3) (b . 7) (c . 1))
;;

(define-public (delete-duplicates! l :optional =)
  (cond
   ((null? l)		())
   (#t			(cons (car l) (delete-duplicates! (delete! (car l) (cdr l) =) =)))))

(define-public (delete-duplicates l :optional =)
  (delete-duplicates! (list-copy l) =))

;; assoc key alist [=] -> pair or #f 
;; assq key alist -> pair or #f 
;; assv key alist -> pair or #f 
;; 
;; [R5RS+] alist must be an association list -- a list of pairs. These
;; procedures find the first pair in alist whose car field is key, and
;; returns that pair. If no pair in alist has key as its car, then #f
;; is returned. assq uses eq? to compare key with the car fields of
;; the pairs in alist, while assv uses eqv? and assoc uses equal?.
;; 
;; 	(define e '((a 1) (b 2) (c 3)))
;; 	(assq 'a e)                            =>  (a 1)
;; 	(assq 'b e)                            =>  (b 2)
;; 	(assq 'd e)                            =>  #f
;; 	(assq (list 'a) '(((a)) ((b)) ((c))))  =>  #f
;; 	(assoc (list 'a) '(((a)) ((b)) ((c)))) =>  ((a))
;; 	(assq 5 '((2 3) (5 7) (11 13)))	       =>  *unspecified*
;; 	(assv 5 '((2 3) (5 7) (11 13)))        =>  (5 7)
;; 
;; assoc is extended from its R5RS definition to allow the client to
;; pass in an optional equality procedure = used to compare keys.
;; 
;; The comparison procedure is used to compare the elements ei of list
;; to the key parameter in this way: (= key (car ei)) ; list is (E1
;; ... En) That is, the first argument is always key, and the second
;; argument is one of the list elements. Thus one can reliably find
;; the first entry of alist whose key is greater than five with (assoc
;; 5 alist <)
;; 
;; Note that fully general alist searching may be performed with the
;; find-tail and find procedures, e.g.
;; 
;; 	;; Look up the first association in alist with an even key:
;; 	(find (lambda (a) (even? (car a))) alist)
;; 
(define-public assoc assoc)
(define-public assq assq)
(define-public assv assv)


;; alist-cons key datum alist -> alist 
;; 
;; 	(lambda (key datum alist) (cons (cons key datum) alist))
;; 
;; Cons a new alist entry mapping key -> datum onto alist. 
;; 
(define-public alist-cons acons)


;; alist-copy alist -> alist 
;; 
;; Make a fresh copy of alist. This means copying each pair that forms
;; an association as well as the spine of the list, i.e.
;; 
;; 	(lambda (a) (map (lambda (elt) (cons (car elt) (cdr elt))) a))
;; 
(define-public (alist-copy a)
  (map (lambda (elt) (cons (car elt) (cdr elt))) a))


;; alist-delete  key alist [=] -> alist 
;; alist-delete! key alist [=] -> alist 
;; 
;; alist-delete deletes all associations from alist with the given
;; key, using key-comparison procedure =, which defaults to
;; equal?. The dynamic order in which the various applications of =
;; are made is not specified.
;; 
;; Return values may share common tails with the alist argument. The
;; alist is not disordered -- elements that appear in the result alist
;; occur in the same order as they occur in the argument alist.
;; 
;; The comparison procedure is used to compare the element keys ki of
;; alist's entries to the key parameter in this way: (= key ki). Thus,
;; one can reliably remove all entries of alist whose key is greater
;; than five with (alist-delete 5 alist <)
;; 
;; alist-delete! is the linear-update variant of alist-delete. It is
;; allowed, but not required, to alter cons cells from the alist
;; parameter to construct the result.
;; 

(define-public (alist-delete key alist :optional =)
  (delete key alist (lambda (key pair) ((or = equal?) key (car pair)))))

(define-public (alist-delete! key alist :optional =)
  (delete! key alist (lambda (key pair) ((or = equal?) key (car pair)))))


;; set operations on lists
;; 
;; These procedures implement operations on sets represented as lists
;; of elements. They all take an = argument used to compare elements
;; of lists. This equality procedure is required to be consistent with
;; eq?. That is, it must be the case that
;; 
;;	 (eq? x y) => (= x y). 
;; 
;; Note that this implies, in turn, that two lists that are eq? are
;; also set-equal by any legal comparison procedure.  This allows for
;; constant-time determination of set operations on eq? lists.
;; 
;; Be aware that these procedures typically run in time O(n * m) for
;; n- and m-element list arguments.  Performance-critical applications
;; operating upon large sets will probably wish to use other data
;; structures and algorithms.
;; 

;; lset<= = list1 ... -> boolean 
;; 
;; Returns true iff every listi is a subset of listi+1, using = for
;; the element-equality procedure. List A is a subset of list B if
;; every element in A is equal to some element of B. When performing
;; an element comparison, the = procedure's first argument is an
;; element of A; its second, an element of B.
;; 
;; 	(lset<= eq? '(a) '(a b a) '(a b c c)) => #t
;; 
;; 	(lset<= eq?) => #t             ; Trivial cases
;; 	(lset<= eq? '(a)) => #t
;; 
(define-public (lset<= = . lists)
  (cond
   ((null? lists)				#t)
   ((null? (cdr lists))				#t)
   ((and-map
     (lambda (elt)
       (member elt (cadr lists) =))
     (car lists))				(apply lset<= = (cdr lists)))
   (#t						#f)))


;; lset= = list1 list2 ... -> boolean 
;; 
;; Returns true iff every listi is set-equal to listi+1, using = for
;; the element-equality procedure. "Set-equal" simply means that listi
;; is a subset of listi+1, and listi+1 is a subset of listi. The =
;; procedure's first argument is an element of listi; its second is an
;; element of listi+1.
;; 
;; 	(lset= eq? '(b e a) '(a e b) '(e e b a)) => #t
;; 
;; 	(lset= eq?) => #t               ; Trivial cases
;; 	(lset= eq? '(a)) => #t
;; 
(define-public (lset= = . lists)
  (cond
   ((null? lists)				#t)
   ((null? (cdr lists))				#t)
   ((and (lset<= = (car lists) (cadr lists))
	 (lset<= = (cadr lists) (car lists)))	(apply lset= = (cdr lists)))
   (#t						#f)))

;; lset-adjoin = list elt1 ... -> list 
;; 
;; Adds the elti elements not already in the list parameter to the
;; result list. The result shares a common tail with the list
;; parameter. The new elements are added to the front of the list, but
;; no guarantees are made about their order. The = parameter is an
;; equality procedure used to determine if an elti is already a member
;; of list. Its first argument is an element of list; its second is
;; one of the elti.
;; 
;; The list parameter is always a suffix of the result -- even if the
;; list parameter contains repeated elements, these are not reduced.
;; 
;; 	(lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u) 
;;	=> (u o i a b c d c e)
;; 
(define-public (lset-adjoin = lset . elts)
  (cond
   ((null? elts)		lset)
   ((member (car elts) lset =)	(apply lset-adjoin = lset (cdr elts)))
   (#t				(apply lset-adjoin = (cons (car elts) lset) (cdr elts)))))

;; ***
;; lset-delete = list elt1 ... -> list 
;; 
;; Deletes the elti elements from the list parameter.
;; 
;; The result may share a common tail with the list parameter.  The =
;; parameter is an equality procedure used to determine if an elti is
;; a member of list. Its first argument is an element of list;
;; its second is one of the elti.
;; 
;; 
(define-public (lset-delete = lset . elts)
  (let ((=rev (lambda (a b) (= b a))))
    (let loop ((answer	lset)
	       (elts	elts))
      (cond
       ((null? elts)		answer)
       (#t			(loop (delete (car elts) answer =rev) (cdr elts)))))))
  
;; lset-union = list1 ... -> list 
;; 
;; Returns the union of the lists, using = for the element-equality
;; procedure.
;; 
;; The union of lists A and B is constructed as follows: If A is the
;; empty list, the answer is B (or a copy of B).  Otherwise, the
;; result is initialised to be list A (or a copy of A).  Proceed
;; through the elements of list B in a left-to-right order. If b is
;; such an element of B, compare every element r of the current result
;; list to b: (= r b). If all comparisons fail, b is consed onto the
;; front of the result.  However, there is no guarantee that = will be
;; applied to every pair of arguments from A and B. In particular, if
;; A is eq? to B, the operation may immediately terminate.
;; 
;; In the n-ary case, the two-argument list-union operation is simply
;; folded across the argument lists.
;; 
;; 	(lset-union eq? '(a b c d e) '(a e i o u)) => 
;; 	(u o i a b c d e)
;; 
;; 	;; Repeated elements in LIST1 are preserved.
;; 	(lset-union eq? '(a a c) '(x a x)) => (x a a c)
;; 
;; 	;; Trivial cases
;; 	(lset-union eq?) => ()
;; 	(lset-union eq? '(a b c)) => (a b c)
;; 
(define-public (lset-union = . lists)
  (cond
   ((null? lists)			())
   ((null? (cdr lists))			(car lists))
   ((eq? (car lists) (cadr lists))	(apply lset-union = (car lists) (cddr lists)))
   (#t					(apply lset-union
					       =
					       (fold-iterative (lambda (elt answer)
								 (if (member elt answer =)
								     answer
								     (cons elt answer)))
							       (car lists)
							       (cadr lists))
					       (cddr lists)))))


;; lset-intersection = list1 list2 ... -> list 
;; 
;; Returns the intersection of the lists, using = for the
;; element-equality procedure.
;; 
;; The intersection of lists A and B is comprised of every element of
;; A that is = to some element of B: (= a b), for a in A, and b in
;; B. Note this implies that an element which appears in B and
;; multiple times in list A will also appear multiple times in the
;; result.
;; 
;; The order in which elements appear in the result is the same as
;; they appear in list1 -- that is, lset-intersection essentially
;; filters list1, without disarranging element order. The result may
;; share a common tail with list1.
;; 
;; In the n-ary case, the two-argument list-intersection operation is
;; simply folded across the argument lists.  However, the dynamic
;; order in which the applications of = are made is not specified. The
;; procedure may check an element of list1 for membership in every
;; other list before proceeding to consider the next element of list1,
;; or it may completely intersect list1 and list2 before proceeding to
;; list3, or it may go about its work in some third order.
;; 
;; 	(lset-intersection eq? '(a b c d e) '(a e i o u)) => (a e)
;; 
;; 	;; Repeated elements in LIST1 are preserved.
;; 	(lset-intersection eq? '(a x y a) '(x a x z)) => '(a x a)
;; 
;; 	(lset-intersection eq? '(a b c)) => (a b c)     ; Trivial case
;; 
(define-public (lset-intersection = l1 . lists)
  (filter (lambda (elt) (and-map (lambda (l) (member elt l =)) lists))
	  l1))


;; lset-difference = list1 list2 ... -> list 
;; 
;; Returns the difference of the lists, using = for the
;; element-equality procedure -- all the elements of list1 that are
;; not = to any element from one of the other listi parameters.
;; 
;; The = procedure's first argument is always an element of list1; its
;; second is an element of one of the other listi. Elements that are
;; repeated multiple times in the list1 parameter will occur multiple
;; times in the result. The order in which elements appear in the
;; result is the same as they appear in list1 -- that is,
;; lset-difference essentially filters list1, without disarranging
;; element order. The result may share a common tail with list1. The
;; dynamic order in which the applications of = are made is not
;; specified. The procedure may check an element of list1 for
;; membership in every other list before proceeding to consider the
;; next element of list1, or it may completely compute the difference
;; of list1 and list2 before proceeding to list3, or it may go about
;; its work in some third order.
;; 
;; 	(lset-difference eq? '(a b c d e) '(a e i o u)) => (b c d)
;; 
;; 	(lset-difference eq? '(a b c)) => (a b c) ; Trivial case
;; 
(define-public (lset-difference = l1 . lists)
  (filter (lambda (elt) (and-map (lambda (l) (not (member elt l =))) lists))
	  l1))


;; lset-xor = list1 ... -> list 
;; 
;; Returns the exclusive-or of the sets, using = for the
;; element-equality procedure. If there are exactly two lists, this is
;; all the elements that appear in exactly one of the two lists. The
;; operation is associative, and thus extends to the n-ary case -- the
;; elements that appear in an odd number of the lists. The result may
;; share a common tail with any of the listi parameters.
;; 
;; More precisely, for two lists A and B, A xor B is a list of every
;; element a of A such that there is no element b of B such that (= a
;; b), and every element b of B such that there is no element a of A
;; such that (= b a).  However, an implementation is allowed to assume
;; that = is symmetric -- that is, that
;; 
;; 	(= a b) => (= b a). 
;; 
;; This means, for example, that if a comparison (= a b) produces true
;; for some a in A and b in B, both a and b may be removed from
;; inclusion in the result.
;; 
;; In the n-ary case, the binary-xor operation is simply folded across
;; the lists.
;; 
;; 	(lset-xor eq? '(a b c d e) '(a e i o u)) => (d c b i o u)
;; 
;; 	;; Trivial cases.
;; 	(lset-xor eq?) => ()
;; 	(lset-xor eq? '(a b c d e)) => (a b c d e)
;; 
(define-public (lset-xor = . lists)
  (cond
   ((null? lists)		())
   ((null? (cdr lists))		(car lists))
   (#t				(append-map (lambda (l)
					      (apply lset-difference = l (delq l lists)))
					    lists))))
				  

;; lset-diff+intersection = list1 list2 ... -> [list list] 
;; 
;; Returns two values -- the difference and the intersection of the
;; lists. Is equivalent to
;; 
;; 	(values (lset-difference = list1 list2 ...)
;; 		(lset-intersection = list1 (lset-union = list2 ...)))
;; 
;; but can be implemented more efficiently. 
;; 
;; The = procedure's first argument is an element of list1; its second
;; is an element of one of the other listi.
;; 
(define-public (lset-diff+intersection: return = l1 . lists)
  (partition: (lambda (elt)
		(not (any (lambda (l) (member elt l =)) lists)))
	      l1
	      return))
(define-public (lset-diff+intersection = l1 . lists)
  (apply lset-diff+intersection: values = l1 lists))




;; Either of the answer lists may share a common tail with list1. This operation essentially partitions list1. 
;; 
;; lset-union!             = list1 ... -> list 
;; lset-intersection!      = list1 list2 ... -> list 
;; lset-difference!        = list1 list2 ... -> list 
;; lset-xor!               = list1 ... -> list 
;; lset-diff+intersection! = list1 list2 ... -> [list list] 
;; 
;; These are linear-update variants. They are allowed, but not
;; required, to use the cons cells in their first list parameter to
;; construct their answer. lset-union! is permitted to recycle cons
;; cells from any of its list arguments.
;; 
;; [[SRFI 1 requires these, but they have no obvious meaning
;;   in a certainly side-effecting implementation.  So...
;;   
;;   Use of these functions is not recommended.]]

(define-public lset-union! lset-union)
(define-public lset-intersection! lset-intersection)
(define-public lset-difference! lset-difference)
(define-public lset-xor! lset-xor)
(define-public lset-diff+intersection!: lset-diff+intersection:)
(define-public lset-diff+intersection! lset-diff+intersection)

(define-public (lset-delete! = lset . elts)
  (let ((=rev (lambda (a b) (= b a))))
    (let loop ((answer	lset)
	       (elts	elts))
      (cond
       ((null? elts)		answer)
       (#t			(loop (delete! (car elts) answer =rev) (cdr elts)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shorthand for Common Uses of `lset'
;;; 
;;; (*** non-standard)
;;; 

(define-public (adjoinq . args) 	(apply lset-adjoin eq? args))
(define-public (unionq . args) 		(apply lset-union eq? args))
(define-public (intersectionq . args) 	(apply lset-intersection eq? args))
(define-public (set-differq . args) 	(apply lset-difference eq? args))
(define-public (subsetq? . args) 	(apply lset<= eq? args))
(define-public (uniq lst)		(delete-duplicates! lst eq?))

(define-public (adjoinv . args) 	(apply lset-adjoin eqv? args))
(define-public (unionv . args) 		(apply lset-union eqv? args))
(define-public (intersectionv . args) 	(apply lset-intersection eqv? args))
(define-public (set-differv . args) 	(apply lset-difference eqv? args))
(define-public (subsetv . args) 	(apply lset<= eqv? args))
(define-public (uniqv lst)		(delete-duplicates! lst eqv?))

(define-public (adjoin . args) 		(apply lset-adjoin equal? args))
(define-public (union . args) 		(apply lset-union equal? args))
(define-public (intersection . args) 	(apply lset-intersection equal? args))
(define-public (set-difference . args) 	(apply lset-difference equal? args))
(define-public (subset? . args) 	(apply lset<= equal? args))
(define-public (unique lst)		(delete-duplicates! lst equal?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ordered List Sets
;;; 

;; set operations on lists
;; 
;; These procedures implement operations on sets represented as
;; ordered lists of elements. They all take `=' and `<' arguments used
;; to compare elements of lists.  The equality procedure is required
;; to be consistent with eq?. That is, it must be the case that
;; 
;;	 (eq? x y) => (= x y). 
;; 
;; `=' and `<' must implement a total order of possible elements.
;; 
;; Note that this implies, in turn, that two lists that are eq? are
;; also set-equal by any legal comparison procedure.  This allows for
;; constant-time determination of set operations on eq? lists.
;; 
;; The list arguments to these procedures must be sorted, from 
;; least to greatest.
;; 
;; These procedures typically run in time O(min(n,m)) for
;; n- and m-element list arguments.  
;; 

;; ordered-lset<= = < list1 ... -> boolean 
;; 
;; Returns true iff every listi is a subset of listi+1, using = for
;; the element-equality procedure. List A is a subset of list B if
;; every element in A is equal to some element of B. When performing
;; an element comparison, the = procedure's first argument is an
;; element of A; its second, an element of B.
;; 
(define-public (ordered-lset<= = < . lists)
  (or (null? lists)
      (let ((lr 	(cdr lists)))
	(or (null? lr)
	    (and (let loop ((l1	(car lists))
			    (l2 (car lr)))
		   (or (null? l2)
		       (and (not (null? l1))
			    (cond
			     ((< (car l1) (car l2))	(loop (cdr l1) l2))
			     ((= (car l1) (car l2))	(loop (cdr l1) (cdr l2)))
			     (#t			#f)))))
		 (apply ordered-lset<= = < lr))))))
				


;; ordered-lset= = list1 list2 ... -> boolean 
;; 
;; Returns true iff every listi is set-equal to listi+1, using = for
;; the element-equality procedure. "Set-equal" simply means that listi
;; is a subset of listi+1, and listi+1 is a subset of listi. The =
;; procedure's first argument is an element of listi; its second is an
;; element of listi+1.
;; 
;; 	(ordered-lset= eq? '(b e a) '(a e b) '(e e b a)) => #t
;; 
;; 	(ordered-lset= eq?) => #t               ; Trivial cases
;; 	(ordered-lset= eq? '(a)) => #t
;; 
;; (define-public (ordered-lset= = . lists))


;; ordered-lset-adjoin = < list elt1 ... -> list 
;; 
;; Adds the `elti' elements not already in the list parameter to the
;; result list. The result shares a common tail with the list
;; parameter.  The = parameter is an equality procedure used to
;; determine if an `elti' is already a member of list. `<' is used to
;; compare each `elti' to members of `list'.
;; 
;; The list parameter is always a suffix of the result -- even if the
;; list parameter contains repeated elements, these are not reduced.
;; 
;; 	(ordered-lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u) 
;;	=> (u o i a b c d c e)
;; 
;; 
(define-public (ordered-lset-adjoin = < ordered-lset . elts)
  (let ((answer (cons () ())))
    (let loop ((pos 		answer)
	       (ordered-lset 	ordered-lset)
	       (elts 		elts))
      (cond
       ((null? ordered-lset)		(begin
					  (set-cdr! pos elts)
					  (cdr answer)))
       ((null? elts)			(begin
					  (set-cdr! pos ordered-lset)
					  (cdr answer)))
       ((< (car ordered-lset)
	   (car elts))			(begin
					  (set-cdr! pos (cons (car ordered-lset) ()))
					  (loop (cdr pos) (cdr ordered-lset) elts)))
       ((= (car ordered-lset)
	   (car elts))			(begin
					  (set-cdr! pos (cons (car ordered-lset) ()))
					  (loop (cdr pos) (cdr ordered-lset) (cdr elts))))
       (#t				(begin
					  (set-cdr! pos (cons (car elts) ()))
					  (loop (cdr pos) ordered-lset (cdr elts))))))))
	    

;; ordered-lset-delete = < list elt1 ... -> list 
;; 
;; Deletes the elti elements from the list parameter.
;; 
;; The result may share a common tail with the list parameter.  The =
;; parameter is an equality procedure used to determine if an elti is
;; a member of list. Its first argument is an element of list;
;; its second is one of the elti.
;; 
;; 
;; 
(define-public (ordered-lset-delete = < ordered-lset . elts)
  (apply lset-delete = ordered-lset elts))
   

  
;; ordered-lset-union = < list1 ... -> list 
;; 
;; Returns the union of ordered lists, using `=' and `<' to compare
;; elements.  Return an ordered list.  The equality procedure is
;; required to be consistent with eq?. That is, it must be the case
;; that
;; 
;;	 (eq? x y) => (= x y). 
;; 
;; `=' and `<' must implement a total order of possible elements.
;; 
;; 

(define-public (ordered-lset-union = < . lists)
  (let ((answer (cons () ())))
    (let loop ((lists		(remove null? lists))
	       (pos		answer))
      (if (null? lists)
	  (cdr answer)
	  (let* ((next	(apply min-using < (map car lists)))
		(remain	(remove null? (map (lambda (x) (if (= next (car x))
							   (cdr x)
							   x))
					   lists))))
	    (set-cdr! pos (cons next ()))
	    (loop remain (cdr pos)))))))


;; ordered-lset-intersection = < list1 list2 ... -> list 
;; 
;; Returns the intersection of the ordered lists, using `=' for the
;; element-equality procedure and `<' for element ordering.  The input
;; lists must be sorted according to `<' which must be a total
;; ordering of elements that is compatible with `='.
;; 
;; The intersection of lists A and B is comprised of every element of
;; A that is equal to some element of B: `(= a b)', for a in A, and b in
;; B. Note this implies that an element which appears in B and
;; multiple times in list A will also appear multiple times in the
;; result.
;; 
;; The order in which elements appear in the result is the same as
;; they appear in list1 -- that is, ordered-lset-intersection essentially
;; filters list1, without disarranging element order. The result may
;; share a common tail with list1.
;; 
;; In the n-ary case, the two-argument list-intersection operation is
;; simply folded across the argument lists.  However, the dynamic
;; order in which the applications of `=' and `<' are made is not
;; specified. The procedure may check an element of list1 for
;; membership in every other list before proceeding to consider the
;; next element of list1, or it may completely intersect list1 and
;; list2 before proceeding to list3, or it may go about its work in
;; some third order.
;; 
;; 	(ordered-lset-intersection eq? string<? '(a b c d e) '(a e i o u)) => (a e)
;; 
;; 	;; Repeated elements in LIST1 are preserved.
;; 	(ordered-lset-intersection eq? string<? '(a a x y) '(a x x z)) => '(a a x)
;; 
;; 	(ordered-lset-intersection eq? string<? '(a b c)) => (a b c)     ; Trivial case
;; 
(define-public (ordered-lset-intersection = < . lists)
  (cond
   ((null? lists)			())
   ((null? (cdr lists))			(car lists))
   (#t
    (let ((answer	(cons () ())))
      (let loop ((pos		answer)
		 (lists		lists))
	(cond
	 ;; Are any of the lists empty?  If so, we're done.
	 ;;
	 ((or-map null? lists)					(cdr answer))

	 ;; Does the first element in the first list exist in all
	 ;; other lists?  If so, it is in the return value.  We don't
	 ;; require `=' to accept an arbitrary number of arguments.
	 ;;
	 ((and-map (lambda (x) (= (caar lists) (car x)))
		   (cdr lists))					(begin
								  (set-cdr! pos (cons (caar lists) ()))
								  (loop (cdr pos)
									(cons (cdar lists) (cdr lists)))))

	 ;; Is the first element in the first list less than any of
	 ;; the first elements of the other lists?  If so, iterate,
	 ;; discarding the first element of the first list.
	 ;; 
	 ((or-map (lambda (x) (< (caar lists) (car x)))
		  (cdr lists))					(loop pos (cons (cdar lists) (cdr lists))))

	 ;; The first element in the first list is not equal to
	 ;; all of the first elements of the other lists and is
	 ;; not less than any of them.  Loop, discarding first elements
	 ;; of the other lists that are less than the first element
	 ;; of the first list.
	 ;;
	 (#t							(loop pos (cons (car lists)
										(map (lambda (x) (if (< (car x) (caar lists))
												     (cdr x)
												     x))
										     (cdr lists)))))))))))

;; ordered-lset-difference = list1 list2 ... -> list 
;; 
;; Returns the difference of the lists, using = for the
;; element-equality procedure -- all the elements of list1 that are
;; not = to any element from one of the other listi parameters.
;; 
;; The = procedure's first argument is always an element of list1; its
;; second is an element of one of the other listi. Elements that are
;; repeated multiple times in the list1 parameter will occur multiple
;; times in the result. The order in which elements appear in the
;; result is the same as they appear in list1 -- that is,
;; ordered-lset-difference essentially filters list1, without disarranging
;; element order. The result may share a common tail with list1. The
;; dynamic order in which the applications of = are made is not
;; specified. The procedure may check an element of list1 for
;; membership in every other list before proceeding to consider the
;; next element of list1, or it may completely compute the difference
;; of list1 and list2 before proceeding to list3, or it may go about
;; its work in some third order.
;; 
;; 	(ordered-lset-difference eq? '(a b c d e) '(a e i o u)) => (b c d)
;; 
;; 	(ordered-lset-difference eq? '(a b c)) => (a b c) ; Trivial case
;; 
(define-public (ordered-lset-difference = < . lists)
  (cond
   ((null? lists)		lists)
   (#t
    (let ((answer	(cons () ())))
      (let loop ((source	(car lists))
		 (removes	(remove null? (cdr lists)))
		 (pos		answer))
	(cond
	 ((null? source)		(cdr answer))
	 ((null? removes)		(set-cdr! pos source)
					(cdr answer))
	 (#t
	  (let* ((elt		(car source))
		 (remove?	(or-map (lambda (x) (= elt (car x))) removes))
		 (include?	(and-map (lambda (x) (< elt (car x))) removes))
		 (remains	(filter-map (lambda (x) (if (or (= elt (car x))
								(< (car x) elt))
							    (cdr x)
							    x))
					    removes)))
	    (cond
	     (remove?		(loop (cdr source) remains pos))
	     (include?		(set-cdr! pos (cons elt ()))
				(loop (cdr source) remains (cdr pos)))
	     (#t		(loop source remains pos)))))))))))


;; ordered-lset-xor = list1 ... -> list 
;; 
;; Returns the exclusive-or of the sets, using = for the
;; element-equality procedure. If there are exactly two lists, this is
;; all the elements that appear in exactly one of the two lists. The
;; operation is associative, and thus extends to the n-ary case -- the
;; elements that appear in an odd number of the lists. The result may
;; share a common tail with any of the listi parameters.
;; 
;; More precisely, for two lists A and B, A xor B is a list of every
;; element a of A such that there is no element b of B such that (= a
;; b), and every element b of B such that there is no element a of A
;; such that (= b a).  However, an implementation is allowed to assume
;; that = is symmetric -- that is, that
;; 
;; 	(= a b) => (= b a). 
;; 
;; This means, for example, that if a comparison (= a b) produces true
;; for some a in A and b in B, both a and b may be removed from
;; inclusion in the result.
;; 
;; In the n-ary case, the binary-xor operation is simply folded across
;; the lists.
;; 
;; 	(ordered-lset-xor eq? '(a b c d e) '(a e i o u)) => (d c b i o u)
;; 
;; 	;; Trivial cases.
;; 	(ordered-lset-xor eq?) => ()
;; 	(ordered-lset-xor eq? '(a b c d e)) => (a b c d e)
;; 
;; 
(define-public (ordered-lset-xor = < . lists)
  (cond
   ((null? lists)			())
   ((null? (cdr lists))			(car lists))
   (#t
    (let ((answer	(cons () ())))
      (let loop ((pos 	answer)
		 (lists lists))
	(let ((remain	(filter (lambda (x) (not (null? x))) lists)))
	  (if (null? remain)
	      (cdr answer)
	      (let* ((firsts	(map car remain))
		     (least	(apply min-using < firsts))
		     (occurs	(count (lambda (x) (= x least)) firsts))
		     (new-lists (map (lambda (l)
				       (if (= (car l) least)
					   (cdr l)
					   l))
				     remain)))
		(cond
		 ((= 1 occurs)	(begin
				  (set-cdr! pos (cons least ()))
				  (loop (cdr pos) new-lists)))
		 (#t		(loop pos new-lists)))))))))))



;; ordered-lset-diff+intersection = list1 list2 ... -> [list list] 
;; 
;; Returns two values -- the difference and the intersection of the
;; lists. Is equivalent to
;; 
;; 	(values (ordered-lset-difference = list1 list2 ...)
;; 		(ordered-lset-intersection = list1 (ordered-lset-union = list2 ...)))
;; 
;; but can be implemented more efficiently. 
;; 
;; The = procedure's first argument is an element of list1; its second
;; is an element of one of the other listi.
;; 
;; (define-public (ordered-lset-diff+intersection: return = l1 . lists))
;; (define-public (ordered-lset-diff+intersection = l1 . lists))



;; Either of the answer lists may share a common tail with list1. This operation essentially partitions list1. 
;; 
;; ordered-lset-union!             = list1 ... -> list 
;; ordered-lset-intersection!      = list1 list2 ... -> list 
;; ordered-lset-difference!        = list1 list2 ... -> list 
;; ordered-lset-xor!               = list1 ... -> list 
;; ordered-lset-diff+intersection! = list1 list2 ... -> [list list] 
;; 
;; These are linear-update variants. They are allowed, but not
;; required, to use the cons cells in their first list parameter to
;; construct their answer. ordered-lset-union! is permitted to recycle cons
;; cells from any of its list arguments.
;; 
;; [[SRFI 1 requires these, but they have no obvious meaning
;;   in a certainly side-effecting implementation.  So...
;;   
;;   Use of these functions is not recommended.]]

;; (define-public ordered-lset-union! ordered-lset-union)
;; (define-public ordered-lset-intersection! ordered-lset-intersection)
;; (define-public ordered-lset-difference! ordered-lset-difference)
;; (define-public ordered-lset-xor! ordered-lset-xor)
;; (define-public ordered-lset-diff+intersection!: ordered-lset-diff+intersection:)
;; (define-public ordered-lset-diff+intersection! ordered-lset-diff+intersection)
;; (define-public (ordered-lset-delete! = ordered-lset . elts))



(define-public (min-using < . elts)
  (cond
   ((null? elts)	(error "wrong number of arguments to min-using"))
   ((null? (cdr elts))	(car elts))
   (#t			(apply min-using < (if (< (car elts) (cadr elts))
					       (cons (car elts) (cddr elts))
					       (cdr elts))))))

(define-public (max-using < . elts)
  (cond
   ((null? elts)	(error "wrong number of arguments to max-using"))
   ((null? (cdr elts))	(car elts))
   (#t			(apply max-using < (if (< (car elts) (cadr elts))
					       (cdr elts)
					       (cons (car elts) (cddr elts)))))))



(define ((negate pred) . args)
  (not (apply pred args)))

