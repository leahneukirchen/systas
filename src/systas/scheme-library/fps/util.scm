;;; util.scm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord (same terms)
;;;

;; ===================================================================

;; This file contains utility procedures.

;; (add-pts point1 point2)               -> pt
;; (negate-pt point)                     -> pt
;; (scale-pt x-factor y-factor point)    -> pt
;; (deg->rad number)                     -> number
;; (rad->deg number)                     -> number
;; (matrix* matrix1 matrix2)             -> matrix
;; (inch int)                            -> int
;; (bounding-box->rect box)              -> path

;; ===================================================================

;; add two points together
(define (add-pts pt1 . pts)
  (letrec ((add-2-pt (lambda (pt1 pt2)
		       (pt (+ (pt:x pt1) (pt:x pt2))
			   (+ (pt:y pt1) (pt:y pt2))))))
    (let lp ((pt-lst (cons pt1 pts)))
      (if (null? pt-lst)
	  (pt 0 0)
	  (add-2-pt (car pt-lst)
		    (lp (cdr pt-lst)))))))


;; multiply x or y coord or both by -1
(define (negate-pt point)
  (pt (- (pt:x point)) (- (pt:y point))))

;; scale x or y coord or both by factor
(define (scale-pt x-factor y-factor point)
  (pt (* x-factor (pt:x point)) (* y-factor (pt:y point))))

;; convert degree to radian
(define (deg->rad degree) (/ (* degree pi) 180))

;; convert radian to degree
(define (rad->deg rad) (/ (* rad 180) pi))

;; multiply two 3x3 matrices together
(define (matrix* matrix1 matrix2)
  (let* ((a1 (matrix:a matrix1))(b1 (matrix:b matrix1))
	 (p1 (matrix:p matrix1))(q1 (matrix:q matrix1))
	 (x1 (matrix:x matrix1))(y1 (matrix:y matrix1))
	 (a2 (matrix:a matrix2))(b2 (matrix:b matrix2))
	 (p2 (matrix:p matrix2))(q2 (matrix:q matrix2))
	 (x2 (matrix:x matrix2))(y2 (matrix:y matrix2)))
    (matrix (+ (* a1 a2) (* b1 p2))
	    (+ (* a1 b2) (* b1 q2))
	    (+ (* p1 a2) (* q1 p2))
	    (+ (* p1 b2) (* q1 q2))
	    (+ (* x1 a2) (* y1 p2) x2)
	    (+ (* x1 b2) (* y1 q2) y2))))

;; converts to inches (* 72)    
(define (inch n)
  (* 72 n))

(define (mm n)
  (* 2.83464566929 n))

(define (cm n)
  (* 28.3464566929 n))


;; converts bounding box into a rectangular path
(define (bounding-box->rect box)
  (let ((min (bounding-box:min box))
	(max (bounding-box:max box)))
    (rect min (- (pt:x max) (pt:x min))
	      (- (pt:y max) (pt:y min)))))
	  



;; ===== Internal Util ===============================================

;; (receive ...)
;; (forward-list-reduce proc seed lst) ;; needed the forwardness
;; (vector-copy v . opt-len)
;; (vector-assoc key vect)
;; (vector-map proc v)
;; (vector-for-each proc vec)
;; (vector-reduce proc seed vec)
;; (string-for-each proc str)
;; (string-reduce proc seed str)
;; (PSnum num)
;; (comp f g)
;; (display-line thing port)
;; (repeat thunk n)
;; (tf filename thunk) ;; testing util, write output to file
;; (last-pair pair)
;; (dot-produce v1 v2)
;; (mag v)
;; (first-in-composite obj)
;; (last-in-composite obj)
;; (char->hex c)
;; (make-style-with-optional-args opt-args)

;; syntax for receiving multiple values
(define-syntax receive
  (syntax-rules ()
      ((receive var-lst exp body ...)
       (call-with-values (lambda () exp) 
			 (lambda var-lst body ...)))))

(define (forward-list-reduce proc seed lst)
  (let lp ((l lst) (val seed))
    (if (null? l)
	val
	(lp (cdr l) (proc (car l) val)))))

;; assoc for vectors.
(define (vector-assoc key vect)
  (let loop ((pos (- (vector-length vect) 1)))
    (if (>= pos 0)
	(let ((test-v (vector-ref vect pos)))
	  (if (equal? key (vector-ref test-v 0))
	      test-v
	      (loop (- pos 1))))
	#f)))

(define (vector-reduce proc seed vec)
  (let* ((len     (vector-length vec))
	 (new-vec (make-vector len)))
    (let lp ((index (- len 1)) (seed seed))
      (if (= index -1)
	  seed
	  (lp (- index 1) (proc seed (vector-ref vec index)))))))

(define (string-reduce proc seed str)
  (let* ((len     (string-length str))
	 (new-str (make-string len)))
    (let lp ((index (- len 1)) (seed seed))
      (if (= index -1)
	  seed
	  (lp (- index 1) (proc seed (string-ref str index)))))))

	      
;; Utiltiy procedure. Puts out PostScript numerals. i.e. integers
;; and floats.
(define (PSnum num)
  (cond ((integer? num)
	 (if (exact? num) num (inexact->exact num)))
	((real? num)
	 (cond ((exact? num) (exact->inexact num))
	       (else num)))
	(else (error PSnum
		     "Cannot be represented as a PostScript numeral" 
		     num))))

;; Composes two functions.
(define (comp f g) (lambda (x) (f (g x))))


;; Testing util. Repeat a procedure by n times
(define (repeat thunk n)
  (if (not (= n 0)) 
      (begin (thunk) (repeat thunk (- n 1)))))
  
;; Testing util. Repeat and time a procedure by n times
(define (repeat-and-time thunk n)
  (let loop ((start (time)) (n n))
    (if (= n 0)
	(- (time) start)
	(begin (thunk) (loop start (- n 1))))))

;; find last-pair of any list
(define (last-pair x)
  (if (pair? x)
      (let loop ((x x))
	(let ((y (cdr x)))
	  (if (pair? y)
	      (loop y)
	      x)))))

;; Utility. Make a Internal-Bug error message
(define (internal-bug . stuff)
  (apply error (cons "FPS internal error. Please report this bug." stuff)))
		     
;; build a bbox record out of the four min max numbers
(define (min-max-coords->bbox min-x min-y max-x max-y)
  (bbox (pt max-x max-y) (pt min-x max-y)
	(pt min-x min-y) (pt max-x min-y)
	(pt max-x max-y) (pt min-x min-y)))

;; find the dot-product of two vectors
(define (dot-product v1 v2) (+ (* (pt:x v1) (pt:x v2))
			       (* (pt:y v1) (pt:y v2))))

;; find the magnitude of a vector
(define (mag v) (sqrt (+ (expt (pt:x v) 2)
			 (expt (pt:y v) 2))))

;; returns the first obj in the composite list
(define (first-in-composite obj)
  (car (composite:objs obj)))

;; returns the last obj in the composite list
(define (last-in-composite obj)
  (car (reverse (composite:objs obj))))

;; initilize a vector with a init value
(define (initialize-vector len init)
  (let ((vec (make-vector len))
	(i   0))
    (do ((i (- len 1) (- i 1)))
	((< i 0) vec)
      (vector-set! vec i (init i)))
    vec))

;; converts a char to a hex integer
(define (char->hex c)
  (case c
    ((#\0) 0)
    ((#\1) 1)
    ((#\2) 2)
    ((#\3) 3)
    ((#\4) 4)
    ((#\5) 5)
    ((#\6) 6)
    ((#\7) 7)
    ((#\8) 8)
    ((#\9) 9)
    ((#\a #\A) 10)
    ((#\b #\B) 11)
    ((#\c #\C) 12)
    ((#\d #\D) 13)
    ((#\e #\E) 14)
    ((#\f #\F) 15)
    (else (error parse-hex "c is not a valid hex digit" c))))

(define (make-style-with-optional-args opt-arg)
  (cond ((null? opt-arg) (default-style))
	((null? (cdr opt-arg)) 
	 (let ((arg (car opt-arg)))
	   (if (style? arg) arg (vary-default arg))))
	(else (apply vary-default opt-arg))))

(define (check-arg pred val caller)
  (if (pred val) val
      (check-arg pred (error "Bad argument" val pred caller) caller)))



;; ===== End of fps.util.scm =========================================



