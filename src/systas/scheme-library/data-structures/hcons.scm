;;; tag: Tom Lord Tue Dec  4 14:59:30 2001 (data-structures/hcons.scm)
;;;
;;; hcons.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998, 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (data-structures hcons))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eq? hash-consing
;;;
;;; A hash conser maintains a set of pairs s.t. two calls
;;; 
;;;		(eq? (hashq-fn a b) (hashq-fn c d))
;;; 
;;; if and only if:
;;; 
;;;		(and (eq? a c) (eq? b d))
;;;		
;;; A hash conser does not contribute indefinate life (GC protection)
;;; to the pairs it returns.  "make-gc-buffer" returns a procedure that
;;; gives temporary GC protection to objects passed to it.
;;;

;; (hashq-conser hash-tab-or-size)
;; 
;; Return an `eq?' hash-conser that uses the vector
;; `hash-tab-or-size' or a doubly-weak vector whose
;; size is the integer `hash-tab-or-size' to keep track
;; of hash-consed pairs.
;; 
(define-public (hashq-conser hash-tab-or-size)
  (let ((table (if (vector? hash-tab-or-size)
		   hash-tab-or-size
		   (make-doubly-weak-hash-table hash-tab-or-size))))
    (lambda (a d) (hashq-cons table a d))))


;; (make-gc-buffer n)
;; 
;; Return a procedure of one argument.  Calling that procedure
;; has the side effects of saving a reference to the object, and
;; discarding the reference saved on the `n'th previous call.
;; 
;; Thus, the procedure returned from `make-gc-buffer' provides
;; a way to delay garbage collecting some objects.
;; 
(define-public (make-gc-buffer n)
  (let ((ring (make-list n #f)))
    (append! ring ring)
    (lambda (next)
      (set-car! ring next)
      (set! ring (cdr ring))
      next)))

(define (hashq-cons table a d)
  (car (hashq-cons-create-handle! table (cons a d) #f)))

(define (hashq-cons-create-handle! table key init)
  (hashx-create-handle! hashq-cons-hash hashq-cons-assoc table key init))

(define (hashq-cons-hash pair n)
  (modulo (logxor (hashq (car pair) 4194303)
		  (hashq (cdr pair) 4194303))
	  n))

(define (hashq-cons-assoc key l)
  (and l (or (and (pair? l)
		  (pair? (car l))
		  (pair? (caar l))
		  (eq? (car key) (caaar l))
		  (eq? (cdr key) (cdaar l))
		  (car l))
	     (hashq-cons-assoc key (cdr l)))))

