;;; general-table.scm - generalized hash tables from Scheme 48
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998 Tom Lord
;;;
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;;



(define-module (compatability general-table))



(begin
  (define table-type (make-record-type 'table '(n-elts hash-fn assoc-fn data)
				       (lambda (t p w?)
					 (display*-port p
							"#<table 0x"
							(number->string (object-address t) 16)
							">"))))

  (define construct-table (record-constructor table-type))
  (define table-n-elts (record-accessor table-type 'n-elts))
  (define set-table-n-elts! (record-modifier table-type 'n-elts))
  (define table-hash-fn (record-accessor table-type 'hash-fn))
  (define set-table-hash-fn! (record-modifier table-type 'hash-fn))
  (define table-assoc-fn (record-accessor table-type 'assoc-fn))
  (define set-table-assoc-fn! (record-modifier table-type 'assoc-fn))
  (define table-data (record-accessor table-type 'data))
  (define set-table-data! (record-modifier table-type 'data))
  (define-public table? (record-predicate table-type)))


(define-public ((make-table-maker eq-fn hash-fn))
  (construct-table 0
		   hash-fn
		   (assoc-fn-from-eq-fn eq-fn)
		   (make-vector (car table-sizes))))

(define-public (table-ref table key)
  (hashx-ref (table-hash-fn table)
	     (table-assoc-fn table)
	     (table-data table)
	     key
	     #f))

(define-public (table-set! table key value)
  (if (not (table-ref table key))
      (let ((new-n-elts (1+ (table-n-elts table))))
	(set-table-n-elts! table new-n-elts)
	(if (> new-n-elts (vector-length (table-data table)))
	    (enlarge-table! table))))
  (hashx-set! (table-hash-fn table)
	      (table-assoc-fn table)
	      (table-data table)
	      key
	      value)
  value)

(define-public (table-for-each fn table)
  (vector-for-each (lambda (x) (for-each (lambda (p) (fn (car p) (cdr p))) x))
		   (table-data table)))

(define table-sizes '(13 31 61 127 251 509 1021 2039 4093 8191))

(define-public (enlarge-table! table)
  (let* ((size (vector-length (table-data table)))
	 (sizes (memq size table-sizes))
	 (next-size (and sizes (cdr sizes) (cadr sizes))))
    (and next-size
	 (let ((new-data (make-vector next-size '())))
	   (table-for-each (lambda (k v)
			     (hashx-set! (table-hash-fn table)
					 (table-assoc-fn table)
					 new-data
					 k
					 v))
			   table)
	   (set-table-data! table new-data)
	   table))))

(define (assoc-fn-from-eq-fn eq-fn)
  (cond
   ((eq? eq? eq-fn)		assq)
   ((eqv? eqv? eq-fn)		assv)
   ((equal? eqv? eq-fn)		assoc)
   (#t				(letrec ((associate
					  (lambda (key alist)
					    (and alist
						 (if (eq-fn key (caar alist))
						     (car alist)
						     (associate key (cdr alist)))))))
				  associate))))

(define-public make-string-table  (make-table-maker string=? hash))
(define-public make-symbol-table  (make-table-maker eq?      hashq))
(define-public make-integer-table (make-table-maker =	     noop))
