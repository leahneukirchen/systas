;;; hash-tables.scm - A proposed hash table standard (incomplete)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (standard hash-tables))



;; hash-table-for-each proc table
;; 
;; For each `key' and `value' pair in `table', invoke:
;; 
;; 	(proc key value)
;; 
(define-public (hash-table-for-each proc table)
  (vector-for-each (lambda (chain)
		     (for-each (lambda (entry)
				 (proc (car entry) (cdr entry)))
			       chain))
		   table))
