;;; tag: Tom Lord Tue Dec  4 14:59:30 2001 (=bit-rotted/enum.scm)
;;;
;;; enum.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;;



(define-module (data-structures enum))



(begin
  (define enum-type-type (make-record-type 'enum-type '(name table)))
  (define enum-type-constructor (record-constructor enum-type-type))
  (define-public enum-type? (record-predicate enum-type-type))
  (define-public enum-type-name (record-accessor enum-type-type 'name))
  (define enum-type-table (record-accessor enum-type-type 'table))

  (define enum-constant-type (make-record-type 'enum-constant
					       '(enum-type name)
					       (lambda (e p w?)
						 (display*-port p
								"#<enum "
								(enum-type-name
								 (enum-constant-enum-type e))
								" "
								(enum-constant-name e)
								">"))))
  (define enum-constant-constructor (record-constructor enum-constant-type))
  (define-public enum-constant? (record-predicate enum-constant-type))
  (define-public enum-constant-name (record-accessor enum-constant-type 'name))
  (define-public enum-constant-enum-type (record-accessor enum-constant-type 'enum-type)))


(define-public (make-enum-type name . opt-initial-members)
  (let ((type (enum-type-constructor name (make-hash-table))))
    (and opt-initial-members
	 (for-each (lambda (n) (make-enum-constant type n)) (car opt-initial-members)))
    type))

(define-public (make-enum-constant enum-type name)
  (or (hashq-ref (enum-type-table enum-type) name)
      (let ((answer (enum-constant-constructor enum-type name)))
	(hashq-set! (enum-type-table enum-type) name answer)
	answer)))

