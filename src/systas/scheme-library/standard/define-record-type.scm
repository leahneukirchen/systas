;;; define-record-type.scm - nice define-record-type syntax
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (standard define-record-type))



;; (define-record-type type-name constructor predicate . field-specs)
;; (define-public-record-type type-name constructor predicate . field-specs)
;; 
;; Syntax for creating a new record type.  None of the arguments
;; are evaluated.
;; 
;; `type-name' is a symbol, used for printing and debugging.
;; 
;; `constructor' is of the form:
;; 
;; 	(constructor-name field-name ...)
;; 
;; `predicate' is a symbol.
;; 
;; Each `field-spec' is a list of three symbols:
;; 
;; 	(field-name	accessor	modifier)
;; 
;; This defines a new record type and the indicated constructor.
;; It defines a predicate for the type.  For each field, it defines
;; the indicated accessor and modifier.
;; 
;; 


(define generalized-define-record-type
  (syntax-rules ()
    ((generalized-define-record-type definer type-name
       (constructor-name field ...)
       predicate-name
       (field-tag accessor modifier) ...)	(begin
						  (definer type-name (make-record-type 'type-name
										      '(field-tag ...)
										      (lambda (obj port writing?)
											(display "#<" port)
											(display 'type-name port)
											(display " " port)
											(write (list `(field-tag ,(accessor obj)) ...) port)
											(display ">"))))
						  (definer constructor-name (record-constructor type-name '(field ...)))
						  (definer predicate-name (record-predicate type-name))
						  (begin (definer accessor (record-accessor type-name 'field-tag))
							 (definer modifier (record-modifier type-name 'field-tag))) ...))))

(define-public define-record-type
  (syntax-rules ()
    ((define-record-type type-name
       (constructor-name field ...)
       predicate-name
       (field-tag accessor modifier) ...)	(generalized-define-record-type define
										type-name
										(constructor-name field ...)
										predicate-name
										(field-tag accessor modifier) ...))))

(define-public define-public-record-type
  (syntax-rules ()
    ((define-record-type type-name
       (constructor-name field ...)
       predicate-name
       (field-tag accessor modifier) ...)	(generalized-define-record-type define-public
										type-name
										(constructor-name field ...)
										predicate-name
										(field-tag accessor modifier) ...))))

