;;; defrec.scm - an implementation of Olin Shivers' define-record macro
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998 Tom Lord
;;;
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;;



(define-module (compatability defrec)
  :use-module (data-structures ratlist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define-record
;;;
;;; This is a re-implementation of Olin Shiver's 
;;; define-record macro.  Olin's (editted) comments 
;;; follow:
;;;
;;; Syntax for defining record types.
;;;
;;; This implementation works with [Systas Scheme] --
;;;
;;; (define-record name . field&method-specs)
;;;
;;; A field-spec is one of the following:
;;;
;;;     field			; Initialised field
;;;     (field [default])	; Defaulted field.
;;;
;;; An initialised field has its initial value passed as an argument to
;;; the the record maker procedure. A defaulted field takes its value from
;;; the the DEFAULT expression. If a DEFAULT expression is not given, then
;;; the defaulted field's initial value is undefined.
;;; 
;;; Example:
;;;
;;; 	(define-record employee
;;; 	    	       name
;;; 	    	       id
;;; 	    	       (salary 10000)
;;; 	    	       (department)		; Initial value undefined.
;;; 	    	       sex
;;; 	    	       married?)
;;; 
;;; Defines the following:
;;;
;;; - A maker procedure:
;;;
;;;	   (make-employee "John Smith" 742931 'male #f)
;;;
;;;   make-employee takes one argument for each initialised field.
;;; 
;;; - Accessor procedures:
;;;   (employee:name emp)
;;;   (employee:id emp)
;;;   (employee:salary emp)
;;;   (employee:department emp)
;;;   (employee:sex emp)
;;;   (employee:married? emp)
;;; 
;;; - Setter procedures:
;;;   (set-employee:name emp "Janet Q. Random")
;;;   (set-employee:id emp 8271)
;;;   (set-employee:salary emp 20000)
;;;   (set-employee:department emp "Vaporware")
;;;   (set-employee:sex emp 'female)
;;;   (set-employee:married? emp #t)
;;; 
;;; - A type predicate:
;;;   (employee? x)
;;; 
;;; - The record type descriptor:
;;;     type/employee
;;;
;;; NOTE: Method specs are not really supported yet.  Only `disclose' works.
;;;
;;; Method specs are of the form
;;;
;;; ((method self var ...) body ...)
;;;
;;; `disclose' is used by the structure printer. E.g.,
;;; 
;;;   (define-record ship
;;;     x
;;;     y
;;;     name
;;;     ((disclose self) (list (ship:name self))))
;;; 
;;; will cause (make-ship 10 20 "Valdez") to print as
;;; 
;;;   #{ship "Valdez"}
;;;



(define (define-record-expander def name . fields-and-methods)
  (let* ((fields (pick (lambda (x) (or (symbol? x) (and (pair? x) (symbol? (car x)))))
		       fields-and-methods))
	 (methods (pick (lambda (x) (and (pair? x) (pair? (car x)))) fields-and-methods))
	 (disclose (choose (lambda (x) (eq? 'disclose (caar x))) methods))
	 (disclose-lambda (and disclose `(lambda ,(cdar disclose) ,@(cdr disclose))))
	 (disclose-proc #f)
	 (set-disclose (lambda (x) (set! disclose-proc x)))
	 (field-names (map (lambda (x) (if (symbol? x) x (car x))) fields))
	 (field-inits (map (lambda (x) (and (pair? x) (or (cdr x) 'unspecified))) fields))
	 (record-type (make-record-type name
					field-names
					(lambda (r p w?)
					  (with-output-to-port p
					    (lambda ()
					      (display* "#<")
					      (let loop ((l (disclose-proc r)))
						(and l
						     (begin
						       (display* (car l) (if (cdr l) " " ""))
						       (loop (cdr l)))))
					      (display ">"))))))
	 (constructor (record-constructor record-type))
	 (predicate (record-predicate record-type))
	 (field-getters (map (lambda (x) (record-accessor record-type x)) field-names))
	 (field-setters (map (lambda (x) (record-modifier record-type x)) field-names))
	 (maker-arity (length (pick null? field-inits)))
	 (maker (letrec ((maker
			  (lambda args
			    (if (not (= (length args) maker-arity))
				(apply throw
				       'parameter-error
				       "wrong number of arguments"
				       maker
				       args))
			    (let ((r (apply constructor field-inits)))
			      (for-each (lambda (setter init)
					  (cond
					   ((pair? init) (setter r (car init)))
					   ((null? init) (setter r (car args))
							 (set! args (cdr args)))))
					field-setters
					field-inits)
			      r))))
		  maker)))

    `(begin
       (if ,disclose-lambda
	   (,set-disclose ,disclose-lambda)
	   (,set-disclose (lambda (obj)
			    (list ',name ,@(map (lambda (f) `(,f obj)) field-getters)))))
       (,def ,(symbol-append 'make- name) ,maker)
       (,def ,(symbol-append name '?) ,predicate)
       (,def ,(symbol-append 'type/ name) ,record-type)
       ,(and field-setters
	     `(begin ,@(map (lambda (name setter) `(,def ,name ,setter))
			    (map (lambda (x) (symbol-append 'set- name ":" x)) field-names)
			    field-setters)))
       ,(and field-getters
	     `(begin ,@(map (lambda (name getter) `(,def ,name ,getter))
			    (map (lambda (x) (symbol-append name ":" x)) field-names)
			    field-getters)))
       ',name)))

	 

(define-public-memoizing-macro (define-record name . fields-and-methods)
  (apply define-record-expander 'define name fields-and-methods))

(define-public-memoizing-macro (define-record-public name . fields-and-methods)
  (apply define-record-expander 'define-public name fields-and-methods))

