;;; hdml.scm - Hierarchical Document Mark-up Language
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc hdml))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;(h0 "Hierarchical Document Mark-up Language"
;;;	:scheme-module (doc hdml))
;;; 
;;; "HDML", the Hierarchical Document Mark-up Language, is a Scheme
;;; data structure for representing hierarchically structured fancy
;;; text.
;;;
;;; A document may be:
;;;
;;;	()		- the empty document
;;;	a string	- a simple plain-text document
;;; 
;;; or a list of the form:
;;;
;;;	((<tag> . <parameters>) . <sub-documents>)
;;;
;;; where `<tag>' is a symbol that names the style of document,
;;; `<parameters>' is a list of arbitrary values, and `<sub-documents>'
;;; is a list of HDML documents.
;;;
;;; The meaning of tags and parameters is application specific -- HDML is
;;; simply a data structure.
;;;



;;(s make-hdml)
;; (make-hdml tag parameters sub-documents)
;;
;; Construct a new HDML document from the indicated tag, parameters, and
;; sub-documents.
;;
(define-public (make-hdml tag parameters sub-documents)
  (parse-hdml (cons (cons tag parameters) sub-documents)
	      (lambda (tag parameters sub-documents)
		(cons (cons tag parameters) sub-documents))))

;;(s hdml)
;; (hdml tag parameters . sub-documents)
;;
;; Construct a new HDML document from the indicated tag, parameters, and
;; sub-documents.
;;
(define-public (hdml tag parameters . sub-documents)
  (make-hdml tag parameters sub-documents))


;;(s hdml?)
;; (hdml? obj)
;;  
;; Return #t if `obj' is a valid HDML document, #f otherwise.  This is
;; a recursive test that examines all sub-documents of `obj'.
;;
(define-public (hdml? obj)
  (or (null? obj)
      (string? obj)
      (and (pair? l)
	   (pair? (car l))
	   (symbol? (caar l))
	   (list? (cdar l))
	   (and-map hdml? (cdr l)))))

	
;;(s parse-hdml)
;; (parse-hdml document return :optional error-return)
;; 
;; Parse HDML `document' into a tag, parameter list, and sub-document
;; list and invoke:
;;
;;	(apply return tag parameters sub-documents)
;; 
;; If `document' can not be parsed, invoke:
;; 
;;	(error-return document return)
;; 
;; or
;; 
;; 	(throw 'hdml-syntax-error document)
;; 
;; if `error-return' is #f.
;; 
;; Note that the tag, parameters and sub-document list of the document
;; `()' are all `()'.
;; 
;; The tag of a document which is a string is `'string' while its
;; parameter and sub-document lists are `()'.  `'string' is not a
;; valid tag for any other kind of HDML document.
;; 
;; The parsing procedure checks the syntax non-recursively --
;; sub-documents are not checked.
;; 
;; As a special case, if `error-return' is #t, no error checking is
;; performed.  This can save parsing time if it is known that
;; `document' is valid hdml.  If `document' is not valid hdml,
;; errors of an unspecified sort may occur.
;; 
(define-public (parse-hdml document return :optional error-return)
  (cond
   ((null? document)					(return () ()))

   ((string? document)					(return 'string ()))

   ((eq? #t error-return)				(apply return
							       (caar document)
							       (cdar document)
							       (cdr document)))

   ((and (list? document)
	 (pair? (car document))
	 (symbol? (caar document))
	 (list? (cdar document))
	 (not (eq? 'string (caar document))))	      	(apply return
							       (caar document)
							       (cdar document)
							       (cdr document)))
   
   (#t							(if error-return
							    (error-return document return)
							    (throw 'hdml-syntax-error document)))))

(define-public (hdml-tag document)
  (parse-hdml document first-value))

