;;; tag: Tom Lord Tue Dec  4 14:59:32 2001 (doc/pdml.scm)
;;;
;;; pdml.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc pdml)
  :use-module (doc xdml)
  :use-module (doc hdml)
  :use-module (doc html)
  :use-module (doc texinfo)
  :use-module (doc markup)
  :use-module (data-structures ratlist))


(declare-markup-language 'pdml
			 :synopsis "An extension of XDML for program and library documentation."
			 :documentation (value #/quote
					       /
					       PDML is an extension of XDML for documenting programs
					       and programming libraries.

					       In addition to the markups of XDML, it provides for
					       specially formatted entries documenting functions,
					       macros, types, and variables.  Eventually it will
					       also include markups for documenting command-line 
					       interfaces.

					       PDML can currently be rendered into HTML or texinfo.  
					       /#)

			 :parents '(xdml))


(define-public pdml-block-of-text-tags '(c))


(declare-markup-tag 'pdml 'h1
		    :can-contain	'(c))
(declare-markup-tag 'pdml 'h2
		    :can-contain	'(c))
(declare-markup-tag 'pdml 'h3
		    :can-contain	'(c))
(declare-markup-tag 'pdml 'h4
		    :can-contain	'(c))


(declare-markup-tag 'pdml 'c
		    :can-contain 	xdml-block-of-text-tags
		    :parameter-check 	#f ; !!!
		    :synopsis 		"Documentation for a function, type, macro, or variable."
		    :documentation 	#f ; !!!

		    :formatters		`((html 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let* ((categories (or (append (and=> (kw-arg-ref parameters :category) list)
											  (kw-arg-ref parameters :categories))
										  '(function)))
								  (headline (string (char-upcase (string-ref (car categories)))
										    (make-shared-substring (car categories) 1))))
							     #/lazy-string-append
							     /
							     <p><b><u>#,headline;</u> #;
							     <code>#,(format-sub-document `((label :name ,(string (car parameters))
												   :link ,(string (car parameters)))))#;
							     #,(format-sub-document `((index :term ,(string (car parameters))
											     :categories ,categories)))#;
							     #,(string (car parameters))
							     </code>#;
							     </b><br>
							     #,(apply lazy-string-append (map format-sub-document sub-documents))
							     <br>
							     <br>/#)))
							   

					  (texinfo 	,(lambda (document tag parameters sub-documents database format-sub-document)
							   (let* ((categories (or (append (and=> (kw-arg-ref parameters :category) list)
											  (kw-arg-ref parameters :categories))
										  '(function)))
								  (headline (string (char-upcase (string-ref (car categories)))
										    (make-shared-substring (car categories) 1)))
								  (label (sub-document->texinfo `((label :name ,(string (car parameters))))))
								  (index (sub-document->texinfo `((index :term ,(string (car parameters))
													 :categories ,categories)))))
							     #/lazy-string-append
							     /
							     @sp 1
							     @need 1750
							     @noindent
							     @b{* #,(val headline)} @code{#,(string (car parameters))}#;
							     #,label;#;
							     #,index;#;
							     @*
							     #,(apply lazy-string-append
									  (map format-sub-document sub-documents))/#)))))


