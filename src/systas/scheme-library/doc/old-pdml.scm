;;; tag: Tom Lord Tue Dec  4 14:59:31 2001 (doc/old-pdml.scm)
;;;
;;; pdml.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc old-pdml)
  :use-module (doc old-xdml)
  :use-module (doc xdml-html)
  :use-module (doc xdml-texinfo)
  :use-module (doc hdml)
  :use-module (data-structures ratlist))




(define-public old-pdml-block-of-text-tags '(c
					 s
					 program-documentation))

(define-public old-pdml-only-nesting-rules (xdml-combine-nesting-rules
					(xdml-nesting-rules-product xdml-level-0-tags old-pdml-block-of-text-tags)
					(xdml-nesting-rules-product xdml-level-1-tags old-pdml-block-of-text-tags)
					(xdml-nesting-rules-product xdml-level-2-tags old-pdml-block-of-text-tags)
					(xdml-nesting-rules-product xdml-level-3-tags old-pdml-block-of-text-tags)
					(xdml-nesting-rules-product xdml-recursive-block-of-text-tags old-pdml-block-of-text-tags)
					(xdml-nesting-rules-product old-pdml-block-of-text-tags xdml-nonrecursive-block-of-text-tags)))


(define-public old-pdml-nesting-rules (xdml-combine-nesting-rules old-pdml-only-nesting-rules
							      xdml-nesting-rules))

(define-public (old-pdml-parser-extension tag parameters sub-documents nesting-rule further-extension)
  (case tag
    ((c
      s
      program-documentation)		(and parameters (symbol? (car parameters))))
    (else				(if further-extension
					    (further-extension tag parameters sub-documents nesting-rule)
					    (throw 'old-pdml-bad-syntax tag parameters sub-documents)))))

(define-public (old-pdml->html-extension document tag parameters sub-documents database nesting-rules parser-extension further-extension sub-document->html)
  (case tag
    ((c
      s
      program-documentation)		(let* ((categories (or (append (and=> (kw-arg-ref parameters :category) list)
								       (kw-arg-ref parameters :categories))
							     '(function)))
					       (headline (string (char-upcase (string-ref (car categories)))
								 (make-shared-substring (car categories) 1))))
					  #/lazy-string-append
					  /
					  <p><b><u>#,(noop headline)</u> #;
					  <code>#,(sub-document->html (xdml 'label
									    (list :name (string (car parameters))
										  :link (string (car parameters)))))#;
					  #,(sub-document->html (xdml 'index
								      (list :term (string (car parameters))
									    :categories categories)))#;
					  #,(string (car parameters))
					  </code>#;
					  </b><br>
					  #,(apply lazy-string-append
						   (map sub-document->html sub-documents))
					  <br>
					  <br>/#))

    (else				(if further-extension
					    (further-extension tag parameters sub-documents database nesting-rule parser-extension)
					    (throw 'old-pdml-bad-syntax tag parameters sub-documents)))))


(define-public (old-pdml->texinfo-extension document tag parameters sub-documents database nesting-rules parser-extension further-extension sub-document->texinfo)
  (case tag
    ((c
      s
      program-documentation)		(let* ((categories (or (append (and=> (kw-arg-ref parameters :category) list)
								       (kw-arg-ref parameters :categories))
							     '(function)))
					       (headline (string (char-upcase (string-ref (car categories)))
								 (make-shared-substring (car categories) 1)))
					       (label (sub-document->texinfo (xdml 'label (list :name (texinfo-quote (string (car parameters)))))))
					       (index (sub-document->texinfo (xdml 'index
										 (list :term (texinfo-quote (string (car parameters)))
										       :categories categories)))))
					  #/lazy-string-append
					  /
					  @sp 1
					  @need 1750
					  @noindent
					  @b{* #,(noop headline)} @code{#,(texinfo-quote (string (car parameters)))}#;
					  #,label;#;
					  #,index;#;
					  @*
					  #,(if (not sub-documents)
						""
						(sub-document->texinfo (car sub-documents)))#;
					  #,(if (not sub-documents)
						""
						(apply lazy-string-append
						       (map sub-document->texinfo (cdr sub-documents))))/#))

    (else				(if further-extension
					    (further-extension tag parameters sub-documents database nesting-rule parser-extension)
					    (throw 'old-pdml-bad-syntax tag parameters sub-documents)))))
