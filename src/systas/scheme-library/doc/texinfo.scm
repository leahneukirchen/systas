;;; tag: Tom Lord Tue Dec  4 14:59:32 2001 (doc/texinfo.scm)
;;;
;;; texinfo - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc texinfo)
  :use-module (regexps subst)
  :use-module (regexps cached)
  :use-module (standard string-parsing))



(define-public (texinfo-quote str)
  (regexp-subst (once (regcomp "[@{}]"))
		str
		(lambda (s) (string-append #\@ s))
		:global))


(define-public (texinfo-section-title str)
  (and str (apply string (apply join-fields-with "--" (separate-fields-discarding-char #\: str)))))


(define-public (texinfo-index-category->code category)
  (case category
    ((function)					"fu")
    ((macro)					"ma")
    ((variable)					"va")
    ((type)					"ty")
    ((program)					"pr")
    ((general)					"ge")
    (else					"ge")))
