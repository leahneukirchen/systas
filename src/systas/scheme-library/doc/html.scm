;;; tag: Tom Lord Tue Dec  4 14:59:31 2001 (doc/html.scm)
;;;
;;; html.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (doc html)
  :use-module (regexps subst)
  :use-module (regexps cached)
  :use-module (data-structures ratlist)
  :use-module (data-structures string-fun))



(define-public (html-quote s)
  (and s
       (let* ((i (apply string-append-with-separator
			"&amp;"
			(separate-fields-discarding-char #\& s list)))
	      (j (apply string-append-with-separator
			"&lt;"
			(separate-fields-discarding-char #\< i list)))
	      (k (apply string-append-with-separator
			"&gt;"
			(separate-fields-discarding-char #\> j list)))
	      (l (apply string-append-with-separator
			"&quot;"
			(separate-fields-discarding-char #\" j list))))
	 l)))

(define-public (normalize-html-label str)
  (apply string-append-with-separator
	 "_"
	 (separate-fields-discarding-regexp (once (cached-regexp "[\n\t ]\\+"))
					    (sans-surrounding-whitespace str)
					    list)))
