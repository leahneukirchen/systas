;;; tag: Tom Lord Tue Dec  4 14:59:29 2001 (regexps/subst.scm)
;;;
;;; rewrite.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (regexps subst))



(define-public (regexp-subst regexp str rewrite-fn . flags)
  (let ((global? 	(memq :global flags)))
    (let recurse ((str str))
      (regexec regexp
	       str
	       '(< 0 >)
	       ()
	       (lambda (l m r)
		 (string (or l "")
			 (let ((rewrite (rewrite-fn m)))
			   (cond
			    ((string? rewrite)	rewrite)
			    (#t			(->string m))))
			 (if (not r)
			     ""
			     (if (not global?)
				 r
				 (recurse r)))))
	       (lambda () str)))))

