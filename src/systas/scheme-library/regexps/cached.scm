;;; cached.scm - Cached regexps.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (regexps cached)
  :use-module (data-structures hcons))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cached-regexec
;;
;; An interface to regexp that accepts not only compiled regexps, 
;; but also regexp strings -- it keeps a cache of compiled regexps.
;;

(define-public (cached-regexec r . args) (apply regexec (cached-regexp r) args))

(define regcomp-table (make-vector 63 ()))
(define regexp-buffer (make-gc-buffer 32))
(define-public (cached-regexp s)
  (if (compiled-regexp? s) 
      s
      (regexp-buffer (or (hash-ref regcomp-table s)
			 (let ((x (regcomp s)))
			   (hash-set! regcomp-table s x)
			   x)))))

