;;; string-lib.scm - string utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2000, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (standard string-lib))



(define-public (string-downcase s)
  (string-downcase! (string s)))

(define-public (string-upcase s)
  (string-upcase! (string s)))

(define-public (string-capitalize! str)
  (let loop ((make-upper?		#t)
	     (pos			0))

    (cond
     ((>= pos (string-length str))				str)

     ((not (char-alphabetic? (string-ref str pos)))		(loop #t (1+ pos)))

     (make-upper?						(string-set! str pos (char-upcase (string-ref str pos)))
								(loop #f (1+ pos)))

     (#t							(string-set! str pos (char-downcase (string-ref str pos)))
								(loop #f (1+ pos))))))


(define-public (string-capitalize str)
  (string-capitalize! (string str)))

