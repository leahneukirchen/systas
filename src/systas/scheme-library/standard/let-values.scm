;;; let-values.scm - S48-style let-values and let*-values
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2000 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (standard let-values))



(define-public let*-values
  (syntax-rules ()
    ((let*-values ((formals exp) . rest) . body)
     (call-with-values (lambda () exp)
		       (lambda formals
			 (let*-values rest . body))))
    ((let*-values () . body) (begin . body))))


(define-public let-values
  (syntax-rules ()
    ((let-values ((formals exp) . rest) . body)
     (call-with-values (lambda () exp)
		       (let-values rest (lambda formals . body))))
    ((let-values () . body) (begin . body))))


