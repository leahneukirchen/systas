;;; tag: Tom Lord Tue Dec  4 14:59:34 2001 (compatability/conditionals.scm)
;;;
;;; conditionals.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; See the file "=scsh-copyright".
;;;



(define-module (compatability conditionals))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Novel Conditionals Syntax from FPS
;;; 

(define-public-syntax when
  (syntax-rules ()
    ((when bool body1 body2 ...)
     (if bool (begin body1 body2 ...)))))


(define-public-syntax unless
  (syntax-rules ()
    ((unless bool body1 body2 ...)
     (if (not bool) (begin body1 body2 ...)))))

(define-public-syntax ?	; ? is synonym for COND.
  (syntax-rules ()
    ((? clause ...) (cond clause ...))))


;; (switch eq val . cases)
;; (switchq eq val . cases)
;; 
;; Like `case', but you specify the key-comparison procedure.
;; `switch' evaluates its keys each time through the conditional.
;; 
;; `switchq' keys are not evaluated -- they are simply constants.
;; `switch' keys are evaluated -- they are simply constants.
;; 
;;   (switchq string=? (vector-ref vec i)
;;     (("plus" "minus") ...)
;;     (("times" "div")  ...)
;;     (else ...))
;; 
(define-public-syntax switchq
  (syntax-rules ()
    ((switchq compare key clause ...)
     (let ((k key)			; Eval KEY and COMPARE
	   (c compare))			; just once, then call %switch.
       (%switchq c k clause ...)))))	; C, K are vars, hence replicable.

(define-public-syntax %switchq
  (syntax-rules (else)
    ((%switchq compare key ((key1 ...) body1 body2 ...) rest ...)
     (if (or (compare key 'key1) ...)
	 (begin body1 body2 ...)
	 (%switchq compare key rest ...)))

    ((%switchq compare key ((key1 ...)) rest ...)	; Null body.
     (if (not (or (compare key 'key1) ...))
	 (%switchq compare key rest ...)))
    
    ((%switchq compare key (else body ...))
     (begin body ...))

    ((%switchq compare key) '#f)))


(define-public-syntax switch
  (syntax-rules ()
    ((switch compare key clause ...)
     (let ((k key)			; Eval KEY and COMPARE
	   (c compare))			; just once, then call %switch.
       (%switch c k clause ...)))))	; C, K are vars, hence replicable.

(define-public-syntax %switch
  (syntax-rules (else)
    ((%switch compare key ((key1 ...) body1 body2 ...) rest ...)
     (if (or (compare key key1) ...)
	 (begin body1 body2 ...)
	 (%switch compare key rest ...)))

    ((%switch compare key ((key1 ...)) rest ...)	; Null body.
     (if (not (or (compare key key1) ...))
	 (%switch compare key rest ...)))
    
    ((%switch compare key (else body ...))
     (begin body ...))

    ((%switch compare key) '#f)))

