;;; tag: Tom Lord Tue Dec  4 14:59:34 2001 (formatting/numbers.scm)
;;;
;;; ordinals.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;;



(define-module (formatting numbers))



(define-public (digit-ordinal n)
  (string-append (number->string n)
		 (case (abs n)
		   ((0) (throw 'no-ordinal-for-0 digit-ordinal n))
		   ((1)	"st")
		   ((2) "nd")
		   ((3) "rd")
		   (else "th"))))

    