;;; fluid.scm - fluid variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998 Tom Lord
;;;
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;;



(define-module (compatability fluid))



(define fluid-type (make-record-type 'fluid-variable '(value)
				     (lambda (f p w?)
				       (display*-port p "#<fluid " (fluid f) ">"))))

(define-public make-fluid (record-constructor fluid-type))
(define-public fluid (record-accessor fluid-type 'value))
(define-public set-fluid! (record-modifier fluid-type 'value))

(define-public (let-fluid f val thunk)
  (let ((swap (lambda ()
		(let ((temp (fluid f)))
		  (set-fluid! f val)
		  (set! val temp)))))
    (dynamic-wind swap thunk swap)))

