;;; state-machine.scm - coherently organzied collections of variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;;



(define-module (standard state-machine)
  :use-module (standard define-record-type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; State Machines 
;;; 
;;; A "state machine" consists of a list of variables and a transition
;;; procedure.  The variables may be local variables from any closure
;;; or global variables from any module.
;;; 
;;; The "state machine transition procedure" of a state machine is a
;;; procedure that calls all of the individual transition procedures
;;; to determine new values for the variables.  This makes it easy to
;;; synchronize some collection of variables even if the definitions
;;; of those variables are scattered throughout source code.
;;; 
;;;

;; A state machine's state is:
;;
;;	transitions-alist	An association of first-class variables
;;				to (re-)initializer procedures.
;;
(define-record-type state-machine
  (make-state-machine)
  state-machine?
  (transitions-alist	state-machine:transitions-alist	set-state-machine:transitions-alist!))

;; make-state-machine
;;
;; Return a new state machine.
;;
(define-public make-state-machine make-state-machine)
(define-public state-machine? state-machine?)


;; state-machine-transition-procedure state-machine
;; 
;; Return a procedure of arbitrary arguments that sets all variables
;; in `state-machine' to the values returned by their transition
;; procedures.  Arguments to the procedure returned are passed to
;; the individual initializer procedures.
;; 
(define-public (state-machine-transition-procedure state-machine)
  (lambda args
    (let ((transitions (state-machine:transitions-alist state-machine)))
      (for-each (lambda (variable-initializer)
		  (variable-set! (car variable-initializer) (apply (cdr variable-initializer) args)))
		transitions))))


;; set-state-machine-component-transition state-machine variable transition
;; 
;; Add `variable', a first-class variable, to `state-machine' if it is
;; not already present.  Make the transition procedure for that
;; variable `transition'.  
;;
;; For example:
;;
;;	(define state-machine (make-state-machine))
;;	(define reset-state-machine (state-machine-transition-procedure state-machine))
;;
;;	(define x-frob (let ((x 'initial-value))
;;	  		 (set-state-machine-component-transition state-machine
;;								 (denoted-variable 'x (the-environment))
;;								 (lambda () 'initial-value))
;;			 (lambda args
;;			   (if (null? args)
;;			       x
;;			       (set! x (car args))))))
;;
;;	(x-frob) => initial-value
;;	(x-frob 'new-value) => new-value
;;	(x-frob) => new-value
;;	(reset-state-machine) => <unspecified>
;;	(x-frob) => initial-value
;;
(define-public (set-state-machine-component-transition state-machine variable transition)
  (let ((transitions (state-machine:transitions-alist state-machine)))
    (set-state-machine:transitions-alist! state-machine (assq-set! transitions variable transition))))


;; define-state-machine-component name state-machine initial-value
;;
;; Syntax.
;; 
;; Create a new private definition for `name' (unevaluated), binding
;; it to `initial-value' (evaluated).  Add the variable denoted by
;; `name' to `state-machine' (evaluated) giving it an initializer
;; procedure:
;;
;;		(lambda () initial-value)
;;
;; See also `set-state-machine-component-transition' and `define-public-state-machine-component'
;;
(define-public-syntax define-state-machine-component
  (syntax-rules ()
    ((define-state-machine-component name state initialization)		(state-machine-component-definer define name state initialization))))


;; define-public-state-machine-component name state-machine initial-value
;;
;; Syntax.
;;
;; Create a new publicly visible definition for `name' (unevaluated),
;; binding it to `initial-value' (evaluated).  Add the variable denoted
;; by `name' to `state-machine' (evaluated) giving it an initializer
;; procedure:
;;
;;		(lambda () initial-value)
;;
;; See also `set-state-machine-component-transition' and `define-state-machine-component'
;;
(define-public-syntax define-public-state-machine-component
  (syntax-rules ()
    ((define-public-state-machine-component name state initialization)	(state-machine-component-definer define-public name state initialization))))


;; state-machine-component-definer definer name state initialization
;;
;; Expand either `define-public-state-machine-component' or 
;; `define-state-machine-component' depending on the value of `definer'
;; which should be either `define' or `define-public'.
;;
(define-syntax state-machine-component-definer
  (syntax-rules ()
    ((state-component-definer definer name state initialization)	(begin
									  (definer name initialization)
									  (set-state-machine-component-transition state
														   (denoted-variable 'name (the-environment))
														   (lambda () initialization))))))
