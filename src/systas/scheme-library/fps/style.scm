;;; style.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;


;; ===================================================================
;; Functional PostScript 

;; style.scm

;; This file contains style and attribute building, checking, and font
;; selection and checking procedures.

;; (with-style style exp ...)          syntax
;; (with-attrib (attrib ...) exp ...)  syntax
;; (vary-default . attribs)            -> style
;; (build-style style . attribs)       -> style
 
;; (!color val)        -> attrib
;; (!line-width val)   -> attrib
;; (!line-cap val)     -> attrib
;; (!line-join val)    -> attrib
;; (!miter-limit val)  -> attrib
;; (!dash-pattern vec) -> attrib
;; (!dash-offset  int) -> attrib

;; ===================================================================
 
;; temporarily set the set to the default style
(define-syntax with-style
  (syntax-rules ()
     ((with-style style exp ...)
      (with-style* style (lambda () exp ...)))))

;; the procedure that goes with the with-style macro
(define (with-style* style thunk)
  (let-fluid default-style* style
     (lambda ()
       (thunk))))

;; temporarily alter the attributes of the default style
(define-syntax with-attrib
  (syntax-rules ()
     ((with-attrib (attrib ...) exp ...)
      (with-style (apply build-style (default-style) (list attrib ...))
		  exp ...))))

;; build a style by varying the attributes of the defaults
(define (vary-default . attribs)
  (if (null? (filter (lambda (x) (not (attrib? x))) attribs))
      (apply build-style (default-style) attribs)
      (error vary-default
	     "Arguments must all be attributes"
	     attribs)))
 

;; build a style by altering the attributes of a given style 
(define (build-style style . attribs)
  (let ((new-style (dup-style style)))
    (for-each (lambda (a) ((attrib:setter a) new-style (attrib:val a)))
	      attribs)
    new-style))

;; Duplicate the given style to produce another independent style record 
(define (dup-style s)
  (make-style* (style:color s)
	       (style:line-width s)
	       (style:line-cap s)
	       (style:line-join s)
	       (style:miter-limit s)
	       (style:dash-pattern s)
	       (style:dash-offset s)))


;; === Attributes ====================================================

(define (!color val)
  (make-attrib set-style:color val))

(define (!line-width val)
  (make-attrib set-style:line-width val))

(define (!line-cap val)
  (make-attrib set-style:line-cap val))

(define (!line-join val)
  (make-attrib set-style:line-join val))

(define (!miter-limit val)
  (make-attrib set-style:miter-limit val))

(define (!dash-pattern pattern-vect)
  (make-attrib set-style:dash-pattern pattern-vect))

(define (!dash-offset offset)
  (make-attrib set-style:dash-offset offset))


;; ===== Style Consistency check =====================================

;; checks a style of a given picture against the current style.
;; update if different.
(define (update-style current-style style color-func channel)
  (let ((interface    (channel:interface channel))
	(color        (color-func (style:color style)))
	(line-width   (style:line-width   style))
	(line-cap     (style:line-cap     style))
	(line-join    (style:line-join    style))
	(miter-limit  (style:miter-limit  style))
	(dash-pattern (style:dash-pattern style))
	(dash-offset  (style:dash-offset  style)))

    (if (not (color= (style:color current-style) color))
        ((channel:setcolor channel) color interface))
  
    (if (not (= (style:line-width current-style) line-width))
        ((channel:setlinewidth channel) line-width interface))
  
    (if (not (equal? (style:line-cap current-style) line-cap))
        ((channel:setlinecap channel) line-cap interface))
    
    (if (not (equal? (style:line-join current-style) line-join))
        ((channel:setlinejoin channel) line-join interface))
    
    (if (not (equal? (style:miter-limit current-style) miter-limit))
        ((channel:setmiterlimit channel) miter-limit interface))
    
    (if (or (not (equal? (style:dash-pattern current-style) dash-pattern))
	    (not (= (style:dash-offset  current-style) dash-offset)))
        ((channel:setdash channel) dash-pattern dash-offset interface))))
  

;; ===== End of style.scm ========================================
