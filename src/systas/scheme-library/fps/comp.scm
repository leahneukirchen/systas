;;; comp.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;


;; ===================================================================
;; Functional PostScript

;; comp.scm

;; This file contains path and picture composition procedures

;; (compose obj  [more-objs])  -> composite
;; (compose-path [paths])      -> composite
;; (compose-pict [picts])      -> composite
;; (join    obj  [more-objs])  -> composite
;; (join-path    [paths])      -> composite
;; (join-pict    [picts])      -> composite
;; (link         [more-paths]) -> composite

;; ===================================================================


;; COMPOSE, COMPOSE-PATH, COMPOSE-PICT:   Make a composite object
;; without any transformation. The first obj is put down on the page,
;; and the second obj is put down right on top of the first, etc.
;; Paths that have been composed toghether *remain disjoint*.

;; compose is the polymorphic form that takes either paths or picture.
;; It must have at least one argument. compose-path and compose-pict compose
;; paths and pictures respectively.

(define (compose obj . more-objs)

  ;; make sure arguments are all paths or all pictures
  (if (null? (let ((p (lambda (x) (not ((if (path? obj) path? picture?) x)))))
	       (filter p (cons obj more-objs))))
      
      (let ((objs (filter not-the-empty-obj? (cons obj more-objs))))
	(cond ((null? objs) obj)              ; the empty path or picture
	      ((null? (cdr objs)) (car objs)) ; one element left
	      (else (make-composite objs))))  ; make composite
      (error compose 
	     "Arguments to compose must be all paths or all pictures"
	     obj more-objs)))

(define (compose-path . paths)
  (if (null? (filter (lambda (x) (not (path? x))) paths))
      
      (let ((paths (filter not-the-empty-path? paths)))
	(cond ((null? paths) the-empty-path)
	      ((null? (cdr paths)) (car paths))
	      (else (make-composite paths))))
      
      (error compose-path
	     "Arguments to compose-path must be all paths"
	     paths)))
  
(define (compose-pict . picts)
  (if (null? (filter (lambda (x) (not (picture? x))) picts))
      
      (let ((picts (filter not-the-empty-pict? picts)))
	(cond ((null? picts) the-empty-pict)
	      ((null? (cdr picts)) (car picts))
	      (else (make-composite picts))))
      (error compose-pict
	     "Arguments to compose-pict must be all pictures"
	     picts)))
	 


;; JOIN, JOIN-PATH, JOIN-PICT: join one or more paths/pictures
;; together by translating the second obj on the end of the first one
;; and then linking them. Paths that have been joined together become
;; a single path.

(define (join obj . more-objs)
  (if (null? (let ((p (lambda (x) (not ((if (path? obj) path? picture?) x)))))
	       (filter p (cons obj more-objs))))
      
      (let ((objs (filter not-the-empty-obj? (cons obj more-objs))))
	(cond ((null? objs) obj)
	      ((null? (cdr objs)) (car objs))
	      (else 
	       (make-linked-composite
		(cons (car objs)
		      (let lp ((curr-end-pt (end-pt (car objs)))
			       (objs (cdr objs)))
			(if (null? objs)
			    '()
			    (let* ((curr-obj (car objs))
				   (start    (start-pt curr-obj))
				   (start-x  (pt:x start))
				   (start-y  (pt:y start)))
			      (let ((translated
				     (translate (- (pt:x curr-end-pt) start-x)
						(- (pt:y curr-end-pt) start-y)
						curr-obj)))
				(cons translated
				      (lp (end-pt translated)
					  (cdr objs))))))))))))
      (error join
	     "Arguments must be all paths or all pictures"
	     obj more-objs)))


(define (join-path . paths)
  (if (null? (filter (lambda (x) (not (path? x))) paths))
      (apply join (cons the-empty-path paths))
      (error join-path
	     "Arguments must be all paths"
	     paths)))
  
(define (join-pict . picts)
  (if (null? (filter (lambda (x) (not (picture? x))) picts))
      (apply join (cons the-empty-pict picts))
      (error join-pict
	     "Arguments must be all picts"
	     picts)))

;; LINK:   linking makes two paths into a single path by appending a
;; straight line segment in between them, which garantees that the two
;; paths are no longer disjoint subpaths. The line-join attribute
;; applies at the junction of these paths.

;; There are, of course, some exceptions. Some paths always remain
;; disjoint by nature: close-path, glyphpath, and stroke-outline-path. 
;; Generally it is not a good idea to link these paths with others 
;; because they cannot be 'fused' with other paths.

(define (link .  paths)
  (if (null? (filter (lambda (x) (not (path? x))) paths))
      
      (let ((paths (filter not-the-empty-path? paths)))
	(cond ((null? paths) the-empty-path)
	      ((null? (cdr paths)) (car paths))
	      (else (make-linked-composite paths))))
      (error link
	     "Arguments to link must be all paths"
	     paths)))



;; ===== End of comp.scm =========================================

