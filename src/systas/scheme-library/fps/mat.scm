;;; mat.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;


;; =================================================================
;; Functional PostScript

;; mat.scm

;; This file contains transformation procedures that create instance
;; objects (path or pictures) by general transformation, translation,
;; rotation, and scaling.

;; (translate x-offset y-offset obj)  -> instance obj
;; (rotate angle obj)                 -> instance obj
;; (scale factor-x factor-y obj)      -> instance obj
;; Note: obj can be path or picture

;; ==================================================================

;; apply TM to an object
(define (transform TM obj)
  (make-instance TM obj TM))

(define (translate x-offset y-offset obj)
  (if (the-empty-obj? obj)
      obj
      (if (or (path? obj) (picture? obj))
	  (make-instance (t-matrix x-offset y-offset) obj 
			 (pt x-offset y-offset))
	  (error translate
		 "Object to be translated must be a path or a picture"
		 obj))))


(define (rotate angle obj)
  (if (the-empty-obj? obj)
      obj
      (if (or (path? obj) (picture? obj))
	  (make-instance (r-matrix angle) obj angle)
	  (error rotate
		 "Object to be rotated must be a path or a picture"
		 obj))))

 
(define (scale factor-x factor-y obj)
  (if (the-empty-obj? obj)
      obj
      (if (or (path? obj) (picture? obj))
	  (make-instance (s-matrix factor-x factor-y) obj 
			 (list factor-x factor-y))
	  (error scale
		 "Object to be scaled must be a path or a picture"
		 obj))))


;; returns a translation matrix given at offset point
(define (t-matrix x-offset y-offset)
  (matrix 1 0 0 1 x-offset y-offset))


;; returns a rotation matrix given an angle (in RADIAN)
(define (r-matrix angle)
  (let ((sine   (sin angle))
	(cosine (cos angle)))
    (matrix cosine sine (- sine) cosine 0 0))) 


;; given a scale matrix given x and y strech-factors
(define (s-matrix x y)
  (matrix x 0 0 y 0 0))


;; tranform a point with the given TM
(define (transform-pt TM point)
  (let ((a (matrix:a TM))
	(b (matrix:b TM))
	(p (matrix:p TM))
	(q (matrix:q TM))
	(x (matrix:x TM))
	(y (matrix:y TM))
	(xcoord (pt:x point))
	(ycoord (pt:y point)))
    (pt (+ (* xcoord a) (* ycoord p) x)
	(+ (* xcoord b) (* ycoord q) y))))


;; Compare two matrices to see if they are the same.
(define (matrix= tm1 tm2)
  (or (eq? tm1 tm2)
      (let ((a matrix:a)
	    (b matrix:b)
	    (p matrix:p)
	    (q matrix:q)
	    (x matrix:x)
	    (y matrix:y))
	(and (= (a tm1) (a tm2))
	     (= (b tm1) (b tm2))
	     (= (p tm1) (p tm2))
	     (= (q tm1) (q tm2))
	     (= (x tm1) (x tm2))
	     (= (y tm1) (y tm2))))))

	 

;; ===== End of mat.scm ==========================================
