;;; color.scm -
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord (same terms)
;;;


;; ===================================================================
;;
;; This file contains FPS color and related procedures.
;; Color makers, color descriptors, and color comparators.

;; (rgb r g b)      -> color
;; (hsb h s b)      -> color
;; (cmyk c m y k)   -> color
;; (rgb:r color) (rgb:g color) (rgb:b color)                   -> number
;; (hsb:h color) (hsb:s color) (hsb:b color)                   -> number
;; (cmyk:c color) (cmyk:m color) (cmyk:y color) (cmyk:k color) -> number
;; (color?  c)      -> boolean
;; (color= c1 c2)   -> boolean

;; ===================================================================

;; FPS COLORS:

;; There are four models of color description in FPS. 
;;   - RGB  [Red Green Blue]
;;   - HSB  [Hue Saturation Brightness]
;;   - CMYK [Cyan Magenta Yellow Black]
;;   - Gray [Gray]

;; A color can be described by any of the model above; except
;; for gray, which can only describe colors in monochrome. 

;; To make a color, simply describe the color in one of the four
;; models using the appropriate color maker. 

;; Once a color is made, you don't need to remember its original 
;; model of description or the detail of description because FPS
;; automatically translates color between color models for you.
;; For example, you can create a color in RGB, and then find out
;; about its brightness by calling hsb:h on that color.

;; Record types for the four models of color
(define-record rgb red green blue)
(define-record hsb hue sat bri)
(define-record cmyk cyan mag yel black)
(define-record gray gray-val)

;; make color in RGB color model
(define (rgb red green blue)
  (let ((r (check-range red))
	(g (check-range green))
	(b (check-range blue)))
    (make-rgb r g b)))

;; make color in gray color model
(define (gray gray-val)
  (rgb gray-val gray-val gray-val))

;; make color in HSB color model
(define (hsb hue sat bri)
  (let ((h (check-range hue))
	(s (check-range sat))
	(b (check-range bri)))
    (make-hsb h s b)))

;; make color in CMYK color model
(define (cmyk cyan magenta yellow black)
  (let ((c (check-range cyan))
	(m (check-range magenta))
	(y (check-range yellow))
	(k (check-range black)))
    (make-cmyk c m y k)))

;; Internal utilty procedure. Since all color components values are
;; to be between 0 and 1, values outside of this range will be approx
;; down to the nearest legal value.
(define (check-range c) (cond ((> c 1) 1) ((< c 0) 0) (else c)))


;; ===== RGB Color Descriptors =======================================

(define (rgb:r color)
  (cond ((rgb? color) (rgb:red color))
	((hsb? color) (rgb:red (hsb->rgb (hsb:hue color)
					 (hsb:sat color)
					 (hsb:bri color))))
	((cmyk? color) (- 1 (cmyk:cyan color)))
	((gray? color) (gray:gray-val color))
	(else (error rgb:r
		     "the argument to rgb:r must be a color" color))))

(define (rgb:g color)
  (cond ((rgb? color) (rgb:green color))
	((hsb? color) (rgb:green (hsb->rgb (hsb:hue color)
					   (hsb:sat color)
					   (hsb:bri color))))
	((cmyk? color) (- 1 (cmyk:mag color)))
	((gray? color) (gray:gray-val color))
	(else (error rgb:g
		     "the argument to rgb:g must be a color" color))))

(define (rgb:b color)
  (cond ((rgb? color) (rgb:blue color))
	((hsb? color) (rgb:blue (hsb->rgb (hsb:hue color)
					  (hsb:sat color)
					  (hsb:bri color))))
	((cmyk? color) (- 1 (cmyk:yel color)))
	((gray? color) (gray:gray-val color))
	(else (error rgb:b
		     "the argument to rgb:b must be a color" color))))


;; ===== HSB Color Descriptors =======================================

(define (hsb:h color)
  (let ((find-hue (lambda (r g b)
		    (let* ((max (max r g b))
			   (min (min r g b))
			   (delta (- max min))
			   (hue (* 1/6
				   (cond ((= max min) 0)
					 ((= max r) (/ (- g b) delta))
					 ((= max g) (+ 2 (/ (- b r) delta)))
					 ((= max b) (+ 4 (/ (- r g) delta)))
					 (else (internal-bug hsb:h
							     "cannot translate rgb"
							     color))))))
		      (if (< hue 0) (+ 1 hue) hue)))))
    (cond ((hsb? color) (hsb:hue color))
	  ((rgb? color) (find-hue (rgb:red color) 
				  (rgb:green color)
				  (rgb:blue color)))
	  ((cmyk? color) (find-hue (- 1 (rgb:red color))
				   (- 1 (rgb:green color))
				   (- 1 (rgb:blue color))))
	  ((gray? color) 0)
	  (else (error hsb:h
		       "the argument to hsb:h must be a color" color)))))

(define (hsb:s color)
  (let ((find-sat (lambda (r g b)
		    (let ((max (max r g b))
			  (min (min r g b)))
		      (if (= max 0) 0 (/ (- max min) max))))))
    (cond ((hsb? color) (hsb:sat color))
	  ((rgb? color) (find-sat (rgb:red color) 
				  (rgb:green color)
				  (rgb:blue color)))
	  ((cmyk? color) (find-sat (- 1 (rgb:red color))
				   (- 1 (rgb:green color))
				   (- 1 (rgb:blue color))))
	  ((gray? color) 0)
	  (else (error hsb:s 
		       "the argument to hsb:s must be a color" color)))))

(define (hsb:b color)
  (cond ((hsb? color) (hsb:bri color))
	((rgb? color) (max (rgb:red color) 
			   (rgb:green color)
			   (rgb:blue color)))
	((cmyk? color) (max (- 1 (rgb:red color))
			    (- 1 (rgb:green color))
			    (- 1 (rgb:blue color))))
	((gray? color) (gray:gray-val color))
	(else (error hsb:b
		     "the argument to hsb:b must be a color" color))))

;; Utility procedure that makes a rgb color out of hsb description.
(define (hsb->rgb h s b) 
  (if (= s 0) 
      (rgb b b b)
      (let* ((i (floor (* h 6)))
	     (f (- (* h 6) i))
	     (p (* b (- 1 s)))
	     (q (* b (- 1 (* s f))))
	     (t (* b (- 1 (* s (- 1 f))))))
	(case (if (inexact? i) (inexact->exact i) i)
	  ((0 6) (rgb b t p))
	  ((1) (rgb q b p))
	  ((2) (rgb p b t))
	  ((3) (rgb p q b))
	  ((4) (rgb t p b))
	  ((5) (rgb b p q))
	  (else (internal-bug hsb->rgb
			      "is an illegal hsb remainder value" i))))))


;; ===== CMYK Color Descriptors ======================================

(define (cmyk:c color)
  (cond ((cmyk? color) (cmyk:cyan color))
	((rgb? color) (- 1 (rgb:red color)))
	((hsb? color) (- 1 (rgb:red (hsb->rgb (hsb:hue color)
					      (hsb:sat color)
					      (hsb:bri color)))))
	((gray? color) (- 1 (gray:gray-val color)))
	(else (error cmyk:c 
		     "the argument to cmyk:c must be a color" color))))

(define (cmyk:m color)
  (cond ((cmyk? color) (cmyk:mag color))
	((rgb? color) (- 1 (rgb:green color)))
	((hsb? color) (- 1 (rgb:green (hsb->rgb (hsb:hue color)
						(hsb:sat color)
						(hsb:bri color)))))
	((gray? color) (- 1 (gray:gray-val color)))
	(else (error cmyk:m
		     "the argument to cmyk:m must be a color" color))))

(define (cmyk:y color)
  (cond ((cmyk? color) (cmyk:yel color))
	((rgb? color) (- 1 (rgb:blue color)))
	((hsb? color) (- 1 (rgb:blue (hsb->rgb (hsb:hue color)
					       (hsb:sat color)
					       (hsb:bri color)))))
	((gray? color) (- 1 (gray:gray-val color)))
	(else (error cmyk:y 
		     "the argument to cmyk:y must be a color" color))))

(define (cmyk:k color)
  (cond ((cmyk? color) (cmyk:black color))
	((rgb? color) 0)
	((hsb? color) 0)
	((gray? color) 0)
	(else (error cmyk:k 
		     "the argument to cmyk:k must be a color" color))))

;; GRAY Color Descriptor
(define (gray:val color)
  (cond ((gray? color) (gray:gray-val color))
	((rgb? color))
	((hsb? color))
	((cmyk? color))
	(else (error gray:val 
		     "the argument to gray:val must be a color" color))))
  


;; ===== Color Comparators ===========================================

;; test if something is a color
(define (color? c)
  (or (gray? c)
      (rgb?  c)
      (hsb?  c)
      (cmyk? c)))

;; test if two colors are the same
(define (color= c1 c2)
  (if (and (color? c1) (color? c2))
      (cond ((rgb? c1) (let ((r rgb:r)
			     (g rgb:g)
			     (b rgb:b))
			 (and (= (r c1) (r c2))
			  (= (g c1) (g c2))
			  (= (b c1) (b c2)))))
	    ((hsb? c1) (let ((h hsb:h)
			     (s hsb:s)
			     (b hsb:b))
			 (and (= (h c1) (h c2))
			      (= (s c1) (s c2))
			      (= (b c1) (b c2)))))
	    ((cmyk? c1) (let ((c cmyk:c)
			      (m cmyk:m)
			      (y cmyk:y)
			      (k cmyk:k))
			  (and (= (c c1) (c c2))
			       (= (m c1) (m c2))
			       (= (y c1) (y c2))
			       (= (k c1) (k c2)))))
	    ((gray? c1) (and (= (gray:val c1) (gray:val c2)))))
      (error color=
	     "both arguments must be FPS colors" c1 c2)))
  
;; ===== End of fps.color.scm  =======================================

