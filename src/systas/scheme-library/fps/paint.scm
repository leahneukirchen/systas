;;; paint.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;


;; ===================================================================
;; Functional PostScript

;; paint.scm

;; This file contains procedures that make pictures from paths,
;; bitmap, and other pictures.

;; (stroke path  [style or attribs or glyphpath-painter])  -> path-pict picture
;; (fill   path  [style or attribs or glyphpath-painter])  -> path-pict picture
;; (clip   path  picture)                    -> clipped picture
;; (paint-glyphpath path [style or attribs]) -> path-pict picture
;; (colormap c-func pict)                    -> colormap picture
;; (bitmap->pict bitmap)                     -> bitmap pictuure

;; ===================================================================

;; STROKE: make a picture by painting along the path. opt-args can 
;; contain a style, some attributes and/or glyphpath-painter. The
;; style/attribs specify how the path is to be painted, and the 
;; glyphpath-painter specifies how the glyphpath is to be painted.
;; If no optional args, the path is painted in default-style and
;; glyphpath is painted in its native method.

(define (stroke path . opt-args)
  (if (the-empty-path? path)
      the-empty-pict
      (let ((painter
	     (let ((result (filter symbol? opt-args)))
	       (cond ((null? result) 'stroke-show)
		     ((null? (cdr result)) 
		      (case (car result)
			((native) 'stroke-show)
			((stroke) 'stroke-stroke)
			((fill)   'stroke-fill)
			(else (error stroke
				     "illegal glyphphpath-painter" 
				     result))))
		     (else (error stroke
				  "more than one glyphpath-painter" 
				  result)))))
	    (style
	     (let ((result (filter (lambda (x) (not (symbol? x))) opt-args)))
	       (cond ((null? result) (default-style))
		     ((null? (cdr result)) (let ((arg (car result)))
					     (if (style? arg) 
						 arg
						 (vary-default arg))))
		     (else (if (null? (filter (lambda (x) (not (attrib? x))) 
					      result))
			       (apply vary-default result)
			       (error stroke
				      "optional arg must be a style or some attribs"
				      result)))))))
	(make-path-pict path style painter))))


;; FILL: make a picture by painting inside the path. opt-args can
;; contain a style, some attributes and/or glyphpath-painter. The
;; style/attribs specify how the path is to be painted, and the
;; glyphpath-painter specifies how the glyphpath is to be painted.  If
;; no optional args, the path is painted in default-style and
;; glyphpath is painted in its native method.

(define (fill path . opt-args)
  (if (the-empty-path? path)
      the-empty-pict
      (let ((painter
	     (let ((result (filter symbol? opt-args)))
	       (cond ((null? result) 'fill-show)
		     ((null? (cdr result)) 
		      (case (car result)
			((native) 'fill-show)
			((stroke) 'fill-stroke)
			((fill)   'fill-fill)
			(else (error fill
				     "illegal glyphphpath-painter" result))))
		     (else (error fill
				  "more than one glyphpath-painter" result)))))
	    (style
	     (let ((result (filter (lambda (x) (not (symbol? x))) opt-args)))
	       (cond ((null? result) (default-style))
		     ((null? (cdr result)) (let ((arg (car result)))
					     (if (style? arg) 
						 arg
						 (vary-default arg))))
		     (else 
		      (if (null? (filter (lambda (x) (not (attrib? x))) 
					 result))
			  (apply vary-default result)
			  (error stroke
				 "optional arg must be a style or some attribs"
				 result)))))))
	(make-path-pict path style painter))))


;; CLIP: make a picture by cropping the given picture with the given
;; path.

(define (clip path picture)
  (if (or (the-empty-path? path)
	  (the-empty-pict? picture))
      (if (picture? picture)
	  picture
	  (error clip
		 "Second arg to procedure must be a picture"
		 picture))
      (if (and (path? path) (picture? picture))
	  (make-clipped path picture)
	  (error clip
		 "Args to clip must be (clip path picture)"
		 path picture))))

;; PAINT-GLYPHPATH:  make a picture from glyphpath by painting the
;; glyphpath in its native method (stroke or fill) as defined by the
;; font with which the glyphpath was created. The method is
;; automatically selected by the PostScript show operator.

(define (paint-glyphpath path . style-or-attrib)
  (if (path? path)
      (if (the-empty-path? path)
	  the-empty-pict
	  (let ((style (make-style-with-optional-args style-or-attrib)))
	    (make-path-pict path style 'show)))
      (error paint-glyphpath
	     "the first arg must be a path"
	     path)))
  
  
;; COLORMAP:  make a picture by changing the colors of another
;; picture. colormap takes a colromap-func and a picture. The
;; colormap-func must be function that takes an FPS color and returns 
;; an FPS color

(define colormap make-colormap)


;; BITMAP->BITMAP: make a picture from a bitmap.

(define (bitmap->pict bmap) ((bitmap:build-pict bmap) bmap))


;; ===== End of paint.scm ==============================================
