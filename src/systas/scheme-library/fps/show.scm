;;; show.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;


;; ===================================================================
;; Functional PostScript

;; show.scm

;; This file contains procedures that are used to parse fps graphics
;; data objects and output them to PS engine through the selected
;; channel.

;; (show channel picture [options]) 

;; ===================================================================


;; Show the picture object through the channel. PS2 text channel, for
;; example, outputs PostScript level 2 text code that describes the
;; picture.  options are FPS options. see fps.options.scm.

(define (show channel picture . options)
  (let ((interface (channel:interface channel)))
    ((channel:read-show-options channel) channel options)
    ((channel:check-picture-resources channel) channel picture)
    (draw-picture picture channel)
    ((channel:showpage channel) channel)))


;; draw-picture takes a picture object, dissembles it, and calls the
;; the appropriate procedures in the channel to draw the picture.
 
(define (draw-picture picture channel)
  (cond ((path-pict? picture)   (render-path-pict     picture channel))
	((clipped?   picture)   (render-clip          picture channel))
	((instance?  picture)   (render-instance-pict picture channel))
	((bitmap-pict? picture) (render-bitmap-pict   picture channel))
	((colormap?  picture)   (set-channel:color-func
				 channel
				 (comp (channel:color-func channel)
				       (colormap:func picture)))
				(draw-picture (colormap:picture picture) 
					      channel))
	((composite? picture)   (for-each 
				 (lambda (obj) (draw-picture obj channel))
				 (composite:objs picture)))
	((the-empty-pict? picture) channel)
	(else (error draw-picture
		     "This object is not a picture and cannot be drawn"
		     picture))))

;; construct-path takes a path, dissembles it, and calls the
;; appropriate procedures in the channel to construct the
;; path. construct-path and all the procedures it calls are required
;; to return a boolean that indicates whether the path it just
;; contructed was continuous.

;; A continuous path is a path that has no disjoint subpaths.  In
;; PostScript term, a continuous path is a single subpath.  We need to
;; keep track of whether paths are continuous so that FPS close-path
;; would know whether to call the PS closepath operator.

(define (construct-path path channel)
  (let ((interface (channel:interface channel)))
    ;; (display path) (newline) ; debug line
    (cond ((line?        path) (construct-line           path channel))
	  ((arc?         path) (construct-arc            path channel))
	  ((tangent-arc? path) (construct-tangent-arc    path channel))
	  ((curve?       path) (construct-curve          path channel))
	  ((rect?        path) (construct-rect           path channel))
	  ((glyphpath?   path) (construct-glyphpath      path channel))
	  ((bitmap-path? path) (construct-bitmap-path    path channel))
	  ((close-path?  path) (construct-close-path     path channel))
	  ((instance?    path) (construct-instance-path  path channel))
	  ((composite?   path) (construct-composite-path path channel))
	  ((stroke-outline-path? path) (construct-stroke-outline path channel))
	  ((the-empty-path? path) #t)
	  (else (error construct-path
		       "argument must be a path" path)))))

;; ===== Path Constructors ===========================================

;; LINE: continuous
(define (construct-line path channel)
  ((channel:line channel) (line:points path) 
			  (channel:interface channel))
  #t)

;; ARC: continuous
(define (construct-arc path channel)
  ((channel:arc channel) (arc:center path) 
			 (arc:radius path)
			 (arc:start-ang path)
			 (arc:end-ang path)
			 (channel:interface channel))
  #t)
    
;; TANGENT-ARC: continuous
(define (construct-tangent-arc path channel)
  ((channel:tangent-arc channel) (tangent-arc:pt1 path)
				 (tangent-arc:pt2 path)
				 (tangent-arc:pt3 path)
				 (tangent-arc:radius path)
				 (channel:interface channel))
  #t)


;; CURVE: continuous
(define (construct-curve path channel)
  ((channel:curve channel) (curve:start-pt path)
			   (curve:ctrl-pt1 path)
			   (curve:ctrl-pt2 path)
			   (curve:end-pt   path)
			   (channel:interface channel))
  #t)



;; RECT: discontinuous
(define (construct-rect path channel)
  ((channel:rect channel) (rect:pt     path)
			  (rect:width  path)
			  (rect:height path)
			  (channel:method    channel)
			  (channel:interface channel))
  #f)

;; STROKE-OUTLINE-PATH: discontinuous
(define (construct-stroke-outline path channel)
  (let ((interface (channel:interface channel))
	(current-style (channel:style channel))
	(current-color-func (channel:color-func channel))
	(style (stroke-outline-path:style path))
	(path  (stroke-outline-path:path  path)))
    (update-style current-style style current-color-func channel)
    (set-channel:current-pt channel (start-pt path))
    ((channel:moveto channel) (start-pt path) interface)
    (construct-path path channel)
    (set-channel:current-pt channel (end-pt path))
    ((channel:stroke-outline-path channel) interface)
    ;; restore original style
    (update-style style current-style current-color-func channel)
    #f))
    
;; GLYPHPATH: discontinuous
(define (construct-glyphpath path channel)
  (let ((interface (channel:interface channel))
	(method    (channel:method    channel))
	(glyphs    (glyphpath:glyphs  path))
	(font      (glyphpath:font    path)))

    ;; check font
    (if (not (equal? font (channel:font channel)))
	(begin (set-channel:font channel font)
	       ((channel:selectfont channel) font interface)))

    ;; process glyph vector
    (vector-for-each
     (lambda (ele)
       ((cond ((string? ele) render-simple-string)
	      ((glyph? ele)  render-by-glyphname)
	      (else (internal-bug construct-glyphpath
				  "illegal glyphpath element" ele)))
	ele method channel interface))
     glyphs)
    #f))
       

;; BITMAP-PATH: discontinuous
(define (construct-bitmap-path path channel)
  ((channel:imagemask channel) (bitmap-path:row path)
			       (bitmap-path:col path)
			       (bitmap-path:transparent-val path)
			       (bitmap-path:color-array path)
			       (channel:interface channel))
  #f)

;; CLOSEPATH: discontinuous
(define (construct-close-path path channel)
  (let ((continuity (construct-path (close-path:path path) channel)))
    ((channel:moveto channel) (end-pt path) (channel:interface channel))
    (set-channel:current-pt channel (end-pt path))
    (if continuity
	((channel:close-path channel) (channel:interface channel))))
  #f)


;; INSTANCE: continuous if element path is continuous
(define (construct-instance-path path channel)
  (let ((interface (channel:interface channel)))
    ((channel:savetm channel) interface)
    ((channel:concat channel) path interface)
    (let ((continuity (construct-path (instance:obj path) channel)))
      ((channel:restoretm channel) interface)
      continuity)))

		    
;; COMPOSITE: continuous if linked and all element paths are continuous
(define (construct-composite-path path channel)
  (let ((interface (channel:interface channel))
	(paths     (composite:objs path)))
    (let ((first-continuity (construct-path (car paths) channel)))
      (set-channel:current-pt channel (end-pt (car paths)))
      (and (forward-list-reduce 
	    (lambda (p continuity)
	      (and (let ((start (start-pt p)))
		     (if (composite:linked? path)
			 (if (not (pt= start (channel:current-pt channel)))
			     ((channel:line channel) (list start) interface))
			 ((channel:moveto channel) start interface))
		     (construct-path p channel)
		     (set-channel:current-pt channel (end-pt p)))
		   continuity))
	    #t
	    (cdr paths))
	   first-continuity
	   (composite:linked? path)))))
	   

;; ===== Picture Renderers ===========================================


(define (render-path-pict picture channel)
  (let ((path  (path-pict:path picture))
	(method (path-pict:paint-method picture))
	(style (path-pict:style picture))
	(current-style (channel:style channel))
	(current-color-func (channel:color-func channel))
	(interface (channel:interface channel)))
    (update-style current-style style current-color-func channel)
    (set-channel:style channel style)
    (set-channel:method channel method)
    (set-channel:current-pt channel (start-pt path))
    ((channel:moveto channel) (start-pt path) interface)
    (construct-path path channel)
    ((channel:paint channel) method interface)
    channel))

(define (render-clip picture channel)
  (let* ((current-font (channel:font channel))
	 (interface (channel:interface channel)))
    (set-channel:method channel 'clip)
    ((channel:savegstate channel) interface)
    (set-channel:current-pt channel (start-pt (clipped:path picture)))
    ((channel:moveto channel) (start-pt (clipped:path picture)) interface)
    (construct-path (clipped:path picture) channel)
    ((channel:clip channel) interface)
    (draw-picture (clipped:picture picture) channel)
    ((channel:restoregstate channel) interface)))

(define (render-instance-pict picture channel)
  (let ((interface (channel:interface channel)))
    ((channel:savetm channel) interface)
    ((channel:concat channel) picture interface)
    (draw-picture (instance:obj picture) channel)
    ((channel:restoretm channel) interface)
    channel))

(define (render-bitmap-pict picture channel)
  ((channel:image channel) (bitmap-pict:row picture)
			   (bitmap-pict:col picture)
			   (bitmap-pict:resolution picture)
			   (bitmap-pict:colorspace picture)
			   (bitmap-pict:color-array picture)
			   (channel:interface channel))
  channel)


;; ===== Helper Procedures ===========================================

(define (render-simple-string ele method channel interface)
  (case method
    ((show stroke-show fill-show)
     ((channel:show channel) ele interface))
    (else
     ((channel:charpath channel) ele method interface))))

(define (render-by-glyphname ele method channel interface)
  (case method
    ((show stroke-show fill-show)
     ((channel:glyphshow channel) ele interface))
    (else
     ((channel:glyphnamepath channel) ele method (channel:font channel)
				      interface))))

;; ===== End of show.scm =========================================
