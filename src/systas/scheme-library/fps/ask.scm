;;; ask.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;


;; ===================================================================
;; Functional PostScript                             

;; fps.ask.scm

;; This file contains FPS tools for finding out information about
;; path and pictures: starting and ending point, glyphpath width,
;; and bounding boxes.

;; (bounding-box obj) --> bbox
;; (start-pt obj)     --> pt
;; (end-pt   obj)     --> pt


;; ===== Bounding Box ================================================

;; Returns the bounding box of a path or picture in the bbox record
;; in the fields 'ur' and 'll' for the up-right and the lower-left
;; corner. The bounding box record also contains a generic four
;; corner bounding box that is not necessarily rectangular (a rotated
;; rectangle, for example) but is tighter. These coordinates are not
;; exposed to the user; only the rectangular ur and ll corners are.

(define (bounding-box obj)
  (let loop ((obj obj) (TM identity-matrix))
    (if (path? obj)

	(cond ((line?        obj) (line-bbox        obj TM))
	      ((arc?         obj) (arc-bbox         obj TM))
	      ((tangent-arc? obj) (tangent-arc-bbox obj TM))
	      ((curve?       obj) (curve-bbox       obj TM))
	      ((glyphpath?   obj) (glyphpath-bbox   obj TM))
	      ((bitmap-path? obj) (bitmap-path-bbox obj TM))
	      ((composite?   obj) (composite-bbox   obj TM))
	      ((rect?        obj) (rect-bbox        obj TM))
	      ((close-path?  obj) (loop (close-path:path obj) TM))
	      ((stroke-outline-path? obj) (loop (stroke-outline-path:path obj)
						TM))
	      ((instance?    obj) (or (let ((box (instance:bbox obj)))
					(and box (transform-bbox TM box)))
				      (let ((box (loop (instance:obj obj)
						       (instance:TM obj))))
					(set-instance:bbox obj box) box)))
	      (else (error bounding-box "Not a path, no bounding box" obj)))
	
	(cond ((path-pict?   obj) (loop (path-pict:path obj)   TM))
	      ((colormap?    obj) (loop (colormap:picture obj) TM))
	      ((composite?   obj) (composite-bbox   obj TM))
	      ((clipped?     obj) (clipped-bbox     obj TM))
	      ((bitmap-pict? obj) (bitmap-pict-bbox obj TM))
	      ((instance?    obj) (or (instance:bbox obj)
				      (let ((box (loop (instance:obj obj)
						       (matrix* (instance:TM obj)
								TM))))
					(set-instance:bbox obj box) box)))
	      (else (error bounding-box "Not a picture, no bounding box" obj))))))


;; transform the corner points in the bbox with the TM and pick
;; out the max min points for the two rectangular points
(define (transform-bbox TM box)
  (let* ((ur (transform-pt TM (bbox:box-ur box)))
	 (ul (transform-pt TM (bbox:box-ul box)))
	 (lr (transform-pt TM (bbox:box-lr box)))
	 (ll (transform-pt TM (bbox:box-ll box)))
	 (urx (pt:x ur)) (ulx (pt:x ul)) (lrx (pt:x lr)) (llx (pt:x ll))
	 (ury (pt:y ur)) (uly (pt:y ul)) (lry (pt:y lr)) (lly (pt:y ll))
	 (max-x (max urx ulx lrx llx)) (min-x (min urx ulx lrx llx))
	 (max-y (max ury uly lry lly)) (min-y (min ury uly lry lly)))
    (bbox ur ul lr ll (pt max-x max-y) (pt min-x min-y))))


;; The bbox of a path-pict is the bbox of its path, but the corners
;; are widened by the the path-pict's linewidth, to take line thickness
;; into account
(define  (path-pict-bbox pict TM)
  (transform-bbox TM
   (let* ((width (style:line-width (path-pict:style pict)))  
	  (adj (/ width 2))
	  (path-bbox (bounding-box (path-pict:path pict)))
	  (ur (bbox:max path-bbox)) (ll (bbox:min path-bbox))
	  (min-x (pt:x ll)) (min-y (pt:y ll))
	  (max-x (pt:x ur)) (max-y (pt:y ur))
	  (pict-bbox (min-max-coords->bbox (- min-x adj) (- min-y adj)
					   (+ max-x adj) (+ max-y adj))))
     (set-path-pict:bbox pict pict-bbox)
     pict-bbox)))
	  

;; The bbox of a clipped picture is the intersection bbox of the 
;; clipping path and the picture
(define (clipped-bbox pict TM)
  (transform-bbox TM
   (let* ((pict-bbox (bounding-box (clipped:picture pict)))
	  (path-bbox (bounding-box (clipped:path pict)))
	  (pict-ur (bbox:max pict-bbox)) (pict-ll (bbox:min pict-bbox))
	  (path-ur (bbox:max path-bbox)) (path-ll (bbox:min path-bbox))
	  (pict-urx (pt:x pict-ur)) (pict-ury (pt:y pict-ur))
	  (pict-llx (pt:x pict-ll)) (pict-lly (pt:y pict-ll))
	  (path-urx (pt:x path-ur)) (path-ury (pt:y path-ur))
	  (path-llx (pt:x path-ll)) (path-lly (pt:y path-ll)))
     (let* ((min-x (max pict-llx path-llx))
	    (min-y (max pict-lly path-lly))
	    (max-x (min pict-urx path-urx))
	    (max-y (min pict-ury path-ury))
	    (box (if (and (> max-y min-y) (> max-x min-x))
		     (min-max-coords->bbox min-x min-y max-x max-y)
		     (make-bbox origin origin origin origin origin origin))))
       (set-clipped:bbox pict box)
       box))))


;; The bbox of a composite object is the union bounding boxes
(define (composite-bbox obj TM)
  (let ((answer
	 (transform-bbox TM
			 (or (composite:bbox obj)
			     (let* ((objs (composite:objs obj))
				    ;; find bbox of first obj for init min max values
				    (box (bounding-box (car objs)))
				    (urx (pt:x (bbox:max box))) (ury (pt:y (bbox:max box)))
				    (llx (pt:x (bbox:min box))) (lly (pt:y (bbox:min box))))

			       ;; loop over the objects and find the union of the bboxes
			       (let loop ((objs (cdr objs))
					  (min-x llx) (min-y lly) (max-x urx) (max-y ury))
				 (if (null? objs)
				     (let ((box (min-max-coords->bbox min-x min-y max-x max-y)))
				       (set-composite:bbox obj box)
				       box)
				     (let* ((box (bounding-box (car objs)))
					    (urx (pt:x (bbox:max box))) (ury (pt:y (bbox:max box)))
					    (llx (pt:x (bbox:min box))) (lly (pt:y (bbox:min box))))
				       (loop (cdr objs)
					     (min llx min-x) (min lly min-y)
					     (max urx max-x) (max ury max-y))))))))))
    answer))

(define (line-bbox path TM)
  (transform-bbox TM
   (or (line:bbox path)
       (let* ((points (line:points path)) (point (car points))
	      (1st-x (pt:x point))        (1st-y (pt:y point)))
	 (let loop ((points points) 
		    (min-x 1st-x) (min-y 1st-y) (max-x 0) (max-y 0))
	   (if (null? points)
	       (let ((box (min-max-coords->bbox min-x min-y max-x max-y)))
		 (set-line:bbox path box)
		 box)
	       (let* ((point (car points)) 
		      (x (pt:x point)) (y (pt:y point)))
		 (loop (cdr points)
		       (min min-x x) (min min-y y)
		       (max max-x x) (max max-y y)))))))))

(define (arc-bbox path TM)
  (transform-bbox TM
   (or (arc:bbox path)
       (let* ((center (arc:center path))    (radius (arc:radius path))
	      (start  (arc:start-ang path)) (end    (arc:end-ang path))
	      (c-min-x (- (pt:x center) radius))
	      (c-min-y (- (pt:y center) radius))
	      (c-max-x (+ (pt:x center) radius))
	      (c-max-y (+ (pt:y center) radius)))
	 (if (<= 2pi (abs (- end start)))
	     (let ((box (min-max-coords->bbox 
			 c-min-x c-min-y c-max-x c-max-y)))
	       (set-arc:bbox path box)
	       box)
	     (receive (min-x min-y max-x max-y)
	         (find-arc-bbox path)
	       (let ((box (min-max-coords->bbox min-x min-y max-x max-y)))
		 (set-arc:bbox path box)
		 box)))))))

;; find the bbox of an arc by examining which quadrants the start and 
;; end points lie in.
(define (find-arc-bbox path)
  (let* ((center (arc:center path)) (radius (arc:radius path))
	 (c-min-x (- (pt:x center) radius))(c-min-y (- (pt:y center) radius))
	 (c-max-x (+ (pt:x center) radius))(c-max-y (+ (pt:y center) radius))
	 (start  (adjust-to-0-2pi (arc:start-ang path)))
	 (end    (adjust-to-0-2pi (arc:end-ang   path)))
	 (start-x (+ (pt:x center) (* (cos start) radius)))
	 (start-y (+ (pt:y center) (* (sin start) radius)))
	 (end-x   (+ (pt:x center) (* (cos end)   radius))) 
	 (end-y   (+ (pt:y center) (* (sin end) radius))))
    (cond ((< start 1/2pi) 
	   (cond ((< end 1/2pi) ; I -> I
		  (if (> (- end start) pi)
		      (values c-min-x c-min-y c-max-x c-max-y)
		      (values end-x start-y start-x end-y)))
		 ((< end pi)    ; I -> II
		  (values end-x (min start-y end-y) start-x c-max-y))
		 ((< end 3/2pi) ; I -> III
		  (values c-min-x end-y start-x c-max-y))
		 (else          ; I -> IV
		  (values c-min-x c-min-y (max start-x end-x) c-max-y))))
	  ((< start pi) 
	   (cond ((< end 1/2pi) ; II -> I
		  (values c-min-x c-min-y  c-max-x (max start-y end-y)))
		 ((< end pi)    ; II -> II
		  (if (> (- end start) pi)
		      (values c-min-x c-min-y c-max-x c-max-y)
		      (values end-x end-y start-x start-y)))
		 ((< end 3/2pi) ; II -> III
		  (values c-min-x end-y (max start-x end-x) start-y))
		 (else          ; II -> IV
		  (values c-min-x c-min-y end-x start-y))))
	  ((< start 3/2pi)
	   (cond ((< end 1/2pi) ; III -> I
		  (values start-x c-min-y c-max-x end-y))
		 ((< end pi)    ; III -> II
		  (values (min start-x end-x) c-min-y c-max-x c-max-y))
		 ((< end 3/2pi) ; III -> III
		  (if (> (- end start) pi)
		      (values c-min-x c-min-y c-max-x c-max-y)
		      (values start-x end-y end-x start-y)))
		 (else          ; III -> IV
		  (values start-x c-min-y end-x (max start-y end-y)))))
	  (else 
	   (cond ((< end 1/2pi) ; IV -> I
		  (values (min start-x end-x) start-y c-max-x end-y))
		 ((< end pi)    ; IV -> II
		  (values end-x start-y c-max-x c-max-y))
		 ((< end 3/2pi) ; IV -> III
		  (values c-min-x (min start-y end-y) c-max-x c-max-y))
		 (else          ; IV -> IV
		  (if (> (- end start) pi)
		      (values c-min-x c-min-y c-max-x c-max-y)
		      (values start-x start-y end-x end-y))))))))

;; map the angle to [0,2pi]
(define (adjust-to-0-2pi ang)
  (- ang (* (floor (/ ang 2pi)) 2pi)))


(define (tangent-arc-bbox path TM)
  (transform-bbox TM
   (or (tangent-arc:bbox path)
       (let* ((pt1 (tangent-arc:pt1 path)) 
	      (pt2 (tangent-arc:pt2 path))
	      (pt3 (tangent-arc:pt3 path))
	      (pt1x (pt:x pt1)) (pt2x (pt:x pt2)) (pt3x (pt:x pt3))
	      (pt1y (pt:y pt1)) (pt2y (pt:y pt2)) (pt3y (pt:y pt3))
	      (max-x  (max pt1x pt2x pt3x)) (min-x  (min pt1x pt2x pt3x))
	      (max-y  (max pt1y pt2y pt3y)) (min-y  (min pt1y pt2y pt3y))
	      (box    (min-max-coords->bbox min-x min-y max-x max-y)))
	 (set-tangent-arc:bbox path box) box))))

(define (curve-bbox path TM)
  (transform-bbox TM
   (or (curve:bbox path)
       (let* ((start  (curve:start-pt path)) (ctrl1  (curve:ctrl-pt1 path))
	      (ctrl2  (curve:ctrl-pt2 path)) (end    (curve:end-pt   path))
	      (startx (pt:x start)) (ctrl1x (pt:x ctrl1))
	      (ctrl2x (pt:x ctrl2)) (endx   (pt:x end))
	      (starty (pt:y start)) (ctrl1y (pt:y ctrl1))
	      (ctrl2y (pt:y ctrl2)) (endy   (pt:y end))
	      (max-x  (max startx ctrl1x ctrl2x endx))
	      (min-x  (min startx ctrl1x ctrl2x endx))
	      (max-y  (max starty ctrl1y ctrl2y endy))
	      (min-y  (min starty ctrl1y ctrl2y endy))
	      (box    (min-max-coords->bbox min-x min-y max-x max-y)))
	 (set-curve:bbox path box)
	 box))))

(define (glyphpath-bbox path TM)
  (transform-bbox TM
   (or (glyphpath:bbox path)
       (let* ((font     (glyphpath:font path))
	      (size     (/ (font:fontsize font) 1000))
	      (fontbbox (afm:fontbbox (get-afm (font:fontname font)))))
	 (min-max-coords->bbox (* size (pt:x (bbox:min fontbbox)))
			       (* size (pt:y (bbox:min fontbbox)))
			       (pt:x (end-pt path))
			       (* size (pt:y (bbox:max fontbbox))))))))

(define (bitmap-pict-bbox pict TM)
  (transform-bbox TM 
   (min-max-coords->bbox 0 0 (bitmap-pict:row) (bitmap-pict:col))))

(define (bitmap-path-bbox path TM)
  (transform-bbox TM 
   (min-max-coords->bbox 0 0 (bitmap-path:row) (bitmap-path:col))))

(define (rect-bbox path TM)
  (let* ((p  (rect:pt path))
	 (px (pt:x p)) (py (pt:y p))
	 (w  (rect:width path)) 
	 (h  (rect:height path)))
    (transform-bbox TM 
      (min-max-coords->bbox (min px (+ px w))
			    (min py (+ py h))
			    (max px (+ px w))
			    (max py (+ py h))))))


;; ===== Starting Point ==============================================

;; Return the start-pt of a picture or a path
(define (start-pt obj)
  (let loop ((obj obj) (TM identity-matrix))
    (if (path? obj)
	(cond ((the-empty-path? obj) origin)
	      ((line?           obj) (line-start-pt        obj TM))
	      ((arc?            obj) (arc-start-pt         obj TM))
	      ((tangent-arc?    obj) (tangent-arc-start-pt obj TM))
	      ((curve?          obj) (curve-start-pt       obj TM))
	      ((glyphpath?      obj) (glyphpath-start-pt   obj TM))
	      ((rect?           obj) (rect-start-pt        obj TM))
	      ((bitmap-path?    obj) (transform-pt TM origin))
	      ((close-path?     obj) (loop (close-path:path    obj) TM))
	      ((stroke-outline-path? obj) (loop (stroke-outline-path:path obj)
						TM))
	      ((composite?      obj) (loop (first-in-composite obj) TM))
	      ((instance?       obj) (or (instance:start-pt obj)
					 (let ((start (loop (instance:obj obj) 
							    (matrix* 
							     (instance:TM obj) TM))))
					   (set-instance:start-pt obj start) start)))
	      (else (error start-pt "Non-path obj, no start pt" obj)))
	
	(cond ((the-empty-pict? obj) origin)
	      ((path-pict?      obj) (loop (path-pict:path     obj) TM))
	      ((clipped?        obj) (loop (clipped:path       obj) TM))
	      ((colormap?       obj) (loop (colormap:picture   obj) TM))
	      ((composite?      obj) (loop (first-in-composite obj) TM))
	      ((instance?       obj) (or (instance:start-pt obj)
					 (let ((start (loop (instance:obj obj)
							    (matrix* 
							     (instance:TM obj) TM))))
					   (set-instance:start-pt obj start) start)))
	      ((bitmap-pict? obj) (transform-pt TM origin))
	      (else (error start-pt "Non-picture obj, no starting pt" obj))))))

(define (line-start-pt path TM)
  (transform-pt TM (car (line:points path))))

(define (arc-start-pt path TM)
  (let ((center    (arc:center path))
	(radius    (arc:radius path))
	(start-ang (arc:start-ang path)))
    (transform-pt TM (pt (+ (pt:x center) (* radius (cos start-ang)))
			 (+ (pt:y center) (* radius (sin start-ang)))))))
  
(define (tangent-arc-start-pt path TM)
  (transform-pt TM (tangent-arc:pt1 path)))

(define (curve-start-pt path TM)
  (transform-pt TM (curve:start-pt path)))

(define (glyphpath-start-pt path TM)
  (transform-pt TM origin))

(define (rect-start-pt path TM)
  (transform-pt TM (rect:pt path)))



;; ===== Ending Point ================================================

;; Return the end-pt of a picture or a path
(define (end-pt obj)
  (let loop ((obj obj) (TM identity-matrix))
    (if (path? obj)
	(cond ((the-empty-path? obj) origin)
	      ((line?           obj) (line-end-pt        obj TM))
	      ((arc?            obj) (arc-end-pt         obj TM))
	      ((tangent-arc?    obj) (tangent-arc-end-pt obj TM))
	      ((curve?          obj) (curve-end-pt       obj TM))
	      ((glyphpath?      obj) (glyphpath-end-pt   obj TM))
	      ((rect?           obj) (rect-end-pt        obj TM))
	      ((bitmap-path?    obj) (transform-pt TM 
				      (pt (bitmap-path:row) 
					  (bitmap-path:col))))
	      ((close-path?     obj) (transform-pt TM 
				      (start-pt (close-path:path obj))))
	      ((stroke-outline-path? obj) (loop (stroke-outline-path:path obj)
						TM))
	      ((composite?      obj) (loop (last-in-composite obj)  TM))
	      ((instance?       obj) (or (instance:end-pt obj)
					 (let ((end (loop (instance:obj obj)
							  (matrix* 
							   (instance:TM obj) TM))))
					   (set-instance:end-pt obj end) end)))
	      (else (error end-pt "Non-path object, no ending point" obj)))
	
	(cond ((the-empty-pict? obj) origin)
	      ((path-pict?      obj) (loop (path-pict:path obj)    TM))
	      ((clipped?        obj) (loop (clipped:path obj)      TM))
	      ((colormap?       obj) (loop (colormap:picture obj)  TM))
	      ((composite?      obj) (loop (last-in-composite obj) TM))
	      ((instance?       obj) (or (instance:end-pt obj)
					 (let ((end (loop (instance:obj obj)
							  (matrix* 
							   (instance:TM obj) TM))))
					   (set-instance:end-pt obj end) end)))
	      ((bitmap-pict?    obj) (transform-pt TM
                                   (pt (bitmap-pict:row) (bitmap-pict:col))))
	      (else (error end-pt "Non-picture obj, no ending point" obj))))))

(define (line-end-pt path TM)
  (transform-pt TM (car (reverse (line:points path)))))

(define (arc-end-pt path TM)
  (let ((center  (arc:center path))
	(radius  (arc:radius path))
	(end-ang (arc:end-ang path)))
    (transform-pt TM (pt (+ (pt:x center) (* radius (cos end-ang)))
			 (+ (pt:y center) (* radius (sin end-ang)))))))

(define (tangent-arc-end-pt path TM)
  (let* ((pt1 (tangent-arc:pt1 path)) 
	 (pt2 (tangent-arc:pt2 path)) 
	 (pt3 (tangent-arc:pt3 path))
	 (v1 (add-pts pt1 (negate-pt pt2))) 
	 (v2 (add-pts pt3 (negate-pt pt2)))
	 (radius  (tangent-arc:radius path))
	 (ang     (/ (acos (/ (dot-product v1 v2) (* (mag v1) (mag v2)))) 2))
	 (ptt-pt2 (/ radius (tan ang))) ;; length from target pt to pt 2
         (ratio   (/ ptt-pt2 (mag v2))))
    (transform-pt TM (add-pts (pt (* ratio (pt:x v2)) (* ratio (pt:y v2))) 
			      pt2))))

(define (curve-end-pt path TM)
  (transform-pt TM (curve:end-pt path)))

(define (glyphpath-end-pt path TM)
  (transform-pt TM 
   (or (glyphpath:end-pt path)
       (let ((end-pt (pt (glyphpath-width path) 0)))
	 (set-glyphpath:end-pt path end-pt)
	 end-pt))))

(define (rect-end-pt path TM)
  (transform-pt TM (rect:pt path)))

;; Finds the total width of the glyphs in the glyphpath. The width
;; of a glyphpath is not the same as the x coordinate of its end point.
;; The width of a glyphpath is the total widths of all the glyphs in
;; the glyphpath. 

(define (glyphpath-width glyphpath)
  (let* ((font          (glyphpath:font glyphpath))
	 (font-char-map (font:font-char-map font))
	 (size          (/ (font:fontsize font) 1000))
	 (glyph-table   (font:glyph-table font))
	 (add-width            ;; add the widths of glyphs in a string 
	  (lambda (width char) ;; i.e. native mapping characters
	    (+ width 
	       (* size (glyph:width (font-char-map-ref 
				     font-char-map (char->integer char)))))))
	 (add-glyph-widths     ;; add the widths of glyphs in glyphpath
	  (lambda (width ele)
	    (+ width 
	       (cond ((glyph?  ele) (* size (glyph:width ele)))
		     ((string? ele) (string-reduce add-width 0 ele)))))))
    
	(vector-reduce add-glyph-widths 0 (glyphpath:glyphs glyphpath))))
    


;; ===== End of fps.ask.scm ==========================================


