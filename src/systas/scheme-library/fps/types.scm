;;; types.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord (same terms)
;;;



;; =================================================================
;; Functional PostScript


;; fps.type.scm

;; This file contains record definitions and record maker aliases.

;; Notes about angles:

;; - All angles are measured in radians.
;; - All angles are measured from the positive x-axis

;; Record types summary:

;; pt       point in Cartesian coord
;; matrix   3x3 transformation matrix
;; bbox     bounding box that encloses a picture or a pathh
;; font     a collection of glyphs
;; char-map character map that maps characters to glyphname strings
;; int-map  integer map that maps integers to glyphname strings
;; option   extra information provided for rendering or about the picture

;; the-empty-path         a nil path
;; line     a series of pts connected by straight lines
;; rect     a rectangle of a corner pt and width height
;; curve    a bezier curve of four points 
;; arc      arc of a circle drawn counterclockwise
;; tangent-arc    an arc formed along the two tangent lines
;; glyphpath      the path of a series of glyphs 
;; close-path     closes the path by appending an extra segment
;; bitmap-path    a path built from a bitmap
;; stroke-outline-path    outline of a stroked path

;; the-empty-pict         a nil picture
;; path-pict   picture produced by stroking or filling a path
;; clipped     picture clipped with the given with the path
;; colormap    a new picture by applying the color function to the orig
;; bitmap-pict a picture built from a bitmap

;; bitmap      a matrix of colors

;; instance    a transformed path or picture
;; composite   a collectin of paths or a collection of pictures

;; style       a collection of attributes
;; attrib      a piece of information about how the picture is drawn.
;;             ex. color, linewidth.

;; channel     rendering backend.

;; =================================================================

(define-record pt
  x y
  ((disclose self) (list "pt" (pt:x self) (pt:y self))))

(define-record matrix
  a b  p q  x y 
  ;; [a p x] and [b q y] are the first two columns of a
  ;; 3x3 transformation matrix. the last column is always constant.
  ((disclose self) (list "matrix" 
			 (matrix:a self) (matrix:b self)
			 (matrix:p self) (matrix:q self)
			 (matrix:x self) (matrix:y self))))

(define-record bbox
  box-ur box-ul box-lr box-ll
  ;; these are the four 'tight' corners of a bounding box.
  ;; the box specified by these four corners may not be rectangular
  ;; (the bounding box of an rotated object, for example)
  max min   
  ;; these are the max/min points of the rectangular bounding box
  ;; as PostScript is only interested in the rectangular bounding box
  ((disclose self) (list "bbox" (bbox:min self) (bbox:max self))))

(define-record font 
  glyph-table
  fontname fontsize 
  char-map int-map
  font-char-map font-int-map
  (TM identity-matrix)
  ((disclose self) (list "font" (font:fontname self) (font:fontsize self))))

(define-record char-map 
  map
  ((disclose self) (list "char-map" (char-map:map self))))

(define-record int-map 
  map
  ((disclose self) (list "int-map" (int-map:map self))))

(define-record option
  field data)


;===== Paths ===================================================

(define-record the-empty-path)

(define-record line
  points (bbox #f)
  ((disclose self) (list "line" (line:points self))))
  ;; there must be at least one point

(define-record rect
  pt width height
  ((disclose self) 
   (list "rect" (rect:pt self) (rect:width self) (rect:height self))))
   ;; width and height can be positive or negative numbers.

(define-record arc
  center radius start-ang end-ang (bbox #f)
  ((disclose self)
   (list "arc" (arc:center self) (arc:radius self) 
	       (arc:start-ang self) (arc:end-ang self))))

(define-record tangent-arc
  pt1 pt2 pt3 radius (bbox #f)
  ((disclose self)
   (list "tangent-arc" (tangent-arc:pt1 self) (tangent-arc:pt2 self)
	               (tangent-arc:pt3 self) (tangent-arc:radius self))))

(define-record curve
  start-pt ctrl-pt1 ctrl-pt2 end-pt (bbox #f)
  ((disclose self)
   (list "curve" (curve:start-pt self) (curve:ctrl-pt1 self)
	         (curve:ctrl-pt2 self) (curve:end-pt self))))

(define-record glyphpath
  font glyphs (end-pt #f) (bbox #f)
  ((disclose self)
   (list "glyphpath" (glyphpath:font self) (list (glyphpath:glyphs self)))))
  ;; glyphs is a list containing two kinds of elements:
  ;; string -- characters in this string have char-to-glyph mappings
  ;;           that match the fonts native mapping. We can display
  ;;           these characters faster using the show operator.
  ;; glyph record -- these are glyphs that cannot be printed with an
  ;;           an ASCII char.  The copyright symbol, for example. 
  ;;           Back-end procedures decide how these get handled.

(define-record close-path
  path)

(define-record stroke-outline-path
  path style)


;===== Pictures =================================================

(define-record the-empty-pict)

(define-record path-pict
  path style paint-method (bbox #f))
  ;; paint-method can be 'stroke-show 'stroke-stroke 'stroke-fill
  ;; 'fill-show 'fill-stroke 'fill-fill. the second method after
  ;; the hyphen is for glyphpath. the first method is for all other
  ;; paths.

(define-record clipped
  path picture (bbox #f))

(define-record colormap
  func picture)


;===== Instance and Composite ===================================

(define-record instance
  TM obj operand (start-pt #f) (end-pt #f) (bbox #f))
  ;; obj can be a path or a picture

(define-record composite
  objs (linked? #f) (start-pt #f) (end-pt #f) (bbox #f))
  ;; objs a list that contains all paths or all pictures.
  ;; there must be at least two objects in the objs list


;===== Bitmap ===================================================

;; Bitmap is an array of colors built by a bitmap-maker. It can be 
;; turned into path or picture. The manipulation of any bitmap
;; object depends on the particular bitmap-maker that it is
;; built with. Each bitmap-maker can decide how the bitmap
;; data is stored and what the algorithm of the recolor operation is.
;;
;; However, once a bitmap is turned into path or picture, it
;; has a uniform structure since it is manipulated by FPS
;; procedures.

(define-record bitmap
  row
  col
  resolution   
  colorspace   ;; 'gray or 'rgb
  bitmap-data  ;; different bitmap-makers may choose to represent/store 
  recolor      ;; bitmap data differently
  build-path
  build-pict)


;; Bitmap path is a path built from a bitmap. It can used
;; as a masking path. It can be filled and stroked. Though
;; the stroking operation probably won't be implemented for
;; a while. NOTE: only a 1-bit gray scale bitmap can be turned
;; into bitmap path.

(define-record bitmap-path
  row
  col
  transparent-val  ;; the value for which the sample point is transparent
  color-array)
  

;; Bitmap-pict is a picture built from a bitmap. It is
;; just like any other picture and can be combined and
;; manipulated like or with them. Recoloring performed on
;; a picture bitmap will cause the color-res to be set
;; to max, since in the picture model, there is no such
;; thing as "color resolution".

(define-record bitmap-pict
  row 
  col
  resolution    ;; resolution info is kept for optimization. resolution
                ;; is changed to max (12) after colormap.
  colorspace
  color-array)
  

;===== Style and Attributes =====================================

(define-record style
  color            
  line-width
  line-cap
  line-join
  miter-limit
  dash-pattern
  dash-offset)

(define-record attrib
  setter val)
  ;; an attrib installs its value into a style record by calling
  ;; its setter procedure.


;===== channel ==================================================

;; The channels are the FPS output backends. The channel contains
;; PS operators that are required to actually render the FPS pictures.
;; Channel also contains other information such as current font, style,
;; page number, etc. So far we only have the PS2 text channel backend. 

(define-record channel

  interface      ; whatever that is needed to output to the backend
  resources      ; resources that are used/needed

  style          ; channel's current style
  font           ; channel's current font
  color-func     ; channel's current colormap function
  method         ; channel's current drawing method (stroke/fill/clip)
  current-pt     ; channel's current pt
  
  moveto         ; move to a point
  line           ; construct a line
  rect           ; construct a rectangle
  arc            ; construct an arc or circle
  tangent-arc    ; construct a tangent arc
  curve          ; construct a bezier curve
  charpath       ; construct a charpath
  glyphnamepath  ; consturct a charpath given a glyphname
  show           ; render glyphs in string
  glyphshow      ; render glyphs by glyphname
  close-path     ; close a path by appending line from end to start pt
  stroke-outline-path     
  
  savetm         ; push the current TM onto the stack
  restoretm      ; pop the TM off the stack
  savegstate     ; push the current gstate onto the stack
  restoregstate  ; pop the gstate off the stack
  concat         ; change the current TM
  
  setcolor       ; change the current color
  setlinewidth   ; change the current linewidth
  setlinejoin    ; change the current line join setting
  setlinecap     ; change the current line cap setting
  setmiterlimit  ; change the current miter limit setting
  setdash        ; change the current cash setting
  selectfont     ; change the current font selection
  
  paint          ; either stroke or fill
  clip           ; clip picture with path
  image          ; render the bitmap image
  imagemask      ; render the bitmap image mask
  
  showpage       ; show the page and reset grahpics state
  read-show-options ; deal with options presented with
                            ; show call
  check-picture-resources   ; check resources required by a 
                            ; picture (fonts) at show time
  close-channel)


;===== Predicates ====================================================

;; a path can be any one of the following
(define (path? obj)
  (or (the-empty-path?      obj) 
      (line?                obj)
      (arc?                 obj)
      (tangent-arc?         obj)
      (curve?               obj)
      (glyphpath?           obj)
      (rect?                obj)
      (close-path?          obj)
      (stroke-outline-path? obj)
      (bitmap-path?         obj)
      (and (instance?       obj) (path? (instance:obj obj)))
      (and (composite?      obj) (path? (car (composite:objs obj))))))


;; a picture can be any one of the following
(define (picture? obj)
  (or (the-empty-pict? obj) 
      (path-pict?      obj)
      (bitmap?         obj)
      (clipped?        obj)
      (colormap?       obj)
      (bitmap-pict?    obj)
      (and (instance?  obj) (picture? (instance:obj obj)))
      (and (composite? obj) (picture? (car (composite:objs obj))))))


;===== Record Maker Aliases (abstraction) =========================

(define pt make-pt)
(define (pt= p1 p2) (and (= (pt:x p1) (pt:x p2))
			 (= (pt:y p1) (pt:y p2))))
(define bbox make-bbox)
(define matrix make-matrix)
(define option make-option)

(define the-empty-path (make-the-empty-path))
(define the-empty-pict (make-the-empty-pict))
(define (the-empty-obj? obj) (or (the-empty-pict? obj)
				 (the-empty-path? obj)))
(define (not-the-empty-path? obj) (not (the-empty-path? obj)))
(define (not-the-empty-pict? obj) (not (the-empty-pict? obj)))
(define (not-the-empty-obj? obj) (not (or (the-empty-pict? obj)
					  (the-empty-path? obj))))


(define (line pt . points) (make-line (cons pt points)))
(define rect make-rect)
(define arc make-arc)
(define tangent-arc make-tangent-arc)
(define curve make-curve)
(define glyphpath make-glyphpath)
(define stroke-outline-path make-stroke-outline-path)

(define (close-path p) 
  (let ((start (start-pt p))
	(end   (end-pt   p)))
    (make-close-path  (link p 
			    (if (pt= start end)
				the-empty-path
				(line end start))))))

(define (make-linked-composite objs)
  (let ((c (make-composite objs)))
    (set-composite:linked? c #t)
    c))


(define bounding-box:max bbox:max)
(define bounding-box:min bbox:min)

(define bitmap make-bitmap)
(define bitmap-path make-bitmap-path)
(define bitmap-pict make-bitmap-pict)

(define font-char-map-ref vector-ref)
(define font-int-map-ref  vector-ref)
(define int-map-size vector-length)


;; To create a font, user must specify fontname and fontsize. 
;; User can also specify TM, char-map, int-map as optional-args.
;; These optional args need not to be in any order, although they
;; do need to come after fontname and fontsize. Note that these
;; optional args replace the default values for TM, char-map, and
;; int-map.
(define (font fontname fontsize . optional-args)
  (let* ((data  (get-afm fontname))
	 (table (afm:glyph-table data))
	 (new-font (make-font table fontname fontsize
			      (afm:native-char-map data)
			      (afm:native-int-map  data)
			      (afm:native-font-char-map data)
			      (afm:native-font-int-map data))))
    (for-each 
     (lambda (arg)
       (cond ((matrix? arg)   (set-font:TM new-font arg))
	     ((char-map? arg) (set-font:char-map new-font arg)
			      (set-font:font-char-map 
			       new-font 
			       (create-font-map table (char-map:map arg))))
	     ((int-map? arg)  (set-font:int-map new-font arg)
			      (set-font:font-int-map 
			       new-font 
			       (create-font-map table (int-map:map arg))))
	     (else (error font optional-args
			  "Illegal optional argument"))))
     optional-args)
    new-font))

;; create a font-charmap/intmap, which is different from an FPS char/intmap
;; because it is a vector containing pointers to the GLYPH records, instead
;; of glyphnames.
(define (create-font-map table vect)
  (vector-map (lambda (ele)
		(if ele	(table-ref table ele)))
	      vect))
	
;; create bitmap paths and pictures
(define (bitmap->path transparent-val bmap)
  ((bitmap:build-path bmap) transparent-val bmap))

(define (bitmap-recolor bmap c-func)
  ((bitmap:recolor bmap) bmap c-func))

;; This will enable us to make a style using the lower level structure.
(define make-style* make-style)

;; This make-style makes a system default style.
(define (make-style) 
  (make-style*  (rgb 0 0 0)		;color = black
		1			;line-width
		'butt			;line-cap
		'miter			;line-join
		10			;miter-limit
	        '#()			;dash-pattern
		0))			;dash-offset


;===== End of fps.type.scm ===========================================






