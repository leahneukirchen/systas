;;; ps-misc.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;

;; ================================================================
;; Functional PostScript

;; This file contains the rest of the PS procedures
;; Procedures contained in this file are all PS text
;; backend procdures. There are no user level procedures
;; here.

;; ================================================================

;; PS2-text-channel Procedures
;;   PSpaint
;;   PSclip
;;   PSread-show-options
;;   PScheck-picture-resources
;;   PSheader
;;   PStrailer
;;   PSshowpage
;;   PSselectfont
;;   PSset<attribute>
;;   PSsavetm
;;   PSrestoretm
;;   PSconcat
;;   PSdocumentation
;;   PSprinting
;;   PSpagelabel
;;   PSstring, PSfontkey

;; =============================================================

;; Causes painting (stroke or fill) to happen.
(define (PSpaint method port)
  (if (not (eq? method 'show))
      (format port " ~a~%" 
	      (case method
		((stroke-show stroke-stroke stroke-fill)
		 "stroke")
		((fill-show fill-stroke fill-fill)
		 "fill")))))


;; Causes clipping to happen.
(define (PSclip port)
  (display " clip newpath" port))

;; Causes font selection to happen.		       
(define (PSselectfont font port)
  (format port " /~a ~d selectfont~%" 
	  (font:fontname font)
	  (PSnum (font:fontsize font))))

;; ==========================================================

;; Only page label options are of interest at show
(define (PSread-show-options channel options)
  (let* ((interface (channel:interface channel))
	 (resources (channel:resources channel))
	 (label-opt (filter label-option? options))
	 (label (cond ((null? label-opt) 
		       (+ 1 (ps2-text-resources:total-pages resources)))
		      ((null? (cdr label-opt)) 
		       (option:data (car label-opt)))
		      (else 
		       (error show "More than one page label option")))))
    (set-ps2-text-resources:total-pages  resources 
					 (+ 1 (ps2-text-resources:total-pages 
					       resources)))
    (PSpagelabel label
		 (ps2-text-resources:total-pages resources)
		 interface)))


;; Check the fonts used by the glyphpaths in this pictures. 
;; Update the fonts in the channel:resources entry and also
;; print out %%IncludeResource: labels.
(define (PScheck-picture-resources channel picture)
  (let ((interface (channel:interface channel))
	(resources (channel:resources channel))
	(font-list '()))
    (set-ps2-text-resources:last-picture resources picture)
    (let loop ((obj picture))
      (cond ((glyphpath?    obj) (let ((name (font:fontname 
					      (glyphpath:font obj))))
				   (if (not (member name font-list))
				      (set! font-list (cons name font-list)))))
	    ((close-path?   obj) (loop (close-path:path    obj)))
	    ((stroke-outline-path? obj) (loop (stroke-outline-path:path obj)))
	    ((instance?     obj) (loop (instance:obj       obj)))
	    ((path-pict?    obj) (loop (path-pict:path     obj)))
	    ((colormap?     obj) (loop (colormap:picture   obj)))
	    ((clipped?      obj) (loop (clipped:path       obj))
				 (loop (clipped:picture    obj)))
	    ((composite?    obj) (for-each loop (composite:objs obj)))))
    (for-each (lambda (name)
		(format interface "%%IncludeResource: font ~a~%" name)
		(if (not (member name 
				 (ps2-text-resources:font-list resources)))
		    (set-ps2-text-resources:font-list
		     resources
		     (cons name (ps2-text-resources:font-list resources)))))
	      font-list)))

    

;; Prints out PS header. Currently it only contains bindings
;; that gives PS procedures shorter names to save space.
(define (PSheader port)
  (for-each (lambda (ele) (format port "~a~%" ele))
	    '("/save { matrix currentmatrix } def"
	      "/set  { setmatrix } def" 
	      "/tra  { translate } def" 
	      "/sca  { scale } def"  
	      "/rot  { rotate } def" 
	      "/cc   { concat } def" 
	      "/m    { moveto } def" 
	      "/l    { lineto } def" 
	      "/s    { stroke } def")))

;; prints the trailer
(define (PStrailer channel)
  (let ((interface (channel:interface channel))
	(resources (channel:resources channel)))
    (format interface "%%Trailer~%")
    (format interface "%%Pages: ~d~%" 
	    (ps2-text-resources:total-pages resources))
    (format interface "%%DocumentNeededResources: font ")
    (for-each (lambda (fontname) (format interface "~a " fontname))
	      (ps2-text-resources:font-list resources))
    (format interface "~%")
    (if (equal? (ps2-text-resources:format resources) "EPS")
	(if (= 1 (ps2-text-resources:total-pages resources))
	    (let* ((pict (ps2-text-resources:last-picture resources))
		   (box  (bounding-box pict))
		   (ur   (bbox:max box)) (urx (pt:x ur)) (ury (pt:y ur))
		   (ll   (bbox:min box)) (llx (pt:x ll)) (lly (pt:y ll)))
	      (format interface "%%BoundingBox: ~d ~d ~d ~d~%" 
		      (PSnum llx) (PSnum lly) (PSnum urx) (PSnum ury)))
	    (error channel 
		   "EPS files cannot have more than one page")))
    (format interface "%%EOF~%")))

;; Causes showpage to happen.
(define (PSshowpage channel)
  (let ((interface (channel:interface channel)))
    (format interface " showpage~%")
    ;; reset channel state info
    (set-channel:style      channel (make-style))
    (set-channel:font       channel #f)
    (set-channel:color-func channel (lambda (c) c))
    (set-channel:method     channel #f)))



;===== Attribute updaters =====================================

;; The following is a list of operators that update the current
;; style. They put out the postscript code to update gstate in
;; the postscript space.

(define (PSsetcolor c port)
  (cond ((rgb? c) (format port " ~d ~d ~d setrgbcolor~%"
			  (PSnum (rgb:r c))
			  (PSnum (rgb:g c))
			  (PSnum (rgb:b c))))
	((hsb? c) (format port " ~d ~d ~d sethsbcolor~%"
			  (PSnum (hsb:h c))
			  (PSnum (hsb:s c))
			  (PSnum (hsb:b c))))
	((cmyk? c) (format port " ~d ~d ~d ~d setcmykcolor~%"
			   (PSnum (cmyk:c c))
			   (PSnum (cmyk:m c))
			   (PSnum (cmyk:y c))
			   (PSnum (cmyk:k c))))
	((gray? c) (format port " ~d setgray~%"
			   (PSnum (gray:val c))))
	(else (error PSsetcolor c
		     "Argument passed to PSsetcolor is not a color"))))

(define (PSsetlinewidth val port)
  (format port " ~d setlinewidth~%" (PSnum val)))

(define (PSsetlinejoin val port)
  (format port " ~d setlinejoin~%" 
	  (case val
	    ((miter)  0)
	    ((round)  1)
	    ((bevel)  2))))

(define (PSsetlinecap val port)
  (format port " ~d setlinecap~%"
	  (case val
	    ((butt)       0)
	    ((round)      1)
	    ((project-square) 2))))

(define (PSsetmiterlimit val port)
  (format port " ~d setmiterlimit~%" (PSnum val)))

(define (PSsetdash pattern offset port)
  (let ((len (vector-length pattern)))
    (format port " [")
    (let lp ((l 0))
      (if (< l len)
	  (begin (format port " ~d" (vector-ref pattern l))
		 (lp (+ l 1)))))
    (format port " ] ~d setdash~%" offset)))
  

;===== TM related procedrues ================================

;; causes TM to be saved.
(define (PSsavetm port)
  (display " save" port))

;; causes TM to be restored.
(define (PSrestoretm port)
  (display " set" port))

;; causes gstate to be saved.
(define (PSsavegstate port)
  (display " gsave" port))

;; causes gstate to be restored.
(define (PSrestoregstate port)
  (display " grestore" port))

;; the path or picture obj is passed to the concat procedure because
;; concat might choose to use the TM directly and not have different
;; case for each different operand .. will think about it.
(define (PSconcat obj port)
  (let ((operand (instance:operand obj)))
    (cond ((pt? operand)
	   (format port " ~a ~a tra~%" 
		   (PSnum (pt:x operand)) (PSnum (pt:y operand))))
	  ((list?  operand)
	   (format port " ~a ~a sca~%" 
		   (PSnum (car operand)) (PSnum (cadr operand))))
	  ((number? operand)
	   (format port " ~a rot~%" 
		   (PSnum (rad->deg operand))))
	  ((matrix? operand) 
	   (format port " ~a cc~%" 
		   (PSmatrix operand)))
	  (else (error PSconcat operand
		       "Argument is not a transformation operand")))))

;===== Channel Options =====================================

;; Handles all options that provide textual and legal
;; documentation. Creator, date, copyright, etc.
(define (PSdocumentation opt port)
  (let ((field (option:field opt))
	(data  (option:data opt)))
    (if (not (eq? field 'format))
	(format port "~a ~a~%"
		(case field
		  ((creator)       "%%Creator:")
		  ((creation-date) "%%CreationDate:")
		  ((title)         "%%Title:")
		  ((copyright)     "%%Copyright:")
		  ((for)           "%%For:")
		  ((routing)       "%%Routing:"))
		data))))
  
(define (PSactivate-req opt port)
  (let ((field (option:field opt))
	(data  (option:data  opt)))
    (format port "<< ~a ~a >> setpagedevice~%"
	    (case field
	      ((duplex)        "/Duplex")
	      ((duplex-tumble) "/Duplex true /Tumble")
	      ((collate)       "/Collate")
	      ((num-copies)    "/NumCopies")
	      ((orientation)   "/Orientation"))
	    (cond ((number? data) (number->string (PSnum data)))
		  ((symbol? data) (case data
				    ((normal)       "0")
				    ((rotate-left)  "1")
				    ((rotate-right) "2")
				    ((upside-down)  "3")
				    (else (error ps2-text-channel
						 "Illegal orientation"))))
		  ((eq? #t data)  "true")
		  ((eq? #f data)  "false")
		  (else data)))))

(define (PSprint-req opt port)
  (let ((field (option:field opt))
	(data  (option:data  opt)))
    (format port "~a "
	    (case field
	      ((duplex)        "duplex")
	      ((duplex-tumble) "duplex(tumble)")
	      ((collate)       "collate")
	      ((num-copies)    (string-append "numcopies(" data ")"))))))

(define (PSpagelabel label pagenum port)
  (format port "%%Page: ~a ~d~%" label pagenum))


;==== PS2 channel procedure utils ========================================

;; print out postscirpt fontkey
(define (PSfontkey font) (string-append "/" (font:fontname font)))

;; prints out matrix in postscript syntax
(define (PSmatrix mat)
  (let ((a (PSnum (matrix:a mat))) (b (PSnum (matrix:b mat)))
        (p (PSnum (matrix:p mat))) (q (PSnum (matrix:q mat)))
        (x (PSnum (matrix:x mat))) (y (PSnum (matrix:y mat))))
    (format #f " [~d ~d ~d ~d ~d ~d]" a b p q x y)))


;===== End of misc.scm =====================================
