;;; ps-path.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;


;; ===============================================================
;; Functional PostScript

;; path.scm

;; This file contains procedures contained in this file are all PS
;; text backend procdures that are part of the PS2-text-channel. 

;; (ps2-text-channel port-or-filename . options)         -> channel
;; (show-w/ps2-text-channel* port-or-filename thunk)     
;; (close-channel channel)                  

;; ===============================================================

;; This resources record is part of the ps2-text-channel
(define-record ps2-text-resources
  (format #f)
  (last-picture #f)
  (total-pages 0)
  (font-list '()))
  
;; ps2-text-channel maker
(define (ps2-text-channel port-or-filename . options)
  (let* ((channel (make-channel      ; interface

		   (if (output-port? port-or-filename) 
		       port-or-filename
		       (open-output-file port-or-filename))
		  
		   (make-ps2-text-resources)
		   
		   (make-style)         ; current style
		   #f                   ; current font
		   (lambda (c) c)       ; current colormap function
		   #f                   ; current drawing method
		   #f                   ; current p
		   
		   PSmoveto
		   PSline
		   PSrect
		   PSarc
		   PStangent-arc
		   PScurve
		   PScharpath
		   PSglyphnamepath
		   PSshow
		   PSglyphshow
		   PSclose-path
		   PSstroke-outline-path
		   
		   PSsavetm
		   PSrestoretm
		   PSsavegstate
		   PSrestoregstate
		   PSconcat
		   
		   PSsetcolor
		   PSsetlinewidth
		   PSsetlinejoin
		   PSsetlinecap
		   PSsetmiterlimit
		   PSsetdash

		   PSselectfont
		   
		   PSpaint
		   PSclip
		   PSimage
		   PSimagemask
		   
		   PSshowpage
		   PSread-show-options
		   PScheck-picture-resources
		   
		   PSclosechannel))
	 
	 (interface (channel:interface channel)))
    (read-channel-options channel options)
    (PSheader interface)
    channel))

;; process options that come with the channel and make
;; output conform to DSC 
(define (read-channel-options channel options)
  (let ((interface  (channel:interface     channel))
	(resources  (channel:resources     channel))
	(doc-opts   (filter doc-option?    options))
	(print-opts (filter print-option?  options))
	(format-opt (filter format-option? options)))

    (cond ((null? format-opt)
	   (format interface "%!PS-Adobe-3.0~%")
	   (if (not (null? print-opts))
	       (begin (format interface "%%Requirements: ")
		      (for-each (lambda (opt) (PSprint-req opt interface)) 
				print-opts)
		      (format interface "~%"))))

	  ((null? (cdr format-opt))
	   (if (equal? (option:data (car format-opt)) "EPS")
	       (begin (format interface "%!PS-Adobe-3.0 EPSF-3.0~%")
		      (format interface "%%BoundingBox: (atend)~%")
		      (set-ps2-text-resources:format resources "EPS"))
	       (error ps2-text-channel 
		      "format not recognized" (cdr format-opt))))

	  (else (error ps2-text-channel 
		       "Cannot specify more than one format options")))
	       
    
    (for-each (lambda (opt) (PSdocumentation opt interface)) doc-opts)
    (format interface "%%LanguageLevel: 2~%")
    (format interface "%%Pages: (atend)~%")
    (format interface "%%DocumentNeededResources: (atend)~%")
    (format interface "%%EndComments~%")
    
    (if (not (equal? (ps2-text-resources:format resources) "EPS"))
	(for-each (lambda (opt) (PSactivate-req opt interface)) 
		  print-opts))))

;; closes the port of a ps2-text channel, thereby closing the channel 
;; (since channel is no longer able to output to anywhere.
(define (close-channel channel)
  ((channel:close-channel channel) channel))

(define (PSclosechannel channel)
  (PStrailer channel)
  (close-output-port (channel:interface channel)))


;; given a port of filename and a thunk that takes the argument channel,
;; this procedure creates a temporary channel from the port-or-filename
;; argument, and exec thunk. If a filename were given as the arg, the
;; temp port created for the temp channel gets wiped out at the end. 
;; If a port were given, everything is left alone.


(define (show-w/ps2-text-channel port-or-filename picture . options)
  (let ((channel (apply ps2-text-channel port-or-filename options)))
    (show channel picture)
    (close-channel channel)))

;===== Geometric Path Constructors ========================================

;; (PSmoveto newpt port)
;; (PSline pts port)
;; (PSrect ll-pt ur-pt method port)
;; (PSarc center radius start-ang-end-ang port)
;; (PSartc pt1 pt2 pt3 radius port)
;; (PScurve start-pt ctrl-pt2 ctrl-pt2 end-pt port)
;; (PScharpath lst port)
;; (PSglyphnamepath glyph font port)
;; (PSshow str port)
;; (PSglyphshow glyph port)
;; (PSclose-path str port)
;; (PSstroke-outline-path port)

;; move the currentpoint to the newpt
(define (PSmoveto newpt port)
  (format port " ~d ~d m~%" 
	  (PSnum (pt:x newpt)) 
	  (PSnum (pt:y newpt))))


;; append a line from currentpoint to pt
(define (PSline pts port)
  (for-each (lambda (pt) 
	      (format port " ~d ~d l~%" 
		      (PSnum (pt:x pt)) 
		      (PSnum (pt:y pt))))
	    pts))

;; draws a rectangle
(define (PSrect p w h method port)
  (let* ((px  (PSnum (pt:x p)))
	 (py  (PSnum (pt:y p)))
	 (llx (PSnum (min px (+ px w))))
	 (lly (PSnum (min py (+ py h))))
	 (urx (PSnum (max px (+ px w))))
	 (ury (PSnum (max py (+ py h)))))
    (if (eq? method 'clip)
	(format port "~d ~d m ~d ~d l ~d ~d l ~d ~d l closepath"
		llx lly urx lly urx ury llx ury)	
	(format port "~d ~d ~d ~d ~a"
		px py w h
		(case method
		  ((stroke-show stroke-stroke stroke-fill)
		   "rectstroke")
		  ((fill-show fill-stroke fill-fill)
		   "rectfill"))))))


;; draws a counterclockwise arc
(define (PSarc center radius start-ang end-ang port)
  (format port " ~d ~d ~d ~d ~d arc~%" 
	  (PSnum (pt:x center)) (PSnum (pt:y center)) 
	  (PSnum radius)
	  (PSnum (rad->deg start-ang))
	  (PSnum (rad->deg end-ang))))

;; draw a tangent arc
(define (PStangent-arc pt1 pt2 pt3 radius port)
  (format port " ~d ~d ~d ~d ~d arct~%" 
	  (PSnum (pt:x pt2)) (PSnum (pt:y pt2))
	  (PSnum (pt:x pt3)) (PSnum (pt:y pt3))
	  (PSnum radius)))

;; append Bezier curveto
(define (PScurve start-pt ctrl-pt1 ctrl-pt2 end-pt port)
  (format port " ~d ~d ~d ~d ~d ~d curveto~%" 
	  (PSnum (pt:x ctrl-pt1)) (PSnum (pt:y ctrl-pt1)) 
	  (PSnum (pt:x ctrl-pt2)) (PSnum (pt:y ctrl-pt2))
	  (PSnum (pt:x end-pt))   (PSnum (pt:y end-pt))))

(define (PScharpath str method port)
  (format port "(~a) ~a charpath~%" 
	  (PSadd-escape str)
	  (case method
	    ((stroke stroke-stroke fill-stroke) "false")
	    (else "true"))))

(define (PSglyphnamepath glyph method font port)
  (let ((fontname (font:fontname font))
	(fontsize (font:fontsize font))
	(glyphname (glyph:glyphname glyph)))
    (format port "/~a findfont~%" fontname)
    (format port "dup length dict begin~%")
    (format port "{1 index /FID ne {def} {pop pop} ifelse} forall~%")
    (format port "/Encoding 256 array def~%")
    (format port "0 1 255 {Encoding exch /.notdef put} for~%")
    (format port "Encoding 97 /~a put~%" glyphname)
    (format port "currentdict end /tmp-font exch definefont pop~%")
    (format port "/tmp-font ~d selectfont~%" fontsize)
    (format port "(a) ~a charpath~%" 
	    (case method 
	      ((stroke stroke-stroke fill-stroke) "false")
	      (else "true")))))

(define (PSshow str port)
  (format port "(~a) show~%" (PSadd-escape str)))

;; Specialized procedure used to make PS string. This procedure takes a 
;; string and adds \ where needed for PS escape chars.
(define (PSadd-escape str)
  (let* ((new-len (string-reduce (lambda (sum ch) 
				   (+ sum (if (memq ch '(#\\ #\( #\))) 
					      2 1)))
				 0 str))
	 (new-str  (make-string new-len)))
    (string-reduce (lambda (new-str-index ch) 
		     (string-set! new-str new-str-index ch)
		     (if (memq ch '(#\) #\( #\\))
			 (begin (string-set! new-str (- new-str-index 1) #\\)
				(- new-str-index 2))
			 (- new-str-index 1)))
		   (- new-len 1)
		   str)
    new-str))

(define (PSglyphshow glyph port)
  (format port " /~a glyphshow~%" (glyph:glyphname glyph)))

;; connect subpath back to its starting point
(define (PSclose-path port)
  (format port " closepath~%"))

;; compute outline of path. problem ..
(define (PSstroke-outline-path port)
  (format port " strokepath~%"))



;===== Image ===================================================

;; (PSimagemask row col trasparent-val color-array port)
;; (PSimage row col resolution colorspace color-array port)


;; output image mask using dictionary structure and filter
(define (PSimagemask row col transparent-val color-array port)
  (let ((array-size (* row col)))
    (format port 
" << /ImageType 1 /Width ~d /Height ~d /Decode ~a
     /ImageMatrix ~a 
     /DataSource currentfile /ASCIIHexDecode filter
  >>
  imagemask~%"
          col row  
	  (if (= transparent-val 1) "[1 0]" "[0 1]")
	  (PSmatrix (matrix row 0 0 (- 0 col) 0 col)))
    (output-hex-string port array-size 1 color-array)))


;; output image using dictionary structure and filter
(define (PSimage row col resolution colorspace color-array port)
  (let ((array-size (* (if (eq? colorspace 'gray) 1 3) row col)))
    (if (eq? colorspace 'color)
	(format port "/DeviceRGB setcolorspace~%"))
    (format port 
"<< /ImageType 1 /Width ~d /Height ~d
    /BitsPerComponent ~d /Decode ~a
    /ImageMatrix ~a
    /DataSource currentfile /ASCIIHexDecode filter
 >>
 image~%"
          col row resolution 
	  (if (eq? colorspace 'gray) "[0 1]" "[0 1 0 1 0 1]")
	  (PSmatrix (matrix row 0 0 (- 0 col) 0 col)))
    (output-hex-string port array-size resolution color-array)))


;;=== Bitmap hex string output section ==================================

;; converts the color-array into hex values with the appropirate
;; resolution and display the hex digit out to the port

(define (output-hex-string port size resolution color-array)
  (receive (display-proc step)
	   (case resolution
	     ((1)  (values output-1  4))
	     ((2)  (values output-2  2))
	     ((4)  (values output-4  1))
	     ((8)  (values output-8  1))
	     ((12) (values output-12 1)))
	   (let lp ((n 0))
	     (if (< n size)
		 (begin (display-proc port n color-array size) 
			(lp (+ n step))))))
  (display ">" port))


;used to output sample points of resolution of 1 bits/sample
(define (output-1 port n color-array size)
  (display (number->string 
	    (+ (if (= (vector-ref color-array n) 0)       0 8)
	       (if (or (>= (+ 1 n) size)
		       (= (vector-ref color-array (+ 1 n)) 0)) 0 4)
	       (if (or (>= (+ 2 n) size) 
		       (= (vector-ref color-array (+ 2 n)) 0)) 0 2)
	       (if (or (>= (+ 3 n) size)
		       (= (vector-ref color-array (+ 3 n)) 0)) 0 1))
	    16)
	   port))


;used to output sample points of resolution of 2 bits/sample
(define (output-2 port n color-array size)
  (display (number->string (+ (* 4 (vector-ref color-array n))
			      (if (< (+ 1 n) size)
				  (vector-ref color-array (+ 1 n))
				  0))
			   16)
	   port))

;used to output sample points of resolution of 4 bits/sample
(define (output-4 port n color-array size)
  (display (number->string (vector-ref color-array n) 16)
	   port))

;; used to output sample point of resolution of 8 bits/sample
(define (output-8 port n color-array size)
  (let* ((hex-string (number->string (vector-ref color-array n) 16))
	 (len (string-length hex-string)))
    (display (if (= 2 len)
		 hex-string
		 (string-append "0" hex-string))
	     port)))

;; used to output sample point of resolution of 12 bits/sample
(define (output-12 port n color-array size)
  (let* ((hex-string (number->string (vector-ref color-array n) 16))
	 (len (string-length hex-string)))
    (display (if (= 3 len)
		 hex-string
		 (string-append (make-string (- 3 len) #\0) hex-string))
	     port)))


;===== End of path.scm ======================================
