;;; afm.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;

;; ======================================================================
;; Functional PostScript

;; afm.scm

;; This file contains procedures that deal with reading and parsing
;; of AFM data from .afm files. No procedures are exported to user.

;; ======================================================================


;; AFM-TABLE is a hash table that uses fontnames (strings) as
;; keys. Each key points to a afm record
(define afm-table (make-string-table))
(define (afm-table-ref fontname) (table-ref afm-table fontname))
(define (afm-table-set! fontname entry) (table-set! afm-table fontname entry))

;; AFM records contains the data that can be extracted from each afm
;; file. These records are entries in the afm-table
(define-record afm
  fontname 
  glyph-table 
  fontbbox
  native-char-map
  native-int-map 
  native-font-char-map
  native-font-int-map)


;; GLYPH-TABLE is a hash table that uses glyphnames (strings) as
;; keys. Each key points to a glyph record. There is one glyph-table
;; for every afm file loaded.
(define make-glyph-table make-string-table)
(define glyph-table-set! table-set!)
(define glyph-table-ref  table-ref)  


;; GLYPH record is a record for the afm data of each glyph. These
;; records are entries in the glyph-table.
(define-record glyph
  glyphname       
  code            
  width 
  bbox-left 
  bbox-bottom 
  bbox-right 
  bbox-top
  ((disclose self) (list "glyph" (glyph:glyphname self))))


;; Look up AFM-TABLE for the AFM record requested.
(define (get-afm fontname)
  (or (afm-table-ref fontname)	; AFM file has been loaded
      ;; Read and construct the afm for the font
      (let ((entry (construct-afm fontname)))
	(afm-table-set! fontname entry)
	entry)))

;; Read in the char metrics GLYPH info from a type 1 and type 3 AFM file
;; into a GLYPH-TABLE, construct the native charmap and intmap along
;; the way and return the data in a AFM record. The size of the char and
;; int maps are limited to the base-map-size for this ASCII implementation.
;; (printable characters are up to ASCII 127 and largest ASCII is 255)

(define construct-afm 
  
  (let ((split-line (lambda (s)
		      (separate-fields-discarding-regexp "[ \t]\\+" s list))))
    (lambda (fontname)
      
      (let* (;; check the FPS_AFM_PATH env var to look for the .afm file
	     (afm-file 
	      (let loop ((lst (afm-directory-list)))
		(if (null? lst)
		    (error construct-afm 
			   "Couldn't find AFM file in AFM directories."
			   fontname (afm-directory-list))
		    (or (maybe-open-input-file 
			 (string-append (car lst) "/"
					(afm-filename fontname)))
			(loop (cdr lst))))))
	     ;; read font bbox data
	     (fontbbox  
	      (let read-bbox ()
		(let ((ln (read-line afm-file)))
		  (cond ((eof-object? ln) 
			 (error afm-file
				"Bad AFM file -- no FontBBox"))
			((string-prefix? "FontBBox" ln) 
			 (let* ((ln (map string->number (cdr (split-line ln))))
				(max-x (list-ref ln 2))
				(min-x (list-ref ln 0))
				(max-y (list-ref ln 3))
				(min-y (list-ref ln 1)))
			   (min-max-coords->bbox min-x min-y max-x max-y)))
			(else (read-bbox)))))))
	
	;; read until StartCharMetrics
	(let skip-to-start ()
	  (let ((ln (read-line afm-file)))
	    (cond ((eof-object? ln) (error afm-file 
					   "Bad AFM file -- no StartCharMetrics"))
		  ((not (string-prefix? "StartCharMetrics" ln)) 
		   (skip-to-start)))))
	
	;; found start of table
	(let* ((current-glyph-table (make-glyph-table))
	       (font-int-map        (make-vector base-int-map-size))
	       (font-char-map       (make-vector base-char-map-size))
	       (int-map  (make-vector base-int-map-size #f))
	       (char-map (make-vector base-char-map-size #f)))
	  
	  ;; process metrics data
	  (let process-data ()
	    (let ((line (read-line afm-file)))
	      (cond ((eof-object? line)
		     (error afm-file
			    "Bad AFM file -- no EndCharMetrics"))
		    ((not (string-prefix? "EndCharMetrics" line))
		     (let* ((glyph      (extract-glyph line))
			    (code       (glyph:code glyph))
			    (glyphname  (glyph:glyphname glyph)))
		       
		       ;; save data to glyph-table
		       (glyph-table-set! current-glyph-table glyphname glyph)
		       
		       ;; save data to native-charmap and native-intmap
		       (if (> code -1)
			   (begin (vector-set! font-int-map code glyph)
				  (vector-set! int-map code glyphname)
				  (if (< code base-char-map-size) 
				      (begin (vector-set! font-char-map 
							  code glyph)
					     (vector-set! char-map code 
							  glyphname)))))
		       (process-data))))))
	  
	  ;; at end of metrics table
	  (close-input-port afm-file)
	  (make-afm fontname      current-glyph-table fontbbox
		    char-map      int-map
		    font-char-map font-int-map))))))



;; Part of procedure construct-afm. Check if the first string matches
;; the prefix.

(define (string-prefix? prefix string) 
  (let ((prefix-len (string-length prefix)))
    (and (<= prefix-len (string-length string))
	 (let lp ((i (- prefix-len 1)))
	   (or (= i 0)
	       (and (char=? (string-ref string i) (string-ref prefix i))
		    (lp (- i 1))))))))

;; Part of procedure construct-afm. Extract and make an glyph record
;; out of a line entry (converted to a list) in the .afm file.

(define	extract-glyph
  (let ((split-kv     (lambda (s)
			(separate-fields-after-regexp "[ \t]*;[ \t]*" s list)))
	(split-fields (lambda (s)
			(separate-fields-matching-regexp "[^ \t]\\+" s list))))
    (lambda (line)
      (let ((glyph        (make-glyph #f #f #f 0 0 0 0)))
    
	(for-each
	 (lambda (kv)
	   (switchq string=? (car kv)
	     (("C")  (set-glyph:code glyph (string->number (cadr kv))))
	     (("CH") (set-glyph:code glyph (string->number (cadr kv) 16)))
	     (("W" 
	       "WX") (set-glyph:width       glyph (string->number (cadr kv))))
	     (("N")  (set-glyph:glyphname   glyph (cadr    kv)))
	     (("B")  (set-glyph:bbox-left   glyph (cadr    kv))
		     (set-glyph:bbox-bottom glyph (caddr   kv))
		     (set-glyph:bbox-right  glyph (cadddr  kv))
		     (set-glyph:bbox-top    glyph (cadddr  (cdr kv))))))
	 (map split-fields (split-kv line)))
	(if (and (glyph:code      glyph) 
		 (glyph:glyphname glyph) 
		 (glyph:width     glyph))
	    glyph
	    (error glyph 
		   "Bad font metrics data in AFM file"
		   line))))))

;; Adds ".afm" extension given a filename
(define (afm-filename fontname) (string-append fontname ".afm"))


;; ===== End of afm.scm ==========================================2






