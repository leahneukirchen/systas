;;; glyph.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;

;; ===================================================================
;; Functional PostScript

;; fps.glyph.scm

;; This file contains procedures that create FPS glyphpaths
;; out of various data structures. Glyphpaths are paths created
;; by concatenating paths of glyphs. 


;; (char->glyphpath          font char   [erorr-tag]) -> glyphpath
;; (int->glyphpath           font number [erorr-tag]) -> glyphpath
;; (glyphname->glyphpath     font string [erorr-tag]) -> glyphpath
;; (vector->glyphpath        font vector [erorr-tag]) -> glyphpath
;; (simple-string->glyphpath font string [erorr-tag]) -> glyphpath
;; (string->glyphpath        font string [erorr-tag]) -> glyphpath

;; error-tag determines what the procedure returns if the character,
;; glyphname, or integer does not map to a glyph. error-tag can be
;; one of the following:

;;   'empty    - the character, glyphname, or integer is ignored.
;;               procedure returns a glyphpath, even though the
;;               glyphpath may be empty. This is the default.
;;   'error    - causes an error/exception
;;   #f        - procedure returns a #f instead of glyphpath.

;; ===================================================================


;; Take a char, map it to a glyph, and return glyphpath. This
;; procedure is for ASCII scheme where all scheme characters have a
;; valid ASCII number. 

(define (char->glyphpath font char . user-error-tag)
  (let* ((error-tag (optional-argument user-error-tag 'empty))
	 (result (check-char font char error-tag)))
    (process-single-element-result font char result error-tag)))


;; Take an integer, map it to a glyph, and return glyphpath.

(define (int->glyphpath font int . user-error-tag)
  (let* ((error-tag (optional-argument user-error-tag 'empty))
	 (result (check-int font int error-tag)))
    (process-single-element-result font int result error-tag)))


;; Take a glyphname and return the glyphpath.

(define (glyphname->glyphpath font glyphname . user-error-tag)
  (let* ((error-tag (optional-argument user-error-tag 'empty))
	 (result (check-glyphname font glyphname error-tag)))
    (process-single-element-result font glyphname result error-tag)))

;; Take a vector whose elements can be character, integer, or glyphname,
;; map each element to glyph, and return the concatenated glyphpath.

(define (vector->glyphpath font vec . user-error-tag)
  (let ((error-tag (optional-argument user-error-tag 'empty)))
    (let loop ((n (- (vector-length vec) 1)) (chars '()) (tmp-lst '()))
      (if (>= n 0)
	  (let* ((ele    (vector-ref vec n))
		 (result ((cond ((char?    ele) check-char)
				((string?  ele) check-glyphname)
				((integer? ele) check-int)
				(else (error vector->glyphpath
					     "Illegal element in vector" vec)))
			  font ele error-tag)))
    
	    (cond ((char? result)
		   (loop (- n 1) (cons result chars) tmp-lst))
		  ((glyph? result)
		   (loop (- n 1) '() (update-tmp-lst result chars tmp-lst)))
		  ((eq? 'empty result)
		   (loop (- n 1) chars tmp-lst))
		  ((not result) #f)
		  ((eq? 'error result)
		   (error simple-string->glyphpath
			  "This char in the vector has no mapping"
			  ele vec font))
		  (else (internal-bug vector->glyphpath
				      "Illegal check-element result" result))))
	  (glyphpath font
		     (list->vector (update-tmp-lst #f chars tmp-lst)))))))
  
;; Take a string, map each char to glyph, and return the conatenated
;; glyphpath. "Simple strings" are strings in which escape sequences
;; are ignored. Useful for I/O.
  
(define (simple-string->glyphpath font str . user-error-tag)
  (let ((error-tag (optional-argument user-error-tag 'empty)))
    (let loop ((n (- (string-length str) 1)) (chars '()) (tmp-lst '()))
      (if (>= n 0)
	  (let* ((ele    (string-ref str n))
		 (result (check-char font ele error-tag)))
	    (cond ((char? result)
		   (loop (- n 1) (cons result chars) tmp-lst))
		  ((glyph? result)
		   (loop (- n 1) '() (update-tmp-lst result chars tmp-lst)))
		  ((eq? 'empty result)
		   (loop (- n 1) chars tmp-lst))
		  ((not result) #f)
		  ((eq? 'error result)
		   (error simple-string->glyphpath 
			  "This char in the simple string has no mapping"
			  ele str font))
		  (else (internal-bug simple-string->glyphpath
				      "Illegal check-element result" result))))
	  (glyphpath font
		     (list->vector (update-tmp-lst #f chars tmp-lst)))))))

    
;; This is the generic string which may contain escape sequences enclosed
;; with '%' and ':'. Returns a glyphpath.

(define string->glyphpath
  (let ((splitter (lambda (s)
		    (separate-fields-matching-regexp "%%\\|%:\\|%[^:%][^:]*:\\|[^%]\\+"
						     s
						     list))))
    (lambda (font str . user-error-tag)
      (let ((error-tag (optional-argument user-error-tag 'empty))
	    (lst (reverse (splitter str))))
	(let loop ((orig-lst lst) (glyphs-lst '()))
	  (if (null? orig-lst)
	      (glyphpath font (list->vector glyphs-lst))
	      (let ((result (let* ((str      (car orig-lst))
				   (first-ch (string-ref str 0)))
			      (cond ((not (char=? first-ch #\%))
				     (process-simple-str 
				      font str glyphs-lst error-tag))
				    ((string=? str "%%")
				     (process-simple-str 
				      font "%" glyphs-lst error-tag))
				    ((string=? str "%:")
					     glyphs-lst)
				    (else
				     (process-escape-str 
				      font str glyphs-lst error-tag))))))
		(and result
		     (loop (cdr orig-lst) result)))))))))
					  

(define (process-simple-str font str glyphs-lst error-tag)
  (let loop ((n (- (string-length str) 1)) (chars '()) (tmp-lst glyphs-lst))
    (if (>= n 0)
	(let* ((ele    (string-ref str n))
	       (result (check-char font ele error-tag)))
	  (cond ((char? result)
		 (loop (- n 1) (cons result chars) tmp-lst))
		((glyph? result)
		 (loop (- n 1) '() (update-tmp-lst result chars tmp-lst)))
		((eq? 'empty result)
		 (loop (- n 1) chars tmp-lst))
		((not result) #f)
		((eq? 'error result)
		 (error simple-string->glyphpath 
			"This char in the simple string has no mapping"
			ele str font))
		(else (internal-bug simple-string->glyphpath
				    "Illegal check-element result" 
				    result))))
	(update-tmp-lst #f chars tmp-lst))))

(define process-escape-str 
  (let ((splitter (lambda (s)
		    (separate-fields-matching-regexp
		     "\\[[0-9A-Fa-f]*\\]\\|([0-9A-Fa-f]*)\\|[^[(]\\+"
		     s
		     list))))
    (lambda (font str tmp-lst error-tag)
      (string-set! str 0 #\space)
      (string-set! str (- (string-length str) 1) #\space)
      (let loop ((orig-lst (reverse (splitter str))) 
		 (chars '()) (tmp-lst tmp-lst))
	(if (null? orig-lst)
	    (update-tmp-lst #f chars tmp-lst)
	    (let* ((str      (car orig-lst))
		   (first-ch (string-ref str 0)))
	      (receive (no-error new-chars new-tmp-lst)
		(cond ((char=? first-ch #\[)
		       (process-16-bit font str chars tmp-lst error-tag))
		      ((char=? first-ch #\()
		       (process-8-bit  font str chars tmp-lst error-tag))
		      (else
		       (process-glyphname font str chars tmp-lst error-tag)))
		(and no-error
		     (loop (cdr orig-lst) new-chars new-tmp-lst)))))))))


(define (process-glyphname font glyphname chars tmp-lst error-tag)
  (let ((result (check-glyphname font glyphname error-tag)))
    (cond ((char? result)
	   (values #t (cons result chars) tmp-lst))
	  ((glyph? result) 
	   (values #t chars (update-tmp-lst result chars tmp-lst)))
	  ((eq? error-tag 'empty) 
	   (values #t chars tmp-lst))
	  ((not result) 
	   (values #f chars tmp-lst))
	  ((eq? error-tag 'error)
	   (error string->glyphpath
		  "Invalid glyphname" glyphname font))
	  (else (internal-bug string->glyphpath
			      "Illegal check-glyphname result"
			      result)))))

(define (process-8-bit font bitstr chars tmp-lst error-tag)
  (let ((len (string-length bitstr)))
    (if (= 0 (modulo len 2))
	(let loop ((n (- len 2)) (chars chars) (tmp-lst tmp-lst))
	  (if (> n 0)
	      (let* ((num (+ (char->hex (string-ref bitstr n))
			     (* 16 (char->hex (string-ref bitstr (- n 1))))))
		     (result (check-int font num error-tag)))
		(receive (no-error new-chars new-tmp-lst)
		  (cond ((char? result)	 (values #t (cons result chars) 
						 tmp-lst))
			((glyph? result) (values #t chars 
						 (update-tmp-lst 
						  result chars tmp-lst)))
			((eq? error-tag 'empty) (values #t chars tmp-lst))
			((not result)           (values #f chars tmp-lst))
			((eq? error-tag 'error) (error string->glyphpath
						       "Invalid 8-bit int"  
						       bitstr font))
			(else (internal-bug string->glyphpath
					    "Illegal check-int result"
					    result)))
		  (if no-error
		      (loop (- n 2) new-chars new-tmp-lst)
		      (values #f chars tmp-lst))))
	      (values #t '() (update-tmp-lst #f chars tmp-lst))))
	(error string->glyphpath
	       "Wrong number of digits in 8-bit bitstring"
	       bitstr))))

(define (process-16-bit font bitstr chars tmp-lst error-tag)
  (let ((len (string-length bitstr)))
    (if (= 0 (modulo (- len 2) 4))
	(let loop ((n (- len 2)) (chars chars) (tmp-lst tmp-lst))
	  (if (> n 0)
	      (let* ((num (+ (char->hex (string-ref bitstr n))
			     (* 16   (char->hex (string-ref bitstr (- n 1))))
			     (* 256  (char->hex (string-ref bitstr (- n 2))))
			     (* 4096 (char->hex (string-ref bitstr (- n 2))))))
		     (result (check-int font num error-tag)))
		(receive (no-error new-chars new-tmp-lst)
		  (cond ((char? result)  (values #t (cons result chars)
						 tmp-lst))
			((glyph? result) (values #t chars 
						 (update-tmp-lst 
						  result chars tmp-lst)))
			((eq? error-tag 'empty) (values #t chars tmp-lst))
			((not result)           (values #f chars tmp-lst))
			((eq? error-tag 'error) (error string->glyphpath
						       "Invalid 16-bit int"  
						       bitstr font))
			(else (internal-bug string->glyphpath
					    "Illegal check-int result"
					    result)))
		  (if no-error
		      (loop (- n 4) new-chars new-tmp-lst)
		      (values #f chars tmp-lst))))
	      (values #t '() (update-tmp-lst #f chars tmp-lst))))
	(error string->glyphpath
	       "Wrong number of digits in 16-bit bitstring"
	       bitstr))))



;; ==== Character, Integer, and Glyphname Check =========================

;; Check a character against the font's font-char-map and return 
;; the best way to render this glyph (either the character or the
;; glyphname). It returns the error-tag if anything is wrong.
;; This procedure assumes that we are running ASCII scheme, where
;; all scheme characters have a valid ASCII number.

(define (check-char font char error-tag)
  (let* ((map       (font:font-char-map font))
	 (ascii     (char->integer char))
	 (glyph     (font-char-map-ref map ascii)))
    (if glyph
	(if (= (glyph:code glyph) ascii) 
	    char 
	    glyph)
	error-tag)))


;; Check an integer against the font's int map and return the best way
;; to render this glyph (either the character or the glyphname).
;; It returns the error-tag if anything is wrong.

(define (check-int font int error-tag)
  (let ((map  (font:font-int-map font)))
    (if (< int (int-map-size map))
	(let ((glyph  (font-int-map-ref map int)))
	  (if (glyph? glyph)
	      (let ((code (glyph:code glyph)))
		(if (< code 128) 
		    (integer->char code) 
		    glyph))
	      error-tag))
	error-tag)))



;; Check a glyphname against the font's afm table to see if it exists.
;; It returns the error-tag if the glyphname does not exist.

(define (check-glyphname font glyphname error-tag)
  (let ((table  (font:glyph-table font)))
    (or (glyph-table-ref table glyphname)
	error-tag)))


;; Process the check result for single element glyphpath creators such
;; as char->glyphpath, int->glyphpath, and glyphname->glyphpath.
;; Return a glyphpath.

(define (process-single-element-result font ele result error-tag)
  (cond ((char? result)      (glyphpath font (vector (string result))))
	((glyph? result)     (glyphpath font (vector result)))
	((eq? 'empty result) (glyphpath font (vector "")))
	((not result)     #f)
	((eq? 'error result) 
	 (cond ((char? ele)    (error char->glyphpath
				      "character does not map to a glyphname"
				      ele font))
	       ((integer? ele) (error int->glyphpath 
				      "integer does not map to a glyphname"
				      ele font))
	       ((glyph? ele)   (error glyphname->glyphpath
				      "glyphname does not exist in this font"
				      ele font))
	       (else (error process-single-element-result  
			    "the element cannot be converted to glyphpath"
			    ele font))))
	(else (internal-bug process-single-element-result 
			    "Illegal check-element result"
			    result))))
	 

(define (update-tmp-lst result chars tmp-lst)
  (let ((lst (if (null? chars)
		 tmp-lst
		 (cons (list->string chars) tmp-lst))))
    (if result
	(cons result lst)
	lst)))
	     
;; ===== End of fps.glyph.scm  =======================================
