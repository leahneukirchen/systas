;;; map.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;

;; ===================================================================
;; Functional PostScript

;; map.scm

;; This file contains procedures that allow user to create and modify
;; character and integer maps. A character map maps characters to
;; glyphnames, and an integer map maps integers to glyphnames. These
;; maps are font independent.

;; base-char-map
;; base-char-map-size
;; (lookup-char-mar-map char-map char)         -> glyphname
;; (function->char-map   func)                 -> char-map
;; (alist->char-map      alist [default-val])  -> char-map
;; (mask-char-map        alist char-map)       -> char-map
;; (native-font-char-map font  [default-val])  -> char-map

;; base-int-map
;; base-int-map-size
;; (lookup-int-map int-map int)                -> glyphname
;; (function->int-map   func map-size)         -> int-map
;; (alist->int-map      alist [default-val])   -> int-map
;; (mask-int-map        alist int-map)         -> int-map
;; (native-font-int-map font  [default-val])   -> int-map

;; ===================================================================


;; This base-char-map is implementation dependent. Since this
;; implementation is for ASCII Scheme, the char-map is less than 128
;; elements long. A unicode Scheme would have a different
;; base-char-map.  For this base-char-map, we use a vector to
;; represent this small set of characters. base-char-map-size is 128
;; long because ASCII goes from 1 to 127.

(define base-char-map-size 128)
(define base-char-map  (make-char-map '#(#f #f #f #f #f #f #f #f #f
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f 
  #f "space" "exclam" "quotedbl" "numbersign" "dollar" "percent"
  "ampersand" "quoteright" "parenleft" "parenright" "asterisk" "plus"
  "comma" "hyphen" "period" "slash" "zero" "one" "two" "three" "four"
  "five" "six" "seven" "eight" "nine" "colon" "semicolon" "less"
  "equal" "greater" "question" "at" "A" "B" "C" "D" "E" "F" "G" "H"
  "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y"
  "Z" "bracketleft" "backslash" "bracketright" "asciicircum"
  "underscore" "quoteleft" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
  "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
  "braceleft" "bar" "braceright" "asciitilde" #f)))


;; Lookup a character in the charmap and return the glyphname that
;; the character is associated with.

(define (lookup-char-map c-map ch)
  (vector-ref (char-map:map c-map) (char->integer ch)))


;; Create a char map with a function. The function takes
;; a character and outputs a glyphname (a string).

(define (function->char-map func)
  (make-char-map (initialize-vector base-char-map-size
				    (lambda (n) (func (integer->char n))))))
			       

;; alist->char-map takes an alist and an optional default value.
;; It creates a map according to the alist, and for the entries
;; not specified in alist, it fills in the default value. User
;; can provide their own default value, otherwise, it is #f.

(define (alist->char-map alist . user-default)
  (let* ((default-val (:optional user-default #f))
	 (new-vect    (make-vector base-char-map-size default-val)))
    (for-each (lambda (pair) 
		(vector-set! new-vect (char->integer (car pair)) (cdr pair)))
	      alist)
    (make-char-map new-vect)))

;; mask-char-map takes an alist and a char-map, and creates a new char-map
;; by masking this alist on the given char-map.

(define (mask-char-map alist c-map)
  (let ((new-vect (vector-copy (char-map:map c-map))))
    (for-each (lambda (pair) 
		(vector-set! new-vect (char->integer (car pair)) (cdr pair)))
	      alist)
    (make-char-map new-vect)))



;; Native-font-char-map returns a font's native character mapping as
;; defined in its AFM file. The character set is only limited to ASCII
;; characters. We'll take a character, find its ascii, and use that as
;; the index to the font's PS encoding table to find the glyph. All
;; the other entries are mapped to user specified default, or system
;; default #f.

(define (native-font-char-map font . user-default)
  (let* ((default-val     (:optional user-default #f))
	 (entry           (afm-table-ref (font:fontname font)))
	 (native-char-map (afm:native-char-map entry)))
    (vector-map (lambda (ele) (or ele default-val)) native-char-map)))


;; ===== Integer Map  ================================================

;; Why Int-map?
;; The ASCII character set has more than enough room in the currently
;; 128 long char-map. However, integer map provides a alternate
;; way to access the glyphs. For example, in the case of CID fontset
;; for Japanese, Korean, and Chinese, there could me as many as 8000
;; characters in the char set, and integer map will be necessary 
;; for accessing these glyphs.

;; Int-map's default val (base-int-map) is implementation dependent
;; Since this implementation is for ASCII Scheme, base-int-map
;; is only 256 elements long. 

(define base-int-map-size 256)
(define base-int-map  (make-int-map '#("space" #f #f #f #f #f #f #f
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
  #f "space" "exclam" "quotedbl" "numbersign" "dollar" "percent"
  "ampersand" "quoteright" "parenleft" "parenright" "asterisk" "plus"
  "comma" "hyphen" "period" "slash" "zero" "one" "two" "three" "four"
  "five" "six" "seven" "eight" "nine" "colon" "semicolon" "less"
  "equal" "greater" "question" "at" "A" "B" "C" "D" "E" "F" "G" "H"
  "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y"
  "Z" "bracketleft" "backslash" "bracketright" "asciicircum"
  "underscore" "quoteleft" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
  "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
  "braceleft" "bar" "braceright" "asciitilde" #f  
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f
  #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f)))


;; lookup-int-map takes a character, put it through the given int-map,
;; and return the glyphname it maps to, or #f if it is not assoc with
;; a name.

(define (lookup-int-map i-map int)
  (vector-ref (int-map:map i-map) int))


;; funcion->int-map takes a function and a range (map-size), puts
;; every int in the range through the function, and maps the integer
;; to the output of function.

(define (function->int-map func map-size)
  (make-int-map (initialize-vector map-size (lambda (n) (func n)))))

;; alist->int-map takes an alist and an optional default value.  It
;; creates a map according to the alist, and for the entries not
;; specified in alist, it fills in the default value. User can provide
;; their own default value, otherwise, it is #f. map-size must be greater
;; than or equal to the largest number in the alist.

(define (alist->int-map alist map-size . user-default)
  (let* ((default-val (:optional user-default #f))
	 (new-vect    (make-vector default-val map-size)))
    (for-each (lambda (pair) (vector-set! new-vect (car pair) (cdr pair)))
	      alist)
    (make-int-map new-vect)))


;; mask-int-map takes an alist and a int-map, and creats a new int-map
;; by masking this alist on the given int-map.
(define (mask-int-map alist i-map)
  (let ((new-vect (vector-copy (int-map:map i-map))))
    (for-each (lambda (pair) (vector-set! new-vect (car pair) (cdr pair)))
	      alist)
    (make-int-map new-vect)))


;; Native-font-int-map returns a font's native character mapping as
;; defined in its AFM file. The character set is only limited to ASCII
;; characters. We'll take a character, find its ascii, and use that as
;; the index to the font's PS encoding table to find the glyph. All
;; the other entries are mapped to user specified default, or system
;; default #f.

(define (native-font-int-map font . user-default)
  (let* ((default-val    (:optional user-default #f))
	 (entry          (afm-table-ref (font:fontname font)))
	 (native-int-map (afm:native-int-map entry)))
    (vector-map (lambda (ele) (or ele default-val)) native-int-map)))
	      

;; ===== End of map.scm ==========================================



