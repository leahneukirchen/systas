;;; bitmap.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;

;; ===============================================================
;; Functional PostScript

;; bitmap.scm

;; This file contains Bitmap MAKERS and their related procedures.

;; (vector->bitmap  row col res colorspace vect)       -> bitmap
;; (hex-string->bitmap row col res colorspace hex-str) -> bitmap
;; (bin-string->bitmap row col res colorspace bin-str) -> bitmap

;; ===============================================================

;; Bitmap makers are procedures that produces FPS bitmaps when
;; given at least row, col, resolution, colorspace, and bitmap data.
;; FPS imposes no restriction on the storage and representation 
;; of the bitmap data. For example, bitmap maker A might simply
;; store the bitmap data as an array of colors, whereas bitmap
;; maker B might decide to store the bitmap data in a compressed
;; format.  Each bitmap maker must supply three procedures:
;;       - recolor operation, so that user may perform colormap
;;         on the bitmap
;;       - build-path procedure,so that this bitmap can be turned
;;         into an FPS bitmap-path
;;       - build-pict procedure, so that this bitmap can be turned
;;         into an FPS bitmap-pict
;;
;; These procedures are included in the FPS bitmap structure itself 
;; (as three fields in the bitmap record) Note that when a bitmap
;; is turned into a path or picture, the bitmap data must be
;; changed to a simple vector of color values.


;; Here's an example of the simplest bitmap maker: a vector->bitmap
;; procedure. The bitmap maker stores the bitmap as an array of
;; colors. The row and col args tell the number of sample points in
;; the bitmap. The vect arg contains (row*col) numbers for gray
;; bitmap, and (row*col*3) numbers for rgb color bitmap. Each number
;; (in gray) or each set of 3 number (in color) represents the color
;; value at that sample point.

(define (vector->bitmap row col res colorspace vect)
  (bitmap row col res colorspace vect
	  vector->bitmap-recolor
	  vector->bitmap-build-path
	  vector->bitmap-build-pict))

(define (hex-string->bitmap row col res colorspace hex-str)
  (bitmap row col res colorspace (parse-hex res hex-str)
	  vector->bitmap-recolor
	  vector->bitmap-build-path
	  vector->bitmap-build-pict))

(define (bin-string->bitmap row col res colorspace bin-str)
  (bitmap row col res colorspace (parse-bin res bin-str)
	  vector->bitmap-recolor
	  vector->bitmap-build-path
	  vector->bitmap-build-pict))

;; bitmap recolor function. color function for gray colorspace has arg
;; of an int and returns an int. color function of rgb colorspace take
;; three integers as args and returns three integers. These integers
;; are color values ranged from 0 to 2^resolution. Note that these
;; integers are different fro FPS color vals.

(define (vector->bitmap-recolor bmap c-func)
  (vector->bitmap (bitmap:col bmap)
		  (bitmap:row bmap)
		  (bitmap:resolution bmap)
		  (bitmap:colorspace bmap)	  
		  (if (eq? (bitmap:colorspace bmap) 'gray)
		      (recolor-gray (bitmap:bitmap-data bmap) c-func)
		      (recolor-rgb  (bitmap:bitmap-data bmap) c-func))))



;; Performs recolor in gray colospace. Every value in the vector
;; is a sample point. 
(define (recolor-gray vect c-func)
  (let* ((len (vector-length vect))
	 (new-vect (make-vector len)))
    (let lp ((n 0))
      (if (< n len)
	  (let ((gray (c-func (vector-ref vect n))))
	    (vector-set! new-vect n gray)
	    (lp (+ n 1)))))
    new-vect))


;; Perform  recolor in rgb colorspace. Every three values in the
;; vector is a sample point.
(define (recolor-rgb vect c-func)
  (let* ((len (vector-length vect))
	 (new-vect (make-vector len)))
    (let lp ((n 0))
      (if (< n len)
	  (receive (red green blue) (c-func (vector-ref vect n)
					    (vector-ref vect (+ n 1))
					    (vector-ref vect (+ n 2)))
		   (vector-set! new-vect n red)
		   (vector-set! new-vect (+ n 1) green)
		   (vector-set! new-vect (+ n 2) blue)
		   (lp (+ n 3)))))
    new-vect))

		   
;; This procedure turns bitmap built by vector->bitmap into
;; a bitmap path. We don't need to convert the bitmap data
;; because it is already a vector of colors.

(define (vector->bitmap-build-path bmap transparent-val)
  (bitmap-path (bitmap:row bmap)
	       (bitmap:col bmap)
	       transparent-val
	       (bitmap:bitmap-data bmap)))


;; This procedure turns bitmap built by vector->bitmap into
;; a bitmap picture. We don't need to convert the bitmap data
;; because it is already a vector of colors.
  
(define (vector->bitmap-build-pict bmap)
  (bitmap-pict (bitmap:row bmap) 
	       (bitmap:col bmap)
	       (bitmap:resolution bmap)
	       (bitmap:colorspace bmap)
	       (bitmap:bitmap-data bmap)))



;=== Hex Parser =============================================

;; Auxilary parser that reads in a string of data and return
;; a vector of sample points (to be fed, for example, into the
;; vector->bitmap bitmap maker) The resolution arg refers to color
;; resolution and it specifies how the string of constants is to 
;; be parsed. (whether every 2 bits makes a sample point or 4 bits 
;; or 8 bits. The more number of bits, the higher the color 
;; resolution) 


;; Hex string parser.

(define (parse-hex resolution hex-string)
  (receive (convert-proc str-step vect-len-mult)
     (case resolution
       ((1)  (values convert-1  1 4))
       ((2)  (values convert-2  1 2))
       ((4)  (values convert-4  1 1))
       ((8)  (values convert-8  2 1/2))
       ((12) (values convert-12 3 1/3)))
     (let* ((len  (string-length hex-string))
	    (vect (make-vector (* vect-len-mult len))))
       (let lp ((n 0))
	 (if (< n len)
	     (begin
	       (convert-proc n hex-string vect)
	       (lp (+ n str-step)))))
       vect)))

(define (convert-1 n hex-string vect)
  (let ((val (char->hex (string-ref hex-string n)))
	(vect-n (* n 4)))
    (vector-set! vect vect-n       
		 (bitwise-and (bitwise-right-shift val 3) 1))
    (vector-set! vect (+ 1 vect-n) 
		 (bitwise-and (bitwise-right-shift val 2) 1))
    (vector-set! vect (+ 2 vect-n) 
		 (bitwise-and (bitwise-right-shift val 1) 1))
    (vector-set! vect (+ 3 vect-n) 
		 (bitwise-and val 1))))

(define (convert-2 n hex-string vect)
  (let ((val (char->hex (string-ref hex-string n)))
	(vect-n (* n 2)))
    (vector-set! vect vect-n
		 (bitwise-and (bitwise-right-shift val 2) 3))
    (vector-set! vect (+ 1 vect-n)
		 (bitwise-and val 3))))

(define (convert-4 n hex-string vect)
  (vector-set! vect n (char->hex (string-ref hex-string n))))

(define (convert-8 n hex-string vect)
  (vector-set! vect (/ n 2)
	       (+ (char->hex (string-ref hex-string (+ n 1)))
		  (* 16 (char->hex (string-ref hex-string n))))))

(define (convert-12 n hex-string vect)
  (vector-set! vect (/ n 3)
	       (+ (char->hex (string-ref hex-string (+ n 2)))
		  (* 16  (char->hex (string-ref hex-string (+ n 1))))
		  (* 256 (char->hex (string-ref hex-string n))))))

;; bitwise-right-shift val by n times
(define (bitwise-right-shift val n)
  (let ((new-val (floor (/ val 2))))
    (if (= n 1) 
	new-val
	(bitwise-right-shift new-val (- n 1)))))


(define (hex->bin hex)
  (case hex
    ((#\0) "0000")
    ((#\1) "0001")
    ((#\2) "0010")
    ((#\3) "0011")
    ((#\4) "0100")
    ((#\5) "0101")
    ((#\6) "0110")
    ((#\7) "0111")
    ((#\8) "1000")
    ((#\9) "1001")
    ((#\a #\A) "1010")
    ((#\b #\B) "1011")
    ((#\c #\C) "1100")
    ((#\d #\D) "1101")
    ((#\e #\E) "1110")
    ((#\f #\F) "1111")
    (else (error parse-hex hex
		 "hex string contains invalid hex digit"))))


;=== Bin Parser =============================================

(define (parse-bin resolution bin-string)
  (let* ((len (string-length bin-string))
	 (vect (make-vector (/ len resolution))))
    (let lp ((n 0) (vect-n 0))
      (if (< n len)
	  (begin
	    (vector-set! vect vect-n
			 (string->number 
			  (substring bin-string n (+ n resolution))
			  2))
	    (lp (+ n resolution) (+ vect-n 1)))))
    vect))

;===== End of bitmap.scm =================================
