;;; fps.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;



(define-module (fps fps)
  :use-module (compatability defrec)
  :use-module (compatability fluid)
  :use-module (compatability general-table)
  :use-module (compatability conditionals)
  :use-module (compatability format)
  :use-module (data-structures ratlist)
  :use-module (data-structures string-fun)
  :use-module (unix file-utils))


(define-memoizing-macro (export name) `(define-public ,name (if (defined? ',name) ,name ())))
(define-memoizing-macro (exports . names) `(begin ,@(map (lambda (x) `(export ,x)) names)))

(exports
 ;; constants
 ;;
 pi
 1/4pi
 1/2pi
 3/4pi
 5/4pi
 3/2pi
 7/4pi
 2pi
 origin
 identity-matrix
 afm-directory-list

 ;; font
 ;;
 font?
 font

 ;; Point and Matrix
 ;;
 pt?
 pt=
 pt
 pt:x
 pt:y
 add-pts
 negate-pt
 scale-pt

 matrix?
 matrix=
 matrix
 matrix*

 ;; Path Makers
 ;;
 path?
 line
 rect
 arc
 tangent-arc
 curve
 close-path
 stroke-outline-path
 bitmap->path
 bounding-box->rect
 the-empty-path

 ;; glyphs construction
 ;;
 char->glyphpath
 int->glyphpath
 glyphname->glyphpath
 vector->glyphpath
 simple-string->glyphpath
 string->glyphpath

 ;; Picture Makers
 ;;
 picture?
 stroke
 fill
 clip
 colormap
 bitmap->pict
 paint-glyphpath
 the-empty-pict

 ;; combination
 ;;
 compose
 compose-path
 compose-pict
 join
 join-path
 join-pict
 link

 ;; transformation
 ;;
 translate
 rotate
 scale

 ;; style
 ;;
 style?
 vary-default
 build-style
 with-style
 with-attrib

 ;; attributes
 ;;
 attrib?
 !color
 !line-cap
 !line-width
 !dash-pattern
 !dash-offset
 !line-join
 !miter-limit

 ;; colors
 ;;
 color?
 color=
 gray
 gray:val
 rgb
 rgb:r
 rgb:g
 rgb:b
 hsb
 hsb:h
 hsb:s
 hsb:b
 cmyk
 cmyk:c
 cmyk:m
 cmyk:y
 cmyk:k

 ;; char map
 ;;
 char-map?
 base-char-map
 lookup-char-map
 function->char-map
 alist->char-map
 mask-char-map
 native-font-char-map

 ;; int map
 ;;
 int-map?
 base-int-map
 lookup-int-map
 function->int-map
 alist->int-map
 mask-int-map
 native-font-int-map

 ;; object info
 ;;
 start-pt
 end-pt
 bounding-box
 bounding-box:max
 bounding-box:min

 ;; channel
 ;;
 channel?
 show
 show-w/ps2-text-channel
 ps2-text-channel
 close-channel

 ;; bitmap
 ;;
 bitmap?
 vector->bitmap
 hex-string->bitmap
 bin-string->bitmap

 ;; options
 ;;
 !format
 !creator
 !creation-date
 !title
 !copyright
 !for
 !routing
 !duplex
 !duplex-tumble
 !collate
 !num-copies
 !orientation
 !page-label

 ;; util
 ;;
 deg->rad
 rad->deg
 inch
 mm
 cm)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compatability Hacks
;;;

;;s optional-argument rest-arg default-exp
;;
;; This form is for evaluating optional arguments and their defaults
;; in simple procedures that take a *single* optional argument. It is
;; a macro so that the default will not be computed unless it is needed.
;; 
;; `rest-arg' is a rest list from a lambda -- e.g., `r' in
;;
;;     (lambda (a b . r) ...)
;;
;; - If `rest-arg' has 0 elements, evaluate `default-exp' and return that.
;; - If `rest-arg' has 1 element, return that element.
;; - If `rest-arg' has >1 element, error.
;;
(define-syntax optional-argument
  (syntax-rules ()
    ((optional-argument rest default-exp)
     (let ((maybe-arg rest))
       (cond ((null? maybe-arg) default-exp)
	     ((null? (cdr maybe-arg)) (car maybe-arg))
	     (else (error "too many optional arguments" maybe-arg)))))))

(define (read-line port)
  (let loop ((buffered (caddr (%% %vfdbuf-get-buffered port))))
    (let ((i (string-index buffered #\nl)))
      (if (not i)
	  (let ((more (%% %vfdbuf-more port 80)))
	    (if (not more)
		(fd->string port)
		(loop more)))
	  (let ((answer (substring buffered 0 i)))
	    (%% %vfdbuf-advance port (1+ i))
	    answer)))))

(define filter pick)

(define bitwise-and logand)

(define (bitwise-right-shift n b) (quotient n (expt 2 b)))



(load "fps/types.scm")
(load "fps/color.scm")
(load "fps/util.scm")
(load "fps/globals.scm")
(load "fps/glyph.scm")
(load "fps/comp.scm")
(load "fps/paint.scm")
(load "fps/map.scm")
(load "fps/afm.scm")
(load "fps/ask.scm")
(load "fps/show.scm")
(load "fps/mat.scm")
(load "fps/style.scm")
(load "fps/bitmap.scm")
(load "fps/options.scm")
(load "fps/ps-path.scm")
(load "fps/ps-misc.scm")

