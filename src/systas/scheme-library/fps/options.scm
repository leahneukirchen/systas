;;; options.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;


;; ===================================================================
;; Functional PostScript 

;; options.scm

;; This file contains procedures that are related to specifying
;; channel and page options.

;; When a channel is created, user can specify a set of options such
;; as title, creator, date, page orientation, etc. When the user
;; calls show-picture, he can specify options such as page number,
;; duplex, collated, etc.

;; (!format        str)   -> option

;; (!creator       str)   -> option
;; (!creation-date str)   -> option
;; (!title         str)   -> option
;; (!copyright     str)   -> option
;; (!for           str)   -> option
;; (!routing       str)   -> option

;; (!duplex        bool)  -> option
;; (!duplex-tumble bool)  -> option
;; (!num-copies    num)   -> option
;; (!collate       bool)  -> option

;; (!orientation   sym)   -> option
;; (!page-label    label) -> option

;; ===================================================================

;; This option determines output file format 

(define (!format str)        (option 'format        str))
(define (format-option? opt) (eq? (option:field opt) 'format))

;; These options are documentary and informational

(define (!creator str)       (option 'creator       str))
(define (!creation-date str) (option 'creation-date str))
(define (!title str)         (option 'title         str))
(define (!copyright str)     (option 'copyright     str))
(define (!for str)           (option 'for           str))
(define (!routing str)       (option 'routing       str))

(define (doc-option? opt) (let ((field (option:field opt)))
			    (or (eq? field 'creator)
				(eq? field 'creation-date)
				(eq? field 'title)
				(eq? field 'copyright)
				(eq? field 'for)
				(eq? field 'routing))))

;; These options determine how pages are printed

(define (!num-copies num)     (option 'num-copies    num))
(define (!duplex bool)        (option 'duplex        bool))
(define (!duplex-tumble bool) (option 'duplex-tumble bool))
(define (!collate bool)       (option 'collate       bool))

(define (print-option? opt) (let ((field (option:field opt)))
			      (or (eq? field 'num-copies)
				  (eq? field 'duplex)
				  (eq? field 'duplex-tumble)
				  (eq? field 'collate)
				  (eq? field 'orientation))))

;; This options Determines how pictures are oriented

(define (!orientation sym)   (option 'orientation   sym))
(define (orientation-label? opt) (eq? (option:field opt) 'orientation))


;; This option determines the label that is associated with each page

(define (!page-label label)  (option 'page-label    label))
(define (label-option? opt) (eq? (option:field opt) 'page-label))

;; ===== End of options.scm ======================================
