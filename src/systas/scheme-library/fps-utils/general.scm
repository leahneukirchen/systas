;;; tag: Tom Lord Tue Dec  4 14:59:35 2001 (fps-utils/general.scm)
;;;
;;; general.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (fps-utils general)
  :use-module (fps fps))



(define-public (path-width p) (- (pt:x (bounding-box:max (bounding-box p)))
				 (pt:x (bounding-box:min (bounding-box p)))))

(define-public (path-height p) (- (pt:y (bounding-box:max (bounding-box p)))
				  (pt:y (bounding-box:min (bounding-box p)))))



(define-public (closed-rectangle left right top bottom)
  (close-path
   (line (pt left bottom)
	 (pt right bottom)
	 (pt right top)
	 (pt left top)
	 (pt left bottom)
	 (pt right bottom))))

