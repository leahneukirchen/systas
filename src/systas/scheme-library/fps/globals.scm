;;; globals.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Original Copyright (C) 1996 by Wandy Sae-Tan and Olin Shivers
;;; Modifications Copyright (C) 1998 Tom Lord
;;;

;; ===================================================================
;; Functional PostScript

;; globals.scm

;; This file contains global constants and variables.

;; pi 1/4pi 1/2pi 3/4pi 5/4pi 3/2pi 7/4pi 2pi
;; origin identity-matrix
;; afm-directory-list

;; ===================================================================

;; Constants
(define pi 3.141593)
(define 1/4pi (* 1/4 pi))
(define 1/2pi (* 1/2 pi))
(define 3/4pi (* 2/3 pi))
(define 5/4pi (* 5/4 pi))
(define 3/2pi (* 3/2 pi))
(define 7/4pi (* 7/4 pi))
(define 2pi   (* 2   pi))

(define origin              (pt 0 0))
(define identity-matrix     (matrix 1 0 0 1 0 0))

;; System Defaults
;; the system maintains a default style that's dynamically scoped
(define default-style* (make-fluid (make-style)))
(define (default-style) (fluid default-style*))
(define default-font   #f)
(define default-afm-dir-path
  (map (lambda (x) (in-vicinity x "ps-fonts/afm")) load-path))


;; ===== Scsh dependent AFM files path mechanism =====================

;; These procedures are for reading environment variables to find
;; out where the AFM files for a font might be. These procedures
;; are scsh dependent. They would need to be reimplemented to have
;; the equivelant function if scsh is not used. These procedures
;; are used procedure "read-in-afm-file" in fps.afm.scm.


;; Given a filename, either return a port, or return a #f on any error.

(define (maybe-open-input-file filename)
  (let ((answer (%i %open filename 'O_RDONLY 0)))
    (and (not (errno? answer))
	 (begin
	   (%% %vfdbuf-buffer-fd answer 4096 'O_RDONLY 'vfdbuf_auto_shift)
	   answer))))

;; Returns the value of the environment variable FPS_AFM_PATH
;; For example: if we did "setenv FPS_AFM_PATH "/mydir:/yourdir"
;; (afm-directory-list) would return ("/mydir" "/yourdir")

(define afm-directory-list
  (let ((splitter (lambda (s) (separate-fields-discarding-char #\: s list))))
    (lambda ()
      (cond ((getenv "FPS_AFM_PATH") => splitter)
	    (else default-afm-dir-path)))))

;; ===== End of globals.scm ==============================================
