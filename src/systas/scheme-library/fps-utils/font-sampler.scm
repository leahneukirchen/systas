;;; tag: Tom Lord Tue Dec  4 14:59:35 2001 (fps-utils/font-sampler.scm)
;;;
;;; font-sampler.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (fps-utils font-sampler)
  :use-module (fps fps)
  :use-module (data-structures ratlist)
  :use-module (unix directories))



;; (generate-font-sampler file)
;; 
;; Write a font sampler postscript picture to `file'.
;; 
(define (generate-font-sampler file)
  (let ((channel (ps2-text-channel file (!title "FPS font sampler"))))
    (show channel (stroke (font-sampler-picture)))
    (close-channel channel)))



;; (font-sampler-picture)
;; 
;; Return a picture of strings in fonts known to FPS.
;; 
(define (font-sampler-picture)
  (let loop ((n 0)
	     (fonts known-fonts)
	     (pics ()))
    (if (not fonts)
	(translate (inch 1) (inch 1) (apply compose pics))
	(begin
	  (pk 'font (car fonts))
	  (loop (+ 1 n)
	      (cdr fonts)
	      (cons (compose (translate (inch .25) (* n 10) (string->glyphpath (font "Helvetica" 8) (car fonts)))
			     (translate (inch 2.5) (* n 10) (string->glyphpath (font (car fonts) 8) "The quick brown fox jumps over the lazy dog.")))
		    pics))))))







(define afm-pattern (regcomp "\\.afm$"))

;; known-fonts is a list of PS fonts known to FPS.
;; 
(define known-fonts
  (sort (uniq (apply append
		     (map (lambda (dir)
			    (let* ((files 	(and (file-is-directory? dir)
						     (directory-files dir)))
				   (afm		(pick (lambda (f) (regexec afm-pattern f)) files))
				   (sans-ext 	(map (lambda (s) (substring s 0 -4)) afm)))
			      sans-ext))
			  (afm-directory-list))))
	string>?))


