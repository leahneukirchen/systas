;;; tag: Tom Lord Tue Dec  4 14:59:29 2001 (=bit-rotted/inline-text.scm)
;;;
;;; inline-text.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;;



(define-module (formatting inline-text)
  :use-module (data-structures string-fun))



(define (read-line :optional port)
  (let ((port (or port (current-input-port))))
    (let loop ((rest (caddr (%% %vfdbuf-get-buffered port))))
      (if (not (string-index rest #\nl))
	  (loop (%% %vfdbuf-more port 0))
	  (let ((answer (split-after-char #\nl rest first-value)))
	    (%% %vfdbuf-advance port (string-length answer))
	    answer)))))

