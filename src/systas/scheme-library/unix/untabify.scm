;;; untabify.scm - convert tabs to spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (unix untabify))



;; (untabify-string s)
;; 
;; Return a new string of the contents of `s' with tabs replaced
;; by spaces (assuming tab stops every eight columns).
;; 
(define-public (untabify-string s)
  (call-with-output-string
   (lambda (output-port)
     (call-with-input-string s
       (lambda (input-port)
	 (%untabify input-port output-port))))))


;; (%untabify in out)
;; 
;; Read from `in' until end of file, copying the contents to `out' but
;; replacing tabs by spaces (assuming tab stops every eight columns).
;; Return #t on success, and an errno value if an I/O error occurs.
;; 
;; As a side effect of this procedure, `in' becomes buffered if it
;; wasn't already (though this shouldn't matter to most callers since
;; this procedure reads from `in' until EOF.
;; 
(define-public (%untabify in-fd out-fd)
  (errno-exceptions-return
   (lambda () (begin
		
		 ;; Make sure that the input descriptor is buffered.
		 ;;
		 (%% %vfdbuf-buffer-fd in-fd #f 'O_RDONLY 'vfdbuf_auto_shift)


		 ;; Use a regular expression to find newlines and tabs
		 ;; in the input buffer.
		 ;;
		 (let ((dfa (once (regexp->dfa "[^\n\t]*[\n\t]"))))

		   ;; Iterate over input buffers, keeping track of the
		   ;; column at the start of the buffer.
		   ;;
		   (let loop ((column 0)
			      (buffered (caddr (%% %vfdbuf-get-buffered in-fd))))

		     ;; Scan for a newline or tab.
		     ;;
		     (reset-dfa! dfa)
		     (let ((len (%% %advance-dfa-to-final-state! dfa buffered)))

		       ;; The scan can yield three possible results:
		       ;;
		       ;;	no tab or newline in the input buffer
		       ;;	found a tab first
		       ;;	found a newline first
		       ;;
		       (cond
			;; no tab or newline in the input buffer
			;;
			((= 0 (dfa-final-tag dfa))		(begin
								  ;; Try to enlarge the input buffer:
								  ;;
								  (let ((larger-buffer (%% %vfdbuf-more in-fd)))
								    ;; If we can't enlarge the input buffer, we
								    ;; must be at the end of file.  Otherwise, 
								    ;; try again with the larger buffer.
								    ;;
								    (if (not larger-buffer)
									(begin (%% %write out-fd buffered)
									       #t)
									(loop column larger-buffer)))))


			;; found a newline
			;;
			((char=? #\nl (string-ref buffered (1- len)))	(begin
									  ;; Write all of the preceeding output, including
									  ;; the newline.  Reset the column counter to 0.
									  ;; Continue.
									  (%% %write-retry out-fd (make-shared-substring buffered 0 len))
									  (%% %vfdbuf-advance in-fd len)
									  (loop 0 (caddr (%% %vfdbuf-get-buffered in-fd)))))

			;; found a tab
			;;
			((char=? #\tab (string-ref buffered (1- len)))	(begin
									  ;; Write all of the preceeding output and replace
									  ;; the tab with spaces.  Compute the new column.
									  ;; Continue.
									  (let* ((column-before-tab (+ column (1- len)))
										 (tab-loss (modulo column-before-tab 8))
										 (tab-as-spaces (make-shared-substring (once (make-string 8 #\space)) tab-loss)))

									    (%% %write out-fd
										(make-shared-substring buffered 0 (1- len)))
									    (%% %write out-fd
										tab-as-spaces)
									    (%% %vfdbuf-advance in-fd len)
									    (loop (+ column-before-tab (- 8 tab-loss))
										  (caddr (%% %vfdbuf-get-buffered in-fd))))))

			(#t					(panic "unreachable in untabify.scm"))))))))))

