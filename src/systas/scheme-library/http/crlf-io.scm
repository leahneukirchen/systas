;;; crlf-io.scm - Read cr/lf and lf terminated lines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;
;;; Portions Copyright (c) 1995 by Olin Shivers
;;; 
;;; This is a nearly complete rewrite of the sunet uri parser, based
;;; on reading Olin's comments and code, and borrowing some
;;; interfaces.  Several comments in this file are quotes from Olin's
;;; code.  All of the code here is rewritten, but while most were written
;;; from scratch, a small number were written by making "correctness
;;; preserving (or increasing)" transformations on Olin's code.
;;; 
;;; See the file "=scsh-copyright" for the permissions for Olin's
;;; code.
;;;


(define-module (http crlf-io))




;; (read-crlf-line [fd/port retain-crlf?]) -> string or EOF object
;; 
;; Read a line terminated by either line-feed or EOF. If RETAIN-CRLF? is #f 
;; (the default), a terminating cr/lf or lf sequence is trimmed from the
;; returned string.  If RETAIN-CRLF? is #t, the line returned will end
;; with \r\n.
;;
;; This is simple and inefficient. It would be save one copy if we didn't
;; use READ-LINE, but replicated its implementation instead.
;;

(define-public (read-crlf-line :optional port retain-crlf?)
  (let* ((port		(or port (current-input-port))))
    (let loop ((chrs	'()))

      (let ((c 		(read-char port)))
	(cond
	 ((eof-object? c)		(if chrs
					    (apply string (reverse! chrs))
					    c))


	 ;; End of line reached.  Guarantee that it ends with \r\n or not,
	 ;; depending on retain-crlf?.
	 ;; 
	 ((char=? #\nl c)		(cond
					 ((null? chrs)			(if retain-crlf?
									    "\r\n"
									    ""))

					 (#t				(cond
									 (retain-crlf?			(if (char=? #\cr (car chrs))
													    (apply string (reverse! (cons c chrs)))
													    (apply string (reverse! (cons "\r\n" chrs)))))
									 ((char=? #\cr (car chrs))	(apply string (reverse! (cdr chrs))))
									 (#t				(apply string (reverse! chrs)))))))

	 (#t				(loop (cons c chrs))))))))


(define-public (write-crlf port)
  (display "\r\n" port)
  (force-output port))


