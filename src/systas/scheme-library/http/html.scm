;;; html.scm:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001, 2002 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (http html))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple HTML
;;; 
;;; A simple HTML document begins:
;;; 
;;; 	<HEAD>
;;; 	<TITLE>title</TITLE>
;;;	</HEAD>
;;; 	<BODY>
;;; 	<H1>title</H1>
;;; 
;;; and ends:
;;; 
;;; 	</BODY>
;;; 
;;; 

(define-public (send-simple-html-start port title)
  (display*-port port
		 "<HEAD>\n"
		 "<TITLE>" title "</TITLE>\n"
		 "</HEAD>\n"
		 "<BODY>\n"
		 "<H1>" title "</H1>\n\n"))

(define-public (send-simple-html-end port)
  (display*-port port
		 "</BODY>\n"))

	    


(define-public (send-html-paragraph port . contents)
  (display*-port port "<P>\n")
  (apply send-html-text port  contents)
  (display*-port port "\n"))

(define-public (send-html-text port . args)
  (for-each (lambda (part)
	      (cond
	       ((procedure? part)			(part))
	       (#t					(display*-port port part))))
	    args))

(define-public (send-html-wth-break port . contents)
  (display*-port port "<BR>\n")
  (apply send-html-text port  contents)
  (display*-port port "\n"))


(define-public (send-html-simple-anchor port location text)
  (display*-port port "<A HREF=\"" location "\">" text "</A>"))

;;; tag: Tom Lord Sat Apr 13 11:26:51 2002 (http/html.scm)
;;;
