;;; http-request.scm - http requests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Portions Copyright (c) 2001 by Tom Lord
;;; 
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;; 
;;; Portions Copyright (c) 1994 by Brian D. Carlstrom and Olin Shivers.
;;; 
;;; This is a nearly complete rewrite of the sunet uri parser, based
;;; on reading Olin and Brian's comments and code, and borrowing some
;;; interfaces.  Several comments in this file are quotes from that
;;; code.  All of the code here is rewritten, but while some was written
;;; from scratch, some amount was written by making "correctness
;;; preserving (or increasing)" transformations on the old code
;;;
;;; See the file "=scsh-copyright" for the permissions for Olin's
;;; code.
;;;



(define-module (http http-request)
  :use-module (http crlf-io)
  :use-module (http http-errors)
  :use-module (http http-replies)
  :use-module (http uri)
  :use-module (http rfc822)
  :use-module (standard define-record-type)
  :use-module (standard string-parsing)
  :use-module (standard regexps))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP request structures
;;; 

(define-public-record-type http-request
  (make-http-request method uri version headers socket)
  http-request?


  ; a string (e.g. "GET" or "PUT")
  ;
  (method		http-request:method	set-http-request:method!)

  ; The escaped URI string as read from request line.
  ;
  (uri			http-request:uri	set-http-request:uri!)

  ; A (major . minor) integer pair.
  ;
  (version		http-request:version	set-http-request:version!)

  ; An rfc822 header alist (see rfc822.scm).
  ;
  (headers		http-request:headers	set-http-request:headers!)

  ; The socket connected to the client.
  ;
  (socket		http-request:socket	set-http-request:socket!))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read and Parse an http request
;;; 


;; read-http-request socket
;; 
;; Read and parse an http request from INPORT.
;; 

(define-public (read-http-request socket)
  (let ((line 		(read-crlf-line socket)))

    (if (eof-object? line)
	(http-error 'bad-request #f "premature eof in http request")

	(let* ((elts 		(separate-fields-discarding-whitespace (sans-surrounding-whitespace line)))
	       (version 	(case (length elts)
				  ((2)		'(0 . 9))
				  ((3)		(or (string->http-version (caddr elts))
						    (http-error 'bad-request #f (string "bad HTTP version: " line))))
				  (else		(http-error 'bad-request #f (string "bad HTTP version: " line)))))
	       (method		(car elts))
	       (uri-string	(cadr elts))
	       (headers		(if (equal? version '(0 . 9))
				    '()
				    (read-rfc822-headers socket))))

	  (make-http-request method uri-string version headers socket)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP versions
;;; 
;;; An http protocol version is an integer pair: (major . minor).
;;; 
;;; Versions can be converted to and from strings: "HTTP/1.0"
;;;


;; string->http-version string => (<major> . <minor>)
;; 
;; Parses strings like:
;; 
;; 		HTTP/1.0
;; 
;; Returns #f on error.
;; 
(define-public string->http-version
  (let ((re		(structured-regexp->procedure `(^$ "HTTP/"
							   (= :major (+ ([] digit)))
							   "."
							   (= :minor (+ ([] digit))))
						      :cflags 'REG_ICASE
						      :pick-spec '((@ :major) (@ :minor)))))

    (lambda (string)
      (let ((strings	(re string)))
	(and strings
	     (cons (string->number (car strings))
		   (string->number (cadr strings))))))))


;; http-version->string version => <string>
;; 
;; Convert a version (a dotted pair of major an minor numbers) to a 
;; string like:
;; 
;; 		HTTP/1.0
;; 
(define-public (http-version->string v)
  (string-append "HTTP/"
		 (number->string (car v))
		 "."
		 (number->string (cdr v))))

(define-public (http-version< v1 v2)
  (or (< (car v1) (car v2))
      (and (= (car v1) (car v2))
	   (< (cdr v1) (cdr v2)))))

(define-public (http-version<= v1 v2) (not (http-version< v2 v1)))

(define-public (http-v0.9-request? req)
  (http-version<= (http-request:version req) '(0 . 9)))


