;;; http-replies.scm: Tools for Handling HTTP Replies
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Portions Copyright (c) 2001, 2002 by Tom Lord
;;; 
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;; 
;;; Portions Copyright (c) 1994 by Brian D. Carlstrom and Olin Shivers.
;;; 
;;; This is a substantial rewrite of the sunet uri parser, based
;;; on reading Olin and Brian's comments and code, and borrowing some
;;; interfaces.  Several comments in this file are quotes from that
;;; code.  All of the code here is rewritten, but while most was written
;;; from scratch, some amount was written by making "correctness
;;; preserving (or increasing)" transformations on the old code
;;;
;;; 
;;; See the file "=scsh-copyright" for the permissions for Olin's
;;; code.
;;;



(define-module (http http-replies)
  :use-module (raw time)
  :use-module (http crlf-io)
  :use-module (http html)
  :use-module (http http-request))
  



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;

(define-public http-server/version		"Systas-Scheme/2002")
(define-public http-server/protocol		"HTTP/1.0")
(define-public http-server/admin		"nobody@you-are-on-your-own")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reply codes
;;; 
;;; These are used mostly in the HTTP-specific exception mechanism.
;;; 
;;; Any exception raised while processing an HTTP request can
;;; be automatically translated to an error reply, however,
;;; if the exception is of type `http-error', with one of these
;;; reply codes as the first parameter (and several other standard
;;; parameters -- see http-errors.scm), the error reply can be
;;; more specific than "500 Internal Server Error".
;;; 
;;; See `send-http-error-reply' in this file.
;;; 


;; http-reply-code->protocol-data code => [integer string]
;; 
;; Return the error number and string for a symbolic http
;; error code.  If the code isn't recognized, return data
;; for an internal error.
;; 
(define-public (http-reply-code->protocol-data code)
  (let ((data		(assq-ref http-reply-codes code)))

    (if data
	(values (car data) (cadr data))

	(values 500 "Internal Server Error"))))


(define-public http-reply-codes
  '((ok				200	"OK")
    (created			201	"Created")
    (accepted			202	"Accepted")
    (prov-info			203	"Provisional Information")
    (no-content			204	"No Content")

    (mult-choice		300	"Multiple Choices")
    (moved-perm			301	"Moved Permanently")
    (moved-temp			302	"Moved Temporarily")
    (method			303	"Method (obsolete)")
    (not-mod			304	"Not Modified")
    (use-proxy			305	"Use Proxy")

    (bad-request		400	"Bad Request")
    (unauthorized		401	"Unauthorized")
    (payment-req		402	"Payment Required")
    (forbidden			403	"Forbidden")
    (not-found			404	"Not Found")
    (method-not-allowed		405	"Method Not Allowed")
    (none-acceptable		406	"None Acceptable")
    (proxy-auth-required	407	"Proxy Authentication Required")
    (timeout			408	"Request Timeout")
    (conflict			409	"Conflict")
    (gone			410	"Gone")

    (internal-error		500	"Internal Server Error")
    (internal-scheme-error	500	"Internal Server Error")
    (not-implemented		501	"Not Implemented")
    (bad-gateway		502	"Bad Gateway")
    (service-unavailable	503	"Service Unavailable")
    (gateway-timeout		504	"Gateway Timeout")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Headers and Status Lines
;;; 

;; (begin-http-reply-headers port reply-code)
;; 
;; Output the first chunk of a reply header.  For example:
;; 
;;	HTTP/1.0 500 Internal Server Error^M
;;	Date: thursday, 26-oct-1 16:26:13 PDT^M
;; 	Server: Systas-Scheme/20011024^M
;; 
;; 
(define-public (begin-http-reply-headers port reply-code)
  (let ((now		(localtime)))
    (send-http-status-line port reply-code)
    (send-http-header port 'Date (http-date-string))
    (send-http-header port 'Server http-server/version)))


;; (end-http-reply-headers port)
;; 
;; Write a "\r\n" which terminates a list of reply headers.
;; 
(define-public (end-http-reply-headers port)
  (write-crlf port))


;; (send-http-status-line port reply-code reply-message)
;; 
;; Send a status reply line such as:
;; 
;;	HTTP/1.0 500 Internal Server Error^M
;; 
;; `reply-code' is a symbol from `http-reply-codes'.
;; 
(define-public (send-http-status-line port reply-code)
  (call-with-values (lambda () (http-reply-code->protocol-data reply-code))
    (lambda (status-number message)
      (display*-port port http-server/protocol " " status-number " " message "\r\n"))))


;; (send-http-header port title contents)
;; 
;; Send a single header item:
;; 
;; 	title: contents^M
;; 
(define-public (send-http-header port title contents)
  (display*-port port title ": " contents "\r\n"))


;; (http-date-string :optional timestamp)
;; 
;; Return a string for `timestamp' or `(localtime)':
;; 
;; 	26-oct-1 16:26:13 PDT
;; 
(define-public (http-date-string :optional timestamp)
  (let ((now		(or timestamp (localtime))))
    (string (full-day-name (kw-arg-ref now :wday))
	    ", " (1+ (kw-arg-ref now :mday))
	    "-" (short-month-name (kw-arg-ref now :mon))
	    "-" (short-year-name (kw-arg-ref now :year))
	    " " (kw-arg-ref now :hour) ":" (kw-arg-ref now :min) ":" (kw-arg-ref now :sec)
	    " " (kw-arg-ref now :zone))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Formatting error replies:
;;; 


;; (send-http-error-reply 
;;  port reply-code :optional request message . extras)
;; 
;; Take an http-error condition, and format it into a reply to the client.
;; 
;; If `port' is #f, use the request port or the current output port.
;; 
;; `reply-code' is a symbol from the alist `http-reply-codes'.
;; 
;; If `request' is missing or #f, we haven't even had a chance to
;; parse and construct the request. This is only used for 400
;; BAD-REQUEST error report, and we make minimal assumptions in this
;; case (0.9 protocol for the reply, for example). 
;; 
;; Some specific reply codes expect arguments of a particular form:
;; 
;; 	moved-temp, moved-perm		`message' should be the new URI
;; 
(define-public (send-http-error-reply port reply-code :optional request message . extras)
  (let* (;; Are we using  a post 0.9 version of the protocol?
	 ;;
	 (new-protocol? 		(and request (not (http-v0.9-request? request))))
	 
	 ;; Is it OK to send back an HTML body explaining things?
	 ;; 
	 (html-ok? 			(or (not request)
					    (not (string=? (http-request:method request) "HEAD"))))
	 
	 (port				(or port
					    (and request (http-request:socket request))
					    (current-output-port)))
	 
	 (send-message-paragraph	(lambda ()
					  (if message
					      (send-html-paragraph port message)))))


    ;; Begin the headers, if appropriate:
    ;; 
    (if new-protocol?
	(begin
	  (begin-http-reply-headers port reply-code)
	  (if html-ok?
	      (display*-port port "Content-type: text/html\r\n"))))


    ;; A pre-condition of this `case', if `new-protocol?' is true,
    ;; is that we're in the middle of generating reply headers.
    ;; We need to call `end-http-reply-headers' before sending 
    ;; any HTML.
    ;; 
    ;; A post-condition of this `case', if `html-ok?' is true,
    ;; is that we're in the middle of outputting a simple HTML body
    ;; and need to finish up by calling `send-simple-html-end'.
    ;; 
    (case reply-code

      ;; For these two error replies: `message' is taken to be the new URI: field.
      ;; 
      ((moved-temp moved-perm)		(if new-protocol?
					    (begin (send-http-header port 'URI message)
						   (send-http-header port 'Location (http-request:uri request))
						   (end-http-reply-headers port)))
					(if html-ok?
					    (begin
					      (send-simple-html-start port "document moved")
					      (send-html-paragraph port
								   "This document has "
								   (if (eq? reply-code 'moved-temp)
								       "temporarily "
								       "permanently ")
								   "moved to a "
								   (lambda () (send-html-simple-anchor port message "new location"))))))


      ;; The message is teh URI of the proxy.
      ;; 
      ((use-proxy)			(if new-protocol?
					    (begin (send-http-header port 'Location message)
						   (end-http-reply-headers port)))
					(if html-ok?
					    (begin
					      (send-simple-html-start port "use proxy")
					      (send-html-paragraph port
								   "This document must be access through the proxy "
								   message "\n"))))


      ((bad-request)			(if new-protocol?
					    (end-http-reply-headers port))

					(if html-ok?
					    (begin
					      (send-simple-html-start port "bad request")
					      (send-html-paragraph port
								   "Client sent a query that this server could not understand.\n")
					      (send-message-paragraph))))

      ((unauthorized)			(if new-protocol?
					    (begin (send-http-header port 'WWW-Authenticate message)
						   (end-http-reply-headers port)))
					(if html-ok?
					    (begin
					      (send-simple-html-start port "authorization required")
					      (send-html-paragraph port 
								   "Browser not authentication-capable or authentication failed.\n")
					      (send-message-paragraph))))


      ((forbidden)			(if new-protocol?
					    (end-http-reply-headers port))

					(if html-ok?
					    (begin
					      (send-simple-html-start port "request not allowed")
					      (send-html-paragraph port
								   "Your client does not have permission to perform a " (http-request:method request) "\n"
								   "operation on url " (http-request:uri request) "\n")
					      (send-message-paragraph))))

      ((not-found)			(if new-protocol?
					    (end-http-reply-headers port))

					(if html-ok?
					    (begin
					      (send-simple-html-start port "URL not found")
					      (send-html-paragraph port "The requested URL was not found on this server.\n")
					      (send-message-paragraph))))

      ((internal-error)			(if new-protocol?
					    (end-http-reply-headers port))

					(if html-ok?
					    (begin
					      (send-simple-html-start port "internal error")
					      (send-html-paragraph port
								   "The server encountered an internal error or\n"
								   "misconfiguration and was unable to complete your request.\n")
					      (send-html-paragraph port
								   "Please inform the server administrator, " http-server/admin ",\n"
								   "of the circumstances leading to the error, and time it occured.\n")
					      (send-message-paragraph))))

      
      ((not-implemented)		(if new-protocol?
					    (end-http-reply-headers port))

					(if html-ok?
					    (begin
					      (send-simple-html-start port "not implemented")
					      (send-html-paragraph port
								   "This server does not currently implement\n"
								   "the requested method (" (http-request:method request) ").\n")
					      (send-message-paragraph))))
      

      (else				(if new-protocol?
					    (end-http-reply-headers port))

					(if html-ok?
					    (begin
					      (send-simple-html-start port "confusing reply")
					      (send-html-paragraph port
								   "The server became confused trying to generate this reply.\n")
					      (send-message-paragraph)))))

    
    (if html-ok?
	(begin
	  (send-html-paragraph port
			       "Additional information:\n")
	  (for-each (lambda (x) (send-html-wth-break port x))
		    extras)

	  (send-simple-html-end port)))

    (force-output port)))



;;; tag: Tom Lord Sat Apr 13 11:07:14 2002 (http/http-replies.scm)
;;;
