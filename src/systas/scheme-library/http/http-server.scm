;;; server.scm:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001, 2002 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (http http-server)
  :use-module (standard let-values)
  :use-module (http http-request)
  :use-module (http http-replies)
  :use-module (http http-errors)
  :use-module (http uri))



(define-public (http-local-server port handler :optional url-callback)
  (let*	((my-hostname		(gethostname))
	 (my-host-entry		(gethostbyname my-hostname))
	 (my-addrs		(cons (inet-aton "127.0.0.1") (kw-arg-ref my-host-entry :addr_list)))
	 (socket		(%% %socket 'AF_INET 'SOCK_STREAM 0)))

    (%bind socket 'AF_INET 'INADDR_ANY (or port 0))

    (if url-callback
	(let* ((sockname	(%% %getsockname socket))
	       (port		(vector-ref sockname 2))
	       (url		(string-append "http://localhost:" (number->string port) "/")))
	  (url-callback url)))

    (%% %listen socket 3)

    (values socket
	    (lambda ()
	      (let* ((connection	(%% %accept socket))
		     (connect-fd	(car connection))
		     (address		(cadr connection))
		     (in-fd		connect-fd)
		     (out-fd		(%% %dup in-fd))
		     (his-addr 		(%% %getpeername in-fd)))

		(if (not (memv (vector-ref his-addr 1) my-addrs))
		    (with-output-to-port (current-error-port)
		      (lambda ()
			(display* "\n\n"
				  "================================================================\n"
				  "http-local-server: CONNECTION FROM SOME OTHER MACHINE\n"
				  "	Perhaps you are being probed by space aliens.\n"
				  "	This is a local server -- for local processes.\n"
				  "	(Closing connection from these mysterious strangers.)\n"
				  "\n"
				  "----------------------------------------------------------------\n"
				  :space-alien-addr " " his-addr " (" (inet-ntoa his-addr) ")\n"
				  "\n"
				  :my-addrs " " my-addrs " (" (map inet-ntoa my-addrs) ")\n"
				  "================================================================\n"
				  "\n\n")
			(%% %close in-fd)
			(%% %close out-fd)
			#t))

		    (begin
		      (%% %vfdbuf-buffer-fd in-fd #f 'O_RDONLY 'vfdbuf_auto_shift)

		      (let ((keep-going?		(catch #t
							  (lambda () (handler in-fd out-fd address))

							  (lambda (type . data)
							    (case type
							      ((signal abort quit)		(apply throw type data))
							      (else				(with-output-to-port (current-error-port)
												  (lambda ()
												    (display* "\n\n"
													      "================================\n"
													      "http-local-server: exception occurred (ignoring)\n"
													      "--------------------------------\n")
												    (write (cons type data))
												    (newline)
												    (display* "================================\n"
													      "\n\n")
												    #f))))

							    #t))))
			keep-going?))))))))




(define-public ((http-simple-uri-displatcher :optional get-handler post-handler) input output address)
  (let* ((request	(pk 'request (read-http-request input)))
	 (method	(pk 'method (http-request:method request)))
	 (uri		(pk 'uri (http-request:uri request)))
	 (error-reply	(lambda ign
			  (send-http-error-reply output 'not-implemented request "not implemented"
						 `(method ,(http-request:method request))
						 `(uri ,(http-request:uri request))
						 `(version ,(http-request:version request))
						 `(headers ,(http-request:headers request))))))

    (let-values (((scheme path-list search-spec fragment)	(parse-uri uri)))

      (let ((path-list		(and path-list (if (string-null? (car path-list)) (cdr path-list) path-list))))
	(if (null? (pk 'path-list path-list))
	    (error-reply)

	    (let* ((path-list 		(map unescape-uri path-list))
		   (operation		(pk 'operation (symbol (string-downcase (car path-list)))))
		   (path-params		(cdr path-list)))

	      (http-catch #f
			  (lambda ()
			    (cond

			     ((string=? "GET" method)		((or get-handler error-reply) output request operation path-params))
			     ((string=? "POST" method)		((or post-handler error-reply) output request operation path-params))
			     (#t				(error-reply))))

			  (lambda (code request message . data)
			    (pk 'errcode code)
			    (pk 'errreq request)
			    (pk 'errmsg message)
			    (pk 'errdata data)
			    (force-output)
			    (apply send-http-error-reply output code request message data)))))))



    (%% %close input)
    (%% %close output)))



;;; tag: Tom Lord Sat Apr 13 11:27:17 2002 (http/http-server.scm)
;;;
