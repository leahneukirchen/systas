;;; rfc822.scm - RFC 822 field-parsing code
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Portions Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;
;;; Portions Copyright (c) 1995 by Olin Shivers
;;; 
;;; This is a slight rewrite of the sunet rfc822 parser, based
;;; on reading Olin's comments and code, and borrowing some
;;; interfaces.  Several comments in this file are quotes from Olin's
;;; code.  All of the code here is rewritten, but while some was written
;;; from scratch, much was written by making "correctness
;;; preserving (or increasing)" transformations on Olin's code.
;;; 
;;; See the file "=scsh-copyright" for the permissions for Olin's
;;; code.
;;;


(define-module (http rfc822)
  :use-module (http crlf-io))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Words of Wisdom from Olin:
;;; 
;;; RFC 822 is the "Standard for the format of ARPA Internet text messages"
;;; -- the document that essentially tells how the fields in email headers
;;; (e.g., the Subject: and To: fields) are formatted. This code is for 
;;; parsing these headers. Here are two pointers to the document:
;;; 
;;; 	Emacs/ange	/ftp@ftp.internic.net:/rfc/rfc822.txt
;;;	URL 		ftp://ftp.internic.net/rfc/rfc822.txt
;;; 
;;; RFC 822 parsing is useful in other contexts as well -- the HTTP protocol
;;; uses it, and it tends to pop up here and there.
;;;
;;; RFC 822 header syntax has two levels: the general syntax for headers,
;;; and the syntax for specific headers. For example, once you have figured
;;; out which chunk of text is the To: line, there are more rules telling
;;; how to split the To: line up into a list of addresses. Another example:
;;; lines with dates, e.g., the Date: header, have a specific syntax for
;;; the time and date.
;;;
;;; This code currently *only* provides routines for parsing the gross
;;; structure -- splitting the message header into its distinct fields.
;;; It would be nice to provide the finer-detail parsers, too. You do it.
;;; 
;;;     -Olin
;;; 
;;; A note on line-terminators:
;;; 
;;; Line-terminating sequences are always a drag, because there's no agreement
;;; on them -- the Net protocols and DOS use cr/lf; Unix uses lf; the Mac
;;; uses cr. One one hand, you'd like to use the code for all of the above,
;;; on the other, you'd also like to use the code for strict applications
;;; that need definitely not to recognise bare cr's or lf's as terminators.
;;;
;;; RFC 822 requires a cr/lf (carriage-return/line-feed) pair to terminate
;;; lines of text. On the other hand, careful perusal of the text shows up
;;; some ambiguities (there are maybe three or four of these, and I'm too
;;; lazy to write them all down). Furthermore, it is an unfortunate fact
;;; that many Unix apps separate lines of RFC 822 text with simple linefeeds
;;; (e.g., messages kept in /usr/spool/mail). As a result, this code takes a 
;;; broad-minded view of line-terminators: lines can be terminated by either
;;; cr/lf or just lf, and either terminating sequence is trimmed.
;;;
;;; If you need stricter parsing, you can call the lower-level procedure
;;; %READ-RFC-822-FIELD and %READ-RFC822-HEADERS procs. They take the
;;; read-line procedure as an extra parameter. This means that you can
;;; pass in a procedure that recognises only cr/lf's, or only cr's (for a
;;; Mac app, perhaps), and you can determine whether or not the terminators 
;;; get trimmed. However, your read-line procedure must indicate the 
;;; header-terminating empty line by returning *either* the empty string or
;;;  the two-char string cr/lf (or the EOF object).


;; (read-rfc822-headers [port])
;; (%read-rfc822-headers read-line port)
;; 
;; Read in and parse up a section of text that looks like the header portion
;; of an RFC 822 message. Return an alist mapping a field name (a symbol
;; such as 'date or 'subject) to a list of field bodies -- one for
;; each occurence of the field in the header. So if there are five
;; "Received-by:" fields in the header, the alist maps 'received-by
;; to a five element list. Each body is in turn represented by a list
;; of strings -- one for each line of the field. So a field spread across
;; three lines would produce a three element body.
;;
;; The %READ-RFC822-HEADERS variant allows you to specify your own read-line
;; procedure. See notes above for reasons why.

(define-public (read-rfc822-headers :optional port)
  (let loop ((alist	'()))

    (call-with-values (lambda () (read-rfc822-field port))
      (lambda (field body)

	(cond
	 (field			(loop (assq-set! alist field (append! (assq-ref alist field) (list body)))))

	 (#t			alist))))))



;; (read-rfc822-field [port])
;; (%read-rfc822-field read-line port)
;;
;; Read one field from the port, and return two values [NAME BODY]:
;; 
;; - NAME      Symbol such as 'Subject or 'To. The field name is converted
;;             to a lowercase symbol (e.g. 'reply-to)
;;
;; 
;; - BODY      List of strings which are the field's body, e.g. 
;;             ("shivers@lcs.mit.edu"). Each list element is one line from
;;             the field's body, so if the field spreads out over three lines,
;;             then the body is a list of three strings. The terminating
;;             cr/lf's are trimmed from each string.
;; 
;; When there are no more fields -- EOF or a blank line has terminated the
;; header section -- then the procedure returns [#f #f].
;; 
;; The %READ-RFC822-FIELD variant allows you to specify your own read-line
;; procedure. The one used by READ-RFC822-FIELD terminates lines with either
;; cr/lf or just lf, and it trims the terminator from the line.


(define-public (read-rfc822-field :optional port)
  (let ((line1		(read-crlf-line port)))

    (if (or (eof-object? line1)
	    (zero? (string-length line1)))

	(values #f #f)

	(cond
	 ((string-index line1 #\:) =>	(lambda (colon)
					  (let ((name		(string->symbol (string-downcase! (substring line1 0 colon)))))

					    (let loop ((lines	(list (make-shared-substring line1 (+ colon 1)))))

					      (let ((c (peek-char port))) ; Could return EOF.

						(if (or (eqv? c #\space)
							(eqv? c #\ht))
						    (loop (cons (read-crlf-line port) lines))
						    (values name (reverse lines))))))))

	 (#t				(error "Illegal RFC 822 field syntax." line1))))))


		  

;; (rejoin-header-lines alist [separator])
;; 
;; Takes a field alist such as is returned by READ-RFC822-HEADERS and
;; returns an equivalent alist. Each body (string list) in the input
;; alist is joined into a single list in the output alist. SEPARATOR
;; is the string used to join these elements together; it defaults to
;; a single space " ", but can usefully be "\n" or "\r\n".
;; 
;; To rejoin a single body list, use `join-fields-with' from `(standard
;; string-parsing)'.
;; 

(define-public (rejoin-header-lines alist :optional separator)

  (let ((separator		(or separator " ")))

    (map (lambda (entry)
	   (cons (car entry)
		 (map (lambda (body) (join-fields-with separator body))
		      (cdr entry))))
	 alist)))



;;; Given a set of RFC822 headers like this:
;;;     From: shivers
;;; 	To: ziggy,
;;; 	  newts
;;; 	To: gjs, tk
;;;
;;; We have the following definitions:
;;; 	(get-header-all hdrs 'to)   -> ((" ziggy," " newts") (" gjs, tk"))
;;; 	    - All entries, or #f
;;; 	(get-header-lines hdrs 'to) -> (" ziggy," " newts")
;;; 	    - All lines of the first entry, or #f.
;;; 	(get-header hdrs 'to)       -> "ziggy,\n newts"
;;; 	    - First entry, with the lines joined together by newlines.

(define-public (get-header-all headers name)		(assq-ref headers name))

(define-public (get-header-lines headers name)		(and=> (assq-ref headers name) car))

(define-public (get-header headers name :optional sep)	(and=> (get-header-lines headers name)
							       (lambda (h) (apply join-fields-with (or sep "\n") h))))



;;; Other desireable functionality
;;; - Unfolding long lines.
;;; - Lexing structured fields.
;;; - Unlexing structured fields into canonical form.
;;; - Parsing and unparsing dates.
;;; - Parsing and unparsing addresses.
