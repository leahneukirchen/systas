;;; uri.scm: URI syntax, e.g. [scheme] : path [? search ] [# fragmentid]
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Majority Copyright (c) 2001, 2002 by Tom Lord
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


(define-module (http uri)
  :use-module (standard regexps)
  :use-module (standard string-parsing))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Things to fix:
;;; 
;;; 	split-uri-path and simplify-uri-path
;;; 
;;; punt on the issue of multiple adjacent slashes.  What's the
;;; right thing to do there?
;;; 
;;; In the interest of quick prototyping, I'm basically taking
;;; Olin's word on the right way to handle URIs.
;;; 



;;; parse-uri s
;;; 
;;; Return four values:
;;; 
;;; 	scheme path search fragid
;;; 
;;; `path' must be a string.  The others may be a string or #f.
;;; 
;;;
;;; This uses the "tolerant" imprecise parsing technique
;;; recommended by Olin.
;;; 
;;; Olin writes:
;;; 
;;; I wrote a URI parser that slavishly obeyed Tim Berners-Lee's
;;; spec (rfc 1630). This was a waste of time, as most URL's do not
;;; obey his spec, which is incomplete and inconsistent with the URL spec
;;; in any event. This parser is much simpler. It parses a URI into four
;;; fields:
;;; 
;;;     [ <scheme> ] : <path> [ ? <search> ] [ # fragid ]
;;; 
;;; The returned fields are *not* unescaped, as the rules for parsing the
;;; <path> component in particular need unescaped text, and are dependent
;;; on <scheme>. The URL parser is responsible for doing this.
;;; If the <scheme>, <search> or <fragid> portions are not specified,
;;; they are #f. Otherwise, <scheme>, <search>, and <fragid> are strings;
;;; <path> is a non-empty string list.
;;; 
;;; 
;;; The parsing technique is inwards from both ends.
;;; - First we search forwards for the first reserved char (= ; / # ? : space)
;;;   If it's a colon, then that's the <scheme> part, otw no <scheme> part.
;;;   Remove it.
;;; - Then we search backwards from the end for the last reserved char.
;;;   If it's a sharp, then that's the <fragment-id> part -- remove it.
;;; - Then we search backwards from the end for the last reserved char.
;;;   If it's a question-mark, then that's the <search> part -- remove it.
;;; - What's left is the path. Split at slashes. "" -> ("")
;;;
;;; This scheme is tolerant of the various ways people build broken URI's
;;; out there on the Net. It was given to me by Dan Connolly of the W3C.
;;; 
;;; Returns four values: scheme, path, search, frag-id.
;;; Each value is either #f or a string.
;;; 

(define-public (parse-uri s)
  (apply-to-args (split-uri-at-first-reserved s)
    (lambda (head from-reserved)
      (let* ((has-scheme?	(and from-reserved
				     (char=? #\: (string-ref from-reserved))))
	     (scheme		(and has-scheme? head))
	     (sans-scheme	(if has-scheme?
				    (make-shared-substring from-reserved 1)
				    s)))


	(apply-to-args (split-uri-at-last-reserved sans-scheme)
	  (lambda (head reserved-tail)
	    (let* ((has-frag?	(and reserved-tail
				     (char=? #\# (string-ref reserved-tail))))
		   (frag	(and has-frag? (make-shared-substring reserved-tail 1)))
		   (sans-frag	(if has-frag?
				    head
				    sans-scheme)))

	      (apply-to-args (split-uri-at-last-reserved sans-frag)
		(lambda (head reserved-tail)
		  (let* ((has-search?	(and reserved-tail
					     (char=? #\? (string-ref reserved-tail))))
			 (search	(and has-search? (make-shared-substring reserved-tail 1)))
			 (path		(if has-search?
					    head
					    sans-frag)))

		    (values scheme (split-uri-path path) search frag)))))))))))

(define uri-reserved-characters "=;/#?: ")

(define split-uri-at-first-reserved
  (structured-regexp->procedure		`([] ,uri-reserved-characters)
					:pick-spec '(< (0 >))))

(define split-uri-at-last-reserved
  (structured-regexp->procedure		`($ (? ([] ,uri-reserved-characters)
					       (* ([^] ,uri-reserved-characters))))

					:pick-spec `(< 0)))

(define (split-uri-path s)
  (separate-fields-discarding-char #\/ s))



;; unescape-uri s
;; 
;; Return a new string replacing %XX sequences with the corresponding
;; character.
;; 
;; Olin advises:
;; 
;; Caution:
;; Don't use this proc until *after* you've parsed the URL -- unescaping
;; might introduce reserved chars (like slashes and colons) that could
;; blow your parse.
;;

(define-public (unescape-uri s)
  (apply string-append (unescape-uri-to-list s)))

(define (unescape-uri-to-list s)
  (apply-to-args (split-uri-at-escape s)
    (lambda (head esc remain)
      (if (string-null? esc)
	  (list head)
	  (cons head
		(cons (string (string->number (make-shared-substring esc 1) 16))
		      (unescape-uri-to-list remain)))))))

(define split-uri-at-escape
  (structured-regexp->procedure		`(| (& "%"
					       ([] xdigit)
					       ([] xdigit))
					    ($ ""))
					:pick-spec #t))



;; escape-uri s :optional reserved-char-re-proc
;; 
;; Return a new string replacing reserved characters with %XX sequences.
;; 
;; The reserved-char-re-proc should return a list of three values:
;; 
;; 	head reserved-character tail
;; 
;; Where the second two values may be #f or a string, and the first
;; must be a string.
;; 
;; Olin advises:
;; 
;; Caution:
;; All reserved chars (e.g., slash, sharp, colon) get escaped: "=;/#?: "
;; So don't apply this proc to chunks of text with syntactically meaningful
;; reserved chars (e.g., paths with URI slashes or colons) -- they'll be 
;; escaped, and lose their special meaning. E.g. it would be a mistake
;; to apply ESCAPE-URI to "//lcs.mit.edu:8001/foo/bar.html" because the
;; slashes and colons would be escaped.
;; 

(define-public (escape-uri s)
  (apply string-append
	 (escape-uri-to-list s split-uri-at-escape)))

(define-public split-uri-around-reserved-char
  (structured-regexp->procedure `(| ([^] alnum "$-_@.&!*\"'(),+")
				    ($ ""))
				:pick-spec #t))


(define (escape-uri-to-list s re-proc)
  (apply-to-args (re-proc s)
    (lambda (head reserved tail)
      (if (string-null? reserved)
	  (list head)
	  (let* ((char-number	(char->integer (string-ref reserved)))
		 (hex		(string "%"
					(if (< char-number 16)
					    "0"
					    "")
					(number->string char-number 16))))
	    `(,head ,hex ,@(escape-uri-to-list tail re-proc)))))))
					

;; uri-path-list->path plist
;; 
;; Olin advises: 
;; 
;; The elements of `plist' must be escaped in case they contain slashes.
;; This procedure doesn't escape them for you; you must do that yourself:
;; 
;;     (uri-path-list->path (map escape-uri pathlist))
;; 

(define-public (uri-path-list->path plist)
  (apply join-fields-with #\/ plist))


;;; 
;; This page: Olin's documentation
;; 

;; simplify-uri-path p
;; 
;; Remove . and foo/.. elts from path (a list of path elements). 
;; 
;; After simplification, there are no "." elements, and the only
;; ".." elements occur at the beginning of the path (i.e., they attempt
;; to back up past root). One could argue that this is illegal, and we
;; should error out in this case, reporting an unresolvable URL. The
;; URI "spec" is not even slightly clear on this issue.
;;
;; URI's are pathetic. The case of /a/b//../c is ambiguous. Do we
;; 	1) not simplify across multi-slashes?
;; 	2) Flush the "empty" dir, giving /a/b//c
;;	3) Flush across multi-slashes, giving /a/c
;; What is the meaning of //../a ? /../b ? /../../c ?
;; 

(define-public (simplify-uri-path p)
  (let loop	((p	p)
		 (ans	'()))

    (if (null? p)
	(reverse ans)

	(let ((elt				(car p)))
	  (cond
	   ((string=? "." elt)			(loop (cdr p) ans))

	   ((string=? ".." elt)			(if (or (null? ans)
							(string=? ".." (car ans)))
						    (loop (cdr p) (cons elt ans))
						    (loop (cdr p) (cdr ans))))
	   (#t					(loop (cdr p) (cons elt ans))))))))


;;; References:
;;; - ftp://ftp.internic.net/rfc/rfc1630.txt 
;;;   Original RFC
;;; - http://www.w3.org/hypertext/WWW/Addressing/URL/URI_Overview.html
;;;   General Web page of URI pointers.

;;; tag: Tom Lord Sat Apr 13 10:19:54 2002 (http/uri.scm)
;;;

