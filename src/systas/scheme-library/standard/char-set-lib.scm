;;; char-set-lib.scm - SRFI-14 ("Character-Set library" by Olin Shivers)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (standard char-set-lib)
  :use-module (standard define-record-type)
  :use-module (standard list-lib))



(define-record-type char-set
  (make-char-set members)
  char-set?
  (members char-set:members set-char-set:members!))

;; (char-set? x -> boolean)
;; 
;; Is the object X a character set?
;; 
(define-public char-set? char-set?)

;; (char-set= cs1 cs2 ... -> boolean)
;; 
;; Are the character sets equal?
;; 
(define-public (char-set= . sets)
  (apply lset= = (map char-set:members sets)))


;; (char-set<= cs1 cs2 ... -> boolean)
;; 
;; Returns true if every character set CSi is a subset of character
;; set CSi+1.
;; 
(define-public (char-set<= . sets)
  (apply lset<= = (map char-set:members sets)))


;; (char-set-for-each proc cs -> unspecific)
;; 
;; Apply procedure PROC to each character in the character set CS.
;; Note that the order in which PROC is applied to the characters in
;; the set is not specified, and may even change from application to
;; application.
;; 
(define-public (char-set-for-each proc set)
  (for-each proc (char-set:members set)))


;; (char-set-fold kons knil cs -> object)
;; 
;; This is the fundamental iterator for character sets.  Applies the
;; function KONS across the character set CS using initial state value
;; KNIL.  That is, if CS is the empty set, the procedure returns KNIL.
;; Otherwise, some element c of CS is chosen; let cs' be the
;; remaining, unchosen characters.  The procedure returns
;; 
;; 	(char-set-fold KONS (KONS c KNIL) cs')
;; 
;; Examples:
;; 
;; 	;; CHAR-SET-MEMBERS
;; 	(lambda (cs) (char-set-fold cons '() cs))
;; 
;; 	;; CHAR-SET-SIZE
;; 	(lambda (cs) (char-set-fold (lambda (c i) (+ i 1)) 0 cs))
;; 
;; 	;; How many vowels in the char set?
;; 	(lambda (cs) 
;; 	  (char-set-fold (lambda (c i) (if (vowel? c) (+ i 1) i))
;; 			 0
;;			 cs))
;; 
(define-public (char-set-fold kons knil cs)
  (fold kons knil (char-set:members cs)))


;; (char-set-unfold  f p g seed -> char-set)
;; (char-set-unfold! f p g cset0 seed -> char-set)
;; 
;; This is a fundamental constructor for char-sets. 
;; 
;; - G is used to generate a series of "seed" values from the initial
;; seed: SEED, (G SEED), (G^2 SEED), (G^3 SEED), ...
;; 
;; - P tells us when to stop -- when it returns true when applied to
;; one of these seed values.
;; 
;; - F maps each seed value to a character. These characters are
;; collected together to form the character set (for CHAR-SET-UNFOLD),
;; or added to CSET0 in a linear-update (for CHAR-SET-UNFOLD!).
;; 
;; More precisely, the following definitions hold (although the actual
;; implementation may be more efficient):
;; 
;;     (define (char-set-unfold p f g seed) 
;;       (char-set-unfold! p f g (char-set-copy char-set:empty) seed))
;; 
;;     (define (char-set-unfold! p f g cset0 seed)
;; 	 (let lp ((seed seed) (cset cset0))
;; 	    (if (p seed)
;;		cset			; P says we are done.
;; 		(lp (g seed)		; Loop on (G SEED).
;; 		    (char-set-adjoin! cset
;;				      (f seed)))))) ; Add (F SEED) to set.
;; 
;; 
;; Examples:
;; 
;;     (port->char-set p) = (char-set-unfold eof-object? values
;;                         		     (lambda (x)
;;					        (read-char p))
;; 					     (read-char p))
;; 
;;     (list->char-set lis) = (char-set-unfold null? car cdr lis)
;; 
(define-public (char-set-unfold p f g seed) 
  (char-set-unfold! p f g (char-set-copy char-set:empty) seed))

(define-public (char-set-unfold! p f g cset0 seed)
  (let lp ((seed seed)
	   (cset cset0))
    (if (p seed)
	cset
	(lp (g seed)
	    (char-set-adjoin! cset (f seed))))))


;; (char-set-map proc char-set -> char-set)
;; 
;; PROC is a char->char procedure. Apply it to all the characters in
;; the char-set argument, and collect the results into a new character
;; set.
;; 
;; Essentially lifts PROC from a char->char procedure to a char-set ->
;; char-set procedure.
;; 
;; Example:
;; 	(char-set-map char-upcase cset)
;; 
(define-public (char-set-map proc set)
  (make-char-set
   (delete-duplicates!
    (map proc (char-set:members set))
    char=?)))


;; (char-set-map! proc char-set -> char-set)
;; 
;; Map char->char procedure PROC over the elements of the char-set
;; argument, collecting the results and modifying `char-set'
;; to contain those elements.
;; 
(define-public (char-set-map! proc set)
  (set-char-set:members! set
			 (delete-duplicates! (map proc (char-set:members set))
					     char=?))
  set)


;; (char-set char1 ... -> char-set)
;; 
;; Return a character set containing the given characters.
;; 
(define-public (char-set . elts)
  (make-char-set (delete-duplicates! elts char=?)))

;; (chars->char-set chars -> char-set)
;; 
;; Return a character set containing the characters in the list CHARS.
;;
(define-public (chars->char-set elts)
  (make-char-set (delete-duplicates! elts char=?)))
(define-public list->char-set chars->char-set)


;; (string->char-set s -> char-set)
;; 
;; Return a character set containing the characters in the string S.
;; 
(define-public (string->char-set s)
  (let ((cs (char-set-copy char-set:empty)))
    (string-for-each (lambda (c) (char-set-adjoin! cs c)) s)
    cs))


;; (ascii-range->char-set lower upper -> char-set)
;; 
;; Returns a character set containing every character whose ASCII
;; (code lies in the half-open range [LOWER,UPPER).)
;; 
;; What is the modern-day, Latin-1/Unicode equivalent to this procedure?
;; 
(define-public (ascii-range->char-set from to)
  (if (or (< to from)
	  (< from 0)
	  (< to 256))
      (throw 'ascii-range->char-set-range-error :from from :to to))
  (list->char-set (map integer->char (iota (- to from) from))))



;; (predicate->char-set pred -> char-set)
;; 
;; Returns a character set containing every character c such that
;; (PRED c) returns true.
;; 
(define-public (predicate->char-set pred)
  (char-set-filter pred char-set:full))


;; (char-set-filter pred cset)
;; 
;; Construct a new character set from cset containing only the
;; elements for which `pred' returns true.
;;
(define-public (char-set-filter pred cset)
  (make-char-set (filter pred (char-set:members cset))))


;; (->char-set x -> char-set)
;; 
;; Coerces X into a char-set. X may be a string, character, char-set,
;; or predicate. A string is converted to the set of its constituent
;; characters; a character is converted to a singleton set; a char-set
;; is returned as-is; a predicate is converted to a char-set using
;; PREDICATE->CHAR-SET.  This procedure is intended for use by other
;; procedures that want to provide "user-friendly," wide-spectrum
;; interfaces to their clients.
;; 
(define-public (->char-set obj)
  (cond
   ((char? obj)		(make-char-set (list obj)))
   ((char-set? obj)	obj)
   ((string? obj)	(string->char-set obj))
   ((procedure? obj)	(predicate->char-set obj))
   (#t			(throw 'unable-to-convert-to-char-set obj))))


;; (char-set-size cs -> integer)
;; 
;; Returns the number of elements in character set CS.
;; 
(define-public (char-set-size cs)
  (length (char-set:members cs)))


;; (char-set-count pred cs -> integer)
;; 
;; Apply PRED to the chars of character set CS, and return the number
;; of chars that caused the predicate to return true.
;; 
(define-public (char-set-count pred cs)
  (count pred (char-set:members cs)))


;; (char-set-members char-set -> character-list)
;; 
;; This procedure returns a list of the members of CHAR-SET.
;;
(define-public (char-set-members cs)
  (list-copy (char-set:members cs)))


;; (char-set->string char-set -> string)
;; 
;; This procedure returns a string of the members of CHAR-SET.
;;
(define-public (char-set->string cs)
  (apply string (char-set:members cs)))


;; (char-set-contains? char-set char -> boolean)
;; 
;; This procedure tests CHAR for membership in set char-set.
;; 
;; The MIT Scheme character set package called this procedure
;; CHAR-SET-MEMBER?, but the argument order isn't consistent with the
;; name.
;; 
(define-public (char-set-contains? set char)
  (->bool (member (char-set:members set) char char=?)))


;; (char-set-every pred cs -> boolean)
;; (char-set-any pred cs -> object)
;; 
;; The CHAR-SET-EVERY procedure returns true if predicate PRED returns
;; true of every character in the character set CS.
;; 
;; Likewise, CHAR-SET-ANY applies PRED to every character in character
;; set CS, and returns the first true value it finds.  If no character
;; produces a true value, it returns false.
;; 
;; The order in which these procedures sequence through the elements
;; of CS is not specified.
;; 
(define-public (char-set-every pred cs)
  (every pred (char-set:members cs)))


(define-public (char-set-any pred cs)
  (any pred (char-set:members cs)))


;; (char-set-adjoin cs char1 ... -> char-set)
;; (char-set-delete cs char1 ... -> char-set)
;; 
;; Add/delete the CHARi characters to/from character set CS.
;; 
(define-public (char-set-adjoin cs . chars)
  (make-char-set (apply lset-adjoin char=? (char-set:members cs) chars)))


(define-public (char-set-delete cs . chars)
  (make-char-set (apply lset-delete char=? (char-set:members cs) chars)))


;; (char-set-adjoin! cs char1 ... -> char-set)
;; 
;; Add the CHARi characters to character set CS, and return the
;; result.  This procedure modifies `cs' to produce the result.
;; 
(define-public (char-set-adjoin! cs . chars)
  (set-char-set:members! cs (apply lset-adjoin char=? (char-set:members cs) chars))
  cs)


;; (char-set-delete! cs char1 ... -> char-set)
;; 
;; Remove the CHARi characters to character set CS, and return the
;; result.  This procedure modifies `cs' to produce the result.
;; 
(define-public (char-set-delete! cs . chars)
  (set-char-set:members! cs (apply lset-delete char=? (char-set:members cs) chars))
  cs)


;; (char-set-invert char-set                       -> char-set)
;; (char-set-union char-set1 ...                   -> char-set)
;; (char-set-intersection char-set1 char-set2 ...  -> char-set)
;; (char-set-difference char-set1 char-set2 ...    -> char-set)
;; (char-set-xor char-set1 char-set2 ...           -> char-set)
;; (char-set-diff+intersection char-set1 char-set2 -> [char-set char-set])
;; 
;; These procedures implement set complement, union, intersection,
;; difference, and exclusive-or for character sets.  The union,
;; intersection, and difference operations are n-ary, associating to
;; the left; the difference function requires at least one argument,
;; while union and intersection may be applied to zero arguments.
;; 
;; CHAR-SET-DIFF+INTERSECTION returns both the difference and the
;; intersection of the arguments.
;; 
(define-public (char-set-invert set)
  (make-char-set (lset-difference char=?
				  (char-set:members char-set:full)
				  (char-set:members set))))


(define-public (char-set-union . sets)
  (make-char-set (apply lset-union char=? (map char-set:members sets))))
(define-public (char-set-intersection . sets)
  (make-char-set (apply lset-intersection char=? (map char-set:members sets))))
(define-public (char-set-difference . sets)
  (make-char-set (apply lset-difference char=? (map char-set:members sets))))
(define-public (char-set-xor . sets)
  (make-char-set (apply lset-xor char=? (map char-set:members sets))))

(define-public (char-set-diff+intersection: return . sets)
  (apply lset-diff+intersection:
	 (lambda (a b) (return (make-char-set a) (make-char-set b)))
	 char=?
	 (map char-set:members sets)))

(define-public (char-set-diff+intersection . sets)
  (apply char-set-diff+intersection: values sets))

(define-public (char-set-union! . sets)
  (let ((first	(if sets (car sets) (char-set-copy char-set:empty))))
    (set-char-set:members! first
			   (apply lset-union char=? (map char-set:members sets)))
    first))
(define-public (char-set-intersection! . sets)
  (let ((first	(if sets (car sets) (char-set-copy char-set:empty))))
    (set-char-set:members! first
			   (apply lset-intersection char=? (map char-set:members sets)))
    first))
(define-public (char-set-difference! . sets)
  (let ((first	(if sets (car sets) (char-set-copy char-set:empty))))
    (set-char-set:members! first
			   (apply lset-difference char=? (map char-set:members sets)))
    first))
(define-public (char-set-xor! . sets)
  (let ((first	(if sets (car sets) (char-set-copy char-set:empty))))
    (set-char-set:members! first
			   (apply lset-xor char=? (map char-set:members sets)))
    first))

(define-public (char-set-diff+intersection!: return . sets)
  (let ((first	(if sets (car sets) (char-set-copy char-set:empty))))
    (apply lset-diff+intersection:
	   (lambda (a b)
	     (set-char-set:members! first a)
	     (return first (make-char-set b)))
	   char=?
	   (map char-set:members sets))))

(define-public (char-set-diff+intersection! . sets)
  (apply char-set-diff+intersection!: values sets))

;; (char-set-copy cs -> char-set)
;; 
;; Returns a copy of the character set CS.  "Copy" means that if
;; either the input parameter or the result value of this procedure is
;; passed to one of the linear-update procedures described below, the
;; other character set is guaranteed not to be altered.  (A system
;; that provides pure-functional implementations of the rest of the
;; linear-operator suite could implement this procedure as the
;; identity function.)
;; 
(define-public (char-set-copy cs)
  (make-char-set (list-copy (char-set:members cs))))


(define-public char-alpha? char-alpha?)
(define-public char-alphabetic? char-alphabetic?)
(define-public char-digit? char-digit?)
(define-public char-numeric? char-numeric?)
(define-public char-space? char-space?)
(define-public char-whitespace? char-whitespace?)
(define-public char-upper? char-upper?)
(define-public char-upper-case? char-upper-case?)
(define-public char-lower? char-lower?)		
(define-public char-lower-case? char-lower-case?)		
(define-public char-alphanumeric? char-alphanumeric?)
(define-public char-graph? char-graph?)
(define-public char-graphic? char-graphic?)
(define-public char-printable? char-printable?)
(define-public char-printing? char-printing?)
(define-public char-blank? char-blank?)
(define-public char-control? char-control?)
(define-public char-punct? char-punct?)
(define-public char-punctuation? char-punctuation?)
(define-public char-xdigit? char-xdigit?)
(define-public char-hex-digit? char-hex-digit?)
(define-public char-ascii? char-ascii?)


(define-public char-set:full (make-char-set (map integer->char (iota 256))))
(define-public char-set:empty (make-char-set ()))
(define-public char-set:lower-case (predicate->char-set char-lower-case?))
(define-public char-set:lower char-set:lower-case)
(define-public char-set:upper-case (predicate->char-set char-upper-case?))
(define-public char-set:upper char-set:upper-case)
(define-public char-set:alphabetic (predicate->char-set char-alphabetic?))
(define-public char-set:alpha char-set:alphabetic)
(define-public char-set:numeric (predicate->char-set char-numeric?))
(define-public char-set:digit char-set:numeric)
(define-public char-set:alphanumeric (predicate->char-set char-alphanumeric?))
(define-public char-set:graphic (predicate->char-set char-graphic?))
(define-public char-set:graph char-set:graphic)
(define-public char-set:printing (predicate->char-set char-printing?))
(define-public char-set:printable char-set:printing)
(define-public char-set:whitespace (predicate->char-set char-whitespace?))
(define-public char-set:space char-set:whitespace)
(define-public char-set:blank (predicate->char-set char-blank?))
(define-public char-set:control (predicate->char-set char-control?))
(define-public char-set:punctuation (predicate->char-set char-punctuation?))
(define-public char-set:punct char-set:punctuation)
(define-public char-set:hex-digit (predicate->char-set char-hex-digit?))
(define-public char-set:xdigit char-set:hex-digit)
(define-public char-set:ascii (predicate->char-set char-ascii?))

