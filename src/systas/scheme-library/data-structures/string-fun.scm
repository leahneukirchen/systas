;;; tag: Tom Lord Tue Dec  4 14:59:30 2001 (data-structures/string-fun.scm)
;;;
;;; installed-scm-file

;;;; 	Copyright (C) 1996 Tom Lord
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;; 


(define-module (data-structures string-fun)
  :use-module (data-structures ratlist)
  :use-module (regexps cached))


;;;;
;;; {String Fun}
;;;
;;; Various string funcitons, particularly those that take
;;; advantage of the "shared substring" capability.
;;;

;;;;
;;; {Dividing Strings Into Fields}
;;; 
;;; The names of these functions are very regular.
;;; Here is a grammar of a call to one of these:
;;;
;;;   <string-function-invocation>
;;;   := (<action>-<seperator-disposition>-<seperator-determination> <seperator-param> <str> <ret>)
;;;
;;; <str>    = the string
;;;
;;; <ret>    = The continuation.  String functions generally return
;;;	       multiple values by passing them to this procedure.
;;;
;;; <action> =    split
;;;		| separate-fields
;;;
;;;		"split" means to divide a string into two parts.
;;;			<ret> will be called with two arguments.
;;;
;;;		"separate-fields" means to divide a string into as many
;;;			parts as possible.  <ret> will be called with
;;;			however many fields are found.
;;;
;;; <seperator-disposition> = 	  before
;;;				| after
;;;				| discarding
;;;
;;;		"before" means to leave the seperator attached to
;;;			the beginning of the field to its right.
;;;		"after" means to leave the seperator attached to
;;;			the end of the field to its left.
;;;		"discarding" means to discard seperators.
;;;
;;;		Other dispositions might be handy.  For example, "isolate"
;;;		could mean to treat the separator as a field unto itself.
;;;
;;; <seperator-determination> =	  char
;;;				| regexp
;;;
;;;		"char" means to use a particular character as field seperator.
;;;
;;; <seperator-param> = A parameter that completes the meaning of the determinations.
;;;			For example, if the determination is "char", then this parameter
;;;			says which character.  If it is "predicate", the parameter is the
;;;			predicate.
;;;
;;;
;;; For example:
;;;
;;;		(separate-fields-discarding-char #\, "foo, bar, baz, , bat" list)
;;;		=> ("foo" " bar" " baz" " " " bat")
;;;
;;;		(split-after-char #\- 'an-example-of-split list)
;;;		=> ("an-" "example-of-split")
;;;
;;; As an alternative to using a determination "predicate", or to trying to do anything
;;; complicated with these functions, consider using regular expressions.
;;;


;;;
;;; split by the first occurence of a particular character
;;;

(define-public ((char-splitter l-inc r-inc) char str ret)
  (let ((end (string-index str char)))
    (if end
	(ret (make-shared-substring str 0 (+ l-inc end))
	     (make-shared-substring str (+ r-inc end)))
	(ret str #f))))

(define-public split-before-char (char-splitter 0 0))
(define-public split-after-char (char-splitter 1 1))
(define-public split-discarding-char (char-splitter 0 1))



;;;
;;; split by the first match of a particular regexp
;;;

(define-public ((regexp-splitter parts-spec) regexp string ret)
  (apply ret (or (cached-regexec regexp string parts-spec)
		 (list string #f))))

(define-public split-before-regexp (regexp-splitter '(< ((0 0) (> 1)))))
(define-public split-after-regexp (regexp-splitter '( ((< 0) (> 0)) >)))
(define-public split-discarding-regexp (regexp-splitter '(< ((0 0) (> 1)))))



;;;
;;; separate by the first occurence of a particular character
;;;

(define-public (separate-fields-before-char ch str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-rindex str ch)
      => (lambda (pos) (loop (cons (make-shared-substring str pos) fields)
			     (make-shared-substring str 0 pos))))
     (else (apply ret (cons str fields))))))


(define-public (separate-fields-after-char ch str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-index str ch)
      => (lambda (pos) (loop (cons (make-shared-substring str 0 (1+ pos)) fields)
			     (make-shared-substring str (1+ pos)))))
     (else (apply ret (reverse (cons str fields)))))))


(define-public (separate-fields-discarding-char ch str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((string-rindex str ch)
      => (lambda (pos) (loop (cons (make-shared-substring str (1+ pos)) fields)
			     (make-shared-substring str 0 pos))))
     (else (apply ret (cons str fields))))))

;;;
;;; separate by the matches of a particular regexp
;;;


(define-public (separate-fields-before-regexp re str ret)
  (let loop ((fields '())
	     (prefix "")
	     (str str))
    (let ((match (cached-regexec re str '(< 0 >))))
      (if match
	  (let ((l (car match))
		(m (cadr match))
		(r (caddr match)))
	    (loop (cons (string-append prefix l) fields) m r))
	  (apply ret (reverse (cons (string-append prefix str) fields)))))))

(define-public (separate-fields-after-regexp re str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((cached-regexec re str '( ((< 0) (> 0))
				>))
      => (lambda (x)
	   (apply-to-args x
	     (lambda (l r)
	       (loop (cons l fields) r)))))
     (else (apply ret (reverse (cons str fields)))))))

(define-public (separate-fields-discarding-regexp re str ret)
  (let loop ((fields '())
	     (str str))
    (cond
     ((cached-regexec re str #t)
      => (lambda (x) 
	   (apply-to-args x
	     (lambda (l m r)
	       (loop (cons l fields) r)))))
     (else (apply ret (reverse (cons str fields)))))))

(define-public (separate-fields-at-regexp-pt re pt str ret)
  (let loop ((fields '())
	     (str str))
    (let ((match (cached-regexec re str `(((< 0) ,pt) (,pt (> 1))))))
      (cond
       ((null? match)			(apply ret (reverse (cons str fields))))
       ((string-null? (cadr match))	(apply ret (reverse (cons (car match) fields))))
       (#t				(loop (cons (car match) fields) (cadr match)))))))

(define-public (separate-fields-discarding-regexp-pt re pt str ret)
  (let loop ((fields '())
	     (str str))
    (let ((match (cached-regexec re str `((< (,pt 0)) ((,pt 1) >)))))
      (cond
       ((null? match)			(apply ret (reverse (cons str fields))))
       (#t				(loop (cons (car match) fields) (cadr match)))))))

(define-public (separate-fields-matching-regexp re str ret)
  (let loop ((fields '())
	     (str str))
    (let ((match (cached-regexec re str `(((0 0) (0 1)) ((0 1) (> 1))))))
      (cond
       ((not match)			(apply ret (reverse (cons str fields))))
       ((string-null? (cadr match))	(apply ret (reverse (cons (car match) fields))))
       (#t				(loop (cons (car match) fields) (cadr match)))))))


;;;;
;;; {String Prefix Predicates}
;;;
;;; Very simple:
;;;
;;; (define-public ((string-prefix-predicate pred?) prefix str)
;;;  (and (<= (string-length prefix) (string-length str))
;;;	  (pred? prefix (make-shared-substring str 0 (string-length prefix)))))
;;;
;;; (define-public string-prefix=? (string-prefix-predicate string=?))
;;;

(define-public ((string-prefix-predicate pred?) prefix str)
  (and (<= (string-length prefix) (string-length str))
       (pred? prefix (make-shared-substring str 0 (string-length prefix)))))

(define-public string-prefix=? (string-prefix-predicate string=?))


;;;;
;;; {Strippers}
;;;
;;; <stripper> = sans-<removable-part>
;;;
;;; <removable-part> = 	  surrounding-whitespace
;;;			| trailing-whitespace
;;;			| leading-whitespace
;;;			| final-newline
;;;

(define-public (sans-surrounding-whitespace s)
  (car (cached-regexec "^[\n\t ]*\\(\\|.*[^\n\t ]\\+\\)[\n\t ]*$" s '(1))))

(define-public (sans-surrounding-linear-space s)
  (car (cached-regexec "^[ \t]*\\(\\|.*[^ \t]\\+\\)[ \t]*$" s '(1))))

(define-public (sans-trailing-whitespace s)
  (car (cached-regexec "^\\(\\|.*[^\n\t ]\\+\\)[\n\t ]*$" s '(1))))

(define-public (sans-trailing-linear-space s)
  (car (cached-regexec "^\\(\\|.*[^ \t]\\+\\)[ \t]*$" s '(1))))

(define-public (sans-leading-whitespace s)
  (car (cached-regexec "^[\n\t ]*\\(.*\\)$" s '(1))))

(define-public (sans-final-newline s)
  (if (and (< 0 (string-length s)) (char=? #\nl (string-ref s (+ -1 (string-length s)))))
      (make-shared-substring s 0 (+ -1 (string-length s)))
      s))

(define-public (sans-final-cr s)
  (if (and (< 0 (string-length s)) (char=? #\cr (string-ref s (+ -1 (string-length s)))))
      (make-shared-substring s 0 (+ -1 (string-length s)))
      s))


;;;;
;;; {has-final-newline?}
;;;

(define-public (has-final-newline? str) (cached-regexec "\n$" str #f))

(define-public (ensure-trailing-newline str)
  (if (has-final-newline? str)
      str
      (string-append str "\n")))



(define-public (string-append-with-separator sep . strings)
  (cond
   ((null? strings)		"")
   ((null? (cdr strings))	(car strings))
   (#t				(reduce-init (lambda (a b) (string-append a sep b))
					     (car strings)
					     (cdr strings)))))
