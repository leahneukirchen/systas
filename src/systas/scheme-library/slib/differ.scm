;;;; "differ.scm" O(NP) Sequence Comparison Algorithm.
;;; Copyright (C) 2001 Aubrey Jaffer
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;@noindent
;;This package implements the algorithm:
;;
;;@ifinfo
;;@example
;;S. Wu, E. Myers, U. Manber, and W. Miller,
;;   "An O(NP) Sequence Comparison Algorithm,"
;;   Information Processing Letters 35, 6 (1990), 317-323.
;;   @url{http://www.cs.arizona.edu/people/gene/vita.html}
;;@end example
;;@end ifinfo
;;@ifset html
;;S. Wu, <A HREF="http://www.cs.arizona.edu/people/gene/vita.html">
;;E. Myers,</A> U. Manber, and W. Miller,
;;<A HREF="http://www.cs.arizona.edu/people/gene/PAPERS/np_diff.ps">
;;"An O(NP) Sequence Comparison Algorithm,"</A>
;;Information Processing Letters 35, 6 (1990), 317-323.
;;@end ifset
;;
;;@noindent
;;If the items being sequenced are text lines, then the computed
;;edit-list is equivalent to the output of the @dfn{diff} utility
;;program.  If the items being sequenced are words, then it is like the
;;lesser known @dfn{spiff} program.
;;
;;@noindent
;;The values returned by @code{diff:edit-length} can be used to gauge
;;the degree of match between two sequences.
;;
;;@noindent
;;I believe that this algorithm is currently the fastest for these
;;tasks, but genome sequencing applications fuel extensive research in
;;this area.

(require 'array)

(define (fp:compare fp Delta snake len2)
  (let loop ((p 0))
    (do ((k (- p) (+ 1 k)))
	((> k (+ -1 Delta)))
      (array-set! fp (snake k (max (+ 1 (array-ref fp (+ -1 k)))
				   (array-ref fp (+ 1 k))))
		  k))
    (do ((k (+ Delta p) (+ -1 k)))
	((< k (+ 1 Delta)))
      (array-set! fp (snake k (max (+ 1 (array-ref fp (+ -1 k)))
				   (array-ref fp (+ 1 k))))
		  k))
    (array-set! fp (snake Delta (max (+ 1 (array-ref fp (+ -1 Delta)))
				     (array-ref fp (+ 1 Delta))))
		Delta)
    (if (= (array-ref fp Delta) len2)
	(+ Delta (* 2 p))
	(loop (+ 1 p)))))

(define (fp->edits fp Delta)
  (let loop ((idx (+ -1 Delta))
	     (ddx (+ 1 Delta))
	     (edits '()))
    (define ivl (array-ref fp idx))
    (define dvl (array-ref fp ddx))
    (if (not (= -1 dvl)) (set! dvl (- dvl ddx)))
    ;;(print idx '-> ivl ddx '-> dvl)
    (cond ((= ivl -1) edits)
	  ((= dvl -1) (loop (+ -1 idx) ddx (cons (list ivl 'insert) edits)))
	  ((> dvl ivl) (loop idx (+ 1 ddx) (cons (list dvl 'delete) edits)))
	  (else       (loop (+ -1 idx) ddx (cons (list ivl 'insert) edits))))))

(define (fp->lcs fp Delta array1 len)
  (define len1 (car (array-dimensions array1)))
  (define lcs (make-array #f len))
  (define (subarray-copy! array1 start1 end1 array2 start2)
    (do ((i start1 (+ i 1))
	 (j start2 (+ j 1))
	 (l (- end1 start1) (- l 1)))
	((<= l 0))
      (array-set! array2 (array-ref array1 i) j)))
  (let loop ((ddx (+ 1 Delta))
	     (pos len1)
	     (dpos len))
    (let* ((dvl (array-ref fp ddx))
	   (sublen (- pos (- dvl ddx -1))))
      (cond ((= dvl -1)
	     (subarray-copy! array1 0 pos lcs 0)
	     lcs)
	    (else
	     (subarray-copy! array1 (- dvl ddx -1) pos lcs (- dpos sublen))
	     (loop (+ 1 ddx) (- dvl ddx) (- dpos sublen)))))))

;;@args array1 array2 =?
;;@args array1 array2
;;@1 and @2 are one-dimensional arrays.  The procedure @3 is used
;;to compare sequence tokens for equality.  @3 defaults to @code{eqv?}.
;;@0 returns a one-dimensional array of length @code{(quotient (- (+
;;len1 len2) (fp:edit-length @1 @2)) 2)} holding the longest sequence
;;common to both @var{array}s.
(define (diff:longest-common-subsequence array1 array2 . =?)
  (define len1 (car (array-dimensions array1)))
  (define len2 (car (array-dimensions array2)))
  (define (snake k y)
    (let snloop ((x (- y k))
		 (y y))
      (if (and (< x len1) (< y len2) (=? (array-ref array1 x)
					 (array-ref array2 y)))
	  (snloop (+ 1 x) (+ 1 y))
	  y)))
  (set! =? (if (null? =?) eqv? (car =?)))
  (if (> len1 len2)
      (diff:longest-common-subsequence array2 array1)
      (let ((Delta (- len2 len1))
	    (fp (make-array -1 (list (- (+ 1 len1)) (+ 1 len2)))))
	(fp->lcs fp Delta array1
		 (quotient (- (+ len1 len2) (fp:compare fp Delta snake len2))
			   2)))))

;;@args array1 array2 =?
;;@args array1 array2
;;@1 and @2 are one-dimensional arrays.  The procedure @3 is used
;;to compare sequence tokens for equality.  @3 defaults to @code{eqv?}.
;;@0 returns a list of length @code{(fp:edit-length @1 @2)} composed of
;;a shortest sequence of edits transformaing @1 to @2.
;;
;;Each edit is a list of an integer and a symbol:
;;@table @asis
;;@item (@var{j} insert)
;;Inserts @code{(array-ref @1 @var{j})} into the sequence.
;;@item (@var{k} delete)
;;Deletes @code{(array-ref @2 @var{k})} from the sequence.
;;@end table
(define (diff:edits array1 array2 . =?)
  (define len1 (car (array-dimensions array1)))
  (define len2 (car (array-dimensions array2)))
  (define (snake k y)
    (let snloop ((x (- y k))
		 (y y))
      (if (and (< x len1) (< y len2) (=? (array-ref array1 x)
					 (array-ref array2 y)))
	  (snloop (+ 1 x) (+ 1 y)) y)))
  (set! =? (if (null? =?) eqv? (car =?)))
  (if (> len1 len2)
      (diff:reverse-edits (diff:edits array2 array1))
      (let ((Delta (- len2 len1))
	    (fp (make-array -1 (list (- (+ 1 len1)) (+ 1 len2)))))
	(fp:compare fp Delta snake len2)
	;;(do ((idx (- -1 len1) (+ 1 idx))) ((>= idx (+ 1 len2)) (newline)) (printf "%3d" idx))
	;;(do ((idx (- -1 len1) (+ 1 idx))) ((>= idx (+ 1 len2)) (newline)) (printf "%3d" (array-ref fp idx)))
	(fp->edits fp Delta))))

(define (diff:reverse-edits edits)
  (map (lambda (edit)
	 (list (car edit)
	       (case (cadr edit)
		 ((delete) 'insert)
		 ((insert) 'delete))))
       edits))

;;@args array1 array2 =?
;;@args array1 array2
;;@1 and @2 are one-dimensional arrays.  The procedure @3 is used
;;to compare sequence tokens for equality.  @3 defaults to @code{eqv?}.
;;@0 returns the length of the shortest sequence of edits transformaing
;;@1 to @2.
(define (diff:edit-length array1 array2 . =?)
  (define len1 (car (array-dimensions array1)))
  (define len2 (car (array-dimensions array2)))
  (define (snake k y)
    (let snloop ((x (- y k))
		 (y y))
      (if (and (< x len1) (< y len2) (=? (array-ref array1 x)
					 (array-ref array2 y)))
	  (snloop (+ 1 x) (+ 1 y))
	  y)))
  (set! =? (if (null? =?) eqv? (car =?)))
  (if (> len1 len2)
      (diff:edit-length array2 array1)
      (let ((Delta (- len2 len1))
	    (fp (make-array -1 (list (- (+ 1 len1)) (+ 1 len2)))))
	(fp:compare fp Delta snake len2))))

;;@example
;;(diff:longest-common-subsequence '#(f g h i e j c k l m)
;;                                 '#(f g e h i j k p q r l m))
;;                                 @result{} #(f g h i j k l m)
;;
;;(diff:edit-length '#(f g h i e j c k l m)
;;                  '#(f g e h i j k p q r l m))
;;@result{} 6
;;
;;(pretty-print (diff:edits '#(f g h i e j c k l m)
;;                          '#(f g e h i j k p q r l m)))
;;@print{}
;;((3 insert)                           ; e
;; (4 delete)                           ; c
;; (6 delete)                           ; h
;; (7 insert)                           ; p
;; (8 insert)                           ; q
;; (9 insert))                          ; r
;;@end example

;; 12 - 10 = 2
;; -11-10 -9 -8 -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7  8  9 10 11 12
;;  -1 -1 -1 -1 -1 -1 -1 -1 -1  3  7  8  9 12  9  8 -1 -1 -1 -1 -1 -1 -1 -1
;; edit-distance = 6
