;;; tag: Tom Lord Tue Dec  4 14:59:29 2001 (=bit-rotted/bsearch.scm)
;;;


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



(define-module #/ice-9/bsearch)


(define-public (bsearch v from to classifier)
  (let ((probe (quotient (+ from to) 2)))
    (classifier v probe
		(lambda ()
		  (and (< from probe)
		       (bsearch v from (1- probe) classifier)))
		(lambda ()
		  (and (< probe to)
		       (bsearch v (1+ probe) to classifier))))))

(define-public (bsearch-below v from to classifier)
  (let ((probe (quotient (+ from to) 2)))
    (classifier v probe
		(lambda ()
		  (if (< from probe)
		      (bsearch-below v from (1- probe) classifier)
		      (1- from)))
		(lambda ()
		  (if (< probe to)
		      (bsearch-below v (1+ probe) to classifier)
		      probe)))))

    
(define-public ((lt->classifier lt key) v x l g)
  (let ((e (vector-ref v x)))
    (cond
     ((lt key e)	(l))
     ((lt e key)	(g))
     (else		x))))
