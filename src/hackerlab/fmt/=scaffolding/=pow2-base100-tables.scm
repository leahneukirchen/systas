;;; tag: Tom Lord Tue Dec  4 14:59:28 2001 (=scaffolding/=pow2-base100-tables.scm)
;;;
;;; =pow2-base100-tables.scm - generate tables for floating point conversion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998, 1999, 2000 Tom Lord
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;; 



;;; This software comes with NO WARRANTY.
;;; The Liberty Software License, version 2:
;;; 	1. This license is valid for all people.
;;;	2. You may use this software without restriction.
;;;


(define-module (vu =pow2-base100-tables)
  :use-module (cogen autogen))



(define (=pow2-base100-tables-generates)
  `(("cvt-double-tables.h"	,generate-base100-tables.h)
    ("cvt-double-constants.h"	,generate-constants)))

(define (generate-base100-tables.h)
  (generate-ascii100-table)
  (generate-mod100-table)
  (generate-quotient100-table)
  (generate-pow2-table))


;;;	*** Start "/bin/bash -c gprof ,try gmon.out" in ~/hacking/src/vu/ at 02:42:04
;;;	Flat profile:
;;;	
;;;	./,try .0000005 (mod/quotient tables, shifts in pow2_base100)
;;;	Each sample counts as 0.01 seconds. 
;;;	  %   cumulative   self              self     total           
;;;	 time   seconds   seconds    calls  Ts/call  Ts/call  name    
;;;	  %   cumulative   self              self     total           
;;;	 time   seconds   seconds    calls  us/call  us/call  name    
;;;	 29.47      0.28     0.28                             mem_move
;;;	 28.42      0.55     0.27    28000     9.64     9.64  weird_base100_add
;;;	 26.32      0.80     0.25    29000     8.62     8.62  weird_pow2_base100
;;;




;;;	ecvt (wrong answer)
;;;	time ./,try .0000005
;;;	0.10user 0.01system 0:00.13elapsed 84%CPU (0avgtext+0avgdata 0maxresident)k
;;;	50000000000000000000000000000000000000000000000000000000000000000000
;;;
;;;	sprintf
;;;	time ./,try .0000005
;;;	0.18user 0.01system 0:00.21elapsed 90%CPU (0avgtext+0avgdata 0maxresident)k
;;;	4.99999999999999977374055912943129342806969361845403909683227539062500e-07
;;;	time ./,try
;;;	f = 3.273390607896141870013189696827599152216642046043064789483291368096133796404674554883270092325904157150886684127560071009217256545885393053328527589376e+150;
;;;	3.27339060789614187001318969682759915221664204604306478948329136809613e+150
;;;	0.51user 0.02system 0:00.55elapsed 96%CPU (0avgtext+0avgdata 0maxresident)k
;;;
;;;	double_to_string
;;;	4.9999999999999997737405591294312934280696936184540390968322753906250e-7
;;;
;;;	powers-table-frequence == 6
;;;	text	data	bss	dec	hex	filename
;;;	3578   	54995  	...    	...  	...   	../=build/vu/base100.o
;;;	time ./,try .0000005
;;;	2.46user 0.03system 0:02.52elapsed 98%CPU (0avgtext+0avgdata 0maxresident)k
;;;
;;;	powers-table-frequence == 32
;;;	text	data	bss	dec	hex	filename
;;;	3562   	10970   ...     ...     ...   	../=build/vu/base100.o
;;;	time ./,try .0000005
;;;	2.85user 0.04system 0:02.96elapsed 97%CPU (0avgtext+0avgdata 0maxresident)k
;;;	time ./,try .0000005 (mod/quotient tables)
;;;	1.20user 0.01system 0:01.23elapsed 98%CPU (0avgtext+0avgdata 0maxresident)k
;;;	time ./,try .0000005 (mod/quotient tables, shifts in pow2_base100)
;;;	1.16user 0.03system 0:01.21elapsed 98%CPU (0avgtext+0avgdata 0maxresident)k
;;;	time ./,try .0000005 (mod/quotient tables, shifts in pow2_base100, pow2_base100 on demand)
;;;	0.91user 0.04system 0:00.97elapsed 97%CPU (0avgtext+0avgdata 0maxresident)k
;;;	time ./,try .0000005 (mod/quotient tables, shifts, pow2_base100 on demand, -O2)
;;;	0.62user 0.03system 0:00.68elapsed 95%CPU (0avgtext+0avgdata 0maxresident)k
;;;	time ./,try 2048
;;;	0.84user 0.01system 0:00.87elapsed 97%CPU (0avgtext+0avgdata 0maxresident)k
;;;	time ./,try
;;;	f = 3.273390607896141870013189696827599152216642046043064789483291368096133796404674554883270092325904157150886684127560071009217256545885393053328527589376e+150;
;;;	3273390607896141870013189696827599152216642046043064789483291368096133796404674554883270092325904157150886684127560071009217256545885393053328527589376.0e0
;;;	6.08user 0.02system 0:06.14elapsed 99%CPU (0avgtext+0avgdata 0maxresident)k
;;;	time ./,try mod/quotient tables
;;;	2.32user 0.02system 0:02.55elapsed 91%CPU (0avgtext+0avgdata 0maxresident)k
;;;	
;;;
;;;	powers-table-frequence == 64
;;;	text	data	bss	dec	hex	filename
;;;	3562   	5927  	...   	...  	...   	../=build/vu/base100.o
;;;	time ./,try .0000005
;;;	3.25user 0.03system 0:03.35elapsed 97%CPU (0avgtext+0avgdata 0maxresident)k
;;;
;;;	powers-table-frequence == 312
;;;	text	data	bss	dec	hex	filename
;;;	3514   	2284   	...    	...   	...   	../=build/vu/base100.o
;;;	time ./,try .0000005
;;;	7.11user 0.02system 0:07.17elapsed 99%CPU (0avgtext+0avgdata 0maxresident)k
;;;
(begin
  (define powers-table-frequency 32)
  (define max-positive-power
    (* powers-table-frequency (quotient 1023 powers-table-frequency)))
  (define max-negative-power-magnitude
    (* powers-table-frequency
       (+ (if (zero? (modulo 1088 powers-table-frequency))
	      0
	      1)
	  (quotient 1088 powers-table-frequency)))))
  
(define (generate-constants)
  (display* "enum base100_constants\n")
  (display* "{\n")
  (display* "  powers_table_frequency = " powers-table-frequency ",\n")
  (display* "  pow2_base100_non_expt = -" max-negative-power-magnitude " - 1,\n")
  (display* "  pow2_base100_min_expt = -" max-negative-power-magnitude ",\n")
  (display* "  pow2_base100_max_expt = " max-positive-power ",\n")
  (display* "};\n"))

(define (generate-ascii100-table)
  (display "static t_uchar * base100_digits_to_ascii_table[100] = \n")
  (display "{\n")
  (display "  ")
  (let loop ((x 0))
    (and (< x 100)
	 (begin
	   (if (< x 10)
	       (begin (display "\"0") (display x) "\"")
	       (begin (display "\"") (display x) "\""))
	   (display "\", ")
	   (if (= 9 (modulo x 10))
	       (display "\n  "))
	   (loop (+ x 1)))))
  (display "\n};\n\n"))

(define (generate-mod100-table)
  (let ((domain (+ (* 64 99) (quotient (* 64 99) 100))))
    (display* "static t_uchar mod100_table[" domain "] = \n")
    (display "{\n")
    (display "  ")
    (let loop ((x 0))
      (and (< x domain)
	   (begin
	     (display (modulo x 100))
	     (display ", ")
	     (if (= 9 (modulo x 10))
		 (display "\n  "))
	     (loop (+ x 1)))))
    (display "\n};\n\n")))

(define (generate-quotient100-table)
  (let ((domain (+ (* 64 99) (quotient (* 64 99) 100))))
    (display* "static t_uchar quotient100_table[" domain "] = \n")
    (display "{\n")
    (display "  ")
    (let loop ((x 0))
      (and (< x domain)
	   (begin
	     (display (quotient x 100))
	     (display ", ")
	     (if (= 9 (modulo x 10))
		 (display "\n  "))
	     (loop (+ x 1)))))
    (display "\n};\n\n")))

(define (generate-pow2-table)
  (let ((positive-powers (positive-powers-table))
	(negative-powers (negative-powers-table)))
    (display* "static struct base100_float pow2_table[] = \n")
    (display* "{\n")
    (let loop ((expt2 (- max-negative-power-magnitude))
	       (expt100 (/ (- max-negative-power-magnitude) 2))
	       (l negative-powers))
      (and l
	   (let ((digits (base100-digits (car l))))
	     (display* "  {\n")
	     (display* "    " expt2 ", " expt100 ", " (length digits) ",\n")
	     (display-base100-digits digits)
	     (display* "  },\n")
	     (loop (+ powers-table-frequency expt2)
		   (+ expt100 (/ powers-table-frequency 2))
		   (cdr l)))))
    (let loop ((expt2 0)
	       (l positive-powers))
      (and l
	   (let ((digits (base100-digits (car l))))
	     (display* "  {\n")
	     (display* "    " expt2 ", 0, " (length digits) ",\n")
	     (display-base100-digits digits)
	     (display* "  },\n")
	     (loop (+ powers-table-frequency expt2) (cdr l)))))
    (display* "};\n")))

(define (base100-digits n)
  (if (= n 0)
      '(0)
      (let loop ((k n)
		 (answer ()))
	(if (= 0 k)
	    answer
	    (loop (quotient k 100)
		  (cons (modulo k 100) answer))))))

(define (display-base100-digits digits)
  (let loop ((d digits))
    (and d
	 (begin
	   (display* "    \"")
	   (let loop2 ((l d)
		       (i 0))
	     (and (< i 16)
		  (begin
		    (display* "\\" (number->string (car l) 8))
		    (and (cdr l) (loop2 (cdr l) (+ i 1))))))
	   (display* "\"\n")
	   (loop (and (> (length d) 16)
		      (list-cdr-ref d 16)))))))

(define (positive-powers-table)
  (let loop ((x 0)
	     (n 1))
    (if (<= x max-positive-power)
	(begin
	  (if (= 0 (modulo x powers-table-frequency))
	      (cons n (loop (1+ x) (* n 2)))
	      (loop (1+ x) (* n 2)))))))


(define (negative-powers-table)
  (reverse
   (cdr
    (let loop ((x 0)
	       (q 1))
      (if (<= x max-negative-power-magnitude)
	  (begin
	    (if (= 0 (modulo x powers-table-frequency))
		(cons q (loop (1+ x) (quotient (* 10 q) 2)))
		(loop (1+ x)
		      (quotient (* 10 q) 2)))))))))




