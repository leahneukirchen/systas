;;; tag: Tom Lord Tue Dec  4 14:59:22 2001 (=scaffolding/gen-bitset-test.scm)
;;;
;;; gen-bitset-test.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2000 Free Software Foundation, Inc.
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (vu-tests gen-bitset-test)
  :use-module (standard string-parsing)
  :use-module (standard list-lib)
  :use-module (unix output-files))



(define (generate-bitset-tests filename)
  (with-overwriting-output-file filename
    (lambda ()
      (generate-tests-bitset-n)
      (generate-tests-bitset)
      (generate-tests-bitset-range)
      (generate-tests-2-bitset))))


(define bits-per-subset 32)

(define sample-set-sizes `(0
			   1
			   ,(1- bits-per-subset)
			   ,bits-per-subset
			   ,(1+ bits-per-subset)
			   ,(1- (* 2 bits-per-subset))
			   ,(* 2 bits-per-subset)
			   ,(1+ (* 2 bits-per-subset))
			   ,(- (* 7 bits-per-subset) 13)
			   ,(* 7 bits-per-subset)
			   ,(+ (* 7 bits-per-subset) 23)))

(define fenceposts `("0 0"
		     "0 1"
		     "1 0"
		     "1 1"))

(define generate-set-fns
  (list
   (lambda (size) (list "e" '()))
   (lambda (size) (if (= size 0)
		      (list "e" ())
		      (list "f" (iota size))))
   (lambda (size) (let ((s (filter (lambda (n) (toss?)) (iota size))))
		    (list (apply join-fields-with " " (map ->string s))
			  s)))))

(define (set-name s) (car s))

(define (set-members s) (cadr s))
     
(define generate-range-fns
  (list
   ;; full range
   ;; 
   (lambda (size) (list 0 size))

   ;; random empty range
   ;;
   (lambda (size) (let* ((a (roll (1+ size)))
			 (b (roll (max 1 a))))
		    (list a b)))

   ;; random non-empty range
   ;; 
   (lambda (size) (let* ((a (roll (1+ size)))
			 (b (roll (max 1 a))))
		    (list b a)))

   ;; random tail range
   ;;
   (lambda (size) (list (roll (max 1 size)) size))

   ;; random head range
   ;;
   (lambda (size) (list 0 (roll (1+ size))))))

(define (range-start range) (car range))
(define (range-end range) (cadr range))

(define generate-index-fns
  (list
   (lambda (size) 0)
   (lambda (size) (max 0 (1- size)))
   (lambda (size) (roll (max 1 size)))))


(define test-id-counter 0)

(define (test-id) test-id-counter)

(define (test-id+) (let ((n test-id-counter))
		     (set! test-id-counter (1+ test-id-counter))
		     n))




(define (a-set size set fence)
  (display* "a " size " " fence " " (set-name set) "\n"))

(define (b-set size set fence)
  (display* "b " size " " fence " " (set-name set) "\n"))

(define (param-n n)
  (display* "n " n "\n"))

(define (param-range rng)
  (display* "r " (range-start rng) " " (range-end rng) "\n"))

(define (op name)
  (display* "# " (test-id) " " name "\n"))

(define (answer)
  (display* 'ANSWER: (test-id+) " "))

(define (clear)
  (display* "c\n"))

(define (->0/1 x) (if x 1 0))

(define (list->set-name size lst)
  (cond
   ((null? lst)				"e")
   ((= size (list-length lst))		"f")
   (#t					(apply join-fields-with " " (map ->string lst)))))



(define bitset-n-fns
  (list
   ;; is_member
   ;;
   (lambda (size set fence index)
     (a-set size set fence)
     (param-n index)
     (op "is_member")
     (answer)
     (display* (->string (->0/1 (memq index (set-members set)))) "\n")
     (clear))

   ;; bitset_adjoin
   ;;
   (lambda (size set fence index)
     (a-set size set fence)
     (param-n index)
     (op "adjoin")
     (answer)
     (display* size " " fence " " (list->set-name size (ordered-lset-adjoin = < (set-members set) index)) "\n")
     (clear))

   ;; bitset_remove
   ;;
   (lambda (size set fence index)
     (a-set size set fence)
     (param-n index)
     (op "remove")
     (answer)
     (display* size " " fence " " (list->set-name size (ordered-lset-delete = < (set-members set) index)) "\n")
     (clear))

   ;; bitset_toggle
   ;;
   (lambda (size set fence index)
     (a-set size set fence)
     (param-n index)
     (op "toggle")
     (answer)
     (display* size " " fence " "
	       (let* ((sm (set-members set))
		      (as (if (memq index sm)
			      (ordered-lset-delete = < (set-members set) index)
			      (ordered-lset-adjoin = < (set-members set) index))))
		 (list->set-name size as))
	       "\n")
     (clear))

   ))

(define (generate-tests-bitset-n)
  (for bitset-n-fns
       (lambda (fn)
	 (for sample-set-sizes
	      (lambda (size)
		; (pk 'size size)
		(if (> size 0)
		    (for fenceposts
			 (lambda (fence)
			   ; (pk 'fence fence :size size)
			   (for generate-set-fns
				(lambda (gen-set)
				  (for generate-index-fns
				       (lambda (gen-index)
					 ; (pk 'params :size size :fence fence :gen-set gen-set :gen-index gen-index) 
					 (let* ((set	(gen-set size))
						(index	(gen-index size)))
					   ; (pk 'set-index :set set :index index)
					   (fn size set fence index))))))))))))))


(define bitset-fns
  (list
   ;; is_empty
   ;;
   (lambda (size set fence)
     (a-set size set fence)
     (op "is_empty")
     (answer)
     (display* (->0/1 (null? (set-members set))) "\n")
     (clear))

   ;; is_full
   ;;
   (lambda (size set fence)
     (a-set size set fence)
     (op "is_full")
     (answer)
     (display* (->0/1 (= size (list-length (set-members set)))) "\n")
     (clear))

   ;; dup
   ;;
   (lambda (size set fence)
     (a-set size set fence)
     (op "dup")
     (answer)
     (display* size " " (list->set-name size (set-members set)) "\n")
     (clear))

   ;; clear
   ;;
   (lambda (size set fence)
     (a-set size set fence)
     (op "clear")
     (answer)
     (display* size " " fence " e" "\n")
     (clear))

   ;; fill
   ;;
   (lambda (size set fence)
     (a-set size set fence)
     (op "fill")
     (answer)
     (display* size " " fence
	       (if (= size 0)
		   " e"
		   " f")
	       "\n")
     (clear))

   ;; complement
   ;;
   (lambda (size set fence)
     (a-set size set fence)
     (op "complement")
     (answer)
     (display* size " " fence " " (list->set-name size (ordered-lset-difference = < (iota size) (set-members set))) "\n")
     (clear))

   ;; ffs
   ;;
   (lambda (size set fence)
     (a-set size set fence)
     (op "ffs")
     (answer)
     (display* (let ((set (set-members set)))
		 (if set
		     (car set)
		     -1))
	       "\n")
     (clear))

   ;; ffc
   ;;
   (lambda (size set fence)
     (a-set size set fence)
     (op "ffc")
     (answer)
     (display* (let ((clear (ordered-lset-difference = < (iota size) (set-members set))))
		 (if clear
		     (car clear)
		     -1))
	       "\n")
     (clear))

   ;; population
   ;;
   (lambda (size set fence)
     (a-set size set fence)
     (op "population")
     (answer)
     (display* (list-length (set-members set)) "\n")
     (clear))

   ))

(define (generate-tests-bitset)
  (for bitset-fns
       (lambda (fn)
	 (for sample-set-sizes
	      (lambda (size)
		; (pk 'size size)
		(for fenceposts
		     (lambda (fence)
		       ; (pk 'fence fence :size size)
		       (for generate-set-fns
			    (lambda (gen-set)
			      ; (pk 'params :size size :fence fence :gen-set gen-set) 
			      (let* ((set	(gen-set size)))
				; (pk 'set-index :set set)
				(fn size set fence)))))))))))

					 
(define bitset-range-fns
  (list
   ;; is_empty_range
   ;;
   (lambda (size set fence rng)
     (a-set size set fence)
     (param-range rng)
     (op "is_empty_range")
     (answer)
     (let* ((s 		(set-members set))
	    (subset	(ordered-lset-intersection = < (apply range rng) s)))
       (display* (->0/1 (null? subset)) "\n"))
     (clear))

   ;; is_full_range
   ;;
   (lambda (size set fence rng)
     (a-set size set fence)
     (param-range rng)
     (op "is_full_range")
     (answer)
     (let* ((s 		(set-members set))
	    (subset	(ordered-lset-intersection = < (apply range rng) s)))
       (display* (->0/1 (= (max 0 (- (range-end rng) (range-start rng)))
			   (list-length subset)))
		 "\n"))
     (clear))

   ;; clear_range
   ;;
   (lambda (size set fence rng)
     (a-set size set fence)
     (param-range rng)
     (op "clear_range")
     (answer)
     (let* ((s 		(set-members set))
	    (set	(ordered-lset-difference = < s (apply range rng))))
       (display* size " " fence " "
		 (list->set-name size set)
		 "\n"))
     (clear))

   ;; fill_range
   ;;
   (lambda (size set fence rng)
     (a-set size set fence)
     (param-range rng)
     (op "fill_range")
     (answer)
     (let* ((s 		(set-members set))
	    (set	(ordered-lset-union = < s (apply range rng))))
       (display* size " " fence " "
		 (list->set-name size set)
		 "\n"))
     (clear))

   ;; ffs_range
   ;;
   (lambda (size set fence rng)
     (a-set size set fence)
     (param-range rng)
     (op "ffs_range")
     (answer)
     (let* ((s 		(set-members set))
	    (subset	(ordered-lset-intersection = < (apply range rng) s)))
       (display* (if (null? subset) -1 (car subset)) "\n"))
     (clear))

   ;; ffc_range
   ;;
   (lambda (size set fence rng)
     (a-set size set fence)
     (param-range rng)
     (op "ffc_range")
     (answer)
     (let* ((s 		(set-members set))
	    (subset	(ordered-lset-difference = < (apply range rng) s)))
       (display* (if (null? subset) -1 (car subset)) "\n"))
     (clear))

   ;; population_range
   ;;
   (lambda (size set fence rng)
     (a-set size set fence)
     (param-range rng)
     (op "population_range")
     (answer)
     (let* ((s 		(set-members set))
	    (subset	(ordered-lset-intersection = < (apply range rng) s)))
       (display* (list-length subset) "\n"))
     (clear))
   ))


(define (generate-tests-bitset-range)
  (for bitset-range-fns
       (lambda (fn)
	 (for sample-set-sizes
	      (lambda (size)
		; (pk 'size size)
		(for fenceposts
		     (lambda (fence)
		       ; (pk 'fence fence :size size)
		       (for generate-set-fns
			    (lambda (gen-set)
			      (for generate-range-fns
				   (lambda (gen-rng)
				     ; (pk 'params :size size :fence fence :gen-set gen-set :gen-rng gen-rng) 
				     (let* ((set	(gen-set size))
					    (rng	(gen-rng size)))
				       ; (pk 'set-index :set set :rng rng)
				       (fn size set fence rng)))))))))))))




(define 2-bitset-fns
  (list
   ;; is_equal
   ;;
   (lambda (size set-a fence-a set-b fence-b)
     (a-set size set-a fence-a)
     (b-set size set-b fence-b)
     (op "is_equal")
     (answer)
     (display* (->0/1 (equal? (set-members set-a) (set-members set-b))) "\n")
     (clear))

   ;; is_subset
   ;;
   (lambda (size set-a fence-a set-b fence-b)
     (a-set size set-a fence-a)
     (b-set size set-b fence-b)
     (op "is_subset")
     (answer)
     (display* (->0/1 (ordered-lset<= = < (set-members set-a) (set-members set-b))) "\n")
     (clear))

   ;; assign
   ;;
   (lambda (size set-a fence-a set-b fence-b)
     (a-set size set-a fence-a)
     (b-set size set-b fence-b)
     (op "assign")
     (answer)
     (display* size " " fence-a " " (list->set-name size (set-members set-b)) "\n")
     (clear))

   ;; union
   ;;
   (lambda (size set-a fence-a set-b fence-b)
     (a-set size set-a fence-a)
     (b-set size set-b fence-b)
     (op "union")
     (answer)
     (display* size " " fence-a " " (list->set-name size (ordered-lset-union = < (set-members set-a) (set-members set-b))) "\n")
     (clear))


   ;; intersection
   ;;
   (lambda (size set-a fence-a set-b fence-b)
     (a-set size set-a fence-a)
     (b-set size set-b fence-b)
     (op "intersection")
     (answer)
     (display* size " " fence-a " " (list->set-name size (ordered-lset-intersection = < (set-members set-a) (set-members set-b))) "\n")
     (clear))

   ;; difference
   ;;
   (lambda (size set-a fence-a set-b fence-b)
     (a-set size set-a fence-a)
     (b-set size set-b fence-b)
     (op "difference")
     (answer)
     (display* size " " fence-a " " (list->set-name size (ordered-lset-difference = < (set-members set-a) (set-members set-b))) "\n")
     (clear))

   ;; revdifference
   ;;
   (lambda (size set-a fence-a set-b fence-b)
     (a-set size set-a fence-a)
     (b-set size set-b fence-b)
     (op "revdifference")
     (answer)
     (display* size " " fence-a " " (list->set-name size (ordered-lset-difference = < (set-members set-b) (set-members set-a))) "\n")
     (clear))

   ;; xor
   ;;
   (lambda (size set-a fence-a set-b fence-b)
     (a-set size set-a fence-a)
     (b-set size set-b fence-b)
     (op "xor")
     (answer)
     (display* size " " fence-a " " (list->set-name size (ordered-lset-xor = < (set-members set-a) (set-members set-b))) "\n")
     (clear))
   ))

(define (generate-tests-2-bitset)
  (for 2-bitset-fns
       (lambda (fn)
	 (for sample-set-sizes
	      (lambda (size)
		(for fenceposts
		     (lambda (fence-a)
		       (for fenceposts
			    (lambda (fence-b)
			      (for generate-set-fns
				   (lambda (gen-set-a)
				     (for generate-set-fns
					  (lambda (gen-set-b)
					    (let* ((set-a	(gen-set-a size))
						   (set-b	(gen-set-b size)))
					      (fn size set-a fence-a set-a fence-a)
					      (fn size set-a fence-a set-b fence-b)))))))))))))))




(define (toss?) (zero? (logand (random32) 1)))

(define (roll n) (modulo (random32) n))

(define (for list fn) (for-each fn list))

