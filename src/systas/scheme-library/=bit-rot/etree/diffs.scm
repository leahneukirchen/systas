;;; tag: Tom Lord Tue Dec  4 14:59:35 2001 (etree/diffs.scm)
;;;
;;; diffs.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (etree diffs)
  :use-module (standard let-values)
  :use-module (unix output-files)
  :use-module (unix filenames)
  :use-module (unix shell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Directory Structure Diffs
;;; 

(define-public (make-diff-tree output-dir source-a source-b)
  (let ((a-list		(source-files-list source-a))
	(b-list		(source-files-list source-b)))

    (let*-values (((a-dirs a-files) 				(directories-and-files a-list))
		  ((b-dirs b-files) 				(directories-and-files b-list))
		  ((dirs-in-a-only dirs-in-b-only)		(directory-structure-diffs dirs-a dirs-b))
		  ((same-loc diff-loc in-a-only in-b-only)	(file-structure-diffs fuids-a fuids-b)))

      (with-new-output-file (form-filename output-dir "=README")
	(lambda ()
	  (display* "\nPlease write a good description of this patch.\n")))

      (with-new-output-file (form-filename output-dir "=new-directories")
	(lambda ()
	  (for-each (lambda (f) (display* f "\n")) dirs-in-b-only)))

      (with-new-output-file (form-filename output-dir "=removed-directories")
	(lambda ()
	  (for-each (lambda (f) (display* f "\n")) dirs-in-a-only)))

      (with-new-output-file (form-filename "=removed-files")
	(lambda ()
	  (for-each (lambda (f) (display* f "\n")) in-a-only)))

      (with-new-output-file (form-filename "=new-files")
	(lambda ()
	  (for-each (lambda (f) (display* f "\n")) in-b-only)))

      (with-new-output-file (form-filename "=renamed-files")
	(lambda ()
	  (for-each (lambda (fp) (display* (car fp) "\t" (cadr fp) "\n")) diff-loc)))

      (%% %mkdir (form-filename output-dir "=diffs"))
      (%% %mkdir (form-filename output-dir "=removed-files"))

      (checking-processes
       (lambda ()
	 (for-each (lambda (new-file)
		     (make-directories-to (form-filename output-dir "=diffs" (filename-directory new-file)))
		     (run-process `(cp ,(form-filename source-b new-file)
					    ,(form-filename output-dir "=diffs" new-file))))
		   in-b-only)

	 (for-each (lambda (removed-file)
		     (make-directories-to (form-filename output-dir "=removed-files" (filename-directory removed--file)))
		     (run-process `(cp ,(form-filename source-a removed-file)
					    ,(form-filename output-dir "=removed-files" removed-file))))
		   in-a-only)

	 (for-each (lambda (preserved-file)
		     (make-directories-to (form-filename output-dir "=diffs" (filename-directory preserved-file)))
		     (run-process `(diff -c
					 ,(form-filename source-a preserved-file)
					 ,(form-filename source-b preserved-file))
				  `(> ,(form-filename output-dir "=diffs" preserved-file))))
		   same-loc)

	 (for-each (lambda (moved-file)
		     (let ((a-name 	(car moved-file))
			   (b-name	(cadr moved-file)))
		       (make-directories-to (form-filename output-dir "=diffs" (filename-directory b-name)))
		       (run-process `(diff -c
					   ,(form-filename source-a a-name)
					   ,(form-filename source-b b-name))
				  `(> ,(form-filename output-dir "=diffs" b-name)))))))))))
		  
      



(define (directory-structure-diffs dirs-1 dirs-2)
  (let* ((dirs-1	(sort dirs-1 string<?))
	 (dirs-2	(sort dirs-2 string<?))
	 (in-dir1-only	(ordered-lset-difference string=? dirs-1 dirs-2))
	 (in-dir2-only	(ordered-lset-difference string=? dirs-2 dirs-1)))
    (values in-dirs1-only in-dirs2-only)))


(define (file-structure-diffs fuids-1 fuids-2)
  (define (fuid=? a b) (string=? (car a) (car b)))

  (let* ((in-1-only	(map cdr (ordered-lset-difference fuid=? uids-1 uids-2)))
	 (in-2-only	(map cdr (ordered-lset-difference fuid=? uids-2 uids-1)))
	 (common	(map car (ordered-lset-intersection fuid=? uids-1 uids-2)))
	 (common-1	(map (lambda (uid) (assq-ref fuids-1 uid)) common))
	 (common-2	(map (lambda (uid) (assq-ref fuids-1 uid)) common))
	 (same-loc	(filter-map (lambda (a b)
				      (and (string=? (cdr a) (cdr b))
					   (cdr a)))
				    common-1
				    common-2))
	 (diff-loc	(filter-map (lambda (a b)
				      (and (not (string=? (cdr a) (cdr b)))
					   (list (cdr a) (cdr b))))
				    common-1
				    common-2)))
    (values same-loc diff-loc in-1-only in-2-only)))





