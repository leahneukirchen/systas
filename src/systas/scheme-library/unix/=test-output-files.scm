;;; tag: Tom Lord Tue Dec  4 14:59:29 2001 (unix/=test-output-files.scm)
;;;

(define-module (unix +test-output-files)
  :use-module (unix output-files)
  :use-module (unix file-utils)
  :use-module (unix filenames)
  :use-module (test test-macros))


(test (begin (with-new-output-file ",test"
	       (lambda () (write "hello-world")))
	     (%% %chmod ",test" (logior #o400 (logand #o777 (lognot (kw-arg-ref (%% %stat ",test") :permission-bits)))))
	     (create-numbered-backup-file ",test")
	     (create-numbered-backup-file ",test")
	     (create-numbered-backup-file ",test")
	     (let* ((same-contents	(string=? (file->string ",test")
						  (file->string ",test.~1~")
						  (file->string ",test.~2~")
						  (file->string ",test.~3~")))
		    (same-perms		(= (kw-arg-ref (%% %stat ",test") :permission-bits)
					   (kw-arg-ref (%% %stat ",test") :permission-bits)
					   (kw-arg-ref (%% %stat ",test") :permission-bits)))
		    (test-result		(and same-contents same-perms)))

	       (if test-result
		   (begin (%% %unlink ",test")
			  (%% %unlink ",test.~1~")
			  (%% %unlink ",test.~2~")
			  (%% %unlink ",test.~3~")))

	       test-result)))




(test (begin (with-versioning-output-file ",test" (lambda () (display "one")))
	     (with-versioning-output-file ",test" (lambda () (display "two")))
	     (with-versioning-output-file ",test" (lambda () (display "three")))
	     (let ((test-result		(and (string=? (file->string ",test") "three")
					     (string=? (file->string ",test.~1~") "one")
					     (string=? (file->string ",test.~2~") "two"))))
	       (if test-result
		   (begin (%% %unlink ",test")
			  (%% %unlink ",test.~1~")
			  (%% %unlink ",test.~2~")))
	       test-result)))

