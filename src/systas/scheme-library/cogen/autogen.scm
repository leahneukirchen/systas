;;; tag: Tom Lord Tue Dec  4 14:59:30 2001 (cogen/autogen.scm)
;;;
;;; autogen.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (cogen autogen)
  :use-module (unix output-files))



(define-public (generated-files alist)
  (map car alist))

(define-public (generate-files cogen-prog alist)
  (for-each (lambda (x) (autogen-c-file cogen-prog (car x) (cadr x))) alist))

(define-public (display-generated-files alist)
  (for-each (lambda (x) (display* x #\nl)) (generated-files alist)))


;; autogen-c-file file fn
;;
;; With output directed to "file", print a message and then invoke "fn".
;; The message says "This file is generated automatically ..."
;;
(define-public (autogen-c-file cogen-prog file fn)
  (with-overwriting-output-file file
    (lambda ()
      (display* "/* This file is generated automatically by " cogen-prog ".\n")
      (display " * Do not edit it.\n")
      (display " */\n\n")
      (fn))))

;; autogen-documentation-file file fn
;;
;; With output directed to "file", print a message and then invoke "fn".
;; The message says "This file is generated automatically ..."
;;
(define-public (autogen-documentation-file file fn)
  (with-overwriting-output-file file
    (lambda ()
      (display "(This file is generated automatically by \"variable-cogen\". Do not edit it.\n")
      (fn))))
