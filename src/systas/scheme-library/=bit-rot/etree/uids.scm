;;; tag: Tom Lord Tue Dec  4 14:59:34 2001 (etree/uids.scm)
;;;
;;; uids.scm - 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (etree uids))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File Tags
;;; 
;;; Within the first 1024 bytes of a file, if a line
;;; begins with non-alphanumeric characters followed by:
;;; 
;;; 	uid: xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
;;; 
;;; where each `x' is a lowercase hexadecimal digit, that is
;;; the file's unique id.
;;; 


(define-public (files-uid flist)
  (map (lambda (f) (cons (file-uid f) f)) flist))


(define-public (file-uid file)
  (let* ((fd		(%% %open file 'O_RDONLY))
	 (first-1k	(make-string 1024 #\space)))

    (%% %read-retry fd first-1k)
    (uid-parser first-1k)))


(define uid-parser
  (structured-regexp->procedure `(^ ([^] ,char-set:alphabetic)
				    "uid: "
				    (= :uid (+ ([] ,char-set:digit "abcdef"))))
				:pick-spec '(@ :uid)))

