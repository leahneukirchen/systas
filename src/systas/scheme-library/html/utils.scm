 ;;; utils.scm:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2002 Tom Lord
;;;
;;; See the file "COPYING" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (html utils)
  :use-module (standard string-parsing))



(define-public (html-quote s)
  (and s
       (let* ((i (apply join-fields-with
			"&amp;"
			(separate-fields-discarding-char #\& s list)))
	      (j (apply join-fields-with
			"&lt;"
			(separate-fields-discarding-char #\< i list)))
	      (k (apply join-fields-with
			"&gt;"
			(separate-fields-discarding-char #\> j list)))
	      (l (apply join-fields-with
			"&quot;"
			(separate-fields-discarding-char #\" j list))))
	 l)))




;;; tag: Tom Lord Sun Apr 21 15:20:38 2002 (utils.scm)
;;;
