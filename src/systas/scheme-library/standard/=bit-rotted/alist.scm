;;; tag: Tom Lord Tue Dec  4 14:59:30 2001 (=bit-rotted/alist.scm)
;;;
;;; assoc.scm - 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (standard alist))



(define-public (assocx-ref compare? alist key)
  (cond
   ((not alist)				#f)
   ((compare? (caar alist) key)		(cdar alist))
   (#t					(assocx-ref compare? (cdr alist) key))))
