;;; report.scm - collect text for a multi-part report.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1998 UUNET Technologies, Inc.
;;;
;;; See the file "=copyright-conditions" for further information
;;; about the copyright status of this work.
;;;



(define-module (formatting report)
  :use-module (unix file-utils))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Multi-Part Report Generation
;;;
;;; A multi-part report has an arbitrary number of sections, 
;;; each named by a unique value.  Each section is built from a 
;;; list of read-only strings which are appended to form the complete
;;; text of a section.
;;;
;;; Section names are compared using `eq?'.
;;;
;;; Text may be appended to any section at any time using
;;; `report-add-message' or `with-output-to-report'.  The accumulated
;;; text for any section can then be extracted with
;;; `report-section-text'.
;;;


;; report-type
;;
;; A record type for multi-part reports.
;;
(define-public report-type (make-record-type 'report
					     '(sections
					       messages)))

(define construct-report (record-constructor report-type))


;; make-report sections
;;
;; Construct a new, initially empty report with sections
;; `sections' (a list of section names).  Section names
;; are compared using `eq?'.
;;
(define-public (make-report sections)
  (construct-report sections (make-hash-table)))


;; report-sections report
;;
;; Return the list of section names in a report.
;;
(define-public report-sections (record-accessor report-type 'sections))


;; report-messages report
;;
;; Return a hash-table mapping section names to reverse-order lists
;; of strings containing the text of each section.
;;
(define report-messages (record-accessor report-type 'messages))


;; report-add-message report section msg
;;
;; Append `msg' (a read-only string) to the indicated `section'
;; in `report'.
;;
(define-public (report-add-message report section msg)
  (hashq-set! (report-messages report)
	      section
	      (cons msg (hashq-ref (report-messages report) section))))


;; with-output-to-report report section thunk
;;
;; Evaluate `thunk' with the current output port set to a port which
;; builds a string from output.  When `thunk' returns, append that 
;; string to the indicated `section' of `report'.
;;
(define-public (with-output-to-report report section thunk)
  (report-add-message report section (with-output-to-string thunk)))


;; report-section-text report section
;;
;; Return a string containing all text accumulated in the 
;; indicated `section' of `report'.
;;
(define-public (report-section-text report section)
  (let ((parts (hashq-ref (report-messages report) section)))
    (or (and parts (apply string-append (reverse parts)))
	"")))

