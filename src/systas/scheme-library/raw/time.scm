;;; time.scm - quick hack for the net modules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 2001 Thomas Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (raw time))



(define-public short-month-names	#("Jan"
					  "Feb"
					  "Mar"
					  "Apr"
					  "May"
					  "Jun"
					  "Jul"
					  "Aug"
					  "Sep"
					  "Oct"
					  "Nov"
					  "Dec"))


(define-public full-month-names		#("January"
					  "February"
					  "March"
					  "April"
					  "May"
					  "June"
					  "July"
					  "August"
					  "September"
					  "October"
					  "November"
					  "December"))


(define-public short-day-names		#("Sun"
					  "Mon"
					  "Tue"
					  "Wed"
					  "Thu"
					  "Fri"
					  "Sat"))

(define-public full-day-names		#("Sunday"
					  "Monday"
					  "Tuesday"
					  "Wednesday"
					  "Thursday"
					  "Friday"
					  "Saturday"))


(define-public (full-year-name y) (+ 1900 y))
(define-public (short-year-name y) (- y 100))
(define-public (short-month-name month) (vector-ref short-month-names month))
(define-public (full-month-name month) (vector-ref full-month-names month))
(define-public (short-day-name day) (vector-ref short-day-names day))
(define-public (full-day-name day) (vector-ref full-day-names day))
(define-public (two-digits n) (let ((n (number->string n)))
				(if (= 1 (string-length n))
				    (string-append "0" n)
				    n)))
