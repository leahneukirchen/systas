;;; users.scm - Utilities for accessing password file entries.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (unix users))



;; (getpw :optional user)
;; 
;; Return the password file entry for `user' (a user name or uid)
;; or #f if no such user exists.  If `user' is not specified or
;; is #f, return the entry for `(getuid)'.
;; 
(define-public (getpw :optional user)
  (cond
   ((read-only-string? user)	(getpwnam user))
   (#t				(getpwuid (or user (getuid))))))

;; (user->pwent :optional user)
;; 
;; Return the password file entry for `user' (a user name or uid) or
;; signal an error if no such user exists.  If `user' is not specified
;; or is #f, return the entry for `(getuid)'.
;; 
(define-public (user->pwent :optional user)
  (or (getpw user)
      (throw 'no-such-user user)))


;; (username :optional user)
;; 
;; Return the user name for `user' (a user name or uid) or signal an
;; error if no such user exists.  If `user' is not specified or is #f,
;; return the entry for `(getuid)'.
;; 
(define-public (username :optional user)
  (kw-arg-ref (user->pwent user) :name))


;; (home-directory :optional user)
;; 
;; Return the home directory for `user' (a user name or uid) or signal
;; an error if no such user exists.  If `user' is not specified or is
;; #f, return the entry for `(getuid)'.
;; 
(define-public (home-directory :optional user)
  (kw-arg-ref (user->pwent user) :dir))
