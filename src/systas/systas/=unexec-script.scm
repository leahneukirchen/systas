
;;; It's critical that this unexec script produce
;;; an interpreter which acts just like systas-raw,
;;; except that certain modules are pre-loaded.  
;;; That way, people can write scripts that work equally
;;; well with both systas-raw and systas.
;;; 

(set! load-verbosely #t)
(resolve-module '(standard define-record-type))
(resolve-module '(standard let-values))
(resolve-module '(standard list-lib))
(resolve-module '(standard list-sorting))
(resolve-module '(standard list-regexps))
(resolve-module '(standard string-lib))
(resolve-module '(standard string-parsing))
(resolve-module '(unix directories))
(resolve-module '(unix file-utils))
(resolve-module '(unix filenames))
(resolve-module '(unix options))
(resolve-module '(unix output-files))
(resolve-module '(unix shell))
(resolve-module '(unix users))
(resolve-module '(html utils))
(resolve-module '(http uri))

(gc)
(gc)
(pk 'gc-stats (gc-stats))

(%unlink "systas")
(request-unexec "systas")

;;; tag: Tom Lord Mon May 13 02:17:01 2002 (systas/=unexec-script.scm)
;;;
