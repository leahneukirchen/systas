;;; user.scm - A nice interactive read-eval-print loop environment.
;;;	       The module system is defined in here.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 	Copyright (C) 1997 Tom Lord
;;; 	Copyright (C) 1997 Free Software Foundation
;;; 
;;; This software is publicly licensed under the terms of
;;; the GNU General Public License, version 2, which is included
;;; with the source code.
;;; 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low Level Modules
;;;
;;; These are the low level data structures for modules (top-level
;;; name spaces).
;;;

;; %print-module module port depth line-length style print-table
;;
;; This is how modules are printed.
;;
(define (%print-module mod port writing?)
  (display "#<" port)
  (display (or (module-kind mod) "module") port)
  (let ((name (module-name mod)))
    (if name
	(begin
	  (display " " port)
	  (display name port))))
  (display " " port)
  (display (number->string (object-address mod) 16) port)
  (display ">" port))


;; module-type
;;
;; The record type descriptor of module objects.
;;
;;	hash-table	- a hash table in which local symbols are interned
;;
;;	uses		- a list of other modules from which bindings can be
;;		  	  inherited
;;
;;	lazy-binder	- a procedure that can supply bindings on-demand.
;;			  Called:
;;
;;				 (lazy-binding-proc module symbol defining?)
;;					=> variable | #f
;;
;;				module	- the module for which a binding
;;					  is required
;;				symbol	- the name for which a binding
;;					  is required
;;				defining? - #t if the symbol will
;;					  be rebound, #f if it must
;;					  have an existing binding
;;	map-proc	- a procedure that can map over lazilly supplied
;;			  bindings.  Called:
;;
;;				(map-proc procedure module)
;;
;;				For each binding which can be supplied
;;				for `module', `map-proc' should call:
;;
;;					(procedure name variable)
;;
;;	eval-procedure	- a procedure which looks up bindings in
;;			  this module.  Called:
;;
;;				(eval-procedure name defining?)
;;					 => variable | #f
;;
;;	name		- a symbolic name for the module (a list of symbols)
;;
;;	kind		- a symbolic name for type name for the module
;;			  (used in printing, usually "module" or "interface")
;;
;;	n-bindings	- number of hash table entries
;;
(define module-type
  (make-record-type 'module '(hash-table
			      uses
			      binder
			      map-proc
			      eval-procedure
			      name
			      kind
			      n-bindings)
		    %print-module))

(define module-constructor (record-constructor module-type))
(define module-hash-table  (record-accessor module-type 'hash-table))
(define set-module-hash-table! (record-modifier module-type 'hash-table))
(define module-uses  (record-accessor module-type 'uses))
(define set-module-uses! (record-modifier module-type 'uses))
(define module-binder (record-accessor module-type 'binder))
(define set-module-binder! (record-modifier module-type 'binder))
(define module-map-proc (record-accessor module-type 'map-proc))
(define set-module-map-proc! (record-modifier module-type 'map-proc))
(define module-eval-procedure (record-accessor module-type 'eval-procedure))
(define set-module-eval-procedure! (record-modifier module-type 'eval-procedure))
(define module-name (record-accessor module-type 'name))
(define set-module-name! (record-modifier module-type 'name))
(define module-kind (record-accessor module-type 'kind))
(define set-module-kind! (record-modifier module-type 'kind))
(define module-n-bindings (record-accessor module-type 'n-bindings))
(define set-module-n-bindings! (record-modifier module-type 'n-bindings))
(define module? (record-predicate module-type))

;; see also module-eval
;;
(define (eval-in-module exp module)
  (eval2 exp (module-eval-procedure module)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Module Namespace
;;;

(define the-modules (make-hash-table 61))

(define (module-key module)
  (and (module-name module)
       (cons (module-kind module) (module-name module))))

(define (name->module name) (hash-ref the-modules (cons 'module name) #f))

(define (name-module! module name)
  (let ((old-key (module-key module))
	(new-key (cons (module-kind module) name)))
    (if (and old-key (eq? module (hash-ref the-modules old-key)))
	(hash-remove! the-modules old-key))
    (set-module-name! module name)
    (and name (hash-set! the-modules new-key module))
    module))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-module
;;;

(define (make-module . args)
  (let* ((size 		(or (kw-arg-ref args :size) 31))
	 (uses 		(kw-arg-ref args :uses list))
	 (binder 	(kw-arg-ref args :binder))
	 (map-proc 	(kw-arg-ref args :map-proc))
	 (name 		(kw-arg-ref args :name))
	 (kind 		(kw-arg-ref args :kind)))
    (letrec ((answer (module-constructor (make-vector size '())
					 uses
					 binder
					 map-proc
					 (lambda (symbol define?)
					    (if define?
						(module-make-local-var! answer symbol)
						(module-variable answer symbol)))
					 name
					 kind
					 0)))
      (and kind (name-module! answer name))
      answer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module Hash Table Access
;;;

;; module-hash-table-get-handle module key
;;
;; Return the hash-bucket handle (a cons pair in a assoc list)
;; for `key' in `module'.
;;
;; If `key' is a symbol, `hashq-get-handle' is used.  Otherwise,
;; `hash-get-handle' is used.
;;
(define (module-hash-table-get-handle module key)
  (let ((ob (module-hash-table module)))
    ((if (symbol? key) hashq-get-handle hash-get-handle) ob key)))


;; module-hash-table-ref module key
;;
;; Return the hash-table binding for `key' in `module'.
;;
;; If `key' is a symbol, `hashq-ref' is used.  Otherwise,
;; `hash-ref' is used.
;;
(define (module-hash-table-ref module key)
  (let ((ob (module-hash-table module)))
    ((if (symbol? key) hashq-ref hash-ref) ob key)))


;; module-hash-table-set! module key value
;;
;; Set the hash-table binding for `key' in `hash-table'.
;;
;; If `key' is a symbol, `hashq-set!' is used.  Otherwise,
;; `hash-set!' is used.
;;
(define (module-hash-table-set! module key val)
  (let* ((new (cons () ()))
	 (ob (module-hash-table module))
	 (handle ((if (symbol? key) hashq-create-handle! hash-create-handle!) ob key new)))
    (if (eq? new (cdr handle))
	(begin
	  (set-cdr! handle val)
	  (set-module-n-bindings! module (1+ (module-n-bindings module)))
	  (if (> (module-n-bindings module) (quotient (vector-length ob) 2))
	      (let ((new-ob (make-vector (* 2 (vector-length ob)) '())))
		(vector-for-each (lambda (bucket)
				   (for-each (lambda (handle)
					       ((if (symbol? (car handle)) hashq-set! hash-set!) new-ob (car handle) (cdr handle)))
					     bucket))
				 ob)
		(set-module-hash-table! module new-ob)
		))))
    val))


;; module-hash-table-remove! module key
;;
;; Remove the hash-table binding for `key' in `module'.
;;
;; If `key' is a symbol, `hashq-remove!' is used.  Otherwise,
;; `hash-remove!' is used.
;;
(define (module-hash-table-remove! module key)
  (let ((ob (module-hash-table module)))
    ((if (symbol? key) hashq-remove! hash-remove!) ob key)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module Searching in General
;;;
;;; We sometimes want to look for properties of a symbol
;;; just within the hash-table of one module.  If the property
;;; holds, then it is said to hold ``locally'' as in, ``The symbol
;;; `display' is locally rebound in the module `safe-systas'.''
;;;
;;; Other times, we want to test for a symbol property in the hash-table
;;; of M and, if it is not found there, try each of the modules in the
;;; uses list of M.  This is the normal way of testing for some
;;; property, so we state these properties without qualification as
;;; in: "The symbol `fnord' is interned in module M because it is
;;; interned locally in module M2 which is a member of the uses list
;;; of M."
;;;
;;;

;; module-search fn m
;; 
;; return the first non-#f result of `fn' applied to `m' and then to
;; the modules in the uses list of `m', and so on recursively.  If all
;; applications return #f, then so does this function.
;;
(define (module-search fn m v)
  (define (loop pos)
    (and (pair? pos)
	 (or (module-search fn (car pos) v)
	     (loop (cdr pos)))))
  (or (fn m v)
      (loop (module-uses m))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modules, Symbols, and Variables
;;;
;;; A symbol may be:
;;;
;;;	locally-interned?	- if it is a key in the hash table of a 
;;;				  module, regardless of what the key maps to
;;;	interned?		- locally-interned in a module or in
;;;				  one of the modules in its uses list
;;;
;;; A symbol and module may correspond to a:
;;;
;;;	local-variable		- a variable (bound or not) in the module
;;;				  itself
;;;	variable		- a local-variable in the module or in
;;;				  one of the modules in its uses list
;;;
;;; A symbol may be:
;;;
;;;	locally-bound?		- if it corresponds to a bound local variable
;;;	bound?			- if it is locally bound in the module or in
;;;				  one of the modules in its uses list
;;;

;; module-symbol-locally-interned? module symbol
;; 
;; is symbol `interned' (not neccessarily defined) locally in `module'
;; or its uses list?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-locally-interned? m v)
  (->bool (module-hash-table-get-handle m v)))


;; module-symbol-interned? module symbol
;; 
;; is `symbol' interned (not neccessarily defined) anywhere in `module'
;; or its uses list?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-interned? m v)
  (module-search module-symbol-locally-interned? m v))


;; module-local-variable module symbol
;;
;; return the local variable associated with a `module' and `symbol'.
;;
;; If symbol is defined in `module', and if the definition binds symbol
;; to a variable, return that variable object.
;;
;; If the symbols is not found at first, but the module has a lazy binder,
;; then try the binder.
;;
;; If the symbol is not found at all, return #f.
;;
(define (module-local-variable m v)
  (let looked-up ((b (module-hash-table-ref m v)))
    (or (and (variable? b) b)
	(and (module-binder m) (let ((b ((module-binder m) m v #f)))
				 (and b (looked-up b)))))))

;; module-variable module symbol
;; 
;; like module-local-variable, except search the uses list in the 
;; case that `symbol' does not name a variable local to `module'.
;;
(define (module-variable m v)
  (module-search module-local-variable m v))


;; module-locally-bound? module symbol
;;
;; Is `symbol' bound (interned and defined) locally in `module'?
;;
(define (module-locally-bound? m v)
  (let ((var (module-local-variable m v)))
    (and var (variable-bound? var))))


;; module-bound? module symbol
;;
;; Is `symbol' bound (interned and defined) anywhere in `module'
;; or its uses list?
;;
(define (module-bound? m v)
  (module-search module-locally-bound? m v))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding Variables to Modules
;;;

;; module-add! module symbol variable
;; 
;; Map `symbol' to `variable' in `module'.
;;
(define (module-add! m v var) (module-hash-table-set! m v var))


;; module-remove! module symbol
;; 
;; Remove the local variable named `symbol' in `module'.
;;
(define (module-remove! m v) (module-hash-table-remove! m v))


;; module-make-local-var! module symbol
;; 
;; ensure a variable named by `symbol' in the local namespace of `module'
;; If no variable already exists, then create a new and uninitialzied
;; variable.
;;
(define (module-make-local-var! m v)
  (or (let ((b (module-hash-table-ref m v)))
	(and (variable? b) b))
      (and (module-binder m)
	   ((module-binder m) m v #t))
      (begin
	(let ((answer (make-undefined-variable v)))
	  (module-add! m v answer)
	  answer))))


;; module-clear! module
;;
;; Remove all local definitions from `module'
;;
(define (module-clear! m)
  (vector-fill! (module-hash-table m) '()))


;; module-for-each procedure module
;; 
;; Call `procedure' once for each symbol defined locally in `module'.
;;
;; Procedure is called:
;;
;;		(procedure symbol variable)
;;
(define (module-for-each proc module)
  (let ((hash-table (module-hash-table module)))
    (do ((index 0 (+ index 1))
	 (end (vector-length hash-table)))
	((= index end))
      (for-each
       (lambda (bucket)
	 (proc (car bucket) (cdr bucket)))
       (vector-ref hash-table index))))
  (if (module-map-proc module)
      ((module-map-proc module) proc module)))


;; module-map procedure module
;;
;; Call `procedure' once for each symbol defined locally in `module'.
;; Return a list of the values returned from `procedure'.
;;
;; Procedure is called:
;;
;;		(procedure symbol variable)
;;
(define (module-map proc module)
  (let* ((hash-table (module-hash-table module))
	 (end (vector-length hash-table))
	 (mapped
	  (let loop ((i 0)
		     (answer #f))
	    (if (= i end)
		answer
		(loop (+ 1 i)
		      (append!
		       (map (lambda (bucket)
			      (proc (car bucket) (cdr bucket)))
			    (vector-ref hash-table i))
		       answer)))))
	 (from-proc
	  (if (module-map-proc module)
	      ((module-map-proc module) proc module))))
    (append! mapped from-proc)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; module-ref/module-set
;;;

;; module-ref module name ?default?
;;
;; Returns the value of a variable called `name' in `module' or any of its
;; used modules.  If there is no such variable, then if the optional third
;; argument `default' is present, it is returned; otherwise an error is signaled.
;; 
(define (module-ref module name . default)
  (let ((variable (module-variable module name)))
    (if (and variable (variable-bound? variable))
	(variable-ref variable)
	(if (null? default)
	    (error "No variable named" name 'in module)
	    (car default)))))

;; module-set! module name value
;;
;; Sets the variable called `name' in `module' (or in a module that
;; module' uses) to `value'; if there is no such variable, an error 
;; is signaled.
;; 
(define (module-set! module name value)
  (let ((variable (module-variable module name)))
    (if variable
	(variable-set! variable value)
	(error "No variable named" name 'in module))))

;; module-define! module name value
;;
;; Sets the variable called `name' in `module' to `value'; if there 
;; is no such variable, it is added first.
;; 
(define (module-define! module name value)
  (let ((variable (module-local-variable module name)))
    (if variable
	(variable-set! variable value)
	(module-add! module name (make-variable value name)))
    value))


;; module-define-public! module name value
;;
;; Sets the variable called `name' in `module' to `value'; if there 
;; is no such variable, it is added first.  Make the variable
;; exported.
;; 
(define (module-define-public! module name value)
  (let ((variable	(or (module-local-variable module name)
			    (let ((v	(make-variable value name)))
			      (module-add! module name v)
			      v)))
	(public		(module-public-interface module)))
    (variable-set! variable value)
    (module-add! public name variable)
    value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; module-use / module-unuse
;;;

;; module-use! module interface-module
;;
;; Add `interface' to the front of the uses list of `module'.
;; 
(define (module-use! module interface)
  (set-module-uses! module (cons interface (delq! interface (module-uses module)))))

;; module-unuse! module interface-module
;;
;; Remove `interface-module' from the uses list of `module'.
;; 
(define (module-unuse! module interface)
  (set-module-uses! module (delq! interface (module-uses module))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module Public Interfaces
;;; 
;;; A top-level module is associated with a second module 
;;; called its public interface.
;;;

;; module-public-interface module
;;
;; Return the public interface module of `module'.
;;
(define (module-public-interface m) (module-ref m 'local-public-interface #f))

;; module-public-interface module interface-module
;;
;; Set the public interface module of `module'.
;;
(define (set-module-public-interface! m i) (module-define! m 'local-public-interface i))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Current Module
;;;

;; set-current-module module
;;
;; Set the current top-level namespace.
;;
;; If `module' is #f, then switch to the built-in top-level.
;;
;; Return `module'.
;;
(define (set-current-module m)
  (set! the-module m)
  (if m
      (set! *top-level-lookup-thunk* (module-eval-procedure the-module))
      (set! *top-level-lookup-thunk* #f))
  m)


;; current-module
;;
;; Return the current top-level module.
;;
(define (current-module) the-module)


;; the-module
;; 
;; The currently active top-level namespace or #f (if the built-in
;; top level is being used).  Do not set this variable directly.
;; Instead, use `set-current-module'.
;;
(define the-module #f)


;; module-excursion thunk
;;
;; Evaluate `thunk', preserving the current top-level module.
;; `thunk' may call `set-current-module', but the original
;; top-level is restored when thunk returns.
;;  
(define (module-excursion thunk)
  (let ((inner-module (current-module))
	(outer-module #f))
    (dynamic-wind (lambda ()
		    (set! outer-module (current-module))
		    (set-current-module inner-module)
		    (set! inner-module #f))
		  thunk
		  (lambda ()
		    (set! inner-module (current-module))
		    (set-current-module outer-module)
		    (set! outer-module #f)))))


(define (module-eval module expression)
  (module-excursion
   (lambda ()
     (set-current-module module)
     (eval expression))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module-based Loading
;;;
;;; These functions are for loading source files without changing
;;; the current top-level module.
;;;
;;; These definitions replace several file loading functions
;;; defined (for module-less operation) in "basic.scm"
;;;

;; !!! fix overloading of try-load*

(define basic-try-load-with-path try-load-with-path)
(define basic-try-load try-load)
(define basic-load-with-path load-with-path)
(define basic-load load)

(define (try-load-module-with-path . args)
  (module-excursion (lambda () (apply basic-try-load-with-path args))))

(define (try-load-module . args)
  (module-excursion (lambda () (apply basic-try-load args))))

(define (load-module-with-path . args)
  (module-excursion (lambda () (apply basic-load-with-path args))))

(define (load-module . args)
  (module-excursion (lambda () (apply basic-load args))))

(define try-load-with-path try-load-module-with-path)
(define try-load try-load-module)
(define load-with-path load-module-with-path)
(define load load-module)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module System Bootstrapping
;;;
;;; A single top-level namespace is built-in to the intepreter.
;;; All primitive procedures are defined in that namespace.
;;;
;;; These definitions build modules that import bindings from 
;;; the built-in top-level.
;;;

;; root-module-binder
;;
;; A root module uses the symhash table (the built-in top-level
;; hash-table).  Root modules share variables with the built-in 
;; top-level.
;;
(define (root-module-binder m s define?)
  ;; !!! needs a map procedure 
  ;;
  (let ((bi (and (symbol-interned? #f s)
		 (builtin-variable s))))
    (and bi
	 (or define? (variable-bound? bi))
	 (begin
	   (module-add! m s bi)
	   bi))))


;; scm-module-binder
;;
;; An scm module is a module into which the lazy binder copies
;; variable bindings from the system symhash table.  The mapping is
;; one way only; newly introduced bindings in an scm module are not
;; copied back into the system symhash table (and can be used to override
;; bindings from the symhash table).
;;
(define (scm-module-binder m s define?)
  ;; !!! needs a map procedure
  ;;
  (let ((bi (and (symbol-interned? #f s)
		 (builtin-variable s))))
    (and bi
	 (variable-bound? bi)
	 (begin
	   (module-add! m s bi)
	   bi))))


;; the-root-module 
;;
;; A module containing the built-in top-level.
;;
(define the-root-module (make-module :binder root-module-binder
				     :name '(the-root-module)
				     :kind 'module))
;; the-scm-module
;;
;; A module that copies definitions from the built-in top-level.
;;
(define the-scm-module (make-module :binder scm-module-binder
				    :name '(the-scm-module)
				    :kind 'interface-module))
(set-module-public-interface! the-root-module the-scm-module)
(set-current-module the-root-module)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autoloading Modules On-demand
;;;

;; resolve-module module-name
;;
;; Return the indicated module, auto-loading it or creating 
;; an empty module if necessary.
;;
(define (resolve-module name)
  (if (module? name)
      name
      (let ((already (name->module name)))
	(or already
	    (begin
	      (try-module-auto-load name)
	      (or (name->module name)
	      (make-module :name name
			   :kind 'module)))))))

;; resolve-module module-or-module-name
;;
;; Return the indicated module, auto-loading it if necessary and
;; signalling an error if it does not exist and can not be loaded.
;;
(define (existing-module name)
  (if (module? name)
      name
      (let ((already (name->module name)))
	(or already
	    (begin
	      (try-module-auto-load name)
	      (or (name->module name)
		  (error "no such module" name)))))))

;; resolve-interface module-or-module-name
;;
;; Return the interface module of the indicated module,
;; auto-loading or creating the indicated module if necessary.
;;
(define (resolve-interface name)
  (let ((module (resolve-module name)))
    (and module (module-public-interface module))))


;; in module-specification
;;
;; Make the indicated module the current top-level.
;;
;; `module-specification' may be a module or the full name of a module.
;; (See `resolve-module'.)
;;
(define (in module) (set-current-module (existing-module module)))

;; use module-specification
;;
;; Add the indicated module to the use-list of the current 
;; top-level module
;;
;; `module-specification' may be a module or the full name of a module.
;; (See `resolve-module'.)
;;
(define (use module)
  (module-use! the-module (module-public-interface (existing-module module)))
  #t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Module Auto-Loader
;;;


;; try-module-auto-load module-name
;;
;; Try to load code for the indicated module.
;; Return #t if a file was successfully loaded
;; (regardless of whether or not it created the
;; indicated module) and #f otherwise.
;;
;; `module-name' is a list of symbols.
;;
;; A relative file name is constructed from the module name.
;; For example, to autoload '(data-structures ratlist), the
;; system looks along the load path for a module named
;; "ratlist" in the files:
;;
;;		data-structures/ratlist
;;		data-structures/ratlist.scm
;;
;; To load '(piw cogen log-cogen), the system looks for the 
;; module "log-cogen" in:
;;
;;		piw/cogen/log-cogen
;;		piw/cogen/log-cogen.scm
;;
(define (try-module-auto-load module-name)
  (let ((path-hint (let loop ((name "")
			      (parts module-name))
		     (cond
		      ((not parts)			name)
		      ((string-null? name)		(loop (car parts) (cdr parts)))
		      (#t				(loop (in-vicinity name (car parts)) (cdr parts)))))))
    (and (not (auto-load-done-or-in-progress? module-name))
	 (let ((didit #f))
	   (dynamic-wind
	    (lambda () (auto-load-in-progress! module-name))
	    (lambda () 
	      (let loop ((dirs load-path))
		(and (not (null? dirs))
		     (or
		      (let ((d (car dirs))
			    (trys (cons path-hint
					(map (lambda (sfx) (string-append path-hint sfx)) scheme-suffixes))))
			(and (or-map (lambda (f)
				       (let ((full (in-vicinity d f)))
					 (and (not (file-is-directory? full))
					      (file-exists? full)
					      (begin
						(module-excursion
						 (lambda ()
						   (load-with-path f (list d))))
						#t))))
				     trys)
			     (begin
			       (set! didit #t)
			       #t)))
		      (loop (cdr dirs))))))
	    (lambda () (set-auto-loaded! module-name didit)))
	   didit))))


;; auto-loads-in-progress
;;
;; A list of the form 
;;
;;		(module-name ...)
;;
;; of modules currently being auto-loaded.
;;
(define auto-loads-in-progress '())

;; auto-loads-done
;;
;; A list of the form 
;;
;;		(module-name ...)
;;
;; of modules previously auto-loaded.
;;
(define auto-loads-done '())

(define (auto-load-done-or-in-progress? m)
  (->bool (or (member m auto-loads-done)
		(member m auto-loads-in-progress))))

(define (auto-load-done! m)
  (set! auto-loads-in-progress
	  (delete! m auto-loads-in-progress))
  (set! auto-loads-done (cons m (delete! m auto-loads-done))))
	

(define (auto-load-in-progress! m)
  (set! auto-loads-done (delete! m auto-loads-done))
  (set! auto-loads-in-progress (cons m (delete! m auto-loads-in-progress))))

(define (set-auto-loaded! m done?)
  (if done?
      (auto-load-done! m)
      (begin
	(set! auto-loads-done (delete! m auto-loads-done))
	(set! auto-loads-in-progress (delete! m auto-loads-in-progress)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Processing Module Specifications
;;; 
;;; 

;; process-define-module args
;;
;; Process the arguments to `define-module'.
;;
;; `args' must be of the form:
;;
;;	(hierarchical-module-name . parameters)
;;
;; Parameters is a possibly empty list of keyword value pairs.
;; Recognized keywords are:
;;
;;	:use-module hierarchical-module-name
;;		Add the named module to the use list of the module
;;		being defined.
;;
;; process-define-module creates (or auto-loads) the named module
;; if it doesn't already exist.  It ensures that the module has
;; a public interface and includes `the-scm-module' on its uses list.
;; It ammends the use-list of the module to include any modules named 
;; with the :use-module keyword.  Finally, it makes the named module 
;; the current module.
;;
(define (process-define-module args)
  (let*  ((module-id (car args))
	  (module (resolve-module module-id))
	  (kws (cdr args)))
    (if (memq :clean kws)
	(set! kws (delq :clean kws))
	(prepare-user-module! module))
    (let loop ((kws kws))
      (and (not (null? kws))
	   (case (car kws)
	     ((:use-module)		(if (not (pair? (cdr kws)))
					    (error "unrecognized defmodule argument" kws))
					(let* ((used-name (cadr kws))
					       (used-module (resolve-module used-name)))
					  (if (not (module-public-interface used-module))
					      (begin
						(warn "no code for module" (module-name used-module))
						(prepare-user-module! used-module)))
					  (let ((interface (module-public-interface used-module)))
					    (if (not interface)
						(error "missing interface for use-module" used-module))
					    (set-module-uses! module
							      (cons interface (delq! interface (module-uses module))))))
					(loop (cddr kws)))

	     (else	(error "unrecognized defmodule argument" kws)))))
    module))


;; prepare-user-module! module
;;
;; Prepare `module' for use as an ordinary top-level.  That means:
;;
;;	1. Ensuring that it has an associated public interface module.
;;	2. Ensuring that the-scm-module (interface to all the built-ins)
;;	   is in the uses list.
;;
(define (prepare-user-module! module)
  (if (not (module-public-interface module))
      (let ((interface (make-module :name (module-name module)
				    :kind 'interface-module)))
	(set-module-public-interface! module interface)))
  (if (not (memq the-scm-module (module-uses module)))
      (set-module-uses! module (append (module-uses module) (list the-scm-module)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Module System Macros
;;;

;; define-module hierarchical-module-name params	;; syntax
;;
;; The arguments are unevaluated.
;;
;; Parameters is a possibly empty list of keyword value pairs.
;; The only recognized keyword is:
;;
;;	:use-module hierarchical-module-name
;;		Add the named module to the use list of the module
;;		being defined.
;;
;; define-module creates (or auto-loads) the named module
;; if it doesn't already exist.  It ensures that the module has
;; a public interface and includes `the-scm-module' on its uses list.
;; It ammends the use-list of the module to include any modules named 
;; with the :use-module keyword.  Finally, it makes the named module 
;; the current module.
;;
(define-memoizing-macro (define-module . args)
  `(,set-current-module (,process-define-module ',args)))


;; define-private . args	;; syntax
;;
;; Define a variable that is local to the current module.
;;
;; `args' are as to the standard syntax `define'
;;
(define define-private define)


;; define-public . args	;; syntax
;;
;; Define a variable that is exported from the current module.
;; The variable is defined in the current module and in its
;; public interface.
;;
;; `args' are as to the standard syntax `define'
;;
(define-memoizing-macro (define-public . args)
  (define (syntax)
    (error "bad syntax" (list 'define-public args)))
  (define (defined-name n)
    (cond
     ((symbol? n)	n)
     ((pair? n)		(defined-name (car n)))
     (else 		(syntax))))
  (cond
   ((null? args)	(syntax))
   
   (#t 			(let ((name (defined-name (car args))))
			  `(,begin
			    (,let ((public-i (,module-public-interface (,current-module))))
				  ;; Make sure there is a local variable:
				  ;;
				  (,module-define! (,current-module)
						   ',name
						   (,module-ref (,current-module) ',name #f))
			       
				  ;; Make sure that local is exported:
				  ;;
				  (,module-add! public-i
						',name
						(,module-variable (,current-module) ',name)))
			       
			    ;; Now (re)define the var normally.
			    ;;
			    (,define-private ,@ args))))))


(define-memoizing-macro (define-public-memoizing-macro . args)
  (define (syntax)
    (error "bad syntax" (list 'define-public-memoizing-macro args)))
  (define (defined-name n)
    (cond
     ((symbol? n)	n)
     ((pair? n)		(defined-name (car n)))
     (else 		(syntax))))
  (cond
   ((null? args)	(syntax))

   (#t 			(let ((name (defined-name (car args))))
			  `(,begin
			     (,let ((public-i (,module-public-interface (,current-module))))
				   ;; Make sure there is a local variable:
				   ;;
				   (,module-define! (,current-module)
						    ',name
						    (,module-ref (,current-module) ',name #f))
				   
				   ;; Make sure that local is exported:
				   ;;
				   (,module-add! public-i ',name
						 (,module-variable (,current-module) ',name)))
			       
			     ;; Now (re)define the var normally.
			     ;;
			     (,define-memoizing-macro ,@ args))))))


(define define-public-syntax define-public)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Establish the Default Module
;;;

(define-module (the-root-module))



(define-public (application-arguments) (program-arguments))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Read-Eval-Print Loop (REPL)
;;;

;; scm-repl-silent
;;
;; If true, the repl reads and evaluates, but does not print.
;;
(define scm-repl-silent #f)

;; scm-repl-verbose
;;
;; If true, the repl prints statistics about evaluation.
;;
(define scm-repl-verbose #t)

;; scm-repl-prompt
;;
;; If true, the repl prints a prompt string before reading.
;;
(define scm-repl-prompt #t)

;; the-prompt-string
;;
;; The prompt string printed by the repl if scm-repl-prompt is not #f.
;;
(define (the-prompt-string) "systas> ")

;; scm-repl
;;
;; Repeated read and evaluate expressions and print
;; the results.  Print prompts and informative messages.
;; Handle exceptions and signals.
;;
;; This procedure should be invoked with signals masked.
;;
(define (scm-repl)
  (error-catching-repl scm-read scm-eval scm-print))

(define (scm-read)
  (force-output (current-error-port))
  (if scm-repl-prompt
      (begin
	(display (the-prompt-string))
	(force-output)))
  (let ((val (read (current-input-port) #f read-sharp)))
    (if (eof-object? val)
	(begin
	  (if scm-repl-verbose
	      (begin
		(newline)
		(display ";;; EOF -- quitting")
		(newline)))
	  (quit 0)))
    val))

(define (scm-eval source)
  (repl-report-start-timing)
  (eval source))

(define (scm-print result)
  (force-output (current-error-port))
  (if (not scm-repl-silent)
      (begin
	(write result #f :cycles1)
	(newline)
	(if scm-repl-verbose
	    (repl-report))
	(force-output))))



;; error-catching-repl read eval print
;;
;; Repeatedly:
;;
;;	(write (eval (read)))
;;
;; If an error occurs, reastart the loop.
;;
(define (error-catching-repl r e p)
  (error-catching-loop (lambda () (p (e (r))))))

;; error-catching-loop thunk
;;
;; This is the signal and error-handling logic of a
;; top-level repl loop.
;;
;; Repeatedly call `thunk', catching all exceptions.
;; Before calling `thunk', unmask all signals.  When
;; `thunk' returns, remask them.
;;
;; The exception 'quit causes `error-catching-loop' to
;; print a message and return.
;;
;; The exception 'abort causes `error-catching-loop' to 
;; print a message and restart the loop unless the global
;; variable `interactive-mode' is false, in which case,
;; the a message is printed and the process exitted with
;; non-0 status.
;;
;; All other exceptions are passed to `bad-throw' which
;; by default converts the exception to an exception of
;; type 'error.  By default, exceptions of type 'error
;; print a message and generate an exception of type 'abort.
;;
;; This procedure should be called with interrupts masked.
;; It evaluates `thunk' with interrupts enabled.
;;
(define interactive-mode #t)

(define (error-catching-loop thunk)

  (letrec ((with-quit-and-abort-handlers		(lambda (loop-thunk)
							  (let ((next	(catch 'quit (lambda ()
										       
										       (catch 'abort
											 (lambda ()
											   (dynamic-wind
											    (lambda () (unmask-interrupts))
											    (lambda () (loop-thunk))
											    (lambda () (mask-interrupts))))
											 
											 ;; What to do on 'abort
											 ;;
											 (lambda ign
											   (with-quit-and-abort-handlers (lambda ()
															   (catch #t
															     (lambda ()
															       (with-output-to-port (current-error-port)
																 (lambda ()
																   (force-output)
																   (display  ";;; ABORT\n;;;\n")
																   (force-output)
																   #t)))
															     (lambda ign #f))

															   (if (not interactive-mode)
															       ;; If we're not in interactive mode, 
															       ;; exit immediately.
															       ;;
															       (%exit 1)
															       
															       ;; Otherwise, resume the ordinary loop
															       ;;
															       loop-thunk))))))


									       ;; What to do on 'quit:
									       ;;
									       ;; Flush all output buffers and return from the error
									       ;; catching loop, presumably to exit the process.
									       ;;
									       ;;
									       (lambda (quit . args)
										 (catch #t
										   (lambda () (force-output))
										   (lambda ign #f))
										 #f))))

							    (and next (with-quit-and-abort-handlers next)))))

	   (the-loop					(lambda () (thunk) the-loop)))

    (with-quit-and-abort-handlers the-loop)))

;; quit . args
;;
;; Immediately exit the repl by throwing 'quit with the provided 
;; arguments.
;;
(define (quit . args)
  (apply throw 'quit args))

;; abort . args
;;
;; Immediately restart the repl by throwing 'abort with the provided 
;; arguments.
;;
(define (abort . args)
  (apply throw 'abort args))

;; gc-run-time
;;
;; Return the total amount of time taken in garbage collection in this
;; process.  The time is expressed in "internal time units".  The number
;; of time units in a second is `internal-time-units-per-second'.
;;
(define (gc-run-time)
  (cond
   ((assq-ref (gc-stats) 'gc-time-taken) => noop)
   (else				    0)))

;; start-gc-rt
;;
;; The total amount of time taken in garbage collection at the beginning
;; of the current eval phase of the repl loop.
;;
(define start-gc-rt #f)

;; start-rt
;;
;; The total amount of time taken by this process at the beginning
;; of the current eval phase of the repl loop.
;;
(define start-rt #f)


;; repl-report-start-timing
;;
;; Begin measuring time spent in evaluation and garbage collection.
;;
(define repl-report-start-timing (lambda ()
				   (set! start-gc-rt (gc-run-time))
				   (set! start-rt (get-internal-run-time))))

;; repl-report
;;
;; Print statistics about the amount of time spent in evaluation and
;; garbage collection.
;;
(define repl-report (lambda ()
		      (display ";;; ")
		      (display (inexact->exact
				(* 1000 (/ (- (get-internal-run-time) start-rt)
					   internal-time-units-per-second))))
		      (display "  msec  (")
		      (display  (inexact->exact
				 (* 1000 (/ (- (gc-run-time) start-gc-rt)
					    internal-time-units-per-second))))
		      (display " msec in gc)\n")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Start Procedure
;;;
;;; This is ordinarily the first function called by the interpreter
;;; after loading this initialization file.
;;;

(define inhibit-repl #f)

(define (main . ign) #f)

(define (start)
  (cond

   (inhibit-repl				#f)
   
   ((= 1 (length (program-arguments)))	(scm-repl))

   (#t						(exit-on-abort
						 (lambda ()
						   (let loop ((args (cdr (program-arguments))))
						     (and args
							  (let ((arg (car args)))
							    (cond
							     ((string=? arg "--eval")
							      (if (null? (cdr args))
								  (throw 'parameter-error "missing argument for --eval"))
							      (catch #t
								(lambda () (eval (read (cadr args))))
								(lambda exception (apply throw 'error exception)))
							      (loop (cddr args)))
							     (#t (set! application-arguments (lambda () args))
								 (let* ((saved-load-verbosely #f)
									(tmp #f))
								   (dynamic-wind
								    (lambda () (set! tmp load-verbosely
										     load-verbosely saved-load-verbosely
										     saved-load-verbosely tmp))
								    (lambda () (or (if (string-index arg #\/)
										       (try-load arg)
										       (or-map (lambda (d)
												 (try-load (in-vicinity d arg)))
											       load-path))
										   (throw 'parameter-error "could not load" arg)))
								    (lambda () (set! tmp load-verbosely
										     load-verbosely saved-load-verbosely
										     saved-load-verbosely tmp))))
								 (main (application-arguments)))))))
						   #f)))))

(define (exit-on-abort thunk)
  (dynamic-wind
   (lambda () (unmask-interrupts))
   (lambda ()
     (catch 'abort
       (lambda ()
	 (catch 'quit
	   thunk
	   (lambda ign #f)))
       (lambda (key . args)
	 (force-output)
	 (with-output-to-port (current-error-port)
	   (lambda ()
	     (display* ";;; ABORT")
	     (newline)))
	 (%exit 1))))
   (lambda () (mask-interrupts))))



