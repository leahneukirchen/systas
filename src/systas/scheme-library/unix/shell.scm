;;; shell.scm - Unix shell style programming (process and I/O mgt.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (C) 1999, 2001 Tom Lord
;;;
;;; See the file "=copyright-conditions" for further information about
;;; the copyright and warranty status of this work.
;;;



(define-module (unix shell)
  :use-module (unix temp-files)
  :use-module (unix file-utils)
  :use-module (standard define-record-type)
  :use-module (standard string-parsing)
  :use-module (standard list-lib)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Systas SCSH
;;; 
;;; This design and many of the interfaces are lifted from
;;; SCSH, by Olin Shivers.  The implementation is original.
;;; 
;;; There are no macros.  For example, the SCSH macro `exec-epf',
;;; which expands SCSH process notation at "compile time" is replaced
;;; by `invoke' which interprets a syntactically similar list-based
;;; process notation at "run time".
;;; 
;;; Unlike SCSH, ports do not have a reveal count.  Instead, they have
;;; a boolean flag (the `autoclose' flag) that controls whether or not
;;; ports are closed by procedures like `run'.  That flag is set #f by
;;; `move->fdes' (the procedure that performs redirections) and #t by
;;; `integer->file-descriptor'.  It can be set explicitly by
;;; `set-autoclose-file-descriptor!'.  The autoclose flag and the
;;; kernel's close-on-exec flag are independent and unrelated.
;;; 
;;; The `(unix shell)' module provides a flag, `check-processes-mode',
;;; which is similar to the "/bin/sh" operation `set -e'.  When true,
;;; procedures like `run' check the exit status of their subprocesses
;;; and throw an exception in the event of an abnormal subprocess
;;; exit.
;;; 
;;; Job-controlled complex pipelines are supported (`run-job' and
;;; `background-job') by constructing process groups from process
;;; forms.  All of the processes in a group (except any created by a
;;; procedure inside the process form) are immediate subprocesses of
;;; the calling process so the status of an entire job can be
;;; monitored accurately.
;;; 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process Objects
;;; 
;;; Every subprocess created by a procedure in this module has an
;;; associated subprocess object.  When the process exits, that object
;;; caches the exit status so that the procedure `wait' (in this
;;; module, not the system call `%wait') can retrieve that exit status
;;; more than once.  This makes it safe to reap zombies automatically
;;; from an interrupt handler without regard to whether higher-level
;;; code will attempt to reap the same zombie.
;;; 

;; process
;; 
;; A record type for representing subprocesses.
;; 
;; Each record contains a pid and cached exit status.  In addition, a
;; process form is recorded for debugging (to make it easier to
;; identify how the job was created).
;; 
(define-record-type process
  (make-process pid)
  process?
  (pid			process:pid		set-process:pid!)
  (exit-status		process:exit-status	set-process:exit-status!)
  (process-form		process:process-form	set-process:process-form!))


;; (process? obj)
;; 
;; #t if `obj' represents a subprocess, #f otherwise.
;; 
(define-public process? process?)


;; (process:pid subproc)
;; 
;; Return the process id of subprocess `subproc'.
;; 
(define-public process:pid process:pid)


;; (process:exit-status subproc)
;; 
;; Return the cached exit status of subprocess `subproc'.
;; Usually you will want to use `wait' to obtain an up-to-date
;; process status.
;; 
;; (define process:exit-status process:exit-status)


;; (live-process? process)
;; 
;; Return #t if the indicated subprocess has not yet exited,
;; considering only the cached process status.  (This procedure
;; does not call `wait'.
;; 
(define (live-process? process)
  (not (process:exit-status process)))


;; (reap-zombies)
;; 
;; Collect status from known subprocesses and delete dead process from
;; the internal process table.  When the `(unix shell)' module is
;; loaded, this procedure is installed as the handler for `SIGCHLD'.
;; 
(define-public (reap-zombies)
  (without-interrupts
   (lambda ()
     (let ((some-live?	#f))
       (for-each (lambda (pid)
		   (catch 'ECHILD
		     (lambda ()
		       (let* ((status 	(wait pid 'WNOHANG))
			      (state 	(and status (kw-arg-ref status :state))))
			 (if (or (not state)
				 (not (or (eq? state 'exited)
					  (eq? state 'signaled))))
			     (set! some-live? #t))))
		     (lambda ign
		       (forget-process pid))))
 		 process-list)
       some-live?))))


(set! sigchld (lambda ign (reap-zombies)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subprocess Tables
;;; 
;;; Upon `fork' or the discovery of a previously unknown process by
;;; `wait', `remember-process' creates a process object and adds it to
;;; the `process-table'.  As an optimization, its `pid' is added to
;;; the `process-list' so that the list of process numbers for all
;;; live process objects can be found quickly.
;;; 
;;; Upon the discovery that a process has exited or been killed by a
;;; signal, `forget-process' removes its process number from the
;;; `process-list' and its object from the `process-table'.
;;; 
;;; `pid->proc' converts an integer pid to a known process object for
;;; a process which has not yet exited or been killed by a signal.
;;;

;;; 
;; process-list
;; 
;; A list of process numbers of processes for which process for which
;; a process object has been created, but no zombie reaped.  Processes
;; are added to this list by `remember-process' and removed by
;; `forget-process'.
;; 
(define process-list ())


;; process-table
;; 
;; A hash table mapping process numbers to process objects for
;; processes for which a process object has been created, but no
;; zombie reaped.  Processes are added to this list by
;; `remember-process' and removed by `forget-process'.
;; 
(define process-table (make-hash-table 63))


;; (pid->proc pid)
;; 
;; Return the process object for `pid', if that pid is in the
;; `process-table'.  A process is in the `process-table' until
;; `wait' reaps its process zombie or `announce-process-reaped'
;; is passed its `pid' or `process'
;; 
(define-public (pid->proc pid)
  (without-interrupts
   (lambda ()
     (if (process? pid)
	 pid
	 (hashq-ref process-table pid)))))
  

;; (remember-process pid)
;; 
;; Create and record a process object.  Return that object.
;;
(define (remember-process pid)
  (without-interrupts
   (lambda ()
     (let ((object		(make-process pid)))
       (set! process-list (cons pid process-list))
       (hashq-set! process-table pid object)
       object))))


;; (forget-process pid)
;; 
;; Remove a process number from the `process-list' and
;; `process-table'.  This prevents the process status from being
;; checked by `reap-zombies' and prevents the process object from
;; being returned by `pid->proc'.
;; 
(define (forget-process proc)
  (without-interrupts
   (lambda ()
     (set! process-list (delq (process:pid proc) process-list))
     (hashq-remove! process-table (process:pid proc))
     (update-job-for-finished-process proc))))


;; (announce-process-reaped proc/pid)
;; 
;; Declare that proc/pid has exited or been killed.  As a 
;; side effect, the process object is discarded from the internal
;; process table.
;; 
;; Calling this procedure for a process that is not dead is an
;; undetectable error that can result in the existence of multiple
;; process objects for a single process.  That in turn can 
;; cause `wait' to return inaccurate results for the process
;; in question.
;; 
;; Programs that use only `wait' and `fork' from this module, and
;; avoid the direct system calls `%waitpid' and `%fork' (and their C
;; equivalents) never need to call this procedure.  Programs that
;; use the system calls must call this procedure if `%waitpid' 
;; detects the termination of a process previously passed to
;; `remember-process'.
;; 
(define-public (announce-process-reaped proc/pid)
  (let ((obj	(if (process? proc/pid)
		    proc/pid
		    (pid->proc proc/pid))))
    (and obj (forget-process obj))))


;; (clear-processes)
;; 
;; Empty the `process-list' and `process-table'.  This is called from
;; newly forked subprocesses because subprocesses of the parent
;; process are not subprocesses of the forked subprocess.
;; 
(define (clear-processes)
  (without-interrupts
   (lambda ()
     (set! process-list ())
     (set! process-table (make-weak-value-hash-table 63)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jobs
;;; 
;;; A job is a collection of subprocesses which are assigned their
;;; own process group.   This is the basis of job control since
;;; all of the processes in the group can be signaled at once, and
;;; since the group can be made either the foreground or background
;;; group with respect to the controlling terminal.  
;;; 
;;; Jobs are also the basis of reliable shell programming with complex
;;; pipelines of ordinary unix programs because the exit status of all
;;; processes in the pipeline can be monitored.  This is in contrast
;;; to a pipeline started by a procedure like `fork/pipe+' for which
;;; only the exit status of the last process in the pipeline can be
;;; collected.
;;; 

;; job-list
;; 
;; A list of jobs containing processes which have not yet been reaped.
;; 
(define job-list ())


;; job-table
;; 
;; A hash table mapping process objects to jobs for all process objects
;; which do not correspond to reaped processes.
;; 
(define job-table (make-hash-table))


;; finished-jobs-list
;; 
;; A list of jobs which have completed but not yet been reported by
;; `finished-jobs'.
;; 
(define finished-jobs-list ())


;; job
;; 
;; A set of subprocesses constituting a process group.  One of the
;; subprocesses is designated the `group-leader' (its process id is
;; the process group id).  A process form is recorded for a job for
;; debugging (to make it easier to identify how the job was created).
;; 
(define-record-type job
  (construct-job)
  job?
  ;; The group leader is the first element of the process list.
  ;;
  (process-list		job:process-list	set-job:process-list!)

  ;; The `process-group' is an integer group id.
  ;; 
  (process-group	job:process-group	set-job:process-group!)

  ;; The process form (for convenient printing/debugging).
  ;;
  (process-form		job:process-form	set-job:process-form!))


;; (job thunks . kws)
;; 
;; Create a job by forking processes for each procedure in `thunks'.
;; All of the subprocesses are assigned to a new process group whose
;; id is the process id of the first process in the list.
;; 
;; Each process evaluates `(thunk)' and exits.
;; 
;; Recognized keyword arguments are:
;; 
;; 	:foreground	Place the new process group in the foreground.
;;			(See `set-foreground-group'.)
;; 
(define-public (job thunks . kws)
  (if (memq :session-leader kws)
      (error "jobs can not be session leaders"))
  (force-output)
  (let* ((leader		(car thunks))
	 (others		(cdr thunks))
	 (leader-process	(apply fork leader :group-leader kws))
	 (process-group		(process:pid leader-process))
	 (other-procs		(map (lambda (t) (apply fork t :process-group process-group kws)) others)))
    (apply make-job leader-process other-procs)))


;; (job-processes job)
;; 
;; Return a list of the processes in `job', with the process group
;; leader at the head of the list.
;; 
(define-public (job-processes job)
  (list-copy (job:process-list job)))


;; (job-process-group job)
;; 
;; Return the process group id of processes in `job'.
;; 
(define-public (job-process-group job)
  (job:process-group job))


;; (live-jobs)
;; 
;; Return the list of active jobs.
;; 
(define-public (live-jobs)
  (list-copy job-list))


;; (finished-jobs)
;; 
;; Return the list of jobs which have finished, clearing that
;; list.
;;
(define-public (finished-jobs)
  (without-interrupts
   (lambda ()
     (let ((answer finished-jobs-list))
       (set! finished-jobs-list ())
       answer))))


;; (job-finished? job)
;; 
;; Return #t if `job' has completed, #f otherwise.
;; 
(define-public (job-finished? job)
  (without-interrupts
   (lambda ()
     (not (or-map live-process? (job:process-list job))))))
	

;; (job-exited-normally? job)
;; 
;; Return #t if `job' completed with all processes exiting
;; normally, #f otherwise.   (An error is signaled if `job'
;; has not finished).
;; 
(define-public (job-exited-normally? job)
  (and-map (lambda (x)
	     (if (live-process? x)
		 (error "job has not finished" job)
		 (let ((status 	(process:exit-status x)))
		   (and (eq? 'exited (status:state status))
			(= 0 (status:exit-val status))))))
	   (job:process-list job)))


;; (make-job group-leader . other-processes)
;; 
;; Construct a new process containing the process `group-leader'
;; and the processes `other-processes'.  All arguments must be
;; process objects and all processes must be in the process group
;; having the process id of `group-leader' as the process group id.
;; 
(define (make-job group-leader . other-processes)
  (let ((job	(construct-job)))
    (set-job:process-group! job (process:pid group-leader))
    (set-job:process-list! job (cons group-leader other-processes))
    (without-interrupts
     (lambda ()
       (if (or-map live-process? (cons group-leader other-processes))
	   (begin
	     (set! job-list (cons job job-list))
	     (for-each (lambda (proc)
			 (hashq-set! job-table proc job))
		       (cons group-leader other-processes)))
	   (set! finished-jobs-list (cons job finished-jobs-list)))))
    job))


;; (update-job-for-finished-process process)
;; 
;; Check to see whether the job containing `process' has completed.
;; If so, update the job-related data structures.
;; 
(define (update-job-for-finished-process process)
  (without-interrupts
   (lambda ()
     (let* ((job		(hashq-ref job-table process))
	    (processes		(and job (job:process-list job))))
       (and job
	    (not (or-map live-process? processes))
	    (begin
	      (set! job-list (delq job job-list))
	      (set! finished-jobs-list (cons job finished-jobs-list))
	      (for-each (lambda (proc)
			  (hashq-remove! job-table proc))
			processes)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-level `wait'
;;; 

;; (wait job/proc/pid options)
;; 
;; Perform a `%waitpid' system call and return the process status.  If
;; this call reaps a process zombie, record the processes exit status
;; in the appropriate process object.  If subsequent calls to `wait'
;; are passed the process object for a process which has already been
;; reaped, the cached exit status is returned.
;; 
;;      job/proc/pid may be:
;; 
;;              A process object: check the status of that process
;;              only.
;; 
;;              A job object: check the status of any process in
;;              the corresponding process group.
;; 
;;              0: check the status of any subprocess in the process
;;              group of the parent process.  This is not recommended
;;              in programs that use the `%fork' system call directly.
;;              
;;              -1: check the status of any subprocess of the the
;;              parent process.  This is not recommended in programs
;;              that use the `%fork' system call directly.
;;              
;;              -n (n > 1): check the status of any subprocess of the
;;              the parent process in process group `n'.  This is not 
;;              recommended in programs that use the `%fork' system call 
;;              directly.
;; 
;; 	options may be any of the symbols (or a list of any of the symbols):
;; 
;; 		WNOHANG: Return immediatly if no process status is available.
;; 
;; 		WUNTRACED: Return status for stopped processes as well as
;;		terminated processes.
;; 
;; Return:
;; 
;;	#f	options included 'WNOHANG and there are no
;;		stopped or exited children corresponding to
;;		`proc/pid'
;;
;; 	<wait-value>
;; 
;; 		A wait value is a keyword-argument list whose exact
;;		contents depend on the status being reported.
;;		All wait values have these fields (which can be 
;;		retrieved using `kw-arg-ref':
;; 
;; 			:process <obj>
;;				The process object.
;;			:pid <n>
;;				The process id (an integer).
;;			:state <symbol>
;;				One of the symbols:
;;				  exited   - the process called _exit()
;;				  signaled - the process was killed by a signal
;;				  stopped  - the process was stopped by a signal
;; 
;; 		Processes in an `exited' state have the field:
;; 
;; 			:exit-val <n>
;; 				The exit value of the process (an integer).
;; 
;; 		Processes in a `signaled' state have the field:
;; 
;;			:term-signal <n>
;;				The process was killed by signal `n'.  `n'
;;				is a value suitable as an argument to `kill'.
;; 
;; 		Processes in a `stopped' state have the field:
;; 
;;			:stop-signal <n>
;;				The process was stopped by signal `n'.  `n'
;;				is a value suitable as an argument to `kill'.
;; 
;;
;; See also `status:process', `status:pid', `status:state', `status:exit-val',
;; `status:term-signal', and `status:stop-signal'.
;; 
(define-public (wait job/proc/pid :optional options)
  (without-interrupts
   (lambda ()
     (let* ((pid		(cond
				 ((number? job/proc/pid)	job/proc/pid)
				 ((process? job/proc/pid)	(process:pid job/proc/pid))
				 ((job? job/proc/pid)		(- (job:process-group job/proc/pid)))
				 (#t				(throw 'parameter-error "wrong type argument in position 1" 'wait job/proc/pid))))
	    (object		(cond
				 ((process? job/proc/pid)	job/proc/pid)
				 ((< 0 pid)			(pid->proc pid))
				 (#t				#f)))
	    (cached-status	(and object (process:exit-status object))))

       (or cached-status

	   (let ((status	(%% (lambda () (%waitpid pid options)))))

	     (and status
		  (let* ((wait-pid		(kw-arg-ref status :pid))
			 (object		(or (pid->proc wait-pid)
						    (remember-process wait-pid)))
			 (status		(append `(:process ,object) status))
			 (state			(kw-arg-ref status :state)))
		    (if (or (eq? state 'exited)
			    (eq? state 'signaled))
			(begin
			  (set-process:exit-status! object status)
			  (forget-process object)))
		    status))))))))


;; (wait-for-job job :optional flags)
;; 
;; Wait for all process in `job' to finish.
;; 
;; `flags' may any of the symbols (or a list of any of the symbols):
;; 
;; 	WUNTRACED	-- check for stopped processes
;; 	WNOHANG		-- return immediately if status is unavailable
;;			   for any of the processes.
;; 
;; Returns:
;; 
;; 	#t 		-- all processes have exited normally
;;	`stopped' 	-- `flags' includes `WUNTRACED' and 
;;			   the job is stopped
;;	`(abnormal-exit status0 status1 ...)' -- at least one process 
;;			  exited abnormally.  `statusN' is the wait status
;;			  of process N of the job.
;;	#f 		-- some processes have not stoped or finished, and
;;			   `flags' includes `WNOHANG'.
;; 
;; A job is considered "stopped" if each live subprocess is seen to
;; enter a `stopped' state after `wait-for-job' is called.  This
;; doesn't necessarily mean that all live subprocesses are currently
;; stopped:  Some may have been subsequently continued; there is no
;; way to tell.
;; 
;; It is safe to call this procedure for a job which has already
;; finished.
;;
(define-public (wait-for-job job :optional flags)

  ;; Internally, this procedure recurses with an extra argument:
  ;; `stopped', which is a list of processes that have been 
  ;; stopped.
  ;; 
  (define (wait-for-job job :optional flags stopped)

    ;; check-for-stopped-process status
    ;; 
    ;; Call `wait-for-job' tail-recursively, but check `status'.  If
    ;; `status' indicates a stopped subprocess, add that process to
    ;; the list-set `stopped'.
    ;; 
    (define (check-for-stopped-process status)
      (cond
       ((and status
	     (eq? 'stopped (status:state status))		(wait-for-job job flags (lset-adjoin eq? stopped (status:process status)))))
       (#t							(wait-for-job job flags stopped))))

    (cond
     ;; Do we alreay know (without calling `wait') that all of the
     ;; processes have exited?
     ;;
     ((job-finished? job)							(if (job-exited-normally? job)
										    #t
										    (cons 'abnormal-exit
											  (map (lambda (p) (wait p)) (job:process-list job)))))

     ;; Have we seen all remaining live processes enter a stopped
     ;; state?  Note that we must double check that there are still
     ;; live processes at all, since an interrupt handler may have
     ;; reaped the remaining processes.
     ;; 
     ((let ((live (filter live-process? (job:process-list job))))
	(and live (lset= eq? stopped live)))					'stopped)
     

     ;; In the two cases that follow, we call `wait' to collect
     ;; process status.  Note that we must catch the exception `ECHLD'
     ;; because by the time we call `wait', all of the processes in
     ;; job may have exited and been reaped (by a signal handler).
     ;;

     ;; Are we supposed to do this without blocking?
     ;;
     ((or (eq? 'WNOHANG flags)
	  (and (pair? flags) (memq 'WNOHANG flags)))				(and=> (catch 'ECHLD
											 (lambda () (wait job flags))
											 (lambda ign #f))
										       check-for-stopped-process))

     ;; Blocking is permited.  Note that we iterate even if `wait'
     ;; reports that there are no more proceses in the group.  In that
     ;; case, the next pass should see `(job-finished? job)' return
     ;; #t.  (If it doesn't, that probably indicates a bug in how the
     ;; process and job object data structures are managed.)
     ;; 
     (#t									(check-for-stopped-process (catch 'ECHLD
													     (lambda () (wait job flags))
													     (lambda ign #f))))))
  (wait-for-job job flags ()))


;; (status:process status)
;; 
;; Given a process status returned by `wait', return the associated
;; process object.
;; 
(define-public (status:process s)
  (kw-arg-ref s :process))


;; (status:pid status)
;; 
;; Given a process status returned by `wait', return the associated
;; pid.
;; 
(define-public (status:pid s)
  (kw-arg-ref s :pid))


;; (status:state status)
;; 
;; Given a process status returned by `wait', return the process state
;; which is one of:
;; 
;; 	exited
;; 	signaled
;; 	stopped
;; 
;; Note that the state `stopped' is possible only if the flag `WUNTRACED' was
;; passed to `wait'.
;; 
(define-public (status:state s)
  (kw-arg-ref s :state))


;; (status:exit-val status)
;; 
;; Given a process status returned by `wait', for a process in state
;; `exited', return the exit value of the process.
;; 
(define-public (status:exit-val s)
  (if (eq? 'exited (kw-arg-ref s :state))
      (kw-arg-ref s :exit-status)
      (error "exit status has no exit value" s)))


;; (status:term-signal status)
;; 
;; Given a process status returned by `wait', for a process in state
;; `signaled', return the signal that terminated the process.
;; 
(define-public (status:term-signal s)
  (if (eq? 'signaled (kw-arg-ref s :state))
      (kw-arg-ref s :term)
      (error "exit status has no termination signal")))


;; (status:stop-signal status)
;; 
;; Given a process status returned by `wait', for a process in state
;; `stopped', return the signal that stopped the process.
;; 
(define-public (status:stop-signal s)
  (if (eq? 'stopped (kw-arg-ref s :state))
      (kw-arg-ref s :stop-signal)
      (error "exit status has no stop signal")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sending Signals
;;; 

;; (kill  proc/pid/job signal)
;; 
;; Signal a process or process group.  Throw an exception on error.
;; 
;; `proc/pid/job' may be a process object, job object, or integer pid.
;; 
;; `signal' may be an integer signal number, or a symbolic signal name
;; (e.g. `SIGINT').
;; 
(define-public (kill proc/pid/job signal)
  (cond
   ((job? proc/pid/job)		(%% %killpg (job:process-group proc/pid/job) signal))
   ((process? proc/pid/job)	(%% %kill (process:pid proc/pid/job) signal))
   (#t				(%% %kill proc/pid/job signal))))


;; (suspend :optional proc/pid/job)
;; 
;; Signal a process or process group with SIGSTOP.  Throw an exception
;; on error.
;; 
(define-public (suspend :optional proc/pid/job)
  (kill (or proc/pid/job (getpid)) 'SIGSTOP))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Controlling tty
;;; 

;; (controlling-tty-fd)
;; 
;; Return a new descriptor opened on the controlling terminal
;; of this process.
;; 
(define-public (controlling-tty-fd)
  (let ((dev	(%% %ctermid)))
    (%% %open dev 'O_RDWR 0)))


;; (set-foreground-group :optional fd prgrp)
;; 
;; Make `prgrp' (an integer) the foreground group of
;; the controlling terminal open at descriptor `fd'.
;; 
;; `prgrp' defaults to the process group of the calling 
;; process.
;; 
;; `fd' defaults to a descriptor open on the controlling tty
;; of the calling process.
;; 
(define-public (set-foreground-group :optional fd prgrp)
  (let ((tty-fd		(or fd (controlling-tty-fd)))
	(prgrp		(cond
			 ((job? prgrp)		(job:process-group prgrp))
			 (prgrp 		prgrp)
			 (#t			(getpgrp)))))
    (%% %tcsetpgrp tty-fd prgrp)
    (if (not fd)
	(%% %close tty-fd))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High Level Fork
;;; 

;; (fork :optional thunk . kws)
;; 
;; Fork a subprocess.  Before forking, flush output buffers.  
;; In the subprocess, set `interactive-mode' to #f.
;; 
;; To avoid flushing buffers, pass the keyword `:no-flush'.
;; 
;; To avoid setting `interactive-mode' in the subprocess, pass the
;; keyword `:interactive'.  Other permitted keywords are:
;; 
;; 	Process Group and Session Id:
;; 
;; 	:session-leader	Assign the subprocess to its own process group
;;			and give that group a new session id.  The
;;			subprocess then has no controlling tty.
;; 
;; 	:group-leader	Assign the subprocess to its own process group.
;;			This is overridden by `:session-leader'.
;; 
;; 	:group n	Assign the subprocess to group `n' (overriden by
;;			`:group-leader').
;; 
;; 	Controlling Terminal
;; 
;; 	:foreground	Make the process group of the subprocess the
;;			foreground group of the controlling tty.
;;			This is overridded by `:session-leader'.
;; 
;; 
;; In the parent process, return a process object for the new process.
;; 
;; In the child process, if thunk is not #f, evaluate `(thunk)' and
;; exit with status 0 if `thunk' returns.  An exception in `thunk'
;; can cause `fork' to exit non-locally in the child process.
;; 
;; In the child process, if `thunk' is #f, return #f.
;; 
(define-public (fork :optional thunk . kws)
  (if (not (memq :no-flush kws))
      (force-output))
  (without-interrupts
   (lambda ()
     (let ((pid	(%fork)))

       ;; It is necessary to process :session-leader, :group-leader
       ;; and :group options in both the parent and the child process.
       ;; In the child process, we must guarantee that the group has
       ;; been set by the time we return or invoke `thunk'.  In the
       ;; parent process, we must guarantee that the group has been
       ;; set by the time we return.  Imitating /bin/sh we make these
       ;; guarantees by calling `%setpgid' in both the parent and
       ;; child processes, but we must ignore errors in the parent
       ;; process.  There are two possible errors: (1) The child
       ;; process has already invoked `%exec' and we receive an
       ;; `EACCES' error.  In that case, the child process is already
       ;; in the correct group and the error doesn't matter.  (2) The
       ;; child process has invoked `%setsid' and again, is already in
       ;; the correct group.
       ;;
       (if (not (= pid 0))
	   ;; In the parent process, return a process object for the
	   ;; subprocess after (optionally) setting the subprocess group.
	   ;;
	   (begin 
	     ;; Optionally set the subprocess group but with no error
	     ;; checking ... see earlier comment
	     ;;
	     (cond
	      ((or (memq :group-leader kws)
		   (memq :session-leader kws))		(%setpgid pid pid))
	      ((kw-arg-ref kws :process-group) =>	(lambda (pgid) (%setpgid pid pgid))))

	     ;; Create and return a process object for the new process.
	     ;; 
	     (remember-process pid))

	   ;; In the subprocess:
	   ;;
	   (begin
	     ;; Forget about subprocesses of the parent process.
	     ;; 
	     (clear-processes)

	     ;; Arrange to exit on error, unless `:interactive' was
	     ;; provided.
	     ;; 
	     (if (not (memq :interactive kws))
		 (set! interactive-mode #f))

	     ;; Set the process group and make this the foreground process, if requested.
	     ;;
	     (let ((subprocess-group		(cond
						 ((memq :session-leader kws)		(%% %setsid)
											(getpid))
						 ((memq :group-leader kws)		(let ((pgid (getpid)))
											  (%% %setpgid 0 pgid)
											  pgid))
						 ((kw-arg-ref kws :process-group) =>	(lambda (pgid)
											  (%% %setpgid 0 pgid)
											  pgid))
						 (#t					(getpgrp)))))
	       (if (and (memq :foreground kws)
			(not (memq :session-leader kws)))
		   (set-foreground-group #f subprocess-group)))

	     ;; If `thunk' was provided, evaluate `(thunk)' and exit.
	     ;; Otherwise, return #f (indicating that this is the 
	     ;; subprocess).
	     ;; 
	     (and thunk
		  (begin (with-interrupts thunk)
			 (%exit 0)))))))))


;; (fork/pipe :optional thunk . kws)
;; 
;; Like `fork', but before forking, create a pipe.  In the parent
;; process, assign the input half of the pipe to descriptor 0.  In the
;; child process, assign the output half of the pipe to descriptor 1.
;; 
;; In both processes, if a port is already using the descriptor in
;; question, that port is moved to another descriptor.
;; 
;; Ordinarily, no port object is created for the two ends of the pipe
;; and the pipe ends will not be closed by a subsequent `exec'.  To
;; retrieve a port (and cause the pipe end to be closed if the port is
;; garbage collected or `exec' is invoked) use
;; `integer->file-descriptor'.
;; 
(define-public (fork/pipe :optional thunk . kws)
  (apply fork/pipe+ '((1 0)) thunk kws))


;; (fork/pipe+ connections :optional thunk . kws)
;; 
;; Like `fork', but before forking, create a series of pipes.
;; 
;; `connections' is a list of `redirections'.  Each redirection is a
;; list of integer descriptor numbers and/or file descriptor objects.
;; The last element of each redirection designates a `sink descriptor'
;; in the parent process.  Earlier elements of each list designate
;; `source descriptors' in the child process.
;; 
;; For each redirection, a pipe is created.  In the parent process,
;; the input end of the pipe is moved to the sink descriptor.  In the
;; child process, the output end of the pipe and copies of the output
;; end of the pipe are moved to the corresponding source descriptors.
;; 
;; In both processes, if a port is already using the descriptor in question,
;; that port is moved to another descriptor.
;; 
(define-public (fork/pipe+ connections :optional thunk . kws)
  (let* ((pipes			(map (lambda (connection) (cons (reverse connection) (%% %pipe))) connections))
	 (read-connections	(map (lambda (pipe) (cons (caar pipe) (cadr pipe))) pipes))
	 (write-connections	(map (lambda (pipe) (cons (cdar pipe) (caddr pipe))) pipes)))
    (without-interrupts
     (lambda ()
       (let ((proc (apply fork kws)))
	 (cond
	  (proc		(for-each (lambda (write-connection)
				    (%% %close (cdr write-connection)))
				  write-connections)
			(for-each (lambda (read-connection)
				    (let ((descriptor		(car read-connection))
					  (pipe-end		(cdr read-connection)))
				      (move->fdes pipe-end descriptor)))
				  read-connections))
	  (#t		(for-each (lambda (read-connection)
				    (%% %close (cdr read-connection)))
				  read-connections)
			(for-each (lambda (write-connection)
				    (let ((descriptors		(car write-connection))
					  (pipe-end		(cdr write-connection)))
				      (for-each (lambda (desc)
						  (let ((desc-clone 	(%% %dup pipe-end)))
						    ;; (set-autoclose-file-descriptor! desc-clone #f)
						    (move->fdes desc-clone desc)))
						descriptors)
				      (%% %close pipe-end)))
				  write-connections)
			(and thunk
			     (begin (with-interrupts thunk)
				    (%exit 0))))))))))


;; (move->fdes fdes/fd fdes)
;; 
;; Move descriptor `fdes/fd' to descriptor `fdes'.  Either argument
;; may be a file descriptor object (port) or integer descriptor.
;; 
;; If a port already exists at descriptor `fdes', that port is 
;; moved to a new descriptor.
;; 
;; If no port already exists for `fdes/fd' (`fdes/fd' is an integer) a
;; port is created for `fdes/fd' (which becomes a port for `fdes').
;; Regardless, the `autoclose' flag for that port is set to #f meaning
;; that the port will not be closed by any of the high-level `exec' family
;; of Scheme procedures, and will not be closed if that port is garbage 
;; collected.  That port object is returned from this procedure.
;; 
;; (See also `set-autoclose-file-descriptor!'.)
;; 
(define-public (move->fdes port fdes)
  (without-interrupts
   (lambda ()
     (let* ((port   		(integer->file-descriptor port))
	    (target		(file-descriptor->integer fdes))
	    (previous		(integer->existing-file-descriptor target))
	    (source		(file-descriptor->integer port)))
       (if (not (= target source))
	   (begin
	     (if previous
		 (%% %move-fd previous #f))
	     (%% %move-fd port target)))
       (if (file-descriptor? port)
	   (set-autoclose-file-descriptor! port #f))
       port))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High Level Exec
;;; 

;; exec-path-list
;; 
;; A list of directories which are searched for executables
;; by `exec-path' and `exec-path/env'.
;; 
;; This list is initialized from the environment variable "PATH".
;; 
(define-public exec-path-list (parse-path (getenv "PATH")))


;; (exec-path-search program path)
;; 
;; If `program' contains the character "/", return `program'
;; if it is the name of an executable, #f otherwise.
;; 
;; If `program' does not contain "/", search for an executable
;; named `program' in the directories in the list `path', returning
;; the first found or #f.
;; 
(define-public (exec-path-search prog path)
  (if (string-index prog #\/)
      (and (file-executable? prog)
	   prog)
      (let loop ((path path))
	(if (not path)
	    #f
	    (let ((it (in-vicinity (car path) prog)))
	      (if (file-executable? it)
		  it
		  (loop (cdr path))))))))


;; (exec-argv0/env prog env argv0 . args)
;; 
;; Replace the current process with an invocation of `prog' with
;; `argv[0]' equal to `argv0', the remaining arguments `args', and an
;; environment containing the strings in the list `env'.  If `env' is
;; #t, `prog' is invoked with the current process' environment.
;; 
;; If `exec' fails, display an error message on the current error port 
;; and exit with a non-0 status.
;; 
(define-public (exec-argv0/env prog env argv0 . args)
  (without-interrupts
   (catch #t
     (lambda ()
       (let ((ret (%exec prog (map (lambda (x) (if (number? x) (number->string x) x)) (cons argv0 args)) env)))
	 (throw ret)))
     (lambda error
       (catch #t
	 (lambda ()
	   (display "\n;;; exec returned " (current-error-port))
	   (write error (current-error-port))
	   (display " " (current-error-port))
	   (write (list prog env args) (current-error-port))
	   (display "\n" (current-error-port)))
	 (lambda ign #f))
       (%exit 1)))))


;; (exec-path/env prog env . args)
;; 
;; Search for `prog' using `exec-path-search'.  If it is not found,
;; print an error message on the current error port and exit with
;; a non-0 status.  If it is found, replace the current process with
;; an invocation of `prog' by calling `exec-argv0/env'.
;; 
;; If `env' is #t, the program is invoked with the environment of
;; the current process.  Otherwise, `env' should be a list of strings
;; which becomes the environment of the invoked process.
;; 
(define-public (exec-path/env prog env . args)
   (let ((file 	(exec-path-search prog exec-path-list)))
     (if (not file)
	 (begin
	   (display*-port (current-error-port) ";;; Command not found: " prog "\n")
	   (%exit 1)))
     (apply exec-argv0/env file env prog args)))


;; (exec-path prog env . args)
;; 
;; Equivalent to:
;; 
;; 	(lambda (prog . args) 
;; 	  (apply exec-path/env prog #t args))
;; 
(define-public (exec-path prog . args)
  (apply exec-path/env prog #t args))


;; (exec prog . args)
;; 
;; Equivalent to:
;; 
;; 	(lambda (prog . args) 
;; 	   (apply exec-argv0/env prog #t prog args))
;; 
(define-public (exec prog . args)
  (apply exec-argv0/env prog #t prog args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Establishing I/O Environments After Fork
;;; 
;;; These procedures are useful in a program that has just 
;;; called `fork'.   They modify the process' descriptor table
;;; to include descriptors that will be needed by the new
;;; subprocess.
;;; 
;;; Existing descriptors that are not needed by the new subprocess,
;;; but that occupy descriptor numbers that are needed by the
;;; new subprocess, are relocated.
;;; 
;;; Descriptors created for the new subprocess have their
;;; `autoclose-flag' set to #f.  See `close-uninherited-ports'.
;;; 

;; (perform-redirects redirects)
;; 
;; Interpret `redirects', which is a list of "shell redirections".
;; 
;; A shell redirection is of one of the forms:
;; 
;;	redirection:		interpretation by: 
;; 
;;	(< [fd] filename)	redirect-from-file
;;	(> [fd] fliename)	redirect-to-file
;;	(>> [fd] filename)	redirect-to-file-appending
;;	(<< [fd] object)	redirect-from-display
;;	(= [fd] fd)		redirect-descriptor
;;	(- fd)			close-fd
;; 	stdports		redirect-stdports
;; 
;; A call to this procedure is usually followed by a call to
;; `close-uninherited-ports'.
;; 
(define-public (perform-redirects redirects)

  (define (call-with-redirect-arguments args proc)
    (cond
     ((null? (cdr args))		(proc (car args)))
     (#t				(proc (cadr args) (car args)))))

  (and redirects
       (begin
	 (if (eq? (car redirects) 'stdports)
	     (redirect-stdports)
	     (case (caar redirects)
	       ((<)	(call-with-redirect-arguments (cdar redirects) redirect-from-file))
	       ((>)	(call-with-redirect-arguments (cdar redirects) redirect-to-file))
	       ((<<)	(call-with-redirect-arguments (cdar redirects) redirect-from-display))
	       ((>>)	(call-with-redirect-arguments (cdar redirects) redirect-to-file-appending))
	       ((=)	(call-with-redirect-arguments (cdar redirects) redirect-descriptor))
	       ((-)	(close-fd (cadar redirects)))
	       (else	(throw 'unrecognized-redirect (car redirects)))))
	 (perform-redirects (cdr redirects)))))


;; (close-uninherited-ports)
;; 
;; Close all ports which are not standard ports (0, 1, and 2) and for
;; which the autoclose flag is true.  These are presumed to be the
;; ports which should not be inherited by a process invoked by any of
;; the shell procedures (`invoke', `background' (aka `&'), `run' (aka
;; `!'), etc.)
;; 
(define-public (close-uninherited-ports)
  (let ((ports	(all-file-descriptors)))
    (for-each (lambda (p)
		(if (and (fd-is-open? p)
			 (not (member (file-descriptor->integer p) '(0 1 2) =))
			 (autoclose-file-descriptor? p))
		    (%% %close p)))
	      ports)))


;; (redirect-from-file filename :optional fd)
;; 
;; Open `filename' for reading and make it the standard input or
;; the descriptor number indicated by `fd' (a port or integer).
;; 
;; If an existing port is already at the target descriptor, that port
;; is relocated to a new descriptor.
;; 
;; No descriptor object is returned.  The file will not be
;; automatically closed by garbage collection or any of the `exec'
;; family of functions, unless a descriptor object for the descriptor
;; is first retrieved by `integer->file-descriptor'.
;; 
(define-public (redirect-from-file filename :optional fd)
  (let ((file-fd		(%% %open filename 'O_RDONLY 0)))
    (move->fdes file-fd (or fd 0))))


;; (redirect-to-file filename :optional fd)
;; 
;; Exclusively create `filename' for writing and make it the standard
;; output or the descriptor number indicated by `fd' (a port or
;; integer).
;; 
;; If an existing port is already at the target descriptor, that port
;; is relocated to a new descriptor.
;; 
;; No descriptor object is returned.  The file will not be
;; automatically closed by garbage collection or any of the `exec'
;; family of functions, unless a descriptor object for the descriptor
;; is retrieved by `integer->file-descriptor'.
;; 
(define-public (redirect-to-file filename :optional fd)
  (let ((file-fd		(%% %open filename '(O_WRONLY O_CREAT O_EXCL) #o666)))
    (move->fdes file-fd (or fd 1))))


;; (redirect-to-file-appending filename :optional fd)
;; 
;; Open `filename' for appended writing and make it the standard
;; output or the descriptor number indicated by `fd' (a port or
;; integer).
;; 
;; If an existing port is already at the target descriptor, that port
;; is relocated to a new descriptor.
;; 
;; No descriptor object is returned.  The file will not be
;; automatically closed by garbage collection or any of the `exec'
;; family of functions, unless a descriptor object for the descriptor
;; is retrieved by `integer->file-descriptor'.
;; 
(define-public (redirect-to-file-appending filename :optional fd)
  (let ((file-fd		(%% %open filename '(O_WRONLY O_APPEND) 0)))
    (move->fdes file-fd (or fd 1))))


;; (close-fd fd)
;; 
;; Close `fd'.  This is simply `(%% %close fd)'.
;; 
(define-public (close-fd fd)
  (%% %close fd))


;; (redirect-descriptor from-fd to-fd)
;; 
;; Duplicate `from-fd' using `%dup' and move the duplicate to
;; descriptor `to-fd'.
;; 
;; No descriptor object is returned.  The duplicate will not be
;; automatically closed by garbage collection or any of the `exec'
;; family of functions, unless a descriptor object for the descriptor
;; is retrieved by `integer->file-descriptor'.
;; 
(define-public (redirect-descriptor from-fd to-fd)
  (let ((new-fd		(%% %dup from-fd)))
    (move->fdes new-fd to-fd)))


;; (redirect-stdports)
;; 
;; 	(lambda ()
;;	  (redirect-descriptor (current-input-port) 0)
;;	  (redirect-descriptor (current-output-port) 1)
;;	  (redirect-descriptor (current-error-port) 2))
;; 
(define-public (redirect-stdports)
  (redirect-descriptor (current-input-port) 0)
  (redirect-descriptor (current-output-port) 1)
  (redirect-descriptor (current-error-port) 2))


;; (redirect-from-display object :optional fd)
;; 
;; Create an anonymous temporary file which contains the output of
;; `(display object)'.  Redirect a descriptor opened on that file for
;; reading to descriptor 0 using `redirect-descriptor'.
;; 
(define-public (redirect-from-display object :optional fd)
  (redirect-descriptor (open-string-source object) (or fd 0)))


;; (open-string-source object)
;; 
;; Create an anonymous temporary file which contains the output of
;; `(display object)'.  Return a port opened on that file for reading.
;; 
(define-public (open-string-source obj)
  (call-with-values temp-file-channel
		    (lambda (in out)
		      (display obj out)
		      (%% %close out)
		      in)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A High Level Interface for Replacing the Current Processes
;;; 
;;; `process' interprets a process form to create a process (or complex
;;; pipeline) which replaces the current process.
;;; 
;;; `invoke' interprets a series of shell redirections (`perform-redirects'),
;;; closes ports which should not be inherited (`close-uninherited-ports')
;;; and then calls `process'. 
;;; 
;;; 


;; (invoke process-form . redirects)
;; 
;; Perform the shell redirections specified by `redirects' (see
;; `perform-redirects').
;; 
;; Close ports which should not be inherited by a process created by
;; a shell procedure (see `close-uninherited-ports').
;; 
;; Replace the current process by the process specified by
;; `process-form' (see `process').
;; 
;; This procedure does not return.
;; 
(define-public (invoke pf . redirects)
  (perform-redirects redirects)
  (close-uninherited-ports)
  (process pf))


;; (process process-form)
;; 
;; Replace the current process with the process specified by `process-form'.
;; `process-form' may be a procedure or list.  
;; 
;; If `process-form' is a procedure, that procedure is invoked with no
;; arguments, with `interactive-mode' set to #f (unhandled errors
;; cause the process to exit) and with the standard ports
;; (current-input, current-output, and current-error) set to the
;; standard descriptors (0, 1, and 2).
;; 
;; If `process-form' is a list, it may have any of the forms listed
;; below.  In this list, `pf' designates another process form,
;; `redirects' are as described in the documentation for `invoke',
;; `prog' and `arg' designate strings or symbols, and a `connect-list'
;; is as described in the documentation for `fork/pipe+'.
;; 
;; 	process-form:				interpretation:
;; 
;;	`(| pf ... pf)				Create a pipeline of processes
;; 
;;	`(|+ pf-0 connect-list-0 pf1 connect-list-1 ... pf-n)	
;;						Create a pipeline of processes
;;						with `connect-list-K' sources
;;						in `pf-K' and `connect-list-K'
;;						sinks in `pf-K+1'.
;; 
;;	`(! pf redirect ...)			Equivalent to:
;;						(lambda () (invoke 'pf 'redirect ...))
;; 
;;	`(prog arg ...)				Equivalent to:
;;						(lambda () (exec-path 'prog 'arg ...))
;;
(define-public (process pf)
  (if (procedure? pf)

      (without-interrupts
       (lambda ()
	 (set! interactive-mode #f)
	 (with-input-from-port (integer->file-descriptor 0)
	   (lambda ()
	     (with-output-to-port (integer->file-descriptor 1)
	       (lambda ()
		 (with-error-to-port (integer->file-descriptor 2)
		   (lambda ()
		     (with-interrupts pf)))))))
	 (%exit 0)
	 (throw 'exit-returned)))

      (case (car pf)
	((|)		(let loop ((pfs 	(cdr pf)))
			  (cond
			   ((null? pfs)		(error "too few arguments to | form" pf))
			   ((null? (cdr pfs))	(process (car pfs)))
			   (#t
			    (let ((left-pf	(car pfs))
				  (rest 	(cdr pfs)))
			      (fork/pipe (lambda () (process left-pf)))
			      (loop rest))))))
	((|+)		(let loop ((pfs 	(cdr pf)))
			  (cond
			   ((null? pfs)		(error "too few arguments to |+ form" pf))
			   ((null? (cdr pfs))	(process (car pfs)))
			   ((null? (cddr pfs))	(error "odd number of arguments to |+ form" pf))
			   (#t
			    (let ((left-pf	(car pfs))
				  (connections	(cadr pfs))
				  (rest 	(cddr pfs)))
			      (fork/pipe+ connections (lambda () (process left-pf)))
			      (loop rest))))))
	((!)		(apply invoke (cdr pf)))
	(else		(apply exec-path pf)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A High-level Interface to `fork' and `exec'
;;; 
;;; The procedures in this section create process which are not
;;; job controlled.  In each case, a single subprocess is created
;;; which may in turn create a complex pipeline (or simply `exec'
;;; or evaluate a `thunk' and exit).
;;; 

;; (start-process pf . redirects)
;; 
;; Create the process specified by process form `pf' and redirections `redirects'
;; as a subprocess of the current process.  Return the process object immediately.
;; 
;; (See `invoke'.)
;; 
(define-public (start-process pf . redirects)
  (fork (lambda () (apply invoke pf redirects))))


;; (run-process process-form . redirects)
;; 
;; Equivalent to 
;; 
;; 	(lambda () (wait (apply start-process pf redirects)))
;; 
;; except that if `check-processes-mode' is true, and the
;; process exits abnormally, an exception is thrown.
;; 
(define-public (run-process pf . redirects)
  (if check-processes-mode
      (check-process-status pf (wait (apply start-process pf redirects)))
      (wait (apply start-process pf redirects))))


;; (check-processes-mode)
;; 
;; This variable determines the meaning of process exit statuses for
;; processes started by `run-process'.
;; 
;; If true, an abnormal process exit causes an exception to be thrown.
;; If #f, process exit status is ignored.
;; 
(define-public check-processes-mode #f)


;; (checking-processes thunk)
;; 
;; Invoke `thunk' with no arguments in a dynamic context in which
;; `check-processes-mode' is true.  
;; 
;; See also: `without-checking-processes' and `run-process?'.
;; 
(define-public (checking-processes thunk)
  (let ((tmp 		#f)
	(saved-mode	#t))
    (dynamic-wind
     (lambda () (set! tmp check-processes-mode
		      check-processes-mode saved-mode
		      saved-mode tmp))
     thunk
     (lambda () (set! tmp check-processes-mode
		      check-processes-mode saved-mode
		      saved-mode tmp)))))


;; (without-checking-processes thunk)
;; 
;; Invoke `thunk' with no arguments in a dynamic context in which
;; `check-processes-mode' is #f.
;; 
;; See also: `checking-processes' and `run-process?'.
;; 
(define-public (without-checking-processes thunk)
  (let ((tmp 		#f)
	(saved-mode	#f))
    (dynamic-wind
     (lambda () (set! tmp check-processes-mode
		      check-processes-mode saved-mode
		      saved-mode tmp))
     thunk
     (lambda () (set! tmp check-processes-mode
		      check-processes-mode saved-mode
		      saved-mode tmp)))))


;; (check-processes-status process-form status)
;; 
;; `status' should be a process status reported by `wait' for a reaped
;; process.  `process-form' should be the process form that created
;; the process whose status is reported.
;; 
;; If the process exited normally, return `status'.  Otherwise, throw
;; an exception.
;; 
(define-public (check-process-status pf status)
  (if (not (and (eq? 'exited (kw-arg-ref status :state))
		(= 0 (kw-arg-ref status :exit-status))))
      (error "abnormal process exit" pf (kw-arg-ref status :state) (kw-arg-ref status :exit-status))
      status))


;; (run-process? process-form . redirects)
;; 
;; Run `process-form' in the manner of `run', but ignoring
;; the value of `check-processes-mode'.  If the process
;; exits normally, return true, otherwise, return #f.
;; 
(define-public (run-process? pf . redirects)
  (let ((status (wait (apply start-process pf redirects))))
    (and (eq? 'exited (kw-arg-ref status :state))
	 (= 0 (kw-arg-ref status :exit-status)))))


;; (process-or . pfs)
;; 
;; Apply `run-process?' to each process form in succession until one of
;; the subprocesses exits normally (`run-process?' returns true).  If any
;; process exits normally, return true, otherwise #f.
;; 
(define-public (process-or . pfs)
  (and pfs
       (or (run-process? (car pfs))
	   (apply process-or (cdr pfs)))))


;; (process-and . pfs)
;; 
;; Apply `run-process?' to each process form in succession until one of
;; the subprocesses exits abnormally (`run-process?' returns #f).  If
;; all processes exit normally, return true, otherwise #f.
;; 
(define-public (process-and . pfs)
  (or (not pfs)
      (and (run-process? (car pfs))
	   (apply process-and (cdr pfs)))))


;; (start-process/port+proc return pf . redirects)
;; 
;; Fork the subprocess described by `pf' and `redirects' (see `invoke').
;; Return two values:
;; 
;;	port proc
;; 
;; where `port' is the readable end of a pipe connected to the 
;; standard output of the subprocess, and `proc' is the process
;; object associated with the subprocess.
;; 
(define (start-process/port+proc pf . redirects)
  (let ((proc (fork/pipe (lambda () (apply invoke pf redirects)))))
    (values (integer->file-descriptor 0) proc)))


;; (start-process/port pf . redirects)
;; 
;; Fork the subprocess described by `pf' and `redirects' (see
;; `invoke').  Return a port which is the readable end of a pipe
;; connected to the standard output of the subprocess.
;; 
(define-public (start-process/port pf . redirects)
  (call-with-values (lambda () (apply start-process/port+proc pf redirects))
		    first-value))


;; (run-process/port+proc pf . redirects)
;; 
;; Fork the subprocess described by `pf' and `redirects' (see
;; `invoke').  Wait for the subprocess to complete and return
;; two values:
;; 
;;	port status
;; 
;; where `port' is open for reading from an anonymous temporary file
;; which contains the standard output of the subprocess, and `status'
;; is the process status returned for the subprocess by `wait'.
;; 
(define-public (run-process/port+status pf . redirects)
  (call-with-values temp-file-channel
		    (lambda (in out)
		      (let ((status (apply run-process pf `(= 1 ,out) redirects)))
			(%% %close out)
			(values in status)))))


;; (run-process/port pf . redirects)
;; 
;; Fork the subprocess described by `pf' and `redirects' (see
;; `invoke').  Wait for the subprocess to complete and return a port
;; open for reading from an anonymous temporary file which contains
;; the standard output of the subprocess.
;; 
(define-public (run-process/port pf . redirects)
  (call-with-values (lambda () (apply run-process/port+status pf redirects))
		    first-value))


;; (run-process/string pf . redirects)
;; 
;; Fork the subprocess described by `pf' and `redirects' (see
;; `invoke').  Wait for the subprocess to complete and return a string
;; which contains the standard output of the subprocess.
;; 
(define-public (run-process/string pf . redirects)
  (fd->string (apply run-process/port pf redirects)))


;; (run-process/strings pf . redirects)
;; 
;; Fork the subprocess described by `pf' and `redirects' (see
;; `invoke').  Wait for the subprocess to complete and return a list
;; of strings which contains the standard output of the subprocess,
;; divided into newline-terminated lines.
;; 
(define-public (run-process/strings pf . redirects)
  (string->lines (fd->string (apply run-process/port pf redirects))))


;; (run-process/sexp pf . redirects)
;; 
;; Fork the subprocess described by `pf' and `redirects' (see
;; `invoke').  Wait for the subprocess to complete and return an
;; object which is the result of invoking `read' on the standard
;; output of the subprocess.
;; 
(define-public (run-process/sexp pf . redirects)
  (read (apply run-process/port pf redirects)))


;; (run-process/sexps pf . redirects)
;; 
;; Fork the subprocess described by `pf' and `redirects' (see
;; `invoke').  Wait for the subprocess to complete and return an list
;; of objects which is the result of repeatedly invoking `read' on the
;; standard output of the subprocess.
;; 
(define-public (run-process/sexps pf . redirects)
  (let ((answer . (cons () ()))
	(port	(apply run-process/port pf redirects)))
    (let loop ((pos answer))
      (let ((r (read port)))
	(if (eof-object? r)
	    (cdr answer)
	    (begin
	      (set-cdr! answer (cons r ()))
	      (loop (cdr pos))))))))


;; (run-process-collecting: return fds pf . redirects)
;; 
;; Create as many anonymous temporary files as their are descriptor
;; numbers in the list `fds'.  Fork the subprocess described by `pf',
;; redirecting output on the descriptors `fds' to the anonymous
;; temporary files (additionally performing the redirections specified
;; by `redirects' (see `invoke').  Wait for the subprocess to complete.
;; 
;; Invoke return with a first argument which is the exit status of the
;; subprocess, and remaining arguments which are input ports for the
;; temporary files (in the order of the list `fds').
;; 
(define-public (run-process-collecting: return ports pf . redirects)
  (let* ((temp-channels 	(map (lambda ign (call-with-values temp-file-channel cons)) ports))
	 (in-channels		(map car temp-channels))
	 (out-channels		(map cdr temp-channels))
	 (out-directs		(map (lambda (port channel) `(= ,port ,channel)) ports out-channels))
	 (all-redirects		(append out-directs redirects))
	 (status		(apply run-process pf all-redirects)))
    (for-each (lambda (p) (%% %close p)) out-channels)
    (apply return status in-channels)))


;; (run-process-collecting fds pf . redirects)
;; 
;; Create as many anonymous temporary files as their are descriptor
;; numbers in the list `fds'.  Fork the subprocess described by `pf',
;; redirecting output on the descriptors `fds' to the anonymous
;; temporary files (additionally performing the redirections specified
;; by `redirects' (see `invoke').  Wait for the subprocess to complete.
;; 
;; Return multiple values.  The first value is the exit status of the
;; subprocess, and remaining values are input ports for the temporary
;; files (in the order of the list `fds').
;; 
(define-public (run-process-collecting ports pf . redirects)
  (apply run-process-collecting: values ports pf redirects))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Establishing I/O Environments for Jobs
;;; 
;;; These procedures perform redirections for jobs (as opposed to
;;; ordinary subprocesses).
;;; 
;;; Job redirections are performed in three steps, at three
;;; different times, in at least two processes:
;;; 
;;; 	before fork: 	evaluate the redirections, open or otherwise
;;;		     	create the needed descriptors.
;;; 
;;; 	after fork (each child process): move redirected descriptors to 
;;;			the correct descriptor numbers.  Close all
;;;			uninherited descriptors.
;;; 
;;; 	after fork (parent process): close the descriptors that 
;;;			were created for child processes.
;;; 
;;; The first argument to each job redirection procedure is a
;;; `clean-descriptors' procedure.  With one argument, a descriptor,
;;; it adds that descriptor to a list of remembered descriptors.  With
;;; no arguments, it closes all remembered descriptors.  
;;; 
;;; A job redirection procedure does the `before fork' step of a
;;; redirection, calls the `clean-descriptors' procedure with
;;; descriptor arguments in preparation for the post-fork
;;; parent-process step, and returns a thunk which performs the
;;; post-fork child-process step.
;;; 
;;; 


;; (cleanup-descriptors-procedure)
;; 
;; Return a procedure that accepts either 0 or 1 argument.
;; 
;; With one argument, a file descriptor, remember that descriptor.
;; With no arguments, close all descriptors (using `%close'), without
;; checking errors.
;; 
(define (cleanup-descriptors-procedure)
  (let ((fd-list	()))
    (lambda (:optional fd)
      (if fd
	  (set! fd-list (cons fd fd-list))
	  (for-each (lambda (x) (%close x)) fd-list)))))


;; (perform-redirects-thunk clean-descriptors redirects)
;; 
;; Interpret `redirects', which is a list of "shell redirections".
;; 
;; A shell redirection is of one of the forms:
;; 
;;	redirection:		interpretation by: 
;; 
;;	(< [fd] filename)	redirect-from-file-thunk
;;	(> [fd] fliename)	redirect-to-file-thunk
;;	(>> [fd] filename)	redirect-to-file-appending-thunk
;;	(<< [fd] object)	redirect-from-display-thunk
;;	(= [fd] fd)		redirect-descriptor-thunk
;;	(- fd)			close-fd-thunk
;; 	stdports		redirect-stdports-thunk
;; 
;; `clean-descriptors' is a procedure returned by
;; `cleanup-descriptors-procedure'.
;; 
;; The procedure returnd by `perform-redirects-thunk' can be called in
;; a subprocess to complete performing the redirections.  The subprocess
;; should usually follow that with a call to `close-uninherited-ports'.
;; 
;; After forking, the parent process should call `clean-descriptors'
;; (with no arguments) to close descriptors opened solely for the
;; subprocess.
;; 
(define-public (perform-redirects-thunk clean-descriptors redirects)

  (define (call-with-redirect-arguments args proc)
    (cond
     ((null? (cdr args))		(proc clean-descriptors (car args)))
     (#t				(proc clean-descriptors (cadr args) (car args)))))

  (if (not redirects)
      (lambda () ())
      (let ((first-redirect	(if (eq? (car redirects) 'stdports)
				    (redirect-stdports-thunk)
				    (case (caar redirects)
				      ((<)	(call-with-redirect-arguments (cdar redirects) redirect-from-file-thunk))
				      ((>)	(call-with-redirect-arguments (cdar redirects) redirect-to-file-thunk))
				      ((<<)	(call-with-redirect-arguments (cdar redirects) redirect-from-display-thunk))
				      ((>>)	(call-with-redirect-arguments (cdar redirects) redirect-to-file-appending-thunk))
				      ((=)	(call-with-redirect-arguments (cdar redirects) redirect-descriptor-thunk))
				      ((-)	(close-fd (cadar redirects)))
				      (else	(throw 'unrecognized-redirect (car redirects))))))
	    (other-redirects	(perform-redirects-thunk clean-descriptors (cdr redirects))))

	(lambda ()
	  (first-redirect)
	  (other-redirects)))))


;; (redirect-from-file-thunk clean-descriptors filename :optional fd)
;; 
;; Open `filename' for reading and return a thunk which redirects it
;; to the standard input or the descriptor number indicated by `fd' (a
;; port or integer).  Also call:
;; 
;; 		(clean-descriptors fd)
;; 
;; where `fd' is the new fd.
;; 
(define-public (redirect-from-file-thunk clean-descriptors filename :optional fd)
  (let ((file-fd		(%% %open filename 'O_RDONLY 0)))
    (clean-descriptors file-fd)
    (lambda () (move->fdes file-fd (or fd 0)))))


;; (redirect-to-file-thunk clean-descriptors filename :optional fd)
;; 
;; Exclusively create `filename' for writing and return a thunk which
;; redirects it to the standard output or the descriptor number
;; indicated by `fd' (a port or integer).  Also call:
;; 
;; 		(clean-descriptors fd)
;; 
;; where `fd' is the new fd.
;; 
(define-public (redirect-to-file-thunk clean-descriptors filename :optional fd)
  (let ((file-fd		(%% %open filename '(O_WRONLY O_CREAT O_EXCL) #o666)))
    (clean-descriptors file-fd)
    (lambda () (move->fdes file-fd (or fd 1)))))


;; (redirect-to-file-appending-thunk clean-descriptors filename :optional fd)
;; 
;; Open `filename' for appended writing and return a thunk which
;; redirects it to the standard output or the descriptor number
;; indicated by `fd' (a port or integer).  Also call:
;; 
;; 		(clean-descriptors fd)
;; 
;; where `fd' is the new fd.
;; 
;; 
(define-public (redirect-to-file-appending-thunk clean-descriptors filename :optional fd)
  (let ((file-fd		(%% %open filename '(O_WRONLY O_APPEND) 0)))
    (clean-descriptors file-fd)
    (lambda () (move->fdes file-fd (or fd 1)))))


;; (close-fd-thunk fd)
;; 
;; Return a thunk which closes `fd'.
;; 
(define-public (close-fd-thunk fd)
  (lambda () (%% %close fd)))


;; (redirect-descriptor-thunk clean-descriptors from-fd to-fd)
;; 
;; Duplicate `from-fd' using `%dup' and return a thunk which redirects
;; the the duplicate to descriptor `to-fd'.  Also call:
;; 
;; 		(clean-descriptors fd)
;; 
;; where `fd' is the new fd.
;; 
(define-public (redirect-descriptor-thunk clean-descriptors from-fd to-fd)
  (lambda ()
    (let ((new-fd		(%% %dup from-fd)))
      (move->fdes new-fd to-fd))))


;; (redirect-stdports-thunk clean-descriptors)
;; 
;; Return a thunk which redirects duplicates of the standard scheme ports
;; (`current-input-port', `current-output-port', and
;; `current-error-port') to the standard descriptors (0, 1, and 2).
;; Also call: 
;; 
;; 	(clean-descriptors fd)
;; 
;; for each of the three duplicated fds.
;; 
(define-public (redirect-stdports-thunk clean-descriptors)
  (let ((stdin		(redirect-descriptor-thunk clean-descriptors (current-input-port) 0))
	(stdout		(redirect-descriptor-thunk clean-descriptors (current-output-port) 1))
	(stderr		(redirect-descriptor-thunk clean-descriptors (current-error-port) 2)))
    (lambda ()
      (stdin)
      (stdout)
      (stderr))))


;; (redirect-from-display-thunk clean-descriptors object :optional fd)
;; 
;; Create an anonymous temporary file which contains the output of
;; `(display object)'.  Return a thunk which redirects a descriptor opened on that file for
;; reading to descriptor 0.  Also call: 
;; 
;; 	(clean-descriptors fd)
;; 
;; where `fd' is the new descriptor.
;; 
(define-public (redirect-from-display-thunk clean-descriptors object :optional fd)
  (let* ((string-source		(open-string-source object))
	 (redirect-thunk	(redirect-descriptor-thunk clean-descriptors string-source (or fd 0))))
    redirect-thunk))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-level Job Notation
;;; 
;;; The procedures in this section translate process forms and redirections
;;; into procedures suitable for use with `job' for creating job-controlled
;;; process groups.
;;; 

;; (background-job-thunks clean-descriptors pf . redirects)
;; 
;; Interpret the process form `pf' and redirections `redirects'.  Return 
;; a list of thunks which can be passed to `job' to create the specified
;; group of processes.
;; 
;; As a side effect, call:
;; 
;; 	(clean-descriptors fd)
;; 
;; for every descriptor created in this process for use in any of the 
;; subprocesses.  After creating the subprocesses with `job', the parent
;; process should call:
;; 
;; 	(clean-descriptors)
;; 
;; to close those descriptors.
;; 
;; If `process-form' is a list, it may have any of the forms listed
;; below.  In this list, `pf' designates another process form,
;; `redirects' are as described in the documentation for `invoke',
;; `prog' and `arg' designate strings or symbols, and a `connect-list'
;; is as described in the documentation for `fork/pipe+'.
;; 
;; 	process-form:				interpretation:
;; 
;;	`(| pf ... pf)				Create a pipeline of processes
;; 
;;	`(|+ pf-0 connect-list-0 pf1 connect-list-1 ... pf-n)	
;;						Create a pipeline of processes
;;						with `connect-list-K' sources
;;						in `pf-K' and `connect-list-K'
;;						sinks in `pf-K+1'.
;; 
;;	`(! pf redirect ...)			Equivalent to:
;;						(lambda () (invoke 'pf 'redirect ...))
;; 
;;	`(prog arg ...)				Equivalent to:
;;						(lambda () (exec-path 'prog 'arg ...))
;; 
(define-public (background-job-thunks clean-descriptors pf . redirects)

  (define (pf->subprocess-procedures clean-descriptors pf :optional redirect-procedure)
    (let ((redirect-procedure	(or redirect-procedure noop)))
      (if (procedure? pf)

	  (list
	   (lambda ()
	     (without-interrupts
	      (lambda ()
		(redirect-procedure)
		(set! interactive-mode #f)
		(with-input-from-port (integer->file-descriptor 0)
		  (lambda ()
		    (with-output-to-port (integer->file-descriptor 1)
		      (lambda ()
			(with-error-to-port (integer->file-descriptor 2)
			  (lambda ()
			    (with-interrupts pf)))))))
		(%exit 0)
		(throw 'exit-returned)))))

	  (case (car pf)
	    ;; (|+ pf connections pf connections pf ...)
	    ;; 
	    ((|+)		(let* ((pfs-and-connections			(cdr pf))
				       (pf-thunk-lists-and-connections		(let recur ((pac	pfs-and-connections))
										  (cond
										   ((null? pac)		())
										   ((null? (cdr pac))	(list (pf->subprocess-procedures clean-descriptors (car pac))))
										   (#t			(cons (pf->subprocess-procedures clean-descriptors (car pac))
													      (cons (cadr pac)
														    (recur (cddr pac))))))))
				       (job-thunks				(apply pipe+-join-thunks clean-descriptors pf-thunk-lists-and-connections)))
				  (map (lambda (thunk)
					 (lambda ()
					   (redirect-procedure)
					   (thunk)))
				       job-thunks)))

	    ((|)		(pf->subprocess-procedures clean-descriptors
							   `(|+ ,@(cdr (fold (lambda (a b) (append b `( ((1 0)) ,a ))) () (cdr pf))))
							   redirect-procedure))

	    ((!)		(let* ((sub-pf 			(cadr pf))
				       (redirects		(cddr pf))
				       (sub-redirect-procedure	(perform-redirects-thunk clean-descriptors redirects))
				       (new-redirect-procedure	(lambda ()
								  (redirect-procedure)
								  (sub-redirect-procedure)))
				       (sub-thunks		(pf->subprocess-procedures clean-descriptors sub-pf new-redirect-procedure)))
				  sub-thunks))

	    (else		(list (lambda ()
					(redirect-procedure)
					(close-uninherited-ports)
					(apply exec-path pf))))))))

  (pf->subprocess-procedures clean-descriptors pf (perform-redirects-thunk clean-descriptors redirects)))


;; (pipe+-join-thunks clean-descriptors thunks0 connections0 thunks1 connections1 ... thunksN)
;; 
;; Each argument `thunksN' is a list of procedures.  Each argument
;; `connectionsN' is a list of linkages between adjacent processes in
;; a pipeline.
;; 
;; This procedure presumes that a job will be created by assigning one
;; process to each procedure in:
;; 
;; 	(append thunks0 thunks1 ... thunksN)
;; 
;; For every argument `connectionsK', this procedure constructs
;; procedures for the source and sink process of a pipeline connected
;; according to "connection list" `connectionsK'.  A connection list
;; is a list of pipe specifications.  Each pipe specification is a
;; list of descriptor numbers:
;; 
;; 	(fd0 fd1 ... fdN)
;; 
;; The connection described uses a single pipeline to link output
;; descriptors `fd0'...`fdN-1' of the source process to input
;; descriptor `fdN' of the sink process.  The source and sink process
;; are linked by as many pipes as the connection list has elements,
;; and each pipe is assigned to the descriptors indicated by the pipe
;; specification.
;;
;; The source procedure for connection list `N', is (schematicly):
;; 
;;	(lambda ()
;;	   (install-source-ends-of-pipes)
;;	   ((car (last-pair thunksN))))
;; 
;; The sink procedure for connection list `N' is (schematicly):
;; 
;; 	(lambda ()
;; 	   (install-sink-ends-of-pipes)
;; 	   ((car thunksN+1)))
;; 
;; Conceptually, `pipe+-join-thunks' returns the list:
;; 
;; 	(append thunks0 thunks1 ... thunksN)
;; 
;; except that the last thunk in every `thunkK' and the first thunk in
;; every `thunkK+1' have been replaced by the source and sink
;; procedures described by the connection lists.  Note that if
;; `thunkN' contains only one thunk, then it may be both a source and
;; a sink -- it is `replaced' twice.
;; 
;; As a side effect, all of the pipes needed for the pipeline are
;; creatd in the calling process.   The argument procedure `clean-descriptors'
;; is invoked once for each pipe end created:
;; 
;; 	(clean-descriptors pipe-end)
;; 
;; See `cleanup-descriptors-procedure' for more information.
;; 
;; The thunks returned can be turned into processes with a sequence like:
;; 
;; 	(job thunks)			; create the processes
;;	(clean-descriptors)		; close the pipes in the parent process
;; 
(define-public (pipe+-join-thunks clean-descriptors thunk-list-a connections thunk-list-b . rest)
  (let* ((pipes 		(map (lambda ign (%% %pipe)) connections))
	 (source-pipes		(map cadr pipes))
	 (sink-pipes		(map car pipes))
	 (sources		(map (lambda (x) (cdr (reverse x))) connections))
	 (sinks			(map (lambda (x) (car (reverse x))) connections))

	 ;; A procedure for execution in the last `a' subprocess.
	 ;; It installs the write end of pipes in the target descriptors
	 ;; and closes the read ends.
	 ;;
	 (install-sources	(lambda ()
				  (for-each (lambda (sink) (%% %close sink)) sink-pipes)
				  (for-each (lambda (source targets)
					      (for-each (lambda (target)
							  (let ((source-clone 	(%% %dup source)))
							    (move->fdes source-clone target)))
							targets)
					      (%% %close source))
					    source-pipes
					    sources)))
	 ;; A procedure for execution in the first `b' subprocess.
	 ;; It installs the read end of pipes in the target descriptors
	 ;; and closes the write ends.
	 ;;
	 (install-sinks		(lambda ()
				  (for-each (lambda (source) (%% %close source)) source-pipes)
				  (for-each (lambda (sink target)
					      (move->fdes sink target))
					    sink-pipes
					    sinks)))
	 (fixed-a-thunks	(let* ((rev-thunk-list-a (reverse thunk-list-a))
				       (last-a		(car rev-thunk-list-a))
				       (rest-a		(cdr rev-thunk-list-a)))
				  (append (reverse rest-a)
					  (list (lambda ()
						  (install-sources)
						  (last-a))))))
	 (fixed-b-thunks	(let ((first-b		(car thunk-list-b))
				      (rest-b		(cdr thunk-list-b)))
				  (cons (lambda ()
					  (install-sinks)
					  (first-b))
					rest-b))))
    (for-each (lambda (d) (clean-descriptors d)) source-pipes)
    (for-each (lambda (d) (clean-descriptors d)) sink-pipes)
    (if (null? rest)
	(append fixed-a-thunks fixed-b-thunks)
	(append fixed-a-thunks
		(apply pipe+-join-thunks fixed-b-thunks rest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Running Job-controlled Processes
;;; 


;; (run-job return-on-stop? pf . redirects)
;; 
;; Run the job specified by process form `pf' and redirections
;; `redirects' as a foreground job.
;; 
;; Wait for the job to complete.   Return:
;; 
;; 	#t		- The job completed normally.
;; 	(abnormal-exit status0 status1 ...) - The job exited 
;;			  abnormally.   `statusN' is the wait status
;;			  of process N of the job.
;; 
;; If `return-on-stop?' is true, the return value may also be:
;; 
;; 	(stopped <job>)	- The subprocess appears to have been stopped.
;;			  `<job>' is the job object of the stopped
;;			  processes.
;; 
;; A job is considered "stopped" if each live subprocess is seen to
;; enter a `stopped' state after `wait-for-job' is called.  This
;; doesn't necessarily mean that all live subprocesses are currently
;; stopped: Some may have been subsequently continued; there is no way
;; to tell.
;; 
;; When `run-job' detects a stopped job, it makes the calling process'
;; group the foregound job.
;; 
(define-public (run-job return-on-stop? pf . redirects)
  (without-interrupts
   (lambda ()
     (let* ((clean-descriptors		(cleanup-descriptors-procedure))
	    (termination		(catch #t
					  (lambda ()
					    (let ((j	(job (apply background-job-thunks clean-descriptors pf redirects) :foreground)))
					      (set-job:process-form! j pf)
					      (clean-descriptors)
					      (wait-for-job j (if return-on-stop?
								  'WUNTRACED
								  #f))))
					  (lambda exception (set-foreground-group) (apply throw exception)))))
       (set-foreground-group)
       (if check-processes-mode
	   (check-job-status pf termination)
	   termination)))))


;; (continue-job :optional job in-background?)
;; 
;; Make `job' the foreground process and send it signal `SIGCONT'.
;; Wait for the job to complete or become stopped, then make 
;; the current process the foreground process.
;; 
;; If `in-background?' is true, signal the job but do not make it the
;; foreground job and do not wait for it.
;; 
(define-public (continue-job :optional job in-background?)
  (if in-background?
      (kill (or job (getpid)) 'SIGCONT)
      (catch #t
	(lambda ()
	  (set-foreground-group #f job)
	  (kill (or job (getpid)) 'SIGCONT)
	  (let ((termination	(wait-for-job job 'WUNTRACED)))
	    (set-foreground-group)
	    (if check-processes-mode
		(check-job-status termination)
		termination)))
	(lambda exception (set-foreground-group) (apply throw exception)))))


;; (background-job pf . redirects)
;; 
;; Start the job specified by process form `pf' and redirections
;; `redirects' as a background job.
;; 
;; Return the job object.
;; 
(define-public (background-job pf . redirects)
  (let* ((clean-descriptors	(cleanup-descriptors-procedure))
	 (thunks 		(apply background-job-thunks clean-descriptors pf redirects))
	 (j			(job thunks)))
    (set-job:process-form! j pf)
    (clean-descriptors)
    j))



;; (check-job-status process-form status)
;; 
;; `status' should be a job status reported by `wait-for-job'.
;; `process-form' should be the process form that created the job
;; whose status is reported.
;; 
;; If the job exited normally, return `status'.  Otherwise, throw an
;; exception.
;; 
(define-public (check-job-status pf status)
  (if (not (or (eq? #t status)
	       (eq? 'stopped status)))
      (error "job exited abnormaly" pf)
      status))


;; (run-job? process-form . redirects)
;; 
;; Run `process-form' in the manner of `run-job', but ignoring
;; the value of `check-processes-mode'.  If the process
;; exits normally, return true, otherwise, return #f.
;; 
(define-public (run-job? pf . redirects)
  (without-interrupts
   (lambda ()
     (let* ((clean-descriptors		(cleanup-descriptors-procedure))
	    (termination		(catch #t
					  (lambda ()
					    (let ((j	(job (apply background-job-thunks clean-descriptors pf redirects) :foreground)))
					      (set-job:process-form! j pf)
					      (clean-descriptors)
					      (wait-for-job j #f)))
					  (lambda exception (set-foreground-group) (apply throw exception)))))
       (set-foreground-group)
       (eq? #t termination)))))


;; (job-or . pfs)
;; 
;; Apply `run-job?' to each process form in succession until one of
;; the subprocesses exits normally (`run-process?' returns true).  If any
;; process exits normally, return true, otherwise #f.
;; 
(define-public (job-or . pfs)
  (and pfs
       (or (run-job? (car pfs))
	   (apply job-or (cdr pfs)))))


;; (job-and . pfs)
;; 
;; Apply `run-job?' to each process form in succession until one of
;; the subprocesses exits abnormally (`run-job?' returns #f).  If
;; all processes exit normally, return true, otherwise #f.
;; 
(define-public (job-and . pfs)
  (or (not pfs)
      (and (run-job? (car pfs))
	   (apply job-and (cdr pfs)))))


;; (start-job/port+job return pf . redirects)
;; 
;; Start the background job described by `pf' and `redirects' (see
;; `invoke').  Return two values:
;; 
;;	port job
;; 
;; where `port' is the readable end of a pipe connected to the
;; standard output of the subprocess, and `proc' is the process object
;; associated with the subprocess.
;; 
(define (start-job/port+job pf . redirects)
  (let* ((pipe		(%% %pipe))
	 (source	(cadr pipe))
	 (sink		(car pipe))
	 (job		(apply background-job pf `(= 1 ,source) redirects)))
    (%% %close source)
    (values sink job)))


;; (start-job/port pf . redirects)
;; 
;; Start the background job described by `pf' and `redirects' (see
;; `invoke').  Return  the readable end of a pipe connected to the
;; standard output of the subprocess.
;; 
(define-public (start-job/port pf . redirects)
  (call-with-values (lambda () (apply start-job/port+job pf redirects))
		    first-value))


;; (run-job/port+status pf . redirects)
;; 
;; Run the foreground job described by `pf' and `redirects' (see
;; `run-job').  Wait for the subprocess to complete and return
;; two values:
;; 
;;	port status
;; 
;; where `port' is open for reading from an anonymous temporary file
;; which contains the standard output of the subprocess, and `status'
;; is the process status returned for the subprocess by `wait'.
;; 
(define-public (run-job/port+status pf . redirects)
  (call-with-values temp-file-channel
		    (lambda (in out)
		      (let ((status (apply run-job #f pf `(= 1 ,out) redirects)))
			(%% %close out)
			(values in status)))))


;; (run-job/port pf . redirects)
;; 
;; Run  the foreground job described by `pf' and `redirects' (see
;; `run-job').  Wait for the subprocess to complete and return a port
;; open for reading from an anonymous temporary file which contains
;; the standard output of the subprocess.
;; 
(define-public (run-job/port pf . redirects)
  (call-with-values (lambda () (apply run-job/port+status pf redirects))
		    first-value))


;; (run-job/string pf . redirects)
;; 
;; Run the foreground job described by `pf' and `redirects' (see
;; `run-job').  Wait for the subprocess to complete and return a string
;; which contains the standard output of the subprocess.
;; 
(define-public (run-job/string pf . redirects)
  (fd->string (apply run-job/port pf redirects)))


;; (run-job/strings pf . redirects)
;; 
;; Run the foreground job described by `pf' and `redirects' (see
;; `invoke').  Wait for the subprocess to complete and return a list
;; of strings which contains the standard output of the subprocess,
;; divided into newline-terminated lines.
;; 
(define-public (run-job/strings pf . redirects)
  (string->lines (fd->string (apply run-job/port pf redirects))))


;; (run-job/sexp pf . redirects)
;; 
;; Run the foreground job described by `pf' and `redirects' (see
;; `invoke').  Wait for the subprocess to complete and return an
;; object which is the result of invoking `read' on the standard
;; output of the subprocess.
;; 
(define-public (run-job/sexp pf . redirects)
  (read (apply run-job/port pf redirects)))


;; (run-job/sexps pf . redirects)
;; 
;; Run the foreground job described by `pf' and `redirects' (see
;; `invoke').  Wait for the subprocess to complete and return an list
;; of objects which is the result of repeatedly invoking `read' on the
;; standard output of the subprocess.
;; 
(define-public (run-job/sexps pf . redirects)
  (let ((answer . (cons () ()))
	(port	(apply run-job/port pf redirects)))
    (let loop ((pos answer))
      (let ((r (read port)))
	(if (eof-object? r)
	    (cdr answer)
	    (begin
	      (set-cdr! answer (cons r ()))
	      (loop (cdr pos))))))))


;; (run-job-collecting: return fds pf . redirects)
;; 
;; Create as many anonymous temporary files as their are descriptor
;; numbers in the list `fds'.  Run the foreground job described by `pf',
;; redirecting output on the descriptors `fds' to the anonymous
;; temporary files (additionally performing the redirections specified
;; by `redirects' (see `invoke').  Wait for the subprocess to complete.
;; 
;; Invoke return with a first argument which is the exit status of the
;; subprocess, and remaining arguments which are input ports for the
;; temporary files (in the order of the list `fds').
;; 
(define-public (run-job-collecting: return ports pf . redirects)
  (let* ((temp-channels 	(map (lambda ign (call-with-values temp-file-channel cons)) ports))
	 (in-channels		(map car temp-channels))
	 (out-channels		(map cdr temp-channels))
	 (out-directs		(map (lambda (port channel) `(= ,port ,channel)) ports out-channels))
	 (all-redirects		(append out-directs redirects))
	 (status		(apply run-job #f pf all-redirects)))
    (for-each (lambda (p) (%% %close p)) out-channels)
    (apply return status in-channels)))


;; (run-job-collecting fds pf . redirects)
;; 
;; Create as many anonymous temporary files as their are descriptor
;; numbers in the list `fds'.  Run the foreground job described by `pf',
;; redirecting output on the descriptors `fds' to the anonymous
;; temporary files (additionally performing the redirections specified
;; by `redirects' (see `invoke').  Wait for the subprocess to complete.
;; 
;; Return multiple values.  The first value is the exit status of the
;; subprocess, and remaining values are input ports for the temporary
;; files (in the order of the list `fds').
;; 
(define-public (run-job-collecting ports pf . redirects)
  (apply run-job-collecting: values ports pf redirects))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shorthand
;;; 

;; (! pf . redirects)
;; 
;; Create the job specified by process for `pf' and redirections
;; `redirects' as a foreground process.  Return if the job completes
;; or stops.
;; 
;; If the process might stop, it is advisable that the terminal under
;; which this process is running be configured to automatically stop
;; background processes that attempt terminal I/O.  On a BSD-type system,
;; this can be accomplished with:
;; 
;; 	(! '(stty tostop))
;; 
;; 
(define-public (! pf . redirects)
  (apply run-job #t pf redirects))


;; (& pf . redirects)
;; 
;; Start the job specified by process for `pf' and redirections
;; `redirects' as a background process.  Return if the job object.
;; 
;; If the process might attempt terminal I/O, it is advisable that the
;; terminal under which this process is running be configured to
;; automatically stop background processes that attempt terminal I/O.
;; On a BSD-type system, this can be accomplished with:
;; 
;; 	(! '(stty tostop))
;; 
;; 
(define-public (& pf . redirects)
  (apply background-job pf redirects))


;;; temporary

(define-public (use-cwd-prompt)
  (set! the-prompt-string
	(lambda ()
	  (string-append (%getcwd) " systas> "))))



