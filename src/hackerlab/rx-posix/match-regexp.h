/* match-regexp.h - low-level functions for comparing a string to a regexp
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__RX_POSIX__MATCH_REGEXP_H
#define INCLUDE__RX_POSIX__MATCH_REGEXP_H



#include "hackerlab/machine/types.h"
#include "hackerlab/rx/tree.h"



/* rx_off_t	An internal type used by the Posix interface as `regoff_t'.
 * 
 * 		(`regoff_t' is required by Posix.2) Used to represent
 * 		offsets to substrings within a string matched by
 * 		`regexec'.  `regoff_t' is a signed arithmetic type
 * 		that can hold the largest value that can be stored in
 * 		either `off_t' or `long'.
 * 
 */
typedef long rx_off_t;

struct rx_registers
{
  rx_off_t rm_so; 		/* Byte offset from string's start to substring's start.  */
  rx_off_t rm_eo;  		/* Byte offset from string's start to substring's end.  */
  int final_tag;		/* In register 0 of an array of registers, this field
				 * is set to the state label of the last superstate encountered
				 * during a match.
				 */
};


/* struct rx_context_rules
 * 
 * An argument to `rx_basic_make_solutions' used to specify
 * the behavior of `^', `$', and backreferences.
 */
struct rx_context_rules
{
  t_uchar newline_anchor;	/* If true, an anchor at a newline matches.*/
  t_uchar not_bol;	/* If set, the anchors ('^' and '$') don't */
  t_uchar not_eol;	/*     match at the ends of the string.  */
  t_uchar case_indep;
};

/* struct rx_solutions;
 * 
 * A lazilly computed stream of solutions for an expression or
 * subexpression compared to a string.
 */
struct rx_solutions;


/************************************************************************
 *(paragraphs)
 */

/*(c rx_vmfn :category type)
 * typedef int (*rx_vmfn) (void * closure,
 *			   const t_uchar ** burst,
 *			   rx_off_t * len,
 *			   rx_off_t * offset,
 *			   rx_off_t start, rx_off_t end, rx_off_t need);
 * 
 * An `rx_vmfn' is passed to `rx_make_solutions' and used by
 * `rx_next_solution' to access the input string being compared to a
 * regexp.  The purpose of this function is to permit the calling
 * program to only keep part of the input string in memory, and to
 * keep the input string in non-contiguous regions in memory.
 *
 * When called, `rx_vmfn' is passed:
 * 
 * `closure' -- the opaque parameter passed to `rx_make_solutions'.
 *
 * `burst' -- an output parameter that will point to part of the input
 * string.  The pointer returned in this parameter must remain valid
 * until the next call to `rx_vmfn' or `rx_contextfn' for the same
 * call to `rx_next_solution'.
 *
 * `len' -- an output parameter; the length of the string returned in
 * `*burst'.
 *
 * `offset' -- an output parameter; the position of `*burst' within
 * the input string (e.g., 0 for the beginning of the input string, 
 * 9 if `*burst' is the tenth character of the input).
 *
 * `start' through `end' are the input positions requested by Rx.
 * `need' is the input position that must be returned.  `rx_vmfn' is
 * permitted to return any substring of the input that contains
 * `need', but the performance of Rx itself is best if returns a
 * substring containing at least the entire range from `start' to
 * `end'.  The precise performance implications of a particular
 * implementation of `rx_vmfn' are application specific.
 *
 * Note that Rx may access parts of the string out of order and may
 * visit the same part of the string more than once.
 *
 * This function should return 0 on success, and some other value on 
 * error.
 */
typedef int (*rx_vmfn) (void * closure,
			const t_uchar ** burst,
			rx_off_t * len,
			rx_off_t * offset,
			rx_off_t start, rx_off_t end, rx_off_t need);



/*(c rx_contextfn :category type)
 * typedef int (*rx_contextfn) (void * closure,
 * 			        struct rx_exp_node * node,
 * 			        rx_off_t start, rx_off_t end,
 * 			        struct rx_registers * regs);
 * 
 * An `rx_contextfn' is passed to `rx_make_solutions' and used by
 * `rx_next_solution' to access the input string being compared to a
 * regexp.  The purpose of this function is to permit the calling
 * program to only keep part of the input string in memory, and to
 * keep the input string in non-contiguous regions in memory.
 *
 * `rx_contextfn' is responsible for evaluating subexpressions
 * which are anchors (`^' and `$') and subexpressions which are
 * backreferences (e.g. `\1').  
 * 
 * When called, `rx_contextfn' is passed:
 * 
 * `closure' -- the opaque parameter passed to `rx_make_solutions'.
 *
 * `node' -- The regexp syntax tree node of the expression to match.
 *
 * `start' and `end' -- the positions within the input string (from 
 * `start' to `end-1') that must match `node'.
 *
 * `reg' -- subexpression position information for preceeding
 * subexpressions.  This is used for backreferences.  Note that if a
 * previous subexpression was not matched, its starting and ending
 * positions will be recorded as -1.
 *
 * This function should return 1 if the subexpression matches, 0
 * otherwise.
 */
typedef int (*rx_contextfn) (void * closure,
			     struct rx_exp_node * node,
			     rx_off_t start, rx_off_t end,
			     struct rx_registers * regs);


/* automatically generated __STDC__ prototypes */
extern struct rx_solutions * rx_basic_make_solutions (struct rx_registers * regs,
						      struct rx_exp_node * expression,
						      struct rx_exp_node ** subexps,
						      int nsub,
						      rx_off_t start,
						      rx_off_t end,
						      struct rx_context_rules * rules,
						      const t_uchar * str,
						      int small_p);
extern void rx_basic_free_solutions (struct rx_solutions * solns);
extern int rx_next_solution (struct rx_solutions * solns);
extern int rx_solutions_final_tag (struct rx_solutions * solns);
extern struct rx_solutions * rx_make_solutions (struct rx_registers * regs,
						int cset_size,
						struct rx_exp_node * expression,
						struct rx_exp_node ** subexps,
						int nsub,
						rx_off_t start,
						rx_off_t end,
						int interval_x,
						rx_vmfn vmfn,
						rx_contextfn contextfn,
						void * closure,
						int small_p,
						int certainly_fits,
						int certain_final_tag);
extern void * rx_solutions_closure (struct rx_solutions * solns);
extern void rx_free_solutions (struct rx_solutions * solns);
extern int rx_simplify_rexp (struct rx_exp_node ** answer,
			     int cset_size,
			     struct rx_exp_node * node,
			     struct rx_exp_node ** subexps);
extern int rx_analyze_rexp (struct rx_exp_node *** subexps,
			    size_t * re_nsub,
			    struct rx_exp_node * node);
#endif  /* INCLUDE__RX_POSIX__MATCH_REGEXP_H */
