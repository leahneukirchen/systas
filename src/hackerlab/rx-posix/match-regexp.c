/* match-regexp.c - functions for comparing a string to a regexp
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


/* #define TRACE */

#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/str.h"
#include "hackerlab/bitsets/bits.h"
#include "hackerlab/rx/nfa-cache.h"
#include "hackerlab/rx/dfa.h"
#include "hackerlab/rx/escape.h"
#ifdef TRACE
#include "hackerlab/vu/safe.h"
#include "hackerlab/rx/dbug.h"

int rx_trace = 0;
int rx_depth_limit = 0;

#endif
#include "hackerlab/rx-posix/errnorx.h"
#include "hackerlab/rx-posix/match-regexp.h"



static unsigned long solns_allocated;
static unsigned long solns_freed;


#define max0(N)		(((N) >= 0) ? (N) : 0)


/* __STDC__ prototypes for static functions */
static int rx_str_vmfn (void * closure,
			const t_uchar ** burstp,
			rx_off_t * lenp,
			rx_off_t * offsetp,
			rx_off_t start,
			rx_off_t end,
			rx_off_t need);
static int rx_str_contextfn (void * closure,
			     struct rx_exp_node * node,
			     rx_off_t start,
			     rx_off_t end,
			     struct rx_registers * regs);
static int rx_solution_fit_cset_p (struct rx_solutions * solns);
static int rx_solution_fit_str_p (struct rx_solutions * solns);
static int rx_solution_fits (struct rx_solutions * solns);
static int rx_dfa_longest_counting (int * label,
				    size_t * match_len,
				    size_t * matches_count,
				    struct rx_dfa * dfa,
				    const t_uchar * str,
				    size_t len);
static int rx_longest_split_guess (int * label, struct rx_solutions * solns);
static int save_nested_registers (struct rx_solutions * solns);
static void restore_nested_registers (struct rx_solutions * solns);
static int rx_next_solution_internal (struct rx_solutions * solns, jmp_buf * err_escape, int depth);



/************************************************************************
 *(h0 "Regexp Matching" 
 *    :includes ("rx-posix/match-regexp.h"))
 * 
 * 
 * In xref:"Regexp Expression Trees", we saw how to build expression
 * trees for regexps and elsewhere how to convert those regexps which
 * are regular expressions to DFA.  In xref:"DFA String Comparisons"
 * we saw DFA-based functions that can compare a string to a regular
 * expression.
 * 
 * But what about those regexps which are not regular expressions?  So
 * far, except for the quasi-standard functions `regexec' and
 * `regnexec', we have no way to match non-regular regexps.
 * 
 * The functions in this chapter implement regexp matching.  The
 * interface is very detailed, but there are only a few functions and
 * they are conceptually simple.
 * 
 */
/*(menu)
 */

/************************************************************************
 *(h1 "An Overview of the Regexp Matching Interface")
 * 
 * Given a regexp, and an input string, a regexp comparison function
 * should return a value that indicates whether or not the string
 * matches, and an array of values filled in with the substring
 * positions of elements of the input string that matched the
 * parenthesized subexpressions in the match.  (For example, see
 * xref:"regexec".)  We start out intending to build a regexp
 * comparison function that fits the preceeding description and make
 * two observations.
 *
 * First, the function needs to be recursive because we only know how
 * to check whether or not some kinds of regexp match by checking
 * whether their subexpressions match.
 * 
 * Second, the function needs to return more than one set of return
 * values because a regexp can often match a string in more than one
 * way -- returning a different set of values for each way of
 * matching.  To find the best overall match for a regexp, we may have
 * to consider more than one way of matching the subexpressions of
 * that regexp.  In fact, the Posix standard for regexps essentially
 * mandates that we perform a potentially exhaustive search through
 * the possible ways of matching subexpressions.  So, our recursive
 * comparison function has to be able to find all of the different
 * ways of matching a particular regexp and string to facilitate that
 * search.
 *
 * Fortunately, we are able to order the search through the set of
 * subexpressions in such a way that we will often find the best
 * answer (and know it is the best answer) early.  So while our
 * recursive comparison function has to be able to return all possible
 * solutions, it should be free to return them in the order we will
 * search them, and to only actually construct a particular solution
 * if it is actually required during a search.  We can usually save
 * time by not building solutions that we don't need to search.
 * 
 * The functions in this chapter implement such a recursive comparison
 * function.  Given a regexp and a string, the functions return a
 * lazilly-constructed stream of all of the possible ways of matching
 * the regexp to the string.  The stream is sorted; in the solution
 * stream of a regexp subexpression, the best match, according to the
 * rules mandated by Posix, is always the earliest match in the stream
 * that also satisfies the constraints of the overall match of the
 * containing expressions.
 *
 * You build one of these streams of subexpression solutions by calling
 * `rx_make_solutions', passing the regexp and input string.  You
 * read off elements of the stream by calling `rx_next_solution' which
 * actually does the work of computing the next solution in the list.
 *
 * Implementing a function like `regexec' or `regnexec' is easy.  
 * `regexec' iterates over each possible starting point for a match:
 *
 *     for (start_pos = 0; start_pos < string_length; ++start_pos)
 * 
 * At each starting position, `regexec' iterates through each possible
 * ending position:
 *
 *	 for (end_pos = string_length; end_pos >= start_pos; --end_pos)
 *
 * For each possible starting and ending position, `regexec'
 * constructs a stream of regexp solutions for that substring (by
 * calling `rx_make_solutions').  It tries to read the first solution
 * on that stream by calling `rx_next_solution'.  If the stream is not
 * empty, the first solution in the stream is the best possible
 * solution, and it is returned from `regexec'.
 *
 * When `regexec' calls `rx_next_solution' to find out if the stream
 * of solutions is empty, more streams of solutions may be created for
 * the subexpressions of the overall regexp.  Those recursively
 * created streams may have to be searched to find an overall
 * solution.  Even though `regexec' only reads one solution from the
 * stream it creates, the recursive calls to `rx_next_solution' may
 * read many solutions from the streams they create.
 *
 * As just described, you might think `regexec' and `regnexec' do a
 * lot of work (running so many separate regexp comparisons).  You
 * would be right.  Consequently, the real-world implementations of
 * `regexec' and `regnexec' employ several tricks to reduce the number
 * of regexp comparisons actually performed -- they try to avoid
 * calling the functions in this chapter.  In addition, the functions
 * in this chapter employ several tricks to avoid searching
 * recursively created streams of solutions.  Still, in the general
 * case, functions like `regexec' use the interface described here in
 * the computationally expensive way described above.  (The exact
 * complexity of a search depends on the particular regexp but in the
 * worst cases, it is a polynomial function (of high degree) of the
 * length of the string being searched.)
 * 
 */

/************************************************************************
 *(h1 "Regexp Match Function Data Structures")
 * 
 */

/*(c #s"struct rx_solutions" :category type)
 * struct rx_solutions;
 * 
 * This opaque structure type represents the stream of solutions for
 * matching a particular regexp against a particular string.
 */
struct rx_solutions
{
  int small_p;
  int step;

  int cset_size;
  struct rx_exp_node * exp;
  struct rx_exp_node ** subexps;
  int nsub;
  struct rx_registers * regs;

  rx_off_t start;
  rx_off_t end;

  rx_vmfn vmfn;
  rx_contextfn contextfn;
  void * closure;

  struct rx_unfa * dfa;
  struct rx_dfa match_engine;

  int certainly_fits;		/* we know in advance that rx_solution_fits returns 1 */
  int certain_final_tag;

  int no_fancy_guessing;
  struct rx_unfa * left_dfa;
  struct rx_dfa left_match_engine;

  rx_off_t split_guess;
  struct rx_solutions * left;
  struct rx_solutions * right;

  int interval_x;
  int interval_from;
  int interval_to;
  rx_off_t * saved_rm_so;
  rx_off_t * saved_rm_eo;
  int final_tag;
};

static struct rx_solutions no_solutions;

#if 0
/*(c #s"struct rx_registers" :category type)
 * struct rx_registers;
 * 
 * This structure type holds information about the match position
 * of one regexp subexpression in a (potentially) larger string.
 * It includes at least these fields:
 *
 insert*/
struct rx_registers
{
  rx_off_t rm_so;		/* The start of the match. */
  rx_off_t rm_eo;		/* The end of the match. */
};
/*end-insert
 */
#endif


/************************************************************************
 * Low Level Support for Basic String Matching
 * 
 */

struct rx_str_closure
{
  struct rx_context_rules rules;
  const t_uchar * str;
  rx_off_t len;
};


static int
rx_str_vmfn (void * closure,
	     const t_uchar ** burstp,
	     rx_off_t * lenp,
	     rx_off_t * offsetp,
	     rx_off_t start,
	     rx_off_t end,
	     rx_off_t need)
{
  struct rx_str_closure * strc;

  strc = (struct rx_str_closure *)closure;

  if (   (need < 0)
      || (need > strc->len))
    return 0;

  *burstp = strc->str;
  *lenp = strc->len;
  *offsetp = 0;
  return 0;
}


static int
rx_str_contextfn (void * closure,
		  struct rx_exp_node * node,
		  rx_off_t start,
		  rx_off_t end,
		  struct rx_registers * regs)
{
  struct rx_str_closure * strc;

  strc = (struct rx_str_closure *)closure;

  switch (node->intval)
    {
    case '1': case '2': case '3': case '4': case '5':
    case '6': case '7': case '8': case '9':
      {
	int cmp;
	int regn;

	regn = node->intval - '0';
	if (   (regs[regn].rm_so == -1)
	    || ((end - start) != (regs[regn].rm_eo - regs[regn].rm_so)))
	  return 0;
	else
	  {
	    if (strc->rules.case_indep)
	      cmp = str_casecmp_n ((t_uchar *)strc->str + start,
				   end - start,
				   (t_uchar *)strc->str + regs[regn].rm_so,
				   end - start);
	    else
	      cmp = str_cmp_n ((t_uchar *)strc->str + start,
			       end - start,
			       (t_uchar *)strc->str + regs[regn].rm_so,
			       end - start);

	    return (!cmp
		    ? 1
		    : 0);
	  }
      }

    case '^':
      {
	return ((   (start == end)
		 && (   ((start == 0) && !strc->rules.not_bol)
		     || (   (start > 0)
			 && strc->rules.newline_anchor
			 && (strc->str[start - 1] == '\n'))))
		? 1
		: 0);
      }

    case '$':
      {
	return ((   (start == end)
		 && (   ((start == strc->len) && !strc->rules.not_eol)
		     || (   (start < strc->len)
			 && strc->rules.newline_anchor
			 && (strc->str[start] == '\n'))))
		? 1
		: 0);
      }

    default:
      while (1)
	panic ("unrecognized context function in rx_str_contextfn");
    }
}


/************************************************************************
 *(h1 "Building and Freeing a Stream of Solutions -- The Simplified Common Case")
 * 
 */

#if 0
/*(c rx_basic_make_solutions)
 * struct rx_solutions * 
 * rx_basic_make_solutions (struct rx_registers * regs,
 *			    struct rx_exp_node * expression,
 *			    struct rx_exp_node ** subexps,
 *			    int nsub,
 *			    rx_off_t start,
 *			    rx_off_t end,
 *			    struct rx_context_rules * rules,
 *			    const t_uchar * str,
 *			    int small_p);
 * 
 * Construct a stream of solutions for matching the regexp
 * `expression' against the substring of `str' beginning at `start'
 * and ending at `end'.
 * 
 * Part of the output of stream, the positions of sub-expressions in a
 * solution, will be stored in `regs' each time the next solution is
 * read using `rx_next_solution'.
 * 
 * The parameter `context_rules' contains flags which effect the
 * meaning of some regexp operators.  
 * 
 insert*/
struct rx_context_rules
{
  t_uchar newline_anchor;
  /*   	If set, an anchor at a newline matches.*/

  t_uchar not_bol;
  t_uchar not_eol;
  /*	If set, the anchors do not match the beginning and end
	of the complete input string.*/


  t_uchar case_indep;
  /*	If set, all string comparisons should be case independent.*/
};
/*end-insert
 *
 * You must apply `rx_analyze_rexp' to the regexp `expression' before
 * calling his function.  `rx_analyze_rexp' computes recursively
 * defined properties of the subexpressions of the expression and
 * records those properties in the expression tree. During a match,
 * `rx_next_solution' examines those properties to decide how each
 * subexpression should be treated.
 *
 * `rx_analyze_rexp' will return to you an array of the parenthesized
 * subexpressions of `expression' and tell you the total number of
 * subexpressions.  You must pass this information along to
 * `rx_basic_make_solutions' as the `subexps' and `nsub' parameters.
 * 
 * `rx_analyze_rexp' will also fill in the `small_advised_p' field of
 * `expression'.  You can pass this along to `rx_basic_make_solutions'
 * as the parameter `small_p'.  If not 0, `small_p' advises
 * `rx_basic_make_solutions' that the regexp is small and simple and
 * therefore not worthy of several high-latency optimizations that
 * `rx_next_solution' is capable of.  Setting this parameter correctly
 * can improve performance, setting it incorrectly can ruin
 * performance, and `rx_analyze_rexp' can only approximately compute
 * whether or not to set this parameter (it makes a safe guess).  The
 * safest value to pass is 0, which activates the high-latency
 * optimizations, sometimes (slightly) slowing down matches that
 * aren't particularly expensive while (substantially) speeding up
 * some matches that are particularly expensive.
 *
 * `rx_basic_make_solutions' requires that the entire input string be
 * in contiguous memory at one time.  Another function,
 * `rx_make_solutions', can be used if the string is not contiguous in
 * memory or is not all in memory at one time.
 */
#endif
struct rx_solutions *
rx_basic_make_solutions (struct rx_registers * regs,
			 struct rx_exp_node * expression,
			 struct rx_exp_node ** subexps,
			 int nsub,
			 rx_off_t start,
			 rx_off_t end,
			 struct rx_context_rules * rules,
			 const t_uchar * str,
			 int small_p)
{
  struct rx_solutions * solns;
  struct rx_str_closure * closure;

  if (   expression
      && (expression->len >= 0)
      && (expression->len != (end - start)))
    return &no_solutions;

  closure = (struct rx_str_closure *)rx_nfa_cache_malloc (sizeof (*closure));
  if (!closure)
    return 0;
  
  closure->str = str;
  closure->len = end;
  closure->rules = *rules;
  solns =  rx_make_solutions (regs,
			      256, expression, subexps, nsub,
			      start, end, 0,
			      rx_str_vmfn, rx_str_contextfn, (void *)closure, small_p, 0, 0);
  if (!solns)
    {
      rx_nfa_cache_free ((void *)closure);
      return 0;
    }
  return solns;
}


/*(c rx_basic_free_solutions)
 * void rx_basic_free_solutions (struct rx_solutions * solns);
 * 
 * Release all storage held by a stream of regexp solutions allocated
 * by `rx_basic_make_solutions'.
 */
void
rx_basic_free_solutions (struct rx_solutions * solns)
{
  if (solns == 0)
    return;

  if (solns == &no_solutions)
    return;

  rx_nfa_cache_free (rx_solutions_closure (solns));
  rx_free_solutions (solns);
}



/************************************************************************
 * Low Level Support for rx_next_solution
 * 
 * 
 * 
 */

/* static int rx_solution_fit_cset_p (struct rx_solutions * solns);
 * 
 * The expression is a character set node and the current substring is
 * one character long.  Compare the two and return 1 if they match, 0
 * otherwise.
 */
static int
rx_solution_fit_cset_p (struct rx_solutions * solns)
{
  int current_pos;
  const t_uchar * burst;
  rx_off_t burst_addr;
  rx_off_t burst_len;
  rx_off_t rel_pos_in_burst;
  int vmstat;
  const t_uchar * pos;

  current_pos = solns->start;
  vmstat = solns->vmfn (solns->closure,
			&burst, &burst_len, &burst_addr,
			current_pos, current_pos + 1, current_pos);

  if (vmstat)
    return vmstat;

  rel_pos_in_burst = current_pos - burst_addr;
  pos = burst + rel_pos_in_burst;
  return !!bits_is_member (solns->exp->cset, *pos);
}


/* static int rx_solution_fit_str_p (struct rx_solutions * solns);
 * 
 * The expression is a string node and the current substring is the
 * same length.  Compare the two and return 1 if they match, 0
 * otherwise.
 */
static int
rx_solution_fit_str_p (struct rx_solutions * solns)
{
  rx_off_t current_pos;
  const t_uchar * burst;
  rx_off_t burst_addr;
  rx_off_t burst_len;
  rx_off_t burst_end_addr;
  rx_off_t rel_pos_in_burst;
  int vmstat;
  rx_off_t count;
  t_uchar * key;


  current_pos = solns->start;
  count = solns->exp->str_len;
  key = (t_uchar *)solns->exp->str;

 next_burst:
  vmstat = solns->vmfn (solns->closure,
			&burst, &burst_len, &burst_addr,
			current_pos, solns->end,
			current_pos);

  if (vmstat)
    return vmstat;

  rel_pos_in_burst = current_pos - burst_addr;
  burst_end_addr = burst_addr + burst_len;

  {
    const t_uchar * pos;

    pos = burst + rel_pos_in_burst;

    if (burst_end_addr >= solns->end)
      {
	while (count)
	  {
	    if (rx_poll)
	      (*rx_poll)();
	    if (*pos != *key)
	      return 0;
	    ++pos;
	    ++key;
	    --count;
	  }
	return 1;
      }
    else
      {
	rx_off_t part_count;
	rx_off_t part_count_init;

	part_count_init = burst_len - rel_pos_in_burst;
	part_count = part_count_init;
	while (part_count)
	  {
	    if (rx_poll)
	      (*rx_poll)();
	    if (*pos != *key)
	      return 0;
	    ++pos;
	    ++key;
	    --part_count;
	  }
	count -= part_count_init;
	current_pos += burst_len - rel_pos_in_burst;
	goto next_burst;
      }
  }
}


/* static int rx_solution_fits (struct rx_solutions * solns);
 * 
 * Compare a DFA which describes a superset of the regexp expression
 * to the current substring and return 1 if they match, 0 otherwise.
 * That the DFA matches is a necessary but not sufficient condition of
 * the regexp expression itself matching.  This test can be used to
 * avoid a more expensive regexp comparison.  Return -1 for ESPACE.
 */
static int
rx_solution_fits (struct rx_solutions * solns)
{
  const t_uchar * burst;
  rx_off_t burst_addr;
  rx_off_t burst_len;
  rx_off_t burst_end_addr;
  rx_off_t rel_pos_in_burst;
  int vmstat;
  rx_off_t current_pos;
	  
  if (solns->certainly_fits)
    {
      solns->match_engine.final_tag = solns->certain_final_tag;
      return 1;
    }

  current_pos = solns->start;

 next_burst:
  vmstat = solns->vmfn (solns->closure,
			&burst, &burst_len, &burst_addr,
			current_pos, solns->end,
			current_pos);

  if (vmstat)
    return vmstat;

  rel_pos_in_burst = current_pos - burst_addr;
  burst_end_addr = burst_addr + burst_len;

  if (burst_end_addr >= solns->end)
    {
      int fit_label;

      if (rx_dfa_fits (&fit_label,
		       &solns->match_engine,
		       burst + rel_pos_in_burst,
		       solns->end - current_pos))
	return -1;		/* espace */
      return fit_label;
    }
  else
    {
      int fit_status;
      fit_status = rx_dfa_advance (&solns->match_engine,
				   burst + rel_pos_in_burst,
				   burst_len - rel_pos_in_burst);
      if (fit_status < 0)
	{
	  return -1;
	}
      else if (fit_status)
	{
	  current_pos += burst_len - rel_pos_in_burst;
	  goto next_burst;
	}
      else
	return 0;
    }
}


static int
rx_dfa_longest_counting (int * label,
			 size_t * match_len,
			 size_t * matches_count,
			 struct rx_dfa * dfa,
			 const t_uchar * str,
			 size_t len)
{
  size_t pos;
  size_t n_matches;
  size_t best_len;
  int best_label;

  pos = 0;
  n_matches = 0;
  best_len = 0;
  best_label = rx_dfa_tag (dfa);
  
  if (best_label)
    {
      n_matches = 1;
    }
  
  while (pos < len)
    {
      int adv_stat;
      size_t adv_amt;
    
      adv_stat = rx_dfa_advance_to_final (&adv_amt, dfa, str + pos, len - pos);

      if (adv_stat < 0)
	return -1;

      if (!adv_stat)
	break;

      pos += adv_amt;
      if (rx_dfa_tag (dfa))
	{
	  best_len = pos;
	  ++n_matches;
	  best_label = rx_dfa_tag (dfa);
	}
    }

  *label = best_label;
  *match_len = best_len;
  *matches_count = n_matches;
  return 0;
}


/* adjust split_guess downward to the nearest plausible
 * position.  Return 0 if none exists, 1 otherwise.
 */
static int
rx_longest_split_guess (int * label, struct rx_solutions * solns)
{
  const t_uchar * burst;
  rx_off_t burst_addr;
  rx_off_t burst_len;
  rx_off_t burst_end_addr;
  rx_off_t rel_pos_in_burst;
  int vmstat;
  rx_off_t current_pos;
  size_t n_matches;
  rx_off_t best_guess;
  int best_label;
	  
  if (solns->no_fancy_guessing)
    {
      *label = 0;
      return 1;
    }

  current_pos = solns->start;
  n_matches = 0;
  best_guess = -1;
  best_label = 0;

  if (rx_dfa_goto_start_superstate (&solns->left_match_engine, 1))
    return -1;

  while (1)
    {
      int fit_label;
      size_t matches_here;
      size_t len_here;

      vmstat = solns->vmfn (solns->closure,
			    &burst, &burst_len, &burst_addr,
			    current_pos, solns->split_guess,
			    current_pos);

      if (vmstat)
	{
	  rx_clear_dfa_state (&solns->left_match_engine);
	  return vmstat;
	}

      rel_pos_in_burst = current_pos - burst_addr;
      burst_end_addr = burst_addr + burst_len;

      if (rx_dfa_longest_counting (&fit_label,
				   &len_here,
				   &matches_here,
				   &solns->left_match_engine,
				   burst + rel_pos_in_burst,
				   ((burst_end_addr >= solns->split_guess)
				    ? solns->split_guess - current_pos
				    : burst_end_addr - current_pos)))
	{
	  rx_clear_dfa_state (&solns->left_match_engine);
	  return -1;
	}
      
      if (fit_label)
	{
	  n_matches += matches_here;
	  best_guess = len_here + rel_pos_in_burst;
	  best_label = fit_label;
	}

      if (!fit_label || (burst_end_addr >= solns->split_guess))
	{
	  if (!n_matches)
	    {
	      rx_clear_dfa_state (&solns->left_match_engine);
	      return 0;
	    }

	  solns->split_guess = best_guess;
	  *label = best_label;
	  if (n_matches > (best_guess * 2))
	    solns->no_fancy_guessing = 1;

	  rx_clear_dfa_state (&solns->left_match_engine);
	  return 1;
	}

      current_pos += burst_len - rel_pos_in_burst;
    }
}



/************************************************************************
 *(h1 "Reading Elements From a Stream of Solutions")
 * 
 */

/* Save the values of registers for reporting parentheses
 * enclosed in the expression solns->exp.  Initialize
 * those registers to -1.
 * 
 * These are stored in the `solns' structure.  Only one set of register
 * values can be saved, per `solns', at a time.
 */
static int
save_nested_registers (struct rx_solutions * solns)
{
  int first_subexp;
  int last_subexp;
  int n_subexps;
  int x;

  /* Save the value of the set of numbered
   * parentheses in the tree rooted at this
   * expression and initialized them to -1.  If
   * this expression does not match, they will
   * be restored.  If it does match, they get values
   * from that match and not from previous matches.
   */

  first_subexp = solns->exp->min_enclosed_paren;
  last_subexp = solns->exp->max_enclosed_paren;
  n_subexps = 1 + (last_subexp - first_subexp);

  if (!solns->saved_rm_so)
    {
      solns->saved_rm_so = (rx_off_t *)rx_nfa_cache_malloc (sizeof (rx_off_t) * n_subexps);
      solns->saved_rm_eo = (rx_off_t *)rx_nfa_cache_malloc (sizeof (rx_off_t) * n_subexps);
      if (!solns->saved_rm_so || !solns->saved_rm_eo)
	{
	  rx_nfa_cache_free ((void *)solns->saved_rm_so);
	  rx_nfa_cache_free ((void *)solns->saved_rm_eo);
	  return -1;
	}
    }

  for (x = 0; x < n_subexps; ++x)
    {
      solns->saved_rm_so[x] = solns->regs[x + first_subexp].rm_so;
      solns->saved_rm_eo[x] = solns->regs[x + first_subexp].rm_eo;
      solns->regs[x + first_subexp].rm_so = -1;
      solns->regs[x + first_subexp].rm_eo = -1;
    }
  return 0;
}

/* Restore register values saved by `save_nested_registers'.
 * 
 * This doesn't erase the stored values.
 */
static void
restore_nested_registers (struct rx_solutions * solns)
{
  int first_subexp;
  int last_subexp;
  int n_subexps;
  int x;

  first_subexp = solns->exp->min_enclosed_paren;
  last_subexp = solns->exp->max_enclosed_paren;
  n_subexps = 1 + (last_subexp - first_subexp);

  for (x = 0; x < n_subexps; ++x)
    {
      solns->regs[x + first_subexp].rm_so = solns->saved_rm_so[x];
      solns->regs[x + first_subexp].rm_eo = solns->saved_rm_eo[x];
    }
}

/*(c rx_next_solution)
 * int rx_next_solution (struct rx_solutions * solns);
 * 
 * If there are no more solutions in the stream of regexp solutions
 * `solns', return 0.  Otherwise, advance the stream to the next
 * solution and return 1.  Return -1 for ESPACE.
 *
 * Advancing the stream means filling in the `regs' structure that was
 * passed to `rx_basic_make_solutions' or `rx_make_solutions' and
 * establishing the value that will be returned by subsequent calls to
 * `rx_solutions_final_tag'.
 *
 * Solutions are returned in order from best to worst according to the
 * leftmost-longest rule (see xref:"Describing Regexps Formally").
 * 
 * It is possible to asynchronously abort a call to this function.
 * See xref:"Exiting Long-Running Matches".
 */
int
rx_next_solution (struct rx_solutions * solns)
{
  jmp_buf err_escape;

  if (setjmp (err_escape))
    {
      return -1;
    }
  return rx_next_solution_internal (solns, &err_escape, 0);
}


static int
rx_next_solution_internal (struct rx_solutions * solns, jmp_buf * err_escape, int depth)
{
#ifdef TRACE
  if (rx_trace && (!rx_depth_limit || (depth < rx_depth_limit)))
    {
      safe_printfmt (2, "%*s================\n", depth, "");
      safe_printfmt (2, "%*sstep: %d\n", depth, "", solns->step);
      safe_printfmt (2, "%*s----------------\n", depth, "");
      safe_printfmt (2, "%*sexpression:\n", depth, "");
      safe_printfmt (2, "%*s", depth, "");
      rx_unparse_print_rexp (2, 256, solns->exp);
      safe_printfmt (2, "\n");
      safe_printfmt (2, "%*s----------------\n", depth, "");
      safe_printfmt (2, "%*sstring:\n", depth, "");
      {
	const t_uchar * burst;
	rx_off_t burst_addr;
	rx_off_t burst_len;
	rx_off_t rel_pos_in_burst;
	int vmstat;
	
	vmstat = solns->vmfn (solns->closure,
			      &burst, &burst_len, &burst_addr,
			      solns->start, solns->end, solns->start);
	
	if (vmstat)
	  panic ("vmstat failure in trace");
	
	rel_pos_in_burst = solns->start - burst_addr;
	safe_printfmt (2, "%*s%.*s\n", depth, "",
		       ((solns->end - solns->start) <= (burst_len - rel_pos_in_burst)
			? (solns->end - solns->start)
			: (burst_len - rel_pos_in_burst)),
		       burst + rel_pos_in_burst);
      }
      safe_printfmt (2, "%*s----------------\n", depth, "");
    }
#endif

#define KNOWN_LENGTH(EXP)	(!(EXP) \
				 ? 0 \
				 : ((EXP)->len >= 0 \
				    ? (EXP)->len \
				    : ((((EXP)->type == r_context) && ((EXP)->intval >= '0') && ((EXP)->intval <= '9')) \
				       ? ((solns->regs[(EXP)->intval - '0'].rm_so >= 0) \
					  ? (solns->regs[(EXP)->intval - '0'].rm_eo - solns->regs[(EXP)->intval - '0'].rm_so) \
					  : -1) \
				       : -1)))


  if (solns == &no_solutions)
    {
      return 0;
    }

  if (!solns->exp)
    {
      if (solns->step != 0)
	{
	  return 0;
	}
      else
	{
	  solns->step = 1;
	  solns->final_tag = 1;
	  return (solns->start == solns->end
		  ? 1
		  : 0);
	}
    }
  else if (!(solns->small_p || solns->exp->observed))
    {
      if (solns->step != 0)
	{
	  return 0;
	}
      else if (solns->exp->type == r_string)
	{
	  int ans;
	  ans = rx_solution_fit_str_p (solns);
	  solns->final_tag = 1;
	  solns->step = -1;
	  return ans;
	}
      else
	{
	  int ans;
	  ans = rx_solution_fits (solns);
	  if (ans < 0)
	    longjmp (*err_escape, 1);
	  solns->final_tag = solns->match_engine.final_tag;
	  solns->step = -1;
	  return !!ans;
	}
    }
  else				/* if (solns->small_p || solns->exp->observed) */
    {
      int fits;
      switch (solns->step)
	{
	case -2:
	  if (solns->exp->min_enclosed_paren)
	    {
	      int first_subexp;
	      int last_subexp;
	      int x;

	      first_subexp = solns->exp->min_enclosed_paren;
	      last_subexp = solns->exp->max_enclosed_paren;

	      for (x = first_subexp; x <= last_subexp; ++x)
		{
		  solns->regs[x].rm_so = solns->saved_rm_so[x - first_subexp];
		  solns->regs[x].rm_eo = solns->saved_rm_eo[x - first_subexp];
		}
	    }
	  return 0;

	case -1:
	  return 0;

	case 0:
	  if (solns->small_p)
	    {
	      solns->step = 1;
	      goto resolve_fit;
	    }
	  else
	    {
	      fits = rx_solution_fits (solns);
	      if (fits < 0)
		longjmp (*err_escape, 1);
	      /* Set final_tag here because this rough fit test
	       * may be all the matching that gets done.
	       * For example, consider a paren node containing
	       * a true regular expression ending with a cut
	       * operator.
	       */
	      solns->final_tag = solns->match_engine.final_tag;
	      if (fits == 0)
		{
		  solns->step = -1;
		  return fits;
		}
	      else
		{
		  solns->step = 1;
		  goto resolve_fit;
		}
	    }

	default:
	resolve_fit:
	  switch (solns->exp->type)
	    {
	    case r_string:
	      if (!solns->small_p)
		{
		  while (1)
		    panic ("bogus regexp in rx_next_solution_internal");
		}
	      else
		{
		  int answer;
		  answer = rx_solution_fit_str_p (solns);
		  solns->final_tag = 1;
		  solns->step = -1;
		  return answer;
		}
	      
	    case r_cset:
	      if (!solns->small_p)
		{
		  while (1)
		    panic ("bogus regexp in rx_next_solution_internal");
		}
	      else
		{
		  int answer;
		  answer = rx_solution_fit_cset_p (solns);
		  solns->final_tag = 1;
		  solns->step = -1;
		  return answer;
		}
	      
	    case r_cut:
	      if (!solns->small_p)
		{
		  while (1)
		    panic ("bogus regexp in rx_next_solution_internal");
		}
	      else
		{
		  solns->final_tag = solns->exp->intval;
		  solns->step = -1;
		  return !!solns->exp->intval;
		}
	      
	    case r_parens:
	      {
		int paren_stat;
		switch (solns->step)
		  {
		  case 1:
		    if (solns->exp->min_enclosed_paren)
		      {
			if (save_nested_registers (solns))
			  longjmp (*err_escape, 1);
		      }

		    if (   !solns->small_p
			&& (   !solns->exp->left
			    || !solns->exp->left->observed))
		      {
			/* We already proved that this (simple)
			 * subexpression matches.  Optionally, record
			 * its position.  Return success once, and
			 * failure for all subsequent attempts to find
			 * other matches.
			 */
			if (solns->exp->intval)
			  {
			    solns->regs[solns->exp->intval].rm_so = solns->start;
			    solns->regs[solns->exp->intval].rm_eo = solns->end;
			  }
			solns->step = -2;
			/* Keep the final_tag from the fits test. */
			return 1;
		      }

		    /* We must search through possible matches of the subexpression.
		     */
		    solns->left = rx_make_solutions (solns->regs,
						     solns->cset_size,
						     solns->exp->left,
						     solns->subexps,
						     solns->nsub,
						     solns->start,
						     solns->end,
						     0,
						     solns->vmfn,
						     solns->contextfn,
						     solns->closure,
						     solns->small_p,
						     0,
						     0);
		    if (!solns->left)
		      longjmp (*err_escape, 1);

		  case 2:
		    if (solns->exp->min_enclosed_paren)
		      {
			int first_subexp;
			int last_subexp;
			int x;

			/* Initialize the parentheses in the tree
			 * rooted at this expression to -1.  
			 */

			first_subexp = solns->exp->min_enclosed_paren;
			last_subexp = solns->exp->max_enclosed_paren;

			for (x = first_subexp; x <= last_subexp; ++x)
			  {
			    solns->regs[x].rm_so = -1;
			    solns->regs[x].rm_eo = -1;
			  }
		      }

		    paren_stat = rx_next_solution_internal (solns->left, err_escape, depth + 1);
		    if (paren_stat < 0)
		      longjmp (*err_escape, 1);

		    if (paren_stat == 1)
		      {
			if (solns->exp->intval)
			  {
			    solns->regs[solns->exp->intval].rm_so = solns->start;
			    solns->regs[solns->exp->intval].rm_eo = solns->end;
			  }
			solns->final_tag = solns->left->final_tag;
			solns->step = 2;
			return 1;
		      }
		    else 
		      {
			rx_free_solutions (solns->left);
			solns->left = 0;
			if (solns->exp->min_enclosed_paren)
			  {
			    /* This expression didn't match.  Restore the old subexpression
			     * positions.
			     */
			    restore_nested_registers (solns);
			  }
			solns->step = -1;
			return paren_stat;
		      }
		  }
	      }

	    case r_alternate:
	      {
		int alt_stat;
		switch (solns->step)
		  {
		  case 1:
		    solns->left = rx_make_solutions (solns->regs,
						     solns->cset_size,
						     solns->exp->left,
						     solns->subexps,
						     solns->nsub,
						     solns->start,
						     solns->end,
						     0,
						     solns->vmfn,
						     solns->contextfn,
						     solns->closure,
						     solns->small_p,
						     0,
						     0);
		    if (!solns->left)
		      longjmp (*err_escape, 1);

		  case 2:
		    alt_stat = rx_next_solution_internal (solns->left, err_escape, depth + 1);
		    if (alt_stat < 0)
		      longjmp (*err_escape, 1);

		    if (alt_stat == 1)
		      {
			solns->final_tag = solns->left->final_tag;
			solns->step = 2;
			return alt_stat;
		      }
		    else 
		      {
			rx_free_solutions (solns->left);
			solns->left = 0;
		      }

		    solns->right = rx_make_solutions (solns->regs,
						      solns->cset_size,
						      solns->exp->right,
						      solns->subexps,
						      solns->nsub,
						      solns->start,
						      solns->end,
						      0,
						      solns->vmfn,
						      solns->contextfn,
						      solns->closure,
						      solns->small_p,
						      0,
						      0);
		    if (!solns->right)
		      longjmp (*err_escape, 1);

		  case 4:
		    alt_stat = rx_next_solution_internal (solns->right, err_escape, depth + 1);
		    if (alt_stat < 0)
		      longjmp (*err_escape, 1);

		    if (alt_stat == 1)
		      {
			solns->final_tag = solns->right->final_tag;
			solns->step = 4;
			return alt_stat;
		      }
		    else 
		      {
			solns->step = -1;
			rx_free_solutions (solns->right);
			solns->right = 0;
			return alt_stat;
		      }
		  }
	      }

	    case r_concat:
	      {
		switch (solns->step)
		  {
		    int concat_stat;
		    int split_guess_stat;
		    int certain_label;

		  case 1:

		    if (!solns->exp->left)
		      solns->split_guess = solns->start;
		    else
		      {
			rx_off_t known_left_length;

			known_left_length = KNOWN_LENGTH (solns->exp->left);
			if (known_left_length >= 0)
			  {
			    if (known_left_length > (solns->end - solns->start))
			      {
				solns->step = -1;
				return 0;
			      }
			    solns->split_guess = solns->start + known_left_length;
			  }
			else
			  {
			    rx_off_t known_right_length;

			    known_right_length = KNOWN_LENGTH (solns->exp->right);

			    if (known_right_length >= 0)
			      {
				if (known_right_length > (solns->end - solns->start))
				  {
				    solns->step = -1;
				    return 0;
				  }
				solns->split_guess = solns->end - known_right_length;
			      }
			    else
			      solns->split_guess = solns->end;
			  }
		      }

		  concat_split_guess_loop:

		    split_guess_stat = rx_longest_split_guess (&certain_label, solns);

		    if (split_guess_stat < 0)
		      longjmp (*err_escape, 1);

		    if (!split_guess_stat)
		      {
			solns->step = -1;
			return 0;
		      }

		    solns->left = rx_make_solutions (solns->regs,
						     solns->cset_size,
						     solns->exp->left,
						     solns->subexps,
						     solns->nsub,
						     solns->start,
						     solns->split_guess,
						     0,
						     solns->vmfn,
						     solns->contextfn,
						     solns->closure,
						     solns->small_p,
						     !!certain_label,
						     certain_label);
		    if (!solns->left)
		      longjmp (*err_escape, 1);

		  concat_try_next_left_match:

		    concat_stat = rx_next_solution_internal (solns->left, err_escape, depth + 1);
		    if (concat_stat < 0)
		      longjmp (*err_escape, 1);

		    if (concat_stat != 1)
		      {
			rx_free_solutions (solns->left);
			solns->left = 0;
			if (   (KNOWN_LENGTH (solns->exp->left) < 0)
			    && (KNOWN_LENGTH (solns->exp->right) < 0))
			  {
			    --solns->split_guess;
			    if (solns->split_guess >= solns->start)
			      goto concat_split_guess_loop;
			  }

			solns->step = -1;
			return 0;
		      }

		    solns->right = rx_make_solutions (solns->regs,
						      solns->cset_size,
						      solns->exp->right,
						      solns->subexps,
						      solns->nsub,
						      solns->split_guess,
						      solns->end,
						      0,
						      solns->vmfn,
						      solns->contextfn,
						      solns->closure,
						      solns->small_p,
						      0,
						      0);
		    if (!solns->right)
		      longjmp (*err_escape, 1);

		  case 2:
		    concat_stat = rx_next_solution_internal (solns->right, err_escape, depth + 1);
		    if (concat_stat < 0)
		      longjmp (*err_escape, 1);

		    if (concat_stat == 1)
		      {
			solns->final_tag = solns->right->final_tag;
			solns->step = 2;
			return concat_stat;
		      }
		    else 
		      {
			rx_free_solutions (solns->right);
			solns->right = 0;
			goto concat_try_next_left_match;
		      }
		  }
	      }

	    case r_right_concat:
	      {
		switch (solns->step)
		  {
		    int concat_stat;
		    rx_off_t known_left_length;
		    rx_off_t known_right_length;

		  case 1:
		    known_left_length = KNOWN_LENGTH (solns->exp->left);
		    known_right_length = KNOWN_LENGTH (solns->exp->right);
		    if (known_left_length >= 0)
		      {
			solns->split_guess = solns->start + known_left_length;
		      }
		    else if (known_right_length >= 0)
		      {
			solns->split_guess = solns->end - known_right_length;
		      }
		    else
		      solns->split_guess = solns->start;


		  right_concat_split_guess_loop:
		    solns->left = rx_make_solutions (solns->regs,
						     solns->cset_size,
						     solns->exp->left,
						     solns->subexps,
						     solns->nsub,
						     solns->start,
						     solns->split_guess,
						     0,
						     solns->vmfn,
						     solns->contextfn,
						     solns->closure,
						     solns->small_p,
						     0,
						     0);
		    if (!solns->left)
		      longjmp (*err_escape, 1);

		  right_concat_try_next_left_match:

		    concat_stat = rx_next_solution_internal (solns->left, err_escape, depth + 1);
		    if (concat_stat < 0)
		      longjmp (*err_escape, 1);

		    if (concat_stat != 1)
		      {
			rx_free_solutions (solns->left);
			solns->left = 0;
			if (   (KNOWN_LENGTH (solns->exp->left) < 0)
			    && (KNOWN_LENGTH (solns->exp->right) < 0))
			  {
			    ++solns->split_guess;
			    if (solns->split_guess <= solns->end)
			      goto right_concat_split_guess_loop;
			  }

			solns->step = -1;
			return 0;
		      }

		    solns->right = rx_make_solutions (solns->regs,
						      solns->cset_size,
						      solns->exp->right,
						      solns->subexps,
						      solns->nsub,
						      solns->split_guess,
						      solns->end,
						      0,
						      solns->vmfn,
						      solns->contextfn,
						      solns->closure,
						      solns->small_p,
						      0,
						      0);
		    if (!solns->right)
		      longjmp (*err_escape, 1);

		  case 2:
		    concat_stat = rx_next_solution_internal (solns->right, err_escape, depth + 1);
		    if (concat_stat < 0)
		      longjmp (*err_escape, 1);

		    if (concat_stat == 1)
		      {
			solns->final_tag = solns->right->final_tag;
			solns->step = 2;
			return concat_stat;
		      }
		    else 
		      {
			rx_free_solutions (solns->right);
			solns->right = 0;
			goto right_concat_try_next_left_match;
		      }
		  }
	      }

	    case r_star:
	      {
		int star_stat;

		switch (solns->step)
		  {
		  case 1:
		    /* Begin by trying to match the entire string
		     * with a single iteration.
		     */
		    solns->left = rx_make_solutions (solns->regs,
						     solns->cset_size,
						     solns->exp->left,
						     solns->subexps,
						     solns->nsub,
						     solns->start,
						     solns->end,
						     0,
						     solns->vmfn,
						     solns->contextfn,
						     solns->closure,
						     solns->small_p,
						     0,
						     0);
		    if (!solns->left)
		      longjmp (*err_escape, 1);


		  case 2:
		    star_stat = rx_next_solution_internal (solns->left, err_escape, depth + 1);
		    if (star_stat < 0)
		      longjmp (*err_escape, 1);

		    if (star_stat == 1)
		      {
			/* found a single-iteration whole-string match */
			solns->final_tag = solns->left->final_tag;
			solns->step = 2;
			return 1;
		      }
		    
		    /* no more single-iteration whole-string matches */
		    rx_free_solutions (solns->left);
		    solns->left = 0;
		    
		    if (solns->start == solns->end)
		      {
			/* when comaring to the empty string and none of the single-iteration
			 * matches work, try matching with zero iterations, then give up
			 */
			solns->step = -1;
			return 1;
		      }
		    
		    /* when comparing to a non-empty string and none of the single-iteration
		     * matches work, try all matches with two or more non-empty iterations.
		     */
		    if (!solns->exp->left || (solns->exp->left->len == 0))
		      {
			solns->step = -1;
			return 0;
		      }
		    else if (solns->exp->left->len > 0)
		      {
			if (   (solns->exp->left->len >= (solns->end - solns->start))
			    || (0 != ((solns->end - solns->start) % solns->exp->left->len)))
			  {
			    solns->step = -1;
			    return 0;
			  }
			solns->split_guess = solns->end - solns->exp->left->len;
		      }
		    else
		      solns->split_guess = solns->start + 1;
		    
		  case 3:
		  star_3:

		    /* Suppose the string has been split into two parts: s == s1 s2
		     * and we are trying to match R*.
		     *
		     * Recurse by matching s1 against R*, and s2 against R.
		     *
		     * However, if s == s1 and s2 is the empty string, comparing
		     * s1 to R* would cause an infinite recursion.  So instead,
		     * compare s1 to R and s2 to R.
		     *
		     * Comparing s2 to R is often much less expensive than comparing
		     * s1 to R*.  If s2 doesn't match R, time spent comparing s1 to R*
		     * is wasted.  So, before trying s1 against R*, make sure that
		     * s2 can match R.
		     *
		     * Registers are subtle.  There are three possibilities:
		     *
		     *		[A] R contains no reporting parens
		     *		[B] R contains reporting parens
		     *
		     * In case A, matching s1 against R* won't change any registers,
		     * so it is safe to match s to R first.
		     *
		     * In the second case, either
		     * 
		     * 		[B1] R is a reporting parenthesized expression
		     * 		[B2] R is a non-reporting parenthesized expression
		     * 
		     * In either of those two cases, the first step of
		     * comparing s2 to R is to set all of the
		     * registers for nested reporting parentheses to
		     * -1.  As a result, again, s1 against R* has no (observable) effect
		     * on registers -- so it is safe to match s to R first.
		     *
		     */

		    solns->right = rx_make_solutions (solns->regs,
						      solns->cset_size,
						      solns->exp->left,
						      solns->subexps,
						      solns->nsub,
						      solns->split_guess,
						      solns->end,
						      0,
						      solns->vmfn,
						      solns->contextfn,
						      solns->closure,
						      solns->small_p,
						      0,
						      0);
		    if (!solns->right)
		      longjmp (*err_escape, 1);

		    star_stat = rx_next_solution_internal (solns->right, err_escape, depth + 1);
		    if (star_stat < 0)
		      longjmp (*err_escape, 1);

		    if (star_stat != 1)
		      {
			/* No match for s2 against R, so guess again:
			 */
			rx_free_solutions (solns->right);
			solns->right = 0;
			if (solns->exp->left->len > 0)
			  {
			    solns->step = -1;
			    return 0;
			  }
			++solns->split_guess;
			if (solns->split_guess > solns->end)
			  {
			    solns->step = -1;
			    return 0;
			  }
			goto star_3;
		      }

		    /* Aha -- s2 matches R.  Does s1 match R*?
		     *
		     * Testing s1 against R* can modify some registers
		     * that were set while matching s2 to R.  These
		     * registers must be saved and then restored before 
		     * returning the overall match.
		     */
		    if (save_nested_registers (solns))
		      {
			longjmp (*err_escape, 1);
		      }

		    if (solns->split_guess == solns->end)
		      solns->left = rx_make_solutions (solns->regs,
						       solns->cset_size,
						       solns->exp->left,
						       solns->subexps,
						       solns->nsub,
						       solns->start,
						       solns->split_guess,
						       0,
						       solns->vmfn,
						       solns->contextfn,
						       solns->closure,
						       solns->small_p,
						       0,
						       0);
		    else
		      solns->left = rx_make_solutions (solns->regs,
						       solns->cset_size,
						       solns->exp,
						       solns->subexps,
						       solns->nsub,
						       solns->start,
						       solns->split_guess,
						       0,
						       solns->vmfn,
						       solns->contextfn,
						       solns->closure,
						       solns->small_p,
						       0,
						       0);
		    if (!solns->left)
		      longjmp (*err_escape, 1);
		    
		    star_stat = rx_next_solution_internal (solns->left, err_escape, depth + 1);
		    if (star_stat < 0)
		      longjmp (*err_escape, 1);

		    if (star_stat != 1)
		      {
			/* no matches for the left part of the split string
			 * at this split_guess.
			 */
			rx_free_solutions (solns->left);
			rx_free_solutions (solns->right);
			solns->left = 0;
			solns->right = 0;
			if (solns->exp->left->len > 0)
			  {
			    solns->step = -1;
			    return 0;
			  }
			++solns->split_guess;
			if (solns->split_guess > solns->end)
			  {
			    solns->step = -1;
			    return 0;
			  }
			goto star_3;
		      }

		    /* Found at least one solution for both s1 against R* and 
		     * s2 against R.
		     * 
		     * Return that solution.  First, restore the registers from
		     * the s2/R match:
		     * 
		     */

		    restore_nested_registers (solns);
		    solns->final_tag = solns->right->final_tag;
		    solns->step = 4;
		    return 1;
		    
		  case 4:
		    /* This is reached when we've found one solution for
		     * s1 against R* and s2 against R, but that solution
		     * was rejected.
		     * 
		     * Try to find another solution for s2/R, using the same
		     * match for s1/R*.
		     */

		    star_stat = rx_next_solution_internal (solns->right, err_escape, depth + 1);
		    if (star_stat < 0)
		      longjmp (*err_escape, 1);

		    if (star_stat == 1)
		      {
			/* found a match */
			solns->final_tag = solns->right->final_tag;
			solns->step = 4;
		      }

		    /* No more matches for s2/R for the current match of s1/R*
		     * 
		     * We could look for another s1/R* match -- but why bother?
		     * Any changes to registers made by such a match are
		     * not observable after any s/R match.  So, try again with
		     * a different split guess:
		     */

		    rx_free_solutions (solns->left);
		    rx_free_solutions (solns->right);
		    solns->left = 0;
		    solns->right = 0;
		    if (solns->exp->left->len > 0)
		      {
			solns->step = -1;
			return 0;
		      }
		    ++solns->split_guess;
		    if (solns->split_guess > solns->end)
		      {
			solns->step = -1;
			return 0;
		      }
		    goto star_3;
		  }
	      }

	    case r_interval:
	      {
		/* On entry, interval_x is the number of iterations
		 * already charged to this interval.  So, instead
		 * of {intval, intval2}, the interval is actually:
		 * 
		 * 	{ max(0, intval - interval_x), 
		 *	  max(0, intval2 - interval_x) }
		 * 
		 */
		switch (solns->step)
		  {
		    int interval_stat;
		    
		  case 1:
		    solns->interval_from = max0(solns->exp->intval - solns->interval_x);
		    solns->interval_to = max0(solns->exp->intval2 - solns->interval_x);

		    /* Should we try single iteration solutions?
		     *
		     * Yes if either:
		     *
		     *		(interval_from <= 1) && (interval_to >= 1)
		     * 
		     * or
		     * 		((solns->start == solns->end) && (interval_to >= 1))
		     * 
		     * If the string is the empty string (start == end), and any 
		     * iterations are permitted (interval_to >= 1) then all possible
		     * solutions (except the 0-iteration solution) can be found with 
		     * a single iteration.  Repeated iterations for a null match do not
		     * add new solutions compared to a single iteration because the 
		     * single iteration will always have the same side effects as multiple
		     * iterations ending with that single iteration.  
		     */

		    if (   (   (solns->start == solns->end)
			    && (solns->interval_to >= 1)
			    && (   !solns->exp->left
				|| (solns->exp->left->len <= 0)))
			|| (   (solns->interval_from <= 1)
			    && (solns->interval_to >= 1)
			    && (solns->start != solns->end)
			    && solns->exp->left
			    && (   (solns->exp->left->len < 0)
				|| (solns->exp->left->len == (solns->end - solns->start)))))

		      {
			solns->left = rx_make_solutions (solns->regs,
							 solns->cset_size,
							 solns->exp->left,
							 solns->subexps,
							 solns->nsub,
							 solns->start,
							 solns->end,
							 0,
							 solns->vmfn,
							 solns->contextfn,
							 solns->closure,
							 solns->small_p,
							 0,
							 0);
			if (!solns->left)
			  longjmp (*err_escape, 1);
		      case 2:
			interval_stat = rx_next_solution_internal (solns->left, err_escape, depth + 1);
			if (interval_stat < 0)
			  longjmp (*err_escape, 1);

			if (interval_stat != 1)
			  {
			    rx_free_solutions (solns->left);
			    solns->left = 0;
			    goto try_0_iteration_null_match;
			  }
			else
			  {
			    solns->step = 2;
			    return 1;
			  }
		      }

		  try_0_iteration_null_match:

		    /* If we're trying to match the empty string, and 0 iterations are
		     * permitted, that is the next best solution.  If we're trying to
		     * match the empty string, the 0 iteration solution is the only
		     * remaining possibility.
		     */
		    if (solns->start == solns->end)
		      {
			solns->step = -1;
			return (solns->interval_from == 0);
		      }

		    /* The string is not empty.  If only 0 iterations are permitted,
		     * then no solutions are possible.
		     */
		    if (solns->interval_to == 0)
		      {
			solns->step = -1;
			return 0;
		      }

		    /* The string is not empty and no single iteration solution works.
		     * Search through solutions with at least two iterations.  We split
		     * the string into two parts, and try to match the left part with N
		     * iterations (N >= 1) and the right part with 1 iteration.
		     * 
		     * To satisfy the left-to-right longest-subexpression rule, the right
		     * part (matched by the last iteration) should be as long as possible.
		     */

		    if (!solns->exp->left || (solns->exp->left->len == 0))
		      {
			/* never reached unless certain other optimizations are removed
			 */
			solns->step = -1;
			return 0;
		      }
		    else if (solns->exp->left->len > 0)
		      {
			if (   (solns->exp->left->len > (solns->end - solns->start))
			    || (0 != ((solns->end - solns->start) % solns->exp->left->len)))
			  {
			    solns->step = -1;
			    return 0;
			  }
			solns->split_guess = solns->end - solns->exp->left->len;
		      }
		    else
		      solns->split_guess = solns->start;

		  interval_split_start_at_new_split_guess:

		    solns->right = rx_make_solutions (solns->regs,
						      solns->cset_size,
						      solns->exp->left,
						      solns->subexps,
						      solns->nsub,
						      solns->split_guess,
						      solns->end,
						      0,
						      solns->vmfn,
						      solns->contextfn,
						      solns->closure,
						      solns->small_p,
						      0,
						      0);
		    if (!solns->right)
		      longjmp (*err_escape, 1);

		  next_interval_right_solution:
		    interval_stat = rx_next_solution_internal (solns->right, err_escape, depth + 1);
		    if (interval_stat < 0)
		      longjmp (*err_escape, 1);

		    if (interval_stat != 1)
		      {
			rx_free_solutions (solns->right);
			solns->right = 0;

			
			if (solns->exp->left->len < 0)
			  {
			    ++solns->split_guess;
			    if (solns->split_guess <= solns->end)
			      goto interval_split_start_at_new_split_guess;
			  }
			/* No more solutions at all. */
			solns->step = -1;
			return 0;
		      }

		    /* Found a solution for the right half of the split.
		     */

		    if (save_nested_registers (solns))
		      {
			longjmp (*err_escape, 1);
		      }
		    solns->left = rx_make_solutions (solns->regs,
						     solns->cset_size,
						     solns->exp,
						     solns->subexps,
						     solns->nsub,
						     solns->start,
						     solns->split_guess,
						     solns->interval_x + 1,
						     solns->vmfn,
						     solns->contextfn,
						     solns->closure,
						     solns->small_p,
						     0,
						     0);
		    if (!solns->left)
		      longjmp (*err_escape, 1);

		    interval_stat = rx_next_solution_internal (solns->left, err_escape, depth + 1);
		    if (interval_stat < 0)
		      longjmp (*err_escape, 1);

		    if (interval_stat != 1)
		      {
			/* No more solutions for the left part of the string.
			 */
			rx_free_solutions (solns->left);
			solns->left = 0;
			goto next_interval_right_solution;
		      }

		    /* We found an overall solution for the interval.
		     */
		    solns->step = 3;
		    restore_nested_registers (solns);
		    solns->final_tag = solns->right->final_tag;
		    return 1;

		  case 3:
		    rx_free_solutions (solns->left);
		    solns->left = 0;
		    goto next_interval_right_solution;

		  }
	      }

	    case r_context:
	      {
		solns->step = -1;
		solns->final_tag = 1;
		return solns->contextfn (solns->closure,
					 solns->exp,
					 solns->start, solns->end,
					 solns->regs);
	      }


	    }
	}
      while (1)
	panic ("unreached in rx_next_solution_internal");
    }
}

/*(c rx_solutions_final_tag)
 * int rx_solutions_final_tag (struct rx_solutions * solns);
 * 
 * Return the state label of the last DFA state reached during
 * the solution most recently returned by `rx_next_solution'.
 */
int
rx_solutions_final_tag (struct rx_solutions * solns)
{
  return solns->final_tag;
}



/************************************************************************
 *(h1 "Building and Freeing a Stream of Solutions -- The General Case")
 * 
 * The functions in this section are quite general.  They can be used
 * to compare a regexp to a string which is not entirely in memory at
 * any one time.
 */
 
/*(c rx_make_solutions)
 * struct rx_solutions * 
 * rx_make_solutions (struct rx_registers * regs,
 *		      int cset_size,
 *		      struct rx_exp_node * expression,
 *		      struct rx_exp_node ** subexps,
 *		      int nsub,
 *		      rx_off_t start,
 *		      rx_off_t end,
 * 		      int interval_x,
 *		      rx_vmfn vmfn,
 *		      rx_contextfn contextfn,
 *		      void * closure,
 *		      int small_p,
 *		      int certainly_fits,
 *		      int certain_final_tag);
 * 
 * 
 * Construct a stream-of-solutions to a regexp matching problem.  For
 * arguments which are not documented here, see
 * xref:"rx_basic_make_solutions".
 *
 * If `expression' is of type `r_interval', then `interval_x' 
 * is the number of matches already accumulated for the subexpression.
 * In other words, if expression is `RE{M,N}', it will match as:
 * 
 * 	RE{max (0,M - interval_x), max (0, N - interval_x)
 * 
 * This function does not directly accept an input string. Instead,
 * it accepts the parameters `vmfn', `contextfn', and `closure'.
 *
 * `vmfn' is a function which can return the address of a substring
 * containing a chosen character position within the entire input
 * string.  It is used to page-in parts of the input string during a
 * match.  The details of this parameter are given below.
 *
 * `contextfn' is a function that knows how to evaluate regexp
 * expressions which are anchors or backreferences.  These kinds of
 * subexpressions may be defined in terms of parts of the input string
 * that are not in memory or that are not contiguous in memory.  The
 * `contextfn' has the job of sorting that out in an efficient way.
 * The details of this parameter are also given below.
 *
 * `closure' is an opaque parameter that is passed along to `vmfn' and
 * `contextfn'.
 *
 * `small_p', if not 0, means that the regexp is so trivial that
 * heavy-weight optimizations are not likely to pay off.  It is always
 * safe to pass 0 for this parameter.  Internally, Rx passes a non-0
 * value if `expression' is 0 or if `expression->small_advised_p' is
 * non-0.  `expression->small_advised_p' is filled in by
 * `rx_analyze_rexp'.
 * 
 * `certainly_fits', if not 0, means that if `expression' is converted
 * to a true regular expression (such as by `rx_simplify_rexp') and
 * compared to the target string, the target string will certainly match.
 * Setting this field can speed up some matches.  If this field is
 * non-0, `certain_final_tag' must also be set.  It is always safe to 
 * pass 0 for `certainly_fits', though doing so may slow down some matches
 * slightly.
 * 
 * `certain_final_tag' is the final DFA state label returned for a
 * match of simplified form of `expression' against the target string.
 * This field is only used if `certainly_fits' is not 0.
 */
struct rx_solutions *
rx_make_solutions (struct rx_registers * regs,
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
		   int certain_final_tag)
{
  struct rx_solutions * solns;

  if (!expression && (start != end))
    return &no_solutions;

  if (   expression
      && (expression->len >= 0)
      && ((expression->type != r_interval)
	  ? (expression->len != (end - start))
	  : (/* Is an interval expression of fixed length */
	     (!expression->len
	      ? (end != start)
	      : ((expression->len - (interval_x * expression->left->len)) != (end - start))))))
    return &no_solutions;

  solns = (struct rx_solutions *)rx_nfa_cache_malloc (sizeof (*solns));
  if (!solns)
    return 0;

  ++solns_allocated;

  solns->small_p = small_p;
  solns->step = 0;

  solns->cset_size = cset_size;
  solns->exp = expression;
  rx_save_exp (expression);
  solns->subexps = subexps;
  solns->nsub = nsub;
  solns->regs = regs;

  solns->start = start;
  solns->end = end;

  solns->vmfn = vmfn;
  solns->contextfn = contextfn;
  solns->closure = closure;

  solns->dfa = 0;
  solns->match_engine.rx = 0;
  solns->match_engine.state = 0;

  solns->certainly_fits = certainly_fits;
  solns->certain_final_tag = certain_final_tag;

  solns->no_fancy_guessing = 0;
  solns->left_dfa = 0;
  solns->left_match_engine.rx = 0;
  solns->left_match_engine.state = 0;

  solns->split_guess = 0;
  solns->left = 0;
  solns->right = 0;

  solns->interval_x = interval_x;
  solns->interval_from = 0;
  solns->interval_to = 0;

  solns->saved_rm_so = 0;
  solns->saved_rm_eo = 0;
  solns->final_tag = 0;

  if (!small_p)
    {
      if (!solns->exp || !solns->exp->observed)
	{
	  solns->dfa = rx_unfa (expression, cset_size);
	  if (!solns->dfa)
	    {
	      rx_free_solutions (solns);
	      return 0;
	    }
	  rx_init_dfa_from_nfa (&solns->match_engine, solns->dfa->nfa);
	  if (rx_dfa_goto_start_superstate (&solns->match_engine, 1))
	    {
	      rx_free_solutions (solns);
	      return 0;
	    }
	}
      else
	{
	  struct rx_exp_node * simplified;
	  if (rx_simplify_rexp (&simplified, cset_size, solns->exp, subexps))
	    {
	      rx_free_solutions (solns);
	      return 0;
	    }
	  solns->dfa = rx_unfa (simplified, cset_size);
	  if (!solns->dfa)
	    {
	      rx_free_exp (simplified);
	      rx_free_solutions (solns);
	      return 0;
	    }
	  rx_init_dfa_from_nfa (&solns->match_engine, solns->dfa->nfa);
	  if (rx_dfa_goto_start_superstate (&solns->match_engine, 1))
	    {
	      rx_free_exp (simplified);
	      rx_free_solutions (solns);
	      return 0;
	    }
	  rx_free_exp (simplified);
	}
    }


  if (solns->exp && (solns->exp->observed || solns->small_p) && (solns->exp->type == r_concat))
    {
      struct rx_exp_node * left_simplified;
      
      if (rx_simplify_rexp (&left_simplified, cset_size, solns->exp->left, solns->subexps))
	{
	  rx_free_solutions (solns);
	  return 0;
	}

      solns->left_dfa = rx_unfa (left_simplified, cset_size);
      if (!solns->left_dfa)
	{
	  rx_free_exp (left_simplified);
	  rx_free_solutions (solns);
	  return 0;
	}
      rx_init_dfa_from_nfa (&solns->left_match_engine, solns->left_dfa->nfa);
      rx_free_exp (left_simplified);
    }

  return solns;
}


/*(include-documentation "match-regexp.h")
 */


/*(c rx_solutions_closure)
 * void * rx_solutions_closure (struct rx_solutions * solns);
 * 
 * Given a stream of regexp solutions, return the `closure'
 * parameter that was passed to `rx_make_solutions'.
 */
void *
rx_solutions_closure (struct rx_solutions * solns)
{
  return solns->closure;
}


/*(c rx_free_solutions)
 * void rx_free_solutions (struct rx_solutions * solns);
 * 
 * Free all storage associated with a stream of regexp solutions
 * allocated by `rx_make_solutions'.  See also
 * xref:"rx_basic_free_solutions".
 */
void
rx_free_solutions (struct rx_solutions * solns)
{
  if (!solns)
    return;

  if (solns == &no_solutions)
    return;

  if (solns->left)
    {
      rx_free_solutions (solns->left);
      solns->left = 0;
    }

  if (solns->right)
    {
      rx_free_solutions (solns->right);
      solns->right = 0;
    }

  if (solns->dfa)
    {
      rx_free_unfa (solns->dfa);
      solns->dfa = 0;
    }

  rx_clear_dfa_state (&solns->match_engine);

  if (solns->left_dfa)
    {
      rx_free_unfa (solns->left_dfa);
      solns->left_dfa = 0;
    }

  rx_clear_dfa_state (&solns->left_match_engine);

  if (solns->exp)
    {
      rx_free_exp (solns->exp);
      solns->exp = 0;
    }

  if (solns->saved_rm_so)
    rx_nfa_cache_free (solns->saved_rm_so);

  if (solns->saved_rm_eo)
    rx_nfa_cache_free (solns->saved_rm_eo);

  rx_nfa_cache_free (solns);
  ++solns_freed;
}





/****************************************************************
 *(h1 "Regexp Tree To Regular Expression Conversion")
 *
 */

/*(c rx_simplify_rexp)
 * int rx_simplify_rexp (struct rx_exp_node ** answer,
 *                       int cset_size,
 *                       struct rx_exp_node *node,
 *                       struct rx_exp_node ** subexps);
 * 
 * Convert an expression, which might not be a regular expression, into
 * a regular expression matching a superset of the original pattern.
 *
 * This is useful for a matching heuristic: the regular superset
 * language is used to test a candidate string for a match, the
 * original irregular regexp is used to verify the match.  The test
 * using the regular superset is very fast and many non-matching
 * strings can be quickly rejected; the verification using the
 * irregular regexp may be slow, so we avoid it when we can.
 *
 * If the input expression is a regular expression, this is an
 * identity function which increments the reference count of `node'.
 *
 * `answer' is a return parameter.
 *
 * `subexps' is an array of pointers into the expression `node'.
 * Element `N' of the array is the `N'th parenthesized subexpression
 * of the tree rooted at `node'.  This array is usually computed by
 * `rx_analyze_rexp'.
 */
int
rx_simplify_rexp (struct rx_exp_node ** answer,
		  int cset_size,
		  struct rx_exp_node * node,
		  struct rx_exp_node ** subexps)
{
  int err;

  if (!node)
    {
      *answer = 0;
      return 0;
    }

  if (!node->observed)
    {
      rx_save_exp (node);
      *answer = node;
      return 0;
    }

  if (node->simplified)
    {
      rx_save_exp (node->simplified);
      *answer = node->simplified;
      return 0;
    }

  switch (node->type)
    {
    default:
    case r_cset:
    case r_string:
    case r_cut:
      panic ("bogus regexp in rx_simplify_rexp");
      return -1;

    case r_parens:
      err = rx_simplify_rexp (answer, cset_size, node->left, subexps);
      if (err)
	return err;
      break;

    case r_context:
      if (char_is_digit (node->intval))
	{
	  err = rx_simplify_rexp (answer, cset_size, subexps [node->intval - '0'], subexps);
	  if (err)
	    return err;
	}
      else
	*answer = 0;
      break;

    case r_concat:
    case r_right_concat:
    case r_alternate:
    case r_star:
    case r_interval:
      {
	struct rx_exp_node *n;

	n = rx_exp_node (node->type);
	if (!n)
	  return REG_ESPACE;

	if (node->cset)
	  {
	    panic ("found a cset bitset in an unexpected place");
	  }

	n->intval = node->intval;
	n->intval2 = node->intval2;
	err = rx_simplify_rexp (&n->left, cset_size, node->left, subexps);
	if (err)
	  {
	    rx_free_exp (n);
	    return err;
	  }
	err = rx_simplify_rexp (&n->right, cset_size, node->right, subexps);
	if (err)
	  {
	    rx_free_exp (n);
	    return err;
	  }
	*answer = n;
      }
      break;
    }
  
  node->simplified = *answer;
  rx_save_exp (node->simplified);
  return 0;
}



/****************************************************************
 *(h1 "Regexp Tree Analysis")
 *
 */

/*(c rx_analyze_rexp)
 * void rx_analyze_rexp (struct rx_exp_node *** subexps,
 *                       size_t * re_nsub,
 *                       struct rx_exp_node * node);
 * 
 * Recursively analyze the expression tree rooted at `node'.  For each
 * node, fill in the fields `observed', `observation_contingent',
 * `small_advised_p', and `len'.  Build an array of parenthesized
 * sub-expressions in the tree and return that array in `subexps' and
 * the number of elements it contains in `re_nsub'.
 *
 * See `struct rx_exp_node' for information about the meaning of
 * structure fields filled in by this function.
 */
int
rx_analyze_rexp (struct rx_exp_node *** subexps,
		 size_t * re_nsub,
		 struct rx_exp_node * node)
{
#define rx_max(a, b) ((a) > (b) ? (a) : (b))
#define rx_min(a, b) ((a) < (b) ? (a) : (b))
  if (node)
    {
      size_t this_subexp;

      this_subexp = 0;

      if (node->type == r_parens)
	{
	  if (node->intval > 0)
	    {
	      struct rx_exp_node ** array;

	      this_subexp = *re_nsub;
	      ++*re_nsub;
	      if (!*subexps)
		array = ((struct rx_exp_node **)
			 rx_nfa_cache_malloc (sizeof (struct rx_exp_node *) * *re_nsub));
	      else
		array = ((struct rx_exp_node **)
			 rx_nfa_cache_realloc (*subexps,
					       sizeof (struct rx_exp_node *) * *re_nsub));
	      if (!array)
		return -1;
	      else
		*subexps = array;
	    }
	}

      if (node->left && rx_analyze_rexp (subexps, re_nsub, node->left))
	return -1;

      if (node->right && rx_analyze_rexp (subexps, re_nsub, node->right))
	return -1;

      switch (node->type)
	{
	case r_cset:
	  node->len = 1;
	  node->observed = 0;
	  node->observation_contingent = 1;
	  node->small_advised_p = 1;
	  node->max_enclosed_paren = 0;
	  node->min_enclosed_paren = 0;
	  break;

 	case r_string:
 	  node->len = node->str_len;
 	  node->observed = 0;
	  node->observation_contingent = 1;
	  node->small_advised_p = 1;
	  node->max_enclosed_paren = 0;
	  node->min_enclosed_paren = 0;
 	  break;

	case r_cut:
	  node->len = -1;
	  node->observed = 0;
	  node->observation_contingent = 1;
	  node->small_advised_p = 1;
	  node->max_enclosed_paren = 0;
	  node->min_enclosed_paren = 0;
	  break;

	case r_concat:
	case r_right_concat:
	case r_alternate:
	  {
	    int lob, rob;
	    int lobc, robc;
	    long llen, rlen;

	    lob = (!node->left ? 0 : node->left->observed);
	    lobc = (!node->left ? 1 : node->left->observation_contingent);
	    rob = (!node->right ? 0 : node->right->observed);
	    robc = (!node->right ? 1 : node->right->observation_contingent);
	    llen = (!node->left ? 0 : node->left->len);
	    rlen = (!node->right ? 0 : node->right->len);

	    if ((llen < 0) || (rlen < 0))
	      node->len = -1;
	    else if ((node->type == r_concat) || (node->type == r_right_concat))
	      {
		if (((size_t)llen + (size_t)rlen) > SSIZE_MAX)
		  panic ("absurdly large regexp in rx_analyze_rexp");
		node->len = (long)(llen + rlen);
	      }
	    else /* node->type == r_alternate */
	      {
		if (llen == rlen)
		  node->len = llen;
		else
		  node->len = -1;
	      }

	    node->observed = lob || rob;
	    node->observation_contingent = lobc && robc;

	    if ((node->type == r_concat) || (node->type == r_right_concat))
	      {
		node->small_advised_p = (   (!node->left || node->left->small_advised_p)
					 && (!node->right || node->right->small_advised_p));
	      }
	    else /* node->type == r_alternate */
	      {
		node->small_advised_p = 0;
	      }
	    node->max_enclosed_paren = rx_max ((node->left ? node->left->max_enclosed_paren : 0),
					       (node->right ? node->right->max_enclosed_paren : 0));
	    {
	      int lmin;
	      int rmin;
	      lmin = (node->left ? node->left->min_enclosed_paren : 0);
	      rmin = (node->right ? node->right->min_enclosed_paren : 0);
	      node->min_enclosed_paren = (!lmin
					  ? rmin
					  : (!rmin
					     ? lmin
					     : rx_min (lmin, rmin)));
	    }
	    break;
	  }

	case r_star:
	  node->len = ((node->left && node->left->len)
		       ? -1
		       : 0);
	  node->observed = (node->left
			    ? node->left->observed
			    : 0);
	  node->observation_contingent = (node->left
					  ? node->left->observation_contingent
					  : 1);
	  node->small_advised_p = 0;
	  node->max_enclosed_paren = (node->left ? node->left->max_enclosed_paren : 0);
	  node->min_enclosed_paren = (node->left ? node->left->min_enclosed_paren : 0);
	  break;

	case  r_interval:
	  if (!node->left || (node->left->len == 0) || (node->intval2 == 0))
	    node->len = 0;
	  else if (node->left->len < 0)
	    node->len = -1;
	  else if (node->intval == node->intval2)
	    {
	      if (node->left->len > (SSIZE_MAX / node->intval))
		node->len = -1;
	      else
		node->len = node->left->len * node->intval;
	    }
	  else
	    node->len = -1;
	  node->observed = 1;
	  node->observation_contingent = 0;
	  node->small_advised_p = 0;
	  node->max_enclosed_paren = (node->left ? node->left->max_enclosed_paren : 0);
	  node->min_enclosed_paren = (node->left ? node->left->min_enclosed_paren : 0);
	  break;

	case r_parens:
	  if (node->intval > 0)
	    {
	      node->observed = 1;
	      (*subexps)[this_subexp] = node;
	    }
	  else
	    node->observed = (node->left
			      ? node->left->observed
			      : 0);
	  node->observation_contingent = (   node->observed 
					  && (node->left
					      ? node->left->observation_contingent
					      : 1));
	  node->len = (node->left
		       ? node->left->len
		       : 0);
	  node->small_advised_p = (!node->left || node->left->small_advised_p);
	  node->max_enclosed_paren = rx_max (node->intval, (node->left ? node->left->max_enclosed_paren : 0));

	  if (node->left && node->left->min_enclosed_paren)
	    {
	      if (!node->intval)
		node->min_enclosed_paren = node->left->min_enclosed_paren;
	      else
		node->min_enclosed_paren = rx_min (node->intval, node->left->min_enclosed_paren);
	    }
	  else
	    node->min_enclosed_paren = node->intval;
	  break;

	case r_context:
	  switch (node->intval)
	    {
	    default:
	      node->small_advised_p = 1;
	      node->observed = 1;
	      node->observation_contingent = 0;
	      node->len = -1;
	      break;
	    case '^':
	    case '$':
	      node->small_advised_p = 1;
	      node->observed = 1;
	      node->observation_contingent = 0;
	      node->len = 0;
	      break;
	    }
	  node->max_enclosed_paren = 0;
	  node->min_enclosed_paren = 0;
	  break;
	}
    }
  return 0;
}

