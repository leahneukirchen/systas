/* rgx.c - scheme regexp interface
 *
 ****************************************************************
 * Copyright (C) 1997, 2001, 2002 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include <stdlib.h>
#include <setjmp.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "hackerlab/bugs/panic.h"
#include "systas/libsystas/systas.h"
#include "hackerlab/bitsets/bitset.h"
#include "hackerlab/hash/hashtree.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/rx-posix/regexps.h"
#include "hackerlab/rx/escape.h"
#include "hackerlab/rx/dfa.h"
#include "hackerlab/rx/dfa-cache.h"
#include "hackerlab/rx-posix/re8-parse.h"
#include "systas/libsystas/rgx.h"


static long scm_tc16_regex_t;
#define RGX(X)	((regex_t *)SCM_CDR(X))

struct rx_dfa_state
{
  struct rx_dfa frame;
  struct rx_unfa * unfa;
};

static long scm_tc16_dfa_t;
#define DFA(X)	((struct rx_dfa_state *)SCM_CDR(X))



/* __STDC__ prototypes for static functions */
static void scm_rx_poll_fn (void);
static size_t free_regex_t (SCM obj);
static int print_regex_t (SCM obj, SCM port, int writing);
static size_t free_dfa_t (SCM obj);
static int print_dfa_t (SCM obj, SCM port, int writing);
static int scm_is_dfa (SCM obj);
static int scm_cflags_to_int (SCM value);
static int scm_eflags_to_int (SCM value);
static SCM scm_rx_error_to_symbol (int error);



/* cflags 
 */
#define CFLAGS \
    EXPAND_FLAG (REG_EXTENDED) \
    EXPAND_FLAG (REG_ICASE) \
    EXPAND_FLAG (REG_NEWLINE) \
    EXPAND_FLAG (REG_NOSUB) \
    EXPAND_FLAG (REG_DFA_ONLY)

/* eflags 
 */
#define EFLAGS \
    EXPAND_FLAG (REG_NOTBOL) \
    EXPAND_FLAG (REG_NOTEOL) \
    EXPAND_FLAG (REG_NO_SUBEXP_REPORTING)

#define EXPAND_FLAG(NAME)	SCM_SYMBOL (s_ ## NAME, # NAME);
CFLAGS
EFLAGS
#undef EXPAND_FLAG

#undef RX_ERRNO
#define RX_ERRNO(NAME, STRING)	SCM_SYMBOL (s_ ## NAME, # NAME);
RX_ERRNO_LIST
#undef RX_ERRNO

/* symbols used to pick match parts:
 */
SCM_SYMBOL (s_lt, "<");
SCM_SYMBOL (s_gt, ">");
SCM_SYMBOL (s_state_label, "state-label");
SCM_SYMBOL (s_cut, "cut");



/* error messages
 */
SCM_STRING (s_unrecognized_cflag, "unrecognized cflag for regcomp");
SCM_STRING (s_unrecognized_eflag, "unrecognized eflag for regexec");
SCM_STRING (s_allocation_failure, "allocation failure");
SCM_STRING (s_internal_error_in_rx_unfa, "internal error constructing rx_unfa");
SCM_STRING (s_internal_error_in_, "internal error constructing rx_unfa");
SCM_STRING (s_internal_error_in_rx_dfa_advance_to_final, "internal error in rx_dfa_advance_to_final");
SCM_STRING (s_dfa_has_no_current_state, "dfa has no current state"); 


/************************************************************************
 *(h0 "Regexps and Regular Expressions")
 * 
 * 
 * 
 */
/*(menu)
 */


/*(c compiled-regexp?)
 * (compiled-regexp? obj)
 * 
 * Return `#t' if `obj' is a compiled regular expression `#f' otherwise.
 */
SCM_PROC (s_compiled_regexp_p, "compiled-regexp?", 1, 0, 0, scm_compiled_regexp_p);
SCM
scm_compiled_regexp_p (SCM obj)
{
  return (scm_is_compiled_regexp (obj)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}

/************************************************************************
 *(h1 "Compiling Regexps")
 * 
 * 
 * 
 */


/*(c regcomp)
 * (regcomp pattern :optional cflags)
 * 
 * Compile `pattern' to produce a compiled regular expression.
 * 
 * See the documentation for `regcomp'.
 */
SCM_PROC(s_regcomp, "regcomp", 1, 1, 0, scm_regcomp);
SCM
scm_regcomp (SCM pattern, SCM cflags)
{
  SCM answer;
  int icflags;

  SCM_ASSERT (scm_is_ro_string (pattern), pattern, scm_arg1, s_regcomp);

  icflags = scm_cflags_to_int (cflags);

  SCM_NEWCELL (answer);
  SCM_DEFER_INTS;
  {
    regex_t * it;
    int status;
    it = malloc (sizeof (*it));
    if (!it)
      {
      allocation:
	SCM_ALLOW_INTS;
	SCM_ASSERT (0, pattern, s_allocation_failure, s_regcomp);
      }
    status = regncomp (it, SCM_RO_CHARS (pattern), SCM_RO_LENGTH (pattern), icflags);
    if (status)
      {
	free (it);
	answer = scm_rx_error_to_symbol (status);
      }
    else
      {
	it->owner_data = (void *)malloc (SCM_RO_LENGTH (pattern) + 1);
	if (!it->owner_data)
	  {
	    regfree (it);
	    free (it);
	    goto allocation;
	  }
	mem_move (it->owner_data, SCM_RO_CHARS (pattern), SCM_RO_LENGTH (pattern));
	((char *)it->owner_data)[SCM_RO_LENGTH (pattern)] = 0;
	SCM_CAR (answer) = scm_tc16_regex_t;
	SCM_CDR (answer) = (SCM)it;
      }
  }
  SCM_ALLOW_INTS;
  return answer;
}


/************************************************************************
 *(h1 "Comparing Strings to Regexps")
 * 
 * 
 * 
 */

static void
scm_rx_poll_fn (void)
{
  if (1 == scm_async_clock)
    rx_escape ();
}


/*(c regexec)
 * (regexec rgx str :optional match-pick eflags return error-return)
 * 
 * This is a complex procedure, with many convenience features.  If you
 * use regexps, it is worth learning its details.
 * 
 * `regexec' compares `str' to compiled regular expression `rgx'.
 * The argument `match-pick' describes what values to return if a 
 * match occurs.
 * 
 * `eflags' is a list of symbols (such as `REG_NOTBOL')
 * that refine the search done by `regexec' -- see the documentation
 * for `regexec' in the *libhackerlab* reference manual, or the `man'
 * page on most unix-like systems.
 * 
 * If `return' is missing, and a match is found, the data specified by
 * `match-pick' is simply returned.  If `return' is provided, and 
 * `match-pick' specifies that a list should be returned, the `return'
 * is applied to the elements of that list.  If `match-pick' specifies
 * a non-list value, `return' is applied to just that value.
 * 
 * If `error-return' is missing, and no match is found, `#f' is returned.
 * Otherwise, `(error-return)' is invoked.
 * 
 * The most complex part of the interface is the parameter `match-pick'.
 * If `match-pick' is:
 * 
 * \`#f'/ -- return `#t' if `str' contains a match for `rgx'.
 * 
 * \`#t'/ -- return a list of three shared substrings of `str' which are the
 * text to the left of the match, the match itself, and the text to the right
 * of the match.
 * 
 * \a vector whose 0 element is `cut'/ -- If the final state label is `N', return
 * the `N'th element of the vector, or `N' itself if `N' is negative or the vector
 * has fewer than `N' elements.
 * 
 * \a vector/ -- For each vector element `N > 0', if subexpression `N' matched, 
 * fill that vector element with a pair of integers `(start-offset . end-offset)' 
 * describing the position of the subexpression match.  Fill vector element 0 with
 * a pair describing the position of the entire match.  Fill other vector elements
 * with `#f'.
 * 
 * \a keyword/ -- return the keyword.
 * 
 * \`<'/ (the symbol) -- return a shared substring of `str' which is the text to 
 * the left of the match.
 *
 * \`>'/ (the symbol) -- return a shared substring of `str' which is the text to 
 * the right of the match.
 * 
 * \`state-label'/ (the symbol) -- return the integer state label of the final DFA
 * state reached during the match.
 * 
 * \`N'/ (an integer) -- return a shared substring which is the text matched by
 * subexpression `N' (0 for the entire match).
 * 
 * \a list/ -- process each list element individually, returning a
 * list of the indicated results.  Elements of the list may be `<',
 * `>', `state-label', a vector whose 0 element is `cut', or an
 * integer (as above) or a "complex substring specification"
 * (described below).
 * 
 * A "complex substring specification" is a two element list:
 * 
 * 	(start-position end-position)
 * 
 * which describes the starting and ending positions of a substring of
 * `str'.
 * 
 * `start-position' may be:
 * 
 * \`N'/ (an integer) -- the beginning of the substring matched by
 * subexpression `N' (0 for the beginning of the entire match).
 * 
 * \`<'/ -- the beginning of `str'.
 * 
 * \`>'/ -- the end of the match.
 * 
 * \`(part . extreme)'/ -- this is an "arbitrary endpoint specifier",
 * described below.
 * 
 * \`end-position'/ may be:
 * 
 * \`N'/ (an integer) -- the end of the substring matched by
 * subexpression `N' (0 for the beginning of the entire match).
 * 
 * \`<'/ -- the beginning of the match.
 * 
 * \`>'/ -- the end of `str'.
 * 
 * \`(part . extreme)'/ -- this is an "arbitrary endpoint specifier",
 * described below.
 * 
 * An "arbitrary endpoint specifier" is of the form `(part
 * . extreme)'.  `extreme' may be either 0 (meaning ``starting point
 * of'') or 1 (meaning ``end point of'') and `part' may be either an
 * integer subexpression number (0 for the entire match), `<' (text to
 * the left of the match), or '>' (text to the right of the match).
 * 
 */
SCM_PROC (s_low_level_regexec, "low-level-regexec", 4, 0, 0, scm_low_level_regexec);
SCM
scm_low_level_regexec (SCM rgx, SCM str, SCM match_pick, SCM efl)
{
  SCM answer;
  int eflags;
  int pick_is_vector;
  int vector_result;
  int cut_label_table;
  regmatch_t * pmatch;
  SCM malloc_protect;

  SCM_ASSERT (scm_is_compiled_regexp (rgx), rgx, scm_arg1, s_low_level_regexec);
  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg2, s_low_level_regexec);

  eflags = scm_eflags_to_int (efl);
  pick_is_vector = scm_is_vector (match_pick);
  cut_label_table = pick_is_vector && (SCM_LENGTH (match_pick) > 0) && (SCM_VECTOR_ELTS (match_pick)[0] == s_cut);
  vector_result = pick_is_vector && !cut_label_table;

  SCM_DEFER_INTS;
  {
    int status;
    pmatch = 0;
    rx_poll = scm_rx_poll_fn;
    status = regnexec (RGX(rgx), SCM_RO_CHARS (str), SCM_RO_LENGTH (str),
		       0, &pmatch, eflags | REG_ALLOC_REGS);
    rx_poll = 0;
    if (status)
      {
	SCM_ALLOW_INTS;
	if (status == REG_NOMATCH)
	  return SCM_BOOL_F;
	else if (status == REG_MATCH_INTERRUPTED)
	  return scm_makerrno (EINTR);
	else
	  scm_throw (SCM_CAR (scm_intern0 ("regexp-error")),
		     scm_listify (SCM_MAKINUM (status),
				  rgx,
				  str,
				  match_pick,
				  efl,
				  SCM_UNDEFINED));
      }
    if (match_pick == SCM_BOOL_F)
      {
	if (pmatch)
	  free (pmatch);
      }
    else if (pmatch)
      {
	malloc_protect = scm_take_str ((char *)pmatch, RGX(rgx)->re_nsub * sizeof (regmatch_t));
	scm_remember (&malloc_protect);
      }
  }
  SCM_ALLOW_INTS;

  if (match_pick == SCM_BOOL_F)
    return SCM_BOOL_T;
  else if ((match_pick == SCM_BOOL_T) || (match_pick == SCM_UNDEFINED))
    {
      answer = (scm_listify
		(scm_make_shared_substring (str, SCM_MAKINUM (0), SCM_MAKINUM (pmatch[0].rm_so)),
		 scm_make_shared_substring (str, SCM_MAKINUM (pmatch[0].rm_so),
					    SCM_MAKINUM (pmatch[0].rm_eo)),
		 scm_make_shared_substring (str, SCM_MAKINUM (pmatch[0].rm_eo), SCM_UNDEFINED),
		 SCM_UNDEFINED));
      return answer;
    }
  else if (cut_label_table)
    {
      int n;

      n = pmatch[0].final_tag;

      if ((n > 0) && (n < SCM_LENGTH (match_pick)))
	{
	  answer = SCM_VECTOR_ELTS (match_pick)[n];
	  return answer;
	}
      else
	{
	  return scm_long2num ((long)n);
	}
    }
  else if (vector_result)
    {
      int i;
      size_t bound;
      int vlen;

      bound = RGX(rgx)->re_nsub;
      vlen = SCM_LENGTH (match_pick);
      if (vlen < bound)
	bound = vlen;
      for (i = 0; i < bound; ++i)
	if (pmatch[0].rm_so >= 0)
	  SCM_VECTOR_ELTS (match_pick)[i] = scm_cons (SCM_MAKINUM (pmatch[i].rm_so),
					    SCM_MAKINUM (pmatch[i].rm_eo));
	else
	  SCM_VECTOR_ELTS (match_pick)[i] = SCM_BOOL_F;
      while (i < vlen)
	{
	  SCM_VECTOR_ELTS (match_pick)[i] = SCM_BOOL_F;
	  ++i;
	}
      return match_pick;
    }
  else
    {
      int no_list;
      SCM * ans_pos;
      SCM spec;
      size_t bound;

      if (!SCM_IS_IMMEDIATE (match_pick) && SCM_CONSP (match_pick))
	no_list = 0;
      else
	{
	  match_pick = scm_cons (match_pick, SCM_EOL);
	  no_list = 1;
	}

      answer = SCM_EOL;
      ans_pos = &answer;
      bound = RGX(rgx)->re_nsub;

      for (spec = match_pick; spec != SCM_EOL; spec = SCM_CDR (spec))
	{
	  SCM item;
	  SCM frm;
	  SCM to;

	  SCM_ASSERT (!SCM_IS_IMMEDIATE (spec) && SCM_CONSP (spec), spec, scm_arg3, s_low_level_regexec);
	  item = SCM_CAR (spec);

	  if (scm_is_keyword (item))
	    {
	      *ans_pos = scm_cons (item, SCM_EOL);
	    }
	  else if (scm_is_symbol (item))
	    {
	      if (item == s_lt)
		{
		  frm = SCM_INUM0;
		  to = SCM_MAKINUM (pmatch[0].rm_so);
		  *ans_pos = scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL);
		}
	      else if (item == s_gt)
		{
		  frm = SCM_MAKINUM (pmatch[0].rm_eo);
		  to = SCM_UNDEFINED;
		  *ans_pos = scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL);
		}
	      else if (item == s_state_label)
		{
		  *ans_pos = scm_cons (SCM_MAKINUM (pmatch[0].final_tag), SCM_EOL);
		}
	      else
		SCM_ASSERT (0, spec, scm_arg3, s_low_level_regexec);
	    }
	  else if (scm_is_vector (item) && (SCM_LENGTH (item) > 0) && (SCM_VECTOR_ELTS (item)[0] == s_cut))
	    {
	      int n;
	      
	      n = pmatch[0].final_tag;
	      
	      if ((n > 0) && (n < SCM_LENGTH (item)))
		{
		  *ans_pos = scm_cons (SCM_VECTOR_ELTS (item)[n], SCM_EOL);
		}
	      else
		{
		  *ans_pos = scm_cons (scm_long2num ((long)n), SCM_EOL);
		}
	    }
	  else if (!SCM_IS_IMMEDIATE (item) && SCM_CONSP (item))
	    {
	      SCM fp;
	      SCM tp;
	      SCM frm_spec[2];
	      SCM to_spec[2];
	      int part[2];
	      
	      fp = SCM_CAR (item);
	      item = SCM_CDR (item);
	      SCM_ASSERT (!SCM_IS_IMMEDIATE (item) && SCM_CONSP (item), item, scm_arg3, s_low_level_regexec);
	      tp = SCM_CAR (item);
	      
	      if (!SCM_IS_IMMEDIATE (fp) && SCM_CONSP (fp))
		{
		  frm_spec[0] = SCM_CAR (fp);
		  fp = SCM_CDR (fp);
		  SCM_ASSERT (!SCM_IS_IMMEDIATE (fp) && SCM_CONSP (fp), fp, scm_arg3, s_low_level_regexec);
		  frm_spec[1] = SCM_CAR (fp);
		}
	      else
		{
		  frm_spec[0] = fp;
		  frm_spec[1] = SCM_INUM0;
		}
	      
	      if (!SCM_IS_IMMEDIATE (tp) && SCM_CONSP (tp))
		{
		  to_spec[0] = SCM_CAR (tp);
		  tp = SCM_CDR (tp);
		  SCM_ASSERT (!SCM_IS_IMMEDIATE (tp) && SCM_CONSP (tp), tp, scm_arg3, s_low_level_regexec);
		  to_spec[1] = SCM_CAR (tp);
		}
	      else
		{
		  to_spec[0] = tp;
		  to_spec[1] = SCM_MAKINUM (1);
		}

	      SCM_ASSERT (SCM_INUMP (frm_spec[1]), frm_spec[1], scm_arg3, s_low_level_regexec);
	      part[0] = SCM_INUM (frm_spec[1]);

	      SCM_ASSERT (SCM_INUMP (to_spec[1]), to_spec[1], scm_arg3, s_low_level_regexec);
	      part[1] = SCM_INUM (to_spec[1]);

	      if (   SCM_INUMP (frm_spec[0])
		  && (SCM_INUM (frm_spec[0]) < bound)
		  && (SCM_INUM (frm_spec[0]) >= 0))
		{
		  int reg;
		  reg = SCM_INUM (frm_spec[0]);
		  frm = SCM_MAKINUM (part[0] ? pmatch[reg].rm_eo : pmatch[reg].rm_so);
		}
	      else if (scm_is_symbol (frm_spec[0]))
		{
		  if (frm_spec[0] == s_lt)
		    frm = (part[0] ? SCM_MAKINUM (pmatch[0].rm_so) : SCM_INUM0);
		  else if (frm_spec[0] == s_gt)
		    frm = (part[0]
			   ? SCM_MAKINUM (SCM_RO_LENGTH (str))
			   : SCM_MAKINUM (pmatch[0].rm_eo));
		  else
		    SCM_ASSERT (0, frm_spec[0], scm_arg3, s_low_level_regexec);
		}
	      else
		SCM_ASSERT (0, frm_spec[0], scm_arg3, s_low_level_regexec);

	      if (   SCM_INUMP (to_spec[0])
		  && (SCM_INUM (to_spec[0]) < bound)
		  && (SCM_INUM (to_spec[0]) >= 0))
		{
		  int reg;
		  reg = SCM_INUM (to_spec[0]);
		  to = SCM_MAKINUM (part[1] ? pmatch[reg].rm_eo : pmatch[reg].rm_so);
		}
	      else if (scm_is_symbol (to_spec[0]))
		{
		  if (to_spec[0] == s_lt)
		    to = (part[1] ? SCM_MAKINUM (pmatch[0].rm_so) : SCM_INUM0);
		  else if (to_spec[0] == s_gt)
		    to = (part[1]
			  ? SCM_MAKINUM (SCM_RO_LENGTH (str))
			  : SCM_MAKINUM (pmatch[0].rm_eo));
		  else
		    SCM_ASSERT (0, to_spec[0], scm_arg3, s_low_level_regexec);
		}
	      else 
		SCM_ASSERT (0, to_spec[0], scm_arg3, s_low_level_regexec);

	      *ans_pos = scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL);
	    }
	  else
	    {
	      int n;

	      SCM_ASSERT (SCM_INUMP (item), spec, scm_arg3, s_low_level_regexec);
	      n = SCM_INUM (item);
	      SCM_ASSERT ((n >= 0) && (n < bound), spec, scm_outofrange, s_low_level_regexec);
	      if (pmatch[n].rm_so < 0)
		{
		  *ans_pos = scm_cons (SCM_BOOL_F, SCM_EOL);
		  ans_pos = &SCM_CDR (*ans_pos);
		  continue;
		}
	      frm = SCM_MAKINUM (pmatch[n].rm_so);
	      to = SCM_MAKINUM (pmatch[n].rm_eo);
	      *ans_pos = scm_cons (scm_make_shared_substring (str, frm, to), SCM_EOL);
	    }
	  ans_pos = &SCM_CDR (*ans_pos);
	}
      if (!no_list)
	return answer;
      else
	return SCM_CAR (answer);
    }
}


/*(c set-rx-dfa-cache-threshold)
 * (set-rx-dfa-cache-threshold n)
 * 
 * Set the goal threshold (in bytes) of the Rx superstate cache.  *xref*.
 */
SCM_PROC (s_set_rx_dfa_cache_threshold, "set-rx-dfa-cache-threshold", 1, 0, 0, scm_set_rx_dfa_cache_threshold);
SCM
scm_set_rx_dfa_cache_threshold (SCM sn)
{
  int n;

  n = scm_num2long (sn, scm_arg1, s_set_rx_dfa_cache_threshold);
  SCM_DEFER_INTS;
  rx_set_dfa_cache_threshold (n);
  SCM_ALLOW_INTS;
  return SCM_UNSPECIFIED;
}


/************************************************************************
 *(h1 "Compiling Regular Expressions")
 * 
 * 
 * 
 */

/*(c regexp->dfa)
 * (regexp->dfa regexp cflags)
 * 
 * Compile `regexp' to produce a DFA.
 */
SCM_PROC (s_regexp_to_dfa, "regexp->dfa", 1, 1, 0, scm_regexp_to_dfa);
SCM 
scm_regexp_to_dfa (SCM regexp, SCM cfl)
{
  int ret;
  struct rx_dfa_state *r;
  struct rx_exp_node * parsed;
  int ign;
  int cflags;
  char * pattern;
  int len;
  SCM answer;

  SCM_ASSERT (scm_is_ro_string (regexp),
	      regexp, scm_arg1, s_regexp_to_dfa);

  cflags = scm_cflags_to_int (cfl);
  pattern = SCM_RO_CHARS (regexp);
  len = SCM_RO_LENGTH (regexp);

  SCM_NEWCELL (answer);

  SCM_DEFER_INTS;
  ign = 0;
  ret = rx_parse (&parsed,
		  &ign,
		  pattern, 
		  len,
		  (cflags & REG_EXTENDED),
		  (cflags & REG_NEWLINE),
		  (cflags & REG_DFA_ONLY),
		  256,
		  0);
  if (ret)
    {
      SCM_ALLOW_INTS;
      return SCM_MAKINUM (ret);
    }


  {
    struct rx_unfa * unfa;
    unfa = rx_unfa (parsed, 256);
    if (!unfa)
      {
	rx_free_exp (parsed);
	SCM_ALLOW_INTS;
	return SCM_MAKINUM (REG_ESPACE);
      }
    r = (struct rx_dfa_state *)scm_must_malloc (sizeof (struct rx_dfa_state));
    r->unfa = unfa;
  }
  rx_free_exp (parsed);
  if (!r->unfa)
    {
      scm_mallocated -= sizeof (*r);
      scm_must_free ((char *)r);
      SCM_ALLOW_INTS;
      SCM_ASSERT (0, regexp, s_internal_error_in_rx_unfa, s_regexp_to_dfa);
    }

  rx_init_dfa_from_nfa (&r->frame, r->unfa->nfa);
  SCM_CAR (answer) = scm_tc16_dfa_t;
  SCM_CDR (answer) = (SCM)r;
  SCM_ALLOW_INTS;
  return answer;
}

/************************************************************************
 *(h1 "DFA Procedures")
 * 
 * 
 * 
 */

/*(c dfa-fork)
 * (dfa-fork dfa)
 * 
 * Create a copy of `dfa'.  The copy is initially in the same state as `dfa'.
 */
SCM_PROC (s_dfa_fork, "dfa-fork", 1, 0, 0, scm_dfa_fork);
SCM
scm_dfa_fork (SCM dfa)
{
  struct rx_dfa_state *r;
  SCM answer;

  SCM_ASSERT (scm_is_dfa (dfa),
	      dfa, scm_arg1, s_dfa_fork);

  SCM_NEWCELL (answer);

  SCM_DEFER_INTS;
  r = (struct rx_dfa_state *)scm_must_malloc (sizeof (struct rx_dfa_state));
  rx_save_unfa (DFA (dfa)->unfa);
  r->unfa = DFA (dfa)->unfa;
  rx_init_dfa_from_nfa (&r->frame, r->unfa->nfa);
  r->frame.state = DFA(dfa)->frame.state;
  if (r->frame.state)
    rx_lock_superstate (r->frame.rx, r->frame.state);
  SCM_CAR (answer) = scm_tc16_dfa_t;
  SCM_CDR (answer) = (SCM)r;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c reset-dfa!)
 * (reset-dfa! dfa)
 * 
 * Return `dfa' to its starting state.
 */
SCM_PROC (s_reset_dfa_x, "reset-dfa!", 1, 0, 0, scm_reset_dfa_x);
SCM
scm_reset_dfa_x (SCM dfa)
{
  int err;
  SCM_ASSERT (scm_is_dfa (dfa),
	      dfa, scm_arg1, s_reset_dfa_x);
  
  SCM_DEFER_INTS;
  err = rx_dfa_goto_start_superstate (&DFA(dfa)->frame, 1);
  SCM_ALLOW_INTS;
  return (err ? SCM_BOOL_F : dfa);
}


/*(c dfa-final-tag)
 * (dfa-final-tag dfa)
 * 
 * Return the integer state label of the current state of `dfa'.
 */
SCM_PROC (s_dfa_final_tag, "dfa-final-tag", 1, 0, 0, scm_dfa_final_tag);
SCM
scm_dfa_final_tag (SCM dfa)
{
  SCM_ASSERT (scm_is_dfa (dfa),
	      dfa, scm_arg1, s_dfa_final_tag);

  return scm_long2num (DFA (dfa)->frame.final_tag);
}


/*(c dfa-continuable?)
 * (dfa-continuable? dfa)
 * 
 * Return `#t' if there are transitions out of the current state of
 * `dfa'.
 */
SCM_PROC (s_dfa_continuable_p, "dfa-continuable?", 1, 0, 0, scm_dfa_continuable_p);
SCM
scm_dfa_continuable_p (SCM dfa)
{
  SCM_ASSERT (scm_is_dfa (dfa),
	      dfa, scm_arg1, s_dfa_continuable_p);

  return (rx_dfa_can_continue (&DFA (dfa)->frame)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/*(c %advance-dfa-to-final-state!)
 * (%advance-dfa-to-final-state! dfa str)
 * 
 * Advance `dfa' using characters from `str'.  Stop when `dfa' enters
 * a final state.
 */
SCM_PROC (s_sys_advance_dfa_to_final_state_x, "%advance-dfa-to-final-state!", 2, 0, 0, scm_sys_advance_dfa_to_final_state_x);
SCM
scm_sys_advance_dfa_to_final_state_x (SCM dfa, SCM s)
{
  struct rx_dfa_state * d;
  char * str;
  int len;
  size_t matched;
  int status;
  int interrupted;
  
  SCM_ASSERT (scm_is_dfa (dfa),
	      dfa, scm_arg1, s_sys_advance_dfa_to_final_state_x);
  SCM_ASSERT (scm_is_ro_string (s),
	      s, scm_arg2, s_sys_advance_dfa_to_final_state_x);

  str = SCM_RO_CHARS (s);
  len = SCM_RO_LENGTH (s);
  d = DFA (dfa);

  SCM_ASSERT (d->frame.state, dfa, s_dfa_has_no_current_state, s_sys_advance_dfa_to_final_state_x);

  SCM_DEFER_INTS;
  interrupted = 0;
  rx_poll = scm_rx_poll_fn;
  if (setjmp (rx_escape_jmp_buf))
    {
      interrupted = 1;
    }
  else
    {
      status = rx_dfa_advance_to_final (&matched, &d->frame, (t_uchar *)str, len);
    }
  rx_poll = 0;
  SCM_ALLOW_INTS;
  
  if (interrupted)
    return scm_makerrno (EINTR);
  else if (status < 0)
    return scm_makerrno (ENOMEM);
  else
    return scm_return_first (SCM_MAKINUM (matched), dfa, s);
}



static size_t
free_regex_t (SCM obj)
{
  regex_t *r;
  r = RGX(obj);
  free ((char *)(r->owner_data));
  regfree (r);
  return 0;
}

static int
print_regex_t (SCM obj, SCM port, int writing)
{
  regex_t *r;
  int errn;

  r = RGX (obj);
  scm_port_puts (&errn, port, "#<rgx ");
  scm_port_puts (&errn, port, (char *)(r->owner_data));
  scm_port_puts (&errn, port, ">");
  return 1;
}


static scm_small_object_functions regex_t_smob =
{ scm_mark0, free_regex_t, print_regex_t, 0 };

int
scm_is_compiled_regexp (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_CAR (obj) == (SCM)scm_tc16_regex_t);
}



static size_t
free_dfa_t (SCM obj)
{
  struct rx_dfa_state *r;
  r = DFA(obj);
  rx_clear_dfa_state (&r->frame);
  rx_free_unfa (r->unfa);
  scm_must_free ((char *)r);
  return sizeof (struct rx_dfa_state);
}

static int
print_dfa_t (SCM obj, SCM port, int writing)
{
  struct rx_dfa_state *r;
  int errn;

  r = DFA (obj);
  scm_port_puts (&errn, port, "#<dfa ");
  scm_intprint (r->frame.rx->rx_id, 10, port);
  scm_port_puts (&errn, port, ">");
  return 1;
}

static scm_small_object_functions dfa_t_smob =
{ scm_mark0, free_dfa_t, print_dfa_t, 0 };

static int
scm_is_dfa (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_CAR (obj) == (SCM)scm_tc16_dfa_t);
}




static int
scm_cflags_to_int (SCM value)
{
  if ((SCM_UNDEFINED == value) || (SCM_BOOL_F == value))
    return 0;
  else if (SCM_INUMP (value))
    return SCM_INUM (value);
  else
    {
      int answer;
      answer = 0;
      if (scm_is_symbol (value))
	value = scm_cons (value, SCM_EOL);
      while (!SCM_IS_IMMEDIATE (value) && SCM_CONSP (value))
	{
	  SCM f;
	  f = SCM_CAR (value);
#define EXPAND_FLAG(NAME)	if (f == s_ ## NAME) answer |= NAME; else
	  CFLAGS
	    scm_throw (s_unrecognized_cflag, scm_cons (f, SCM_EOL));
#undef EXPAND_FLAG;
	  value = SCM_CDR (value);
	}
      return answer;
    }
}


static int
scm_eflags_to_int (SCM value)
{
  if ((SCM_UNDEFINED == value) || (SCM_BOOL_F == value))
    return 0;
  else if (SCM_INUMP (value))
    return SCM_INUM (value);
  else
    {
      int answer;
      answer = 0;
      if (scm_is_symbol (value))
	value = scm_cons (value, SCM_EOL);
      while (!SCM_IS_IMMEDIATE (value) && SCM_CONSP (value))
	{
	  SCM f;
	  f = SCM_CAR (value);
#define EXPAND_FLAG(NAME)	if (f == s_ ## NAME) answer |= NAME; else
	  EFLAGS
	    scm_throw (s_unrecognized_eflag, scm_cons (f, SCM_EOL));
#undef EXPAND_FLAG;
	  value = SCM_CDR (value);
	}
      return answer;
    }
}


static SCM
scm_rx_error_to_symbol (int error)
{
  if (error == 0)
    panic ("scm_rx_error_to_symbol called with error == 0");

  switch (error)
    {
    default:
      return SCM_MAKINUM (error);

#define RX_ERRNO(NAME, STRING) case NAME: return s_ ## NAME;
      RX_ERRNO_LIST
    }
}



void
scm_init_rgx (void)
{
  scm_tc16_regex_t = scm_newsmob (&regex_t_smob);
  scm_tc16_dfa_t = scm_newsmob (&dfa_t_smob);
#include "systas/libsystas/rgx.x"
}

