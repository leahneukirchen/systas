/* posix.c - posix regexp compatability functions
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/malloc.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/str.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/rx/tree.h"
#include "hackerlab/rx/escape.h"
#include "hackerlab/rx/nfa-cache.h"
#include "hackerlab/rx/dfa.h"
#include "hackerlab/rx-posix/re8-parse.h"
#include "hackerlab/rx-posix/posix.h"


#define RX_MANY_CASES 30


/* __STDC__ prototypes for static functions */
static int rx_regexec (regmatch_t pmatch[],
		       int no_subexp_reporting,
		       int no_pos_reporting,
		       const regex_t *preg,
		       struct rx_context_rules * rules,
		       rx_off_t start,
		       rx_off_t end,
		       const char *string);
static int is_simple_anchored_regexp (int * anchored_start, int * anchored_end, struct rx_exp_node * pattern, int no_subexp_reporting);
static int rx_regmatch (regmatch_t pmatch[],
			int no_subexp_reporting,
			int no_pos_reporting,
			const regex_t *preg,
			struct rx_context_rules * rules,
			rx_off_t start,
			rx_off_t end,
			const char *string,
			int is_bos,
			int is_eos);
static void rx_is_anchored_p (int * left,
			      int * right,
			      struct rx_exp_node * exp);
static int rx_fill_in_fastmap (int cset_size, t_uchar * map, struct rx_exp_node * exp);


/************************************************************************
 *(h1 "Posix Regexp Functions"
 *    :includes ("sys/types.h"
 * 		 "hackerlab/rx-posix/regex.h"))
 * 
 * |Posix regexp functions|
 * |Posix| |Posix.2| |1003.2| |ANSI/IEEE 1003.2| |ISO/IEDC 994502|
 * 
 * The standard Posix regexp functions provided by Rx are:
 * 
 * 	regcomp
 * 	regexec
 * 	regfree
 * 	regerror
 * 
 * Two closely related but nonstandard functions are also provided:
 * 
 * 	regncomp
 * 	regnexec
 * 
 */



#if 0
/*(c regcomp)
 * int regcomp (regex_t * preg, const char * pattern, int cflags);
 * 
 * Compile the 0-terminated regexp specification `pattern'.
 *
 * The compiled pattern is stored in `*preg', which has the field
 * (required by Posix):
 * 
 * 	size_t re_nsub;		   The number of parenthesized
 * 				   subexpressions in the compiled
 * 				   pattern.
 * 
 *
 * `cflags' is a combination of bits which effect compilation:
 *
 insert*/

enum rx_cflags
{
  REG_EXTENDED = 1,
    /*  If REG_EXTENDED is set, then use extended regular expression
	syntax.  If not set, then use basic regular expression
	syntax. In extended syntax, none of the regexp operators are
	written with a backslash. */


  REG_ICASE = (REG_EXTENDED << 1),
  /*   	If REG_ICASE is set, then ignore case when matching.  If not
     	set, then case is significant. */

 
  REG_NOSUB = (REG_ICASE << 1),
  /* 	Report only success/failure in `regexec'.
   	Using this flag can improve performance for
	some regexps. */

  REG_NEWLINE = (REG_NOSUB << 1),
  /*	If REG_NEWLINE is set, then "." and complemented character
	sets do not match at newline characters in the string.  Also,
	"^" and "$" do match at newlines.
	
	If not set, then anchors do not match at newlines and the
	character sets contain newline.*/

  REG_DFA_ONLY = (REG_NEWLINE << 1),
  /*	If this bit is set, then restrict the pattern 
	language to patterns that compile to efficient 
	state machines.  In particular, `regexec' will
	not report positions for parenthesized subexpressions;
	 "^", "$", backreferences ("\n"), and duplication
	 ("{n,m}") are interpreted as normal characters.

	REG_DFA_ONLY is a non-standard flag. */
};
/*end-insert
 * 
 * `regcomp' returns 0 on success and an error code on failure (see
 * xref:"regerror").
 */
#endif
int
regcomp (regex_t * preg, const char * pattern, int cflags)
{
  return regncomp (preg, pattern, str_length (pattern), cflags);
}


/*(c regncomp)
 * int regncomp (regex_t * preg,
 *               const char * pattern,
 *               size_t len,
 *               int cflags);
 * 
 * Compile the `len'-byte regexp specification `pattern'.
 *
 * 
 * The compiled pattern is stored in `*preg', which has the field
 * (required by Posix):
 * 
 * 	size_t re_nsub;		   The number of parenthesized
 * 				   subexpressions in the compiled
 * 				   pattern.
 * 
 * `cflags' is a combination of bits which effect compilation.  See
 * xref:"regcomp".
 *
 * `regncomp' returns 0 on success and an error code on failure (see
 * xref:"regerror").
 * 
 * \Note:/ `regncomp' is not part of the Posix.2 interface for
 * regexp matching.  It is an Rx extension.
 */
int
regncomp (regex_t * preg,
	  const char * pattern,
	  size_t len,
	  int cflags)
{
  int ret;
  struct rx_exp_node * exp;
  int nsub;

  mem_set0 ((char *)preg, sizeof (*preg));

  if (!(cflags & REG_ICASE))
    {
      preg->icase = 0;
      preg->translate = 0;
    }
  else
    {
      unsigned i;

      preg->icase = 1;

      preg->translate = (t_uchar *) rx_nfa_cache_malloc (256);
      if (!preg->translate)
        return (int) REG_ESPACE;

      /* Map uppercase characters to corresponding lowercase ones.  */
      for (i = 0; i < 256; i++)
        preg->translate[i] = char_is_upper (i) ? char_to_lower (i) : i;
    }

  ret = rx_parse (&exp,
		  &nsub,
		  pattern, len,
		  (cflags & REG_EXTENDED),
		  (cflags & REG_NEWLINE),
		  (cflags & REG_DFA_ONLY),
		  256,
		  preg->translate);

  /* POSIX doesn't distinguish between an unmatched open-group and an
   * unmatched close-group: both are REG_EPAREN.
   */
  if ((ret == REG_ELPAREN) || (ret == REG_ERPAREN))
    ret = REG_EPAREN;

  if (ret)
    return (int)ret;

  if (!(cflags & REG_NEWLINE))
    preg->newline_anchor = 0;
  else
    preg->newline_anchor = 1;

  preg->pattern = exp;
  preg->re_nsub = 1;
  preg->subexps = 0;
  if (rx_analyze_rexp (&preg->subexps, &preg->re_nsub, preg->pattern))
    {
      rx_free_exp (preg->pattern);
      rx_nfa_cache_free ((void *)preg->subexps);
      mem_set0 ((char *)preg, sizeof (*preg));
      return REG_ESPACE;
    }
  preg->is_nullable = rx_fill_in_fastmap (256,
					  preg->fastmap,
					  preg->pattern);
  rx_is_anchored_p (&preg->is_left_anchored, &preg->is_right_anchored, preg->pattern);
  preg->no_sub = !!(cflags & REG_NOSUB);
  preg->small_p = (!preg->pattern || preg->pattern->small_advised_p);
  return 0;
}




#if 0
/*(c regexec)
 * int regexec (const regex_t *preg,
 *              const char *string,
 *              size_t nmatch,
 *              regmatch_t pmatch[],
 *              int eflags);
 * 
 * Search for a match of compiled regexp `preg' in `string'.
 * Return the positions of the match and the first `nmatch-1' 
 * parenthesized subexpressions in `pmatch'.
 *
 * Return 0 if a match is found, an error code otherwise.  See
 * xref:"regerror".
 * 
 * It is possible to asynchronously abort a call to `regexec'.  See
 * xref:"Escaping Long-Running Matches".
 *
 * `preg' must have been filled in by `regcomp' or `regncomp'.
 * 
 * `string' must be 0 terminated.  See xref:"regnexec".
 *
 * `nmatch' may be 0 and must not be negative (Posix specifies that
 * the parameter be declared signed).  It is the number of elements
 * in the array pointed to by `pmatch'.
 *
 * `pmatch' may be 0 if `nmatch' is 0.  The details of `regmatch_t' are:
 *
 insert*/
struct rx_registers
{
  regoff_t rm_so;	/* Byte offset to substring start.  */
  regoff_t rm_eo;	/* Byte offset to substring end.  */

  int final_tag;	/* In pmatch[0] this field is set to
			 * the state label of the last DFA state 
			 * encountered during a match.
			 * 
			 * This field is implementation specific.
			 * Applications which intend to be portable
			 * between implementations of Posix should
			 * not use this field.
			 */
};
/*end-insert
 * 
 * The state label |state label (in Posix regexps)| of the final DFA state for most regexps is 1.  If a
 * pattern contains the cut operator |cut (in Posix regexps)| `[[:cut <n>:]]' |[[:cut n:]]| its DFAs will
 * contain a final state with label `n' at that point in the regexp.
 * This is useful for detecting which of several possible alternatives
 * actually occured in a match, as in this example:
 *
 *	pattern: if[[:cut 1:]]\\|while[[:cut 2:]]
 * 
 *	  pmatch[0].final_tag is 1 after matching "if" 
 *	  pmatch[0].final_tag is 2 after matching "while"
 *
 * `eflags' is a bit-wise or (`|') of any of these values:
 *
 insert*/
enum rx_eflags
{
  REG_NOTBOL = 1,
  /* If REG_NOTBOL is set, then the beginning-of-line operator `^'
   * doesn't match the beginning of the input string (presumably
   * because it's not the beginning of a line).  If not set, then the
   * beginning-of-line operator does match the beginning of the
   * string.
   * 
   * (Standardized in Posix.2)
   */

  REG_NOTEOL = (REG_NOTBOL << 1),
  /* REG_NOTEOL is similar to REG_NOTBOL, except that it applies to
   * the end-of-line operator `$' and the end of the input string.
   * 
   * (Standardized in Posix.2)
   */

  REG_NO_SUBEXP_REPORTING = (REG_NOTEOL << 1),
  /* REG_NO_SUBEXP_REPORTING causes `regexec' to fill in only
   * `pmatch[0]' and to ignore other elements of `pmatch'.  For some
   * patterns (those which do not contain back-references or anchors)
   * this can speed up matching considerably.
   * 
   * (non-standard)
   */

  REG_ALLOC_REGS = (REG_NO_SUBEXP_REPORTING << 1),
  /* REG_ALLOC_REGS is only used by `regnexec'.  It causes `regnexec' 
   * to allocate storage for `regmatch_t' values.
   * 
   * (non-standard)
   */
};
/*end-insert
 *
 * The match returned satisfies the left-most longest rule |left-most longest rule (in Posix regexps)| which states
 * a left-most match of the overall regexp will be returned.  Of those
 * matches, one of the longest will be returned.
 *
 * There may be more than one longest match because two matches of
 * equal length may differ in how they fill in the array `pmatch'.  
 * For example:
 *
 *     	"aaaabbbb" can match \(a*\)\(a*b*\)
 * 	     with pmatch[1] == "aaaa"	[*]
 * 	      and pmatch[2] == "bbbb"
 * 	   or
 * 	     with pmatch[1] == "aaa"
 * 	      and pmatch[2] == "abbbb"
 * 	   or
 * 	     with pmatch[1] == "aa"
 * 	      and pmatch[2] == "aabbbb"
 * 	   or
 * 	     with pmatch[1] == "a"
 * 	      and pmatch[2] == "aaabbbb"
 * 	   or
 * 	     with pmatch[1] == ""
 * 	      and pmatch[2] == "aaaabbbb"
 * 
 * 
 * 
 * Of the possible values of `pmatch', Rx implements the standard
 * behavior of returning that match which recursively maximizes the
 * lengths of the substrings matched by each subpattern, from left to
 * right.  In the preceeding example, the correct answer is marked
 * with `[*]'.
 *
 */
#endif
int
regexec (const regex_t *preg,
	 const char *string,
	 size_t nmatch,
	 regmatch_t pmatch[],
	 int eflags)
{
  return regnexec (preg,
		   string,
		   str_length (string),
		   nmatch,
		   &pmatch,
		   (eflags & ~REG_ALLOC_REGS));
}


/*(c regnexec)
 * int regnexec (const regex_t *preg,
 *               const char *string,
 *               regoff_t length,
 *               size_t nmatch,
 *               regmatch_t ** pmatch,
 *               int eflags);
 * 
 * Search for a match of compiled regexp `preg' in `string'.
 * Return the positions of the match and the first `nmatch-1' 
 * parenthesized subexpressions in `*pmatch'.
 *
 * Return 0 if a match is found, an error code otherwise.  See
 * xref:"regerror".
 * 
 * `preg' must have been filled in by `regcomp' or `regncomp'.
 * 
 * `string' must be `length' bytes long.
 *
 * See xref:"regnexec" for details about other parameters but
 * note that `regnexec' and `regexec' use different types for
 * the parameter `pmatch'.
 * 
 * In `regexec', `pmatch' is only used to pass a pointer.  In
 * `regnexec', `pmatch' is used both to pass a pointer, and to return
 * a pointer to the caller.
 *
 * Callers are permitted to pass 0 for `nmatch' and `pmatch'.  Callers
 * are also permitted to pass the address of a pointer whose value is
 * 0 for parameter `pmatch'.  If they do so, and also set the bit
 * `REG_ALLOC_REGS' in `eflags', then `pmatch' will be a return
 * parameter, returning a malloced array of `preg->re_nsub' elements
 * containing the sub-expression positions of a successful match.
 *
 * It is possible to asynchronously abort a call to `regnexec'.  See
 * xref:"Escaping Long-Running Matches".
 *
 * \Note:/ `regnexec' is not part of the Posix.2 interface for
 * regexp matching.  It is an Rx extension.
 */
int
regnexec (const regex_t *preg,
	  const char *string,
	  regoff_t len,
	  size_t nmatch,
	  regmatch_t **pmatch,
	  int eflags)
{
  struct rx_context_rules rules;
  regmatch_t * regs;
  size_t nregs;
  int stat;

  rules.newline_anchor = preg->newline_anchor;
  rules.not_bol = !!(eflags & REG_NOTBOL);
  rules.not_eol = !!(eflags & REG_NOTEOL);
  rules.case_indep = preg->icase;

  if (!preg->no_sub && (nmatch >= preg->re_nsub))
    {
      regs = *pmatch;
      nregs = nmatch;
    }
  else
    {
      regs = (regmatch_t *)malloc (preg->re_nsub * sizeof (*regs));
      if (!regs)
	return REG_ESPACE;
      nregs = preg->re_nsub;
    }

  {
    size_t x;
    for (x = 0; x < nregs; ++x)
      regs[x].rm_so = regs[x].rm_eo = -1;
  }


  stat = rx_regexec (regs,
		     (preg->no_sub || (nmatch == 1) || (eflags & REG_NO_SUBEXP_REPORTING)),
		     preg->no_sub,
		     preg,
		     &rules,
		     0, len,
		     string);
  
  if (!stat && pmatch && !preg->no_sub && (regs != *pmatch))
    {
      size_t x;
      for (x = 0; x < nmatch; ++x)
	(*pmatch)[x] = regs[x];
    }

  if (!stat && (eflags & REG_ALLOC_REGS))
    *pmatch = regs;
  else if (regs && (!pmatch || (regs != *pmatch)))
    free (regs);
  
  return stat;
}


/*(c regfree)
 * void regfree (regex_t *preg);
 * 
 * Release all storage allocated for the compiled regexp `preg'.
 * This does not free `preg' itself.
 */
void
regfree (regex_t *preg)
{
  if (preg->pattern)
    {
      rx_free_exp (preg->pattern);
      preg->pattern = 0;
    }
  if (preg->subexps)
    {
      rx_nfa_cache_free (preg->subexps);
      preg->subexps = 0;
    }
  if (preg->translate != 0)
    {
      rx_nfa_cache_free (preg->translate);
      preg->translate = 0;
    }
}




/*(c regerror)
 * size_t regerror (int errcode,
 *                  const regex_t *preg,
 *                  char *errbuf,
 *                  size_t errbuf_size);
 * 
 * Returns a message corresponding to an error code, `errcode',
 * returned from either `regcomp' or `regexec'.  The size of the
 * message is returned.  At most, `errbuf_size - 1' characters of the
 * message are copied to `errbuf'.  Whatever is stored in `errbuf' is
 * 0-terminated.
 * 
 * |error codes (for Posix regexps)|
 * The POSIX error codes for regexp pattern matchers are:
 * 
 *	REG_NOMATCH	"no match"
 *	REG_BADPAT	"invalid regular expression"
 *	REG_ECOLLATE	"invalid collation character"
 *	REG_ECTYPE	"invalid character class name"
 *	REG_EESCAPE	"trailing backslash"
 *	REG_ESUBREG	"invalid back reference"
 *	REG_EBRACK	"unmatched [ or [^"
 *	REG_EPAREN	"unmatched (, \\(, ) or \\)"
 *	REG_EBRACE	"unmatched \\{"
 *	REG_BADBR	"invalid content of \\{\\}"
 *	REG_ERANGE	"invalid range end"
 *	REG_ESPACE	"memory exhausted"
 *	REG_BADRPT	"invalid preceding regular expression"
 *
 * Rx also provides a non-standard error code that is used if
 * `regexec' or `regnexec' is interrupted (see xref:"Escaping
 * Long-Running Matches").
 * 	
 *	REG_MATCH_INTERRUPTED	"match interrupted"
 * 
 */
size_t
regerror (int errcode,
	  const regex_t *preg,
	  char *errbuf,
	  size_t errbuf_size)
{
  const char *msg;
  size_t msg_size;

  msg = (rx_error_msg[errcode] == 0) ? "Success" : (char *)rx_error_msg[errcode];
  msg_size = str_length (msg) + 1; /* Includes the 0.  */
  if (errbuf_size != 0)
    {
      if (msg_size > errbuf_size)
        {
          str_cpy_n (errbuf, (t_uchar *)msg, errbuf_size - 1);
          errbuf[errbuf_size - 1] = 0;
        }
      else
        str_cpy (errbuf, (t_uchar *)msg);
    }
  return msg_size;
}


static int
rx_regexec (regmatch_t pmatch[],
	    int no_subexp_reporting,
	    int no_pos_reporting,
	    const regex_t *preg,
	    struct rx_context_rules * rules,
	    rx_off_t start,
	    rx_off_t end,
	    const char *string)
{
  int x;
  int stat;
  int left_anchored;
  struct rx_exp_node * simplified;
  struct rx_unfa * unfa;
  struct rx_dfa machine;
  int have_machine;

  left_anchored = preg->is_left_anchored;

  unfa = 0;
  have_machine = 0;
  if (!preg->small_p && ((end - start) > RX_MANY_CASES))
    {
      int err;
      err = rx_simplify_rexp (&simplified, 256, preg->pattern, preg->subexps);
      if (err)
	return REG_ESPACE;

      unfa = rx_unfa (simplified, 256);
      if (!unfa)
	{
	  rx_free_exp (simplified);
	  return REG_ESPACE;
	}
      rx_init_dfa_from_nfa ((struct rx_dfa *)&machine, unfa->nfa);
      have_machine = 1;
      rx_free_exp (simplified);
    }

  for (x = start; x <= end; ++x)
    {
      if (preg->is_nullable
	  || ((x < end)
	      && (preg->fastmap[((t_uchar *)string)[x]])))
	{
	  if (!preg->is_nullable && !preg->small_p && ((end - start) > RX_MANY_CASES))
	    {
	      size_t amt;
	      int adv;

	      if (rx_dfa_goto_start_superstate ((struct rx_dfa *)&machine, 1))
		{
		espace_error:
		  rx_clear_dfa_state ((struct rx_dfa *)&machine);
		  rx_free_unfa ((struct rx_unfa *)unfa);
		  return REG_ESPACE;
		}

	      if (setjmp (rx_escape_jmp_buf))
		{
		  rx_clear_dfa_state ((struct rx_dfa *)&machine);
		  rx_free_unfa ((struct rx_unfa *)unfa);
		  return REG_MATCH_INTERRUPTED;
		}
	      adv = rx_dfa_advance_to_final (&amt, (struct rx_dfa *)&machine, string + x, end - start - x);
	      if (0 > adv)
		goto espace_error;
	      if (!adv || (!machine.final_tag && (amt < (end - start - x))))
		goto nomatch;
	      if (no_pos_reporting && (!preg->pattern || !preg->pattern->observed))
		{
		  rx_clear_dfa_state ((struct rx_dfa *)&machine);
		  rx_free_unfa ((struct rx_unfa *)unfa);
		  return 0;
		}
	    }
	  stat = rx_regmatch (pmatch, no_subexp_reporting, no_pos_reporting, preg, rules, x, end, string, (x == start), 1);
	  if (!stat || (stat != REG_NOMATCH))
	    {
	      if (have_machine)
		rx_clear_dfa_state ((struct rx_dfa *)&machine);
	      rx_free_unfa ((struct rx_unfa *)unfa);
	      return stat;
	    }
	}
    nomatch:
      if (left_anchored)
	{
	  if (!preg->newline_anchor)
	    {
	      if (have_machine)
		rx_clear_dfa_state ((struct rx_dfa *)&machine);
	      rx_free_unfa ((struct rx_unfa *)unfa);
	      return REG_NOMATCH;
	    }
	  else
	    while (x < end)
	      {
		if (string[x] == '\n')
		  break;
		else
		  ++x;
	      }
	}
    }
  if (have_machine)
    rx_clear_dfa_state ((struct rx_dfa *)&machine);
  rx_free_unfa ((struct rx_unfa *)unfa);
  return REG_NOMATCH;
}


static int
is_simple_anchored_regexp (int * anchored_start, int * anchored_end, struct rx_exp_node * pattern, int no_subexp_reporting)
{
  struct rx_exp_node * left;
  struct rx_exp_node * right;
  int left_start;
  int left_end;
  int left_is;
  int right_start;
  int right_end;
  int right_is;


  *anchored_start = 0;
  *anchored_end = 0;
  
  if (!pattern)
    return 0;
  
  if (   (pattern->type == r_context)
      && ((pattern->intval == '^')
	  || (pattern->intval == '$')))
    {
      *anchored_start = (pattern->intval == '^');
      *anchored_end = (pattern->intval == '$');
      return 1;
    }
  
  if (   (pattern->type != r_concat)
      && (pattern->type != r_right_concat)
      && (pattern->type != r_alternate)
      && (   (pattern->type != r_parens)
	  || (!no_subexp_reporting && pattern->intval)))
    return 0;

  if (pattern->type == r_parens)
    return is_simple_anchored_regexp (anchored_start, anchored_end, pattern->left, no_subexp_reporting);

  left = pattern->left;
  right = pattern->right;

  left_is = is_simple_anchored_regexp (&left_start, &left_end, left, no_subexp_reporting);
  right_is = is_simple_anchored_regexp (&right_start, &right_end, right, no_subexp_reporting);

  if (!left_is && !right_is)
    return 0;

  if (!left_is && left)
    {
      if (left->observed && (!no_subexp_reporting || !left->observation_contingent))
	return 0;
    }

  if (!right_is && right)
    {
      if (right->observed && (!no_subexp_reporting || !right->observation_contingent))
	return 0;
    }

  if ((pattern->type == r_concat) || (pattern->type == r_right_concat))
    {
      if (left_is && left_end && right)
	return 0;
      if (right_is && right_start && left)
	return 0;
      *anchored_start = left_start || right_start;
      *anchored_end = left_end || right_end;
      return 1;
    }
  else
    {
      /* r_alternate */
      if ((left_start != right_start) || (left_end != right_end))
	return 0;
      *anchored_start = left_start;
      *anchored_end = left_end;
      return 1;
    }
}


static int
rx_regmatch (regmatch_t pmatch[],
	     int no_subexp_reporting,
	     int no_pos_reporting,
	     const regex_t *preg,
	     struct rx_context_rules * rules,
	     rx_off_t start,
	     rx_off_t end,
	     const char *string,
	     int is_bos,
	     int is_eos)
{
  struct rx_solutions * solutions;
  int answer;
  struct rx_context_rules local_rules;
  rx_off_t orig_end;
  rx_off_t end_lower_bound;
  rx_off_t end_upper_bound;
  int simple_anchored;
  int anchored_start;
  int anchored_end;
  
  local_rules = *rules;
  orig_end = end;

  if (!preg->pattern)
    {
      end_lower_bound = start;
      end_upper_bound = start;
    }
  else if (preg->pattern->len >= 0)
    {
      if ((end - start) < preg->pattern->len)
	return REG_NOMATCH;
      end_lower_bound = start + preg->pattern->len;
      end_upper_bound = start + preg->pattern->len;
    }
  else
    {
      end_lower_bound = start;
      end_upper_bound = end;
    }

  simple_anchored = is_simple_anchored_regexp (&anchored_start, &anchored_end, preg->pattern, no_subexp_reporting);
  if (   simple_anchored
      || !preg->pattern
      || !preg->pattern->observed
      || (preg->pattern->observation_contingent && no_subexp_reporting))
    {
      int free_pattern;
      struct rx_exp_node * pattern;
      struct rx_unfa * unfa;
      struct rx_dfa machine;
      rx_off_t best;
      int best_label;
      rx_off_t pos;
      size_t amt;

      if (simple_anchored && anchored_start && rules->not_bol && is_bos)
	return REG_NOMATCH;

      free_pattern = 0;

      if (!simple_anchored || !preg->pattern || !preg->pattern->observed)
	pattern = preg->pattern;
      else
	{
	  int err;
	  err = rx_simplify_rexp (&pattern, 256, preg->pattern, preg->subexps);
	  if (err)
	    return REG_ESPACE;
	  free_pattern = 1;
	}

      unfa = rx_unfa (pattern, 256);

      if (free_pattern)
	rx_free_exp (pattern);
      if (!unfa)
	return REG_ESPACE;

      rx_init_dfa_from_nfa ((struct rx_dfa *)&machine, unfa->nfa);
      if (rx_dfa_goto_start_superstate ((struct rx_dfa *)&machine, 1))
	{
	espace_error:
	  rx_clear_dfa_state ((struct rx_dfa *)&machine);
	  rx_free_unfa (unfa);
	  return REG_ESPACE;
	}

      if (setjmp (rx_escape_jmp_buf))
	{
	  rx_clear_dfa_state ((struct rx_dfa *)&machine);
	  rx_free_unfa (unfa);
	  return REG_MATCH_INTERRUPTED;
	}
      
      best = -1;
      best_label = 0;
      pos = start;
      if (machine.final_tag)
	{
	  if (   !simple_anchored
	      || !anchored_end
	      || (rules->newline_anchor && (pos < orig_end) && (string[pos] == '\n'))
	      || (!rules->not_eol && is_eos && (orig_end == start)))
	    {
	      if (no_pos_reporting)
		{
		  rx_clear_dfa_state ((struct rx_dfa *)&machine);
		  rx_free_unfa (unfa);
		  return 0;
		}
	      best = start;
	      best_label = machine.final_tag;
	    }
	}
      while (pos < end_upper_bound)
	{
	  int adv;

	  adv = rx_dfa_advance_to_final (&amt, (struct rx_dfa *)&machine, string + pos, end_upper_bound - pos);
	  if (0 > adv)
	    goto espace_error;
	  if (!adv || !machine.final_tag)
	    break;
	  pos += amt;
	  if (   !simple_anchored
	      || !anchored_end
	      || (rules->newline_anchor && (pos < orig_end) && (string[pos] == '\n'))
	      || (!rules->not_eol && is_eos && (pos == orig_end)))
	    {
	      if (no_pos_reporting)
		{
		  rx_clear_dfa_state ((struct rx_dfa *)&machine);
		  rx_free_unfa (unfa);
		  return 0;
		}
	      best = pos;
	      best_label = machine.final_tag;
	    }
	}
      if ((best >= 0) && pmatch)
	{
	  pmatch[0].rm_so = start;
	  pmatch[0].rm_eo = best;
	  pmatch[0].final_tag = best_label;
	}
      rx_clear_dfa_state ((struct rx_dfa *)&machine);
      rx_free_unfa (unfa);
      if (best < 0)
	return REG_NOMATCH;
      else
	return 0;
    }

  answer = 0;

  {
#define n_end_guess 256
    regoff_t end_guesses[n_end_guess];
    int valid_end_guesses;
    int total_end_guesses;
    int end_guess_position;
    int no_end_guess_optimization;
    int next_no_end_guess_optimization;
    struct rx_exp_node * pattern;
    struct rx_unfa * unfa;
    struct rx_dfa machine;
    int end_search_direction;

    next_no_end_guess_optimization = (end_upper_bound == end_lower_bound);
#if 0
    /* to disable DFA optimization: */
    next_no_end_guess_optimization = 1;
#endif

    anchored_start = preg->is_left_anchored;
    anchored_end = preg->is_right_anchored;

    {
      int err;
      err = rx_simplify_rexp (&pattern, 256, preg->pattern, preg->subexps);
      if (err)
	return REG_ESPACE;
    }
    
    unfa = rx_unfa (pattern, 256);
    rx_free_exp (pattern);
    if (!unfa)
      return REG_ESPACE;
    
    rx_init_dfa_from_nfa ((struct rx_dfa *)&machine, unfa->nfa);
    
    /* precondition: end is the last end-point to check for a match.
     *		     That's end_lower_bound no_pos_reporting,
     *		     and end_upper_bound otherwise.
     *
     * 		     if no_pos_reporting is not 0, 
     *		     The DFA is in the right state for 
     *		     processing the character at position `end'.
     *
     *		     end_search_direction = (no_pos_reporting ? 1 : -1)
     */

    if (!no_pos_reporting)
      {
	end = end_upper_bound;
	end_search_direction = -1;
      }
    else
      {
	end = end_lower_bound;
	end_search_direction = 1;

	if (rx_dfa_goto_start_superstate ((struct rx_dfa *)&machine, 1))
	  {
	  espace_error0:
	    rx_clear_dfa_state ((struct rx_dfa *)&machine);
	    rx_free_unfa (unfa);
	    return REG_ESPACE;
	  }

	if (setjmp (rx_escape_jmp_buf))
	  {
	    rx_clear_dfa_state ((struct rx_dfa *)&machine);
	    rx_free_unfa (unfa);
	    return REG_MATCH_INTERRUPTED;
	  }

	if (start < end)
	  {
	    if (!rx_dfa_advance (&machine, string + start, end - start))
	      goto espace_error0;
	  }
      }

    while (no_pos_reporting ? (end <= end_upper_bound) : (end >= end_lower_bound))
      {
	rx_off_t pos;
#define POSITIONS_REMAIN 	(no_pos_reporting ? (pos <= end_upper_bound) : (pos >= end_lower_bound))

#define VALID_FINAL_POSITION \
		(   !anchored_end \
		 || ((pos == orig_end) && is_eos && !rules->not_eol) \
		 || ((pos < orig_end) && rules->newline_anchor && (string[pos] == '\n')))


  	/* invariant:
	 *
	 * end is the next plausible end-position of a match.
	 *
	 * next_no_end_guess_optimization says whether or not DFA checks are
	 *				  likely to pay off by eliminating some possible
	 * 				  values of `end' from consideration.
	 * 
	 * If no_pos_reporting, the DFA is set to process the character
	 * at `end'.
	 */

	no_end_guess_optimization = next_no_end_guess_optimization;
	
	if (no_end_guess_optimization)
	  {
	    /* Arrange to check all possible end points without doing a DFA check.
	     */
	    valid_end_guesses = 0;
	    end_guess_position = n_end_guess - 1;
	    pos = end;

	    while (POSITIONS_REMAIN && (valid_end_guesses < n_end_guess))
	      {
		if (VALID_FINAL_POSITION)
		  {
		    end_guesses[end_guess_position] = pos;
		    --end_guess_position;
		    ++valid_end_guesses;
		  }
		pos += end_search_direction;
	      }
	    end_guess_position = 0;
	    /* Post-condition:
	     *
	     * Treating end_guesses as a circular stack, their are valid_end_guesses
	     * items on the stack which are possible end positions in the order
	     * they should be checked.
	     */
	  }
	else
	  {
	    size_t amt;

	    /* Arrange to check end points that pass a DFA check.
	     */
	    if (no_pos_reporting)
	      {
		/* We'll find possible end positions in the order they should
		 * be checked, so treat end_guesses as a queue.
		 */
		end_guess_position = n_end_guess - 1;

		/* end is the next position to check for possible
		 * end-of-match.  The DFA is already set to process this character.
		 */
		pos = end;
	      }
	    else
	      {
		/* We'll find possible end positions in the opposite of the order they should
		 * be checked, so treat end_guesses as a stack.
		 */
		end_guess_position = 0;

		/* end is the last position to check for possible
		 * end-of-match.  Checking begins at `start'.
		 */
		pos = start;
		if (rx_dfa_goto_start_superstate ((struct rx_dfa *)&machine, 1))
		  {
		  espace_error2:
		    rx_clear_dfa_state ((struct rx_dfa *)&machine);
		    rx_free_unfa (unfa);
		    return REG_ESPACE;
		  }
	      }

	    valid_end_guesses = 0;
	    total_end_guesses = 0;

	    if (setjmp (rx_escape_jmp_buf))
	      {
		rx_clear_dfa_state ((struct rx_dfa *)&machine);
		rx_free_unfa (unfa);
		return REG_MATCH_INTERRUPTED;
	      }

	    if (machine.final_tag && VALID_FINAL_POSITION)
	      {
		end_guesses[end_guess_position] = pos;
		++valid_end_guesses;
		++total_end_guesses;

		end_guess_position -= end_search_direction;
	      }

	    {
	      while (pos < end_upper_bound)
		{
		  int adv;

		  if (no_pos_reporting)
		    adv = rx_dfa_advance_to_final (&amt, (struct rx_dfa *)&machine, string + pos, end_upper_bound - pos);
		  else
		    adv = rx_dfa_advance_to_final (&amt, (struct rx_dfa *)&machine, string + pos, end - pos);
		  if (0 > adv)
		    goto espace_error2;
		  if (!adv || !machine.final_tag)
		    break;
		  pos += amt;
		  if (!VALID_FINAL_POSITION)
		    continue;
		  end_guesses[end_guess_position] = pos;
		  if (no_pos_reporting)
		    {
		      ++valid_end_guesses;
		      if (valid_end_guesses == n_end_guess)
			{
			  end_guess_position = 0;
			  break;
			}
		      else
			--end_guess_position;
		    }
		  else
		    {
		      if (valid_end_guesses < n_end_guess)
			++valid_end_guesses;
		      end_guess_position = ((end_guess_position + 1) % n_end_guess);
		      ++total_end_guesses;
		    }
		}

	      if (no_pos_reporting)
		end_guess_position = 0;
	    }

	    /* end_guesses is now a stack of values which when popped,
	     * are plausible end positions int the correct order to 
	     * check.
	     */
	  }

	if (!valid_end_guesses)
	  {
	    rx_clear_dfa_state ((struct rx_dfa *)&machine);
	    rx_free_unfa (unfa);
	    return REG_NOMATCH;
	  }

	if (no_end_guess_optimization || (total_end_guesses == ((end_upper_bound - end_lower_bound) + 1)))
	  next_no_end_guess_optimization = 1;

	while (valid_end_guesses)
	  {
	    end_guess_position = (((end_guess_position + n_end_guess) - 1) % n_end_guess);
	    --valid_end_guesses;
	    end = end_guesses[end_guess_position];

	    local_rules.not_eol = (rules->not_eol
				   
				   ? (   ((end == orig_end) && is_eos)
				      || !local_rules.newline_anchor
				      || (string[end] != '\n')) /* string[end] is valid because either (end < orig_end) || !is_eos */
				   
				   : (   (end != orig_end)
				      && (!local_rules.newline_anchor
					  || (string[end] != '\n'))));
	    solutions = rx_basic_make_solutions (pmatch, preg->pattern, preg->subexps, preg->re_nsub,
						 start, end, &local_rules, string, preg->small_p);
	    if (!solutions)
	      {
		rx_clear_dfa_state ((struct rx_dfa *)&machine);
		rx_free_unfa (unfa);
		return REG_ESPACE;
	      }
      
	    if (setjmp (rx_escape_jmp_buf))
	      {
		rx_clear_dfa_state ((struct rx_dfa *)&machine);
		rx_free_unfa (unfa);
		rx_basic_free_solutions (solutions);
		return REG_MATCH_INTERRUPTED;
	      }
	    answer = rx_next_solution (solutions);
	    if (answer < 0)
	      {
		rx_clear_dfa_state ((struct rx_dfa *)&machine);
		rx_free_unfa (unfa);
		rx_basic_free_solutions (solutions);
		return REG_ESPACE;
	      }

	    if (answer == 1)
	      {
		if (!no_pos_reporting && pmatch)
		  {
		    pmatch[0].rm_so = start;
		    pmatch[0].rm_eo = end;
		    pmatch[0].final_tag = rx_solutions_final_tag (solutions);
		  }
		rx_basic_free_solutions (solutions);
		rx_clear_dfa_state ((struct rx_dfa *)&machine);
		rx_free_unfa (unfa);
		return 0;
	      }
	    else
	      rx_basic_free_solutions (solutions);
	  }
	/* post condition: end is the last end position tried.
	 */
	end += end_search_direction;
      }
    rx_clear_dfa_state ((struct rx_dfa *)&machine);
    rx_free_unfa (unfa);
  }

  return REG_NOMATCH;
}




/*c rx_is_anchored_p
 * void rx_is_anchored_p (int * left,
 *			  int * right,
 *			  struct rx_exp_node * exp);
 *
 * Is an expression "anchored", meaning, must it match at string
 * or line boundary on either the left or right (`^' or `$')?
 *
 * Knowing whether or not an expression is anchored is useful for
 * optimizing some common kinds of regexp search, so this function
 * computes that property.
 *  
 insert*/
static void
rx_is_anchored_p (int * left,
		  int * right,
		  struct rx_exp_node * exp)
{
  int ign;

  if (!left)
    left = &ign;

  if (!right)
    right = &ign;

  if (!exp)
    {
      *left = 0;
      *right = 0;
      return;
    }

  switch (exp->type)
    {
    default:
    case r_star:
    case r_cset:
    case r_string:
    case r_cut:
      *left = 0;
      *right = 0;
      break;

    case r_parens:
      rx_is_anchored_p (left, right, exp->left);
      break;

    case r_concat:
    case r_right_concat:
      if (!left)
	rx_is_anchored_p (left, right, exp->right);
      else if (!right)
	rx_is_anchored_p (left, right, exp->left);
      else
	{
	  rx_is_anchored_p (left, &ign, exp->left);
	  rx_is_anchored_p (&ign, right, exp->right);
	}
      break;
	  

    case r_alternate:
      {
	int l1;
	int r1;
	int l2;
	int r2;

	rx_is_anchored_p (&l1, &r1, exp->left);
	rx_is_anchored_p (&l2, &r2, exp->right);
	*left = l1 && l2;
	*right = r1 && r2;
	break;
      }

    case r_interval:
      if (exp->intval == 0)
	{
	  *left = 0;
	  *right = 0;
	}
      else
	rx_is_anchored_p (left, right, exp->left);
      break;
      
    case r_context:
      if (exp->intval == '^')
	{
	  *left = 1;
	  *right = 0;
	}
      else if (exp->intval == '$')
	{
	  *left = 0;
	  *right = 1;
	}
      else
	{
	  *left = 0;
	  *right = 0;
	}
      break;
    }
}
/*end-insert
 */

/*c rx_fill_in_fastmap
 * int rx_fill_in_fastmap (int cset_size,
 *                         t_uchar * map,
 *                         struct rx_exp_node * exp);
 *
 * If a pattern can not match the empty string, then there is
 * a set of characters (the "fastmap") from which the first character 
 * of a matching string must come.  For some patterns, the fastmap is 
 * smaller than the complete character set and is easy to compute.  
 * Knowing the fastmap is useful for optimizing some kinds of
 * regexp search.
 *
 * This function returns (in `map') a set represented as an array of
 * 256 bytes, with entries for members of the fastmap set equal to 1,
 * and other entries equal to 0.
 */
static int
rx_fill_in_fastmap (int cset_size, t_uchar * map, struct rx_exp_node * exp)
{
  if (!exp)
    {
    can_match_empty:
      {
	int x;
	for (x = 0; x < cset_size; ++x)
	  map[x] = 1;
      }
      return 1;
    }
  
  switch (exp->type)
    {
    case r_cset:
      {
	int x;
	int most;
	
	most = exp->cset_size;
	for (x = 0; x < most; ++x)
	  if (bits_is_member (exp->cset, x))
	    map[x] = 1;
      }
      return 0;

    case r_string:
      if (exp->str_len)
 	{
	  map[exp->str[0]] = 1;
	  return 0;
 	}
      else
	return 1;

    case r_concat:
    case r_right_concat:
      return (   rx_fill_in_fastmap (cset_size, map, exp->left)
	      && rx_fill_in_fastmap (cset_size, map, exp->right));

    case r_alternate:
      return (   rx_fill_in_fastmap (cset_size, map, exp->left)
	      || rx_fill_in_fastmap (cset_size, map, exp->right));

    case r_parens:
      return rx_fill_in_fastmap (cset_size, map, exp->left);

    case r_star:
      goto can_match_empty;

    case r_interval:
      if (exp->intval == 0)
	goto can_match_empty;
      else
	return rx_fill_in_fastmap (cset_size, map, exp->left);
      
    case r_cut:
      goto can_match_empty;
      
    case r_context:
      goto can_match_empty;

    default:
      while (1)
	panic ("bogus regexp in rx_fill_in_fastmap");
    }
}
