/* posix.h - Posix.2 compatability functions
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#ifndef INCLUDE__RX_POSIX__POSIX_H
#define INCLUDE__RX_POSIX__POSIX_H



#include "hackerlab/rx-posix/errnorx.h"
#include "hackerlab/rx-posix/dup-max.h"
#include "hackerlab/rx-posix/match-regexp.h"




/* RE_DUP_MAX	(Required by Posix.2) An upper bound on the values
 * 		`m' and `n' in a regexp of the form `RE{m,n}'.
 * 
 * Posix requires this to be declared in "<limits.h>".
 * 
 * The correct value for Rx is redefined (as a macro) in "hackerlab/rx-posix/limits.h".
 */


/* regoff_t	(Required by Posix.2)  Used to represent offsets to
 * 		substrings within a string matched by `regexec'.
 * 		`regoff_t' is a signed arithmetic type that can hold
 * 		the largest value that can be stored in either `off_t'
 * 		or `ssize_t'.
 * 
 * 		This declaration fails to conform to Posix.2 if
 * 		either `off_t' or `ssize_t' is `long long' -- but 
 * 		that is unlikely to be the case.
 */
typedef long regoff_t;

/* regmatch_t	(Required by Posix.2).  Offsets to substrings matched
 *		by parenthesized subexpressions.
 *
 * This structure includes the (required) fields:
 * 
 * 	regoff_t rm_so; 	Byte offset from start of string to
 * 				start of substring.
 * 	regoff_t rm_eo;		Byte offset from start of string to
 * 				the first character after the end
 * 				of substring.
 */
typedef struct rx_registers regmatch_t;

/* regex_t	(Required by Posix.2) A compiled regexp, filled in
 * 		by `regcomp'.
 */
typedef struct rx_posix_regex
{
  /* re_nsub	The number of parenthesized subexpressions.
   * 		Filled in by `regcomp'.
   * 
   * This field is required by Posix.2
   * 
   */
  size_t re_nsub;


  /****************************************************************
   * The remaining fields are implementation specific and are 
   * filled in by `regcomp'.
   */

  /* pattern	The expression tree for the pattern.
   * 		The reference count for the expression
   * 		is incremented for `pattern'.
   */
  struct rx_exp_node * pattern;

  /* subexps	Pointers to the `re_nsub' parenthesized subexpressions
   * 		of pattern.  The reference counts are NOT incremented
   * 		for these references.
   */
  struct rx_exp_node ** subexps;


  /* icase	non-0 iff regcomp was passed REG_ICASE
   */
  int icase;

  /* translate	0 or a 256 element mapping from characters to characters.
   * 		Regcomp reads the pattern source string through this table
   * 		and compiles a pattern that acts as if the target string
   * 		is translated through this table.  Used to implement
   * 		REG_ICASE.
   */
  t_uchar * translate;

  /* newline_anchor	If not 0, ^ matches after \n, $ before \n.
   * 			Set by passing REG_NEWLINE to `regcomp'.
   */
  t_uchar newline_anchor;	/* If true, an anchor at a newline matches.*/

  /* is_left_anchored	If not 0, the pattern effectively begins with `^'.
   *			For example, "(^abc)|(^def)" is anchored.
   * 
   * is_right_anchored	Similarly for `$'.
   */
  int is_left_anchored;
  int is_right_anchored;

  /* is_nullable	If not 0, the pattern can match the empty string.
   */
  t_uchar is_nullable;

  /* no_sub		If not 0, REG_NOSUB was passed to regcomp.
   */
  t_uchar no_sub;


  /* fastmap		If `fastmap[c]' is 0, the pattern can not match 
   * 			a string that begins with character `c'.
   */
  t_uchar fastmap[256];

  /* small_p		If not 0, the pattern is "trivial" in such a way that
   * 			the cost of pattern->NFA->DFA conversion should be 
   * 			avoided during matching in favor of matching based
   * 			only on the expression tree for the pattern.
   */
  int small_p;
  
  /* owner_data		Arbitrary data, available to whoever called 
   * 			regcomp.   Do not rely on this feature.
   */
  void * owner_data;
} regex_t;



/* enum rx_cflags
 * 
 * Values which can be combined by a bitwise inclusive OR to 
 * form the `cflags' argument for `regcomp'.
 * 
 * Most of these values are required by Posix.2.  `REG_DFA_ONLY' is an
 * implementation-specific flag and should be avoided by programs
 * which are intended to be portable between implementations of Posix.
 */
enum rx_cflags
{
  REG_EXTENDED = 1,
    /*	If REG_EXTENDED is set, then use extended regular expression
	(ERE) syntax.  If not set, then use basic regular expression
	(BRE) syntax. In extended syntax, none of the regexp operators 
	are written with a backslash. */


  REG_ICASE = (REG_EXTENDED << 1),
  /*   	If REG_ICASE is set, then ignore case when matching.
   	If not set, then case is significant. */

 
  REG_NOSUB = (REG_ICASE << 1),
  /*	Report only success/failure in regexec. */

  REG_NEWLINE = (REG_NOSUB << 1),
  /*	If REG_NEWLINE is set, then "." and complemented character
	sets do not match at newline characters in the string.  Also,
	"^" and "$" do match at newlines.
	
	If not set, then anchors do not match at newlines
	and complimented character sets ordinarilly contain newline. */


  REG_DFA_ONLY = (REG_NEWLINE << 1),
  /*	If this bit is set, then restrict the pattern language to patterns
	that compile to efficient state machines.

	This is a non-standard feature. */
};


/* enum rx_eflags
 * 
 * Values which can be combined by a bitwise inclusive OR to 
 * form the `eflags' argument for `regexec'.
 * 
 * Flags required by Posix.2: 
 * 
 * 	REG_NOTBOL
 * 	REG_NOTEOL
 * 
 * Implementation-specific (non-standard) flags:
 * 
 * 	REG_NO_SUBEXP_REPORTING
 * 	REG_ALLOC_REGS
 */
enum rx_eflags
{
  REG_NOTBOL = 1,
  /* If REG_NOTBOL is set, then the beginning-of-line operator `^'
   * doesn't match the beginning of the input string (presumably
   * because it's not the beginning of a line).  If not set, then the
   * beginning-of-line operator does match the beginning of the
   * string.
   * 
   * (Required by Posix.2)
   */

  REG_NOTEOL = (REG_NOTBOL << 1),
  /* REG_NOTEOL is similar to REG_NOTBOL, except that it applies to
   * the end-of-line operator `$' and the end of the input string.
   * 
   * (Required by Posix.2)
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
  /* REG_ALLOC_REGS is only used by `regnexec'.  It causes `regnexec' to allocate storage
   * for `regmatch_t' values.
   * 
   * (non-standard)
   */
};



/* automatically generated __STDC__ prototypes */
extern int regcomp (regex_t * preg, const char * pattern, int cflags);
extern int regncomp (regex_t * preg,
		     const char * pattern,
		     size_t len,
		     int cflags);
extern void regfree (regex_t *preg);
extern size_t regerror (int errcode,
			const regex_t *preg,
			char *errbuf,
			size_t errbuf_size);
extern int regexec (const regex_t *preg,
		    const char *string,
		    size_t nmatch,
		    regmatch_t pmatch[],
		    int eflags);
extern int regnexec (const regex_t *preg,
		     const char *string,
		     regoff_t len,
		     size_t nmatch,
		     regmatch_t **pmatch,
		     int eflags);
#endif /* INCLUDE__RX_POSIX__POSIX_H */
