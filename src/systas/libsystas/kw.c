/* kw.c - scheme keywords
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#include <stddef.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/kw.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/root.h"
#include "systas/libsystas/numbers.h"


/************************************************************************
 *(h0 "Keywords")
 *
 * A "keyword" is a self-evaluating object with a convenient read
 * syntax.  Keywords are written like ordinary symbols except that
 * whereas a symbol name may not be written starting with ":", keyword
 * names *must* be prefixed by ":".
 * 
 * A "keyword name" is the string is the text to the right of the ":".
 * For example, the name of keyword `:foo' is "foo".  No two keywords 
 * have the same keyword name.
 * 
 * For every keyword, there is a unique symbol, having the same name,
 * called the "keyword symbol".  A keyword and its keyword symbol have
 * the same name.
 *
 * By convention, keywords are used with functions that accept
 * optional arguments to signal which arguments are provided.  Such
 * functions should never be passed keywords as parameters except to
 * indiciate which optional arguments are provided.  For example, this
 * procedure invokes an interactive editor, accepting optional
 * parameters `:directory' (specifying the directory in which to run
 * the editor, the default value is `#f') and `:editor' (specifying
 * the editor program, which defaults to the value of the environment
 * variable "EDITOR"):
 * 
 *	(define-public (call-editor . kws)
 *	  (let ((dir		(kw-arg-ref kws :directory))
 *	        (editor		
 *		  (kw-arg-ref kws :editor
 *			      (lambda () (getenv "EDITOR")))))
 *	    ...))
 *
 *
 * The function could be invoked any of these ways:
 * 
 * 	(call-editor)
 *	(call-editor :directory "/tmp")
 * 	(call-editor :editor "emacs")
 * 	(call-editor :directory "/tmp" :editor "emacs")
 * 	(call-editor :editor "emacs" :directory "/tmp")
 */


/****************************************************************
 * Keywords Small Object Functions
 */
static size_t
free_kw (SCM obj __attribute__((unused)))
{
  SCM_INTS_DISABLED;
  return 0;
}


static int
prin_kw (SCM exp, SCM port, int writing __attribute__ ((unused)))
{
  SCM_INTS_DISABLED;
  int errn;

  scm_port_puts (&errn, port, ":");
  scm_port_write (&errn, port, SCM_RO_CHARS (SCM_CDR (exp)), SCM_RO_LENGTH (SCM_CDR (exp)));
  return 1;
}


static int scm_tc16_kw;
static scm_small_object_functions kw_smob = {scm_markcdr, free_kw, prin_kw, 0};



/*(c keyword?)
 * (keyword? obj)
 * 
 * Return `#t' if `obj' is a keyword, `#f' otherwise.
 */
SCM_PROC(s_keyword_p, "keyword?", 1, 0, 0, scm_keyword_p);
SCM
scm_keyword_p (SCM obj)
{
  SCM_INTS_UNKNOWN;

  return ( scm_is_keyword (obj)
	  ? SCM_BOOL_T
	  : SCM_BOOL_F);
}


/************************************************************************
 *(h1 "Handling Keyword Arguments")
 */

/*(c kw-arg-ref)
 * (kw-arg-ref args kw :optional default)
 * 
 * If the list `args' contains the keyword `kw' followed by another
 * list element which is not a keyword, return that second element.
 * If `args' contains `kw' only as the last element or followed by
 * another keyword, signal an error. If a keyword argument is not
 * provided, but `default' is provided, return `(default)'.
 * Otherwise, return `#f'.
 */
SCM_PROC (s_kw_arg_ref, "kw-arg-ref", 2, 1, 0, scm_kw_arg_ref);
SCM
scm_kw_arg_ref (SCM args, SCM kw, SCM dflt)
{
  SCM l;
  l = scm_member (kw, args, SCM_BOOL_F);
  if (SCM_BOOL_F == l)
    {
      if ((dflt == SCM_BOOL_F) || (dflt == SCM_UNDEFINED))
	return SCM_BOOL_F;
      else
	return scm_apply3 (dflt, SCM_EOL, SCM_EOL, 0);
    }
  else
    {
      SCM p;
      p = SCM_CDR (l);
      SCM_ASSERT ((SCM_EOL != p) && !scm_is_keyword (SCM_CAR (p)),
		  l,
		  scm_missing_kw_arg,
		  s_kw_arg_ref);
      return SCM_CAR (p);
    }
}


/*(c kw-arg-set!)
 * (kw-arg-set! args kw value)
 * 
 * If the list `args' contains the keyword `kw' followed by another
 * list element, replace that list element.  If `kw' is the last
 * element of `args', replace the cdr of the last pair with `(cons
 * value ())' Otherwise, return:
 *
 *	 (cons kw (cons value args))
 *
 */
SCM_PROC (s_kw_arg_set_x, "kw-arg-set!", 3, 0, 0, scm_kw_arg_set_x);
SCM
scm_kw_arg_set_x (SCM args, SCM kw, SCM value)
{
  SCM l;
  l = scm_member (kw, args, SCM_BOOL_F);
  if (SCM_BOOL_F == l)
    return scm_cons (kw, scm_cons (value, args));
  else
    {
      SCM p;
      p = SCM_CDR (l);
      if (!SCM_IS_IMMEDIATE (p) && SCM_CONSP (p))
	SCM_CAR (p) = value;
      else
	SCM_CDR (l) = scm_cons (value, SCM_EOL);
      return args;
    }
}



/************************************************************************
 *(h1 "Keyword <-> Symbol Conversions")
 */

/*(c symbol->keyword)
 * (symbol->keyword symbol)
 * 
 * Return a keyword having the same name as `symbol'.  For example:
 * 
 * 	(symbol->keyword :foo) => foo
 *
 * Note that `keyword->symbol' and `symbol->keyword' are not
 * inverses.  For example:
 * 
 * 	(eq? 'foo (string->hash-table-symbol #f "foo")) 
 *	=> #f
 * 
 * and therefore:
 * 
 *  	(eq? 'foo 
 * 	     (keyword->symbol
 *	       (symbol->keyword (string->hash-table-symbol #f "foo")))
 *	=> #f
 */
SCM_PROC (s_symbol_to_keyword, "symbol->keyword", 1, 0, 0, scm_symbol_to_keyword);
SCM
scm_symbol_to_keyword (SCM symbol)
{
  SCM_INTS_ENABLED;
  SCM vcell;

  SCM_ASSERT (scm_is_symbol (symbol),
	      symbol, scm_arg1, s_symbol_to_keyword);

  /* Canonicalize the symbol.
   */
  symbol = scm_string_to_symbol (symbol);

  SCM_DEFER_INTS;
  vcell = scm_maybe_symbol_to_hash_table_vcell (scm_kw_obarray, symbol);
  if (vcell == SCM_BOOL_F)
    {
      SCM kw;
      SCM_NEWCELL(kw);
      SCM_CAR(kw) = (SCM)scm_tc16_kw;
      SCM_CDR(kw) = symbol;
      scm_intern_symbol (scm_kw_obarray, symbol);
      vcell = scm_maybe_symbol_to_hash_table_vcell (scm_kw_obarray, symbol);
      SCM_CDR (vcell) = kw;
    }
  SCM_ALLOW_INTS;
  return SCM_CDR (vcell);
}


/*(c keyword->symbol)
 * (keyword->symbol keyword)
 * 
 * Return a symbol having the same print-name as `keyword'.
 */
SCM_PROC(s_keyword_to_symbol, "keyword->symbol", 1, 0, 0, scm_keyword_to_symbol);
SCM
scm_keyword_to_symbol (SCM kw)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_keyword (kw), kw, scm_arg1, s_keyword_to_symbol);
  return SCM_CDR (kw);
}


/************************************************************************
 *h1 "The C Interface to Keywords")
 */

/*c scm_is_keyword)
 * int scm_is_keyword (SCM obj);
 * 
 * Return 1 if `obj' is a keyword, 0 otherwise.
 */
int
scm_is_keyword (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_CAR (obj) == scm_tc16_kw);
}



void
scm_init_kw (void)
{
  SCM_INTS_DISABLED;

  scm_tc16_kw = scm_newsmob (&kw_smob);
  scm_kw_obarray = scm_make_vector (SCM_MAKINUM (256), SCM_EOL, SCM_UNDEFINED);
#include "systas/libsystas/kw.x"
}

/************************************************************************
 *(h1 "Rationale -- Keywords")
 * 
 * A self-evaluating, disjoint, symbol-like type with a convenient
 * read/write syntax is exactly what is needed for two purposes:
 * keyword arguments and structure-like lists.
 * 
 * Some recent implementations provide keywords, but use a different
 * syntax, putting the colon at the end of the keyword name:
 * 
 * 	Systas keyword:			Some other implementations:
 *	-----------------------------------------------------------
 * 	:editor				editor:
 * 
 * That other syntax emphasizes that keywords are sometimes used
 * to tag the value that follows in a list:
 *
 *	:editor "/bin/ed"		editor: "/bin/ed"
 * 
 * However, there are two serious problems with the other syntax.
 * First, it breaks the general rule that a Scheme reader can 
 * recognize the type of a value being read by looking at a 
 * prefix of the syntax.  Second, it creates visual confusion when 
 * keywords are used on their own, not tagging any value at all,
 * such as the keyword `bg' in these examples:
 * 
 *	:bg :editor "/bin/ed"		bg: editor: "/bin/ed"
 * 
 */
