/* symbols.c - scheme symbols
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "hackerlab/char/str.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/read-print.h"
#include "systas/libsystas/hash.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/strings.h"
#include "systas/libsystas/alist.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/variable.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/numbers.h"


int scm_symhash_size = 137;
SCM_SYMBOL (s_symbol, "symbol");
SCM_SYMBOL (s_internal_intern_symbol, "internal-intern-symbol");


/************************************************************************
 *(h0 "Symbols")
 *
 * Symbols are a kind of read-only string.  Symbols are used as
 * variable names and for other purposes.
 * 
 * Normally symbol names are made from only these characters:
 * 
 * 	a-z
 * 	A-Z,
 * 	0-9
 * 	-_!@$%^~&*=+|:/?><,.
 * 
 * For example:
 * 
 * 	abc
 * 	2much
 * 	set!
 * 	+
 * 
 * A symbol name must not begin with the character `:' (unless the
 * `#s' syntax describe below is used).  If you write what appears to
 * be a symbol name but it begins with a `:', you'll get a keyword,
 * not a symbol (see xref:"Keywords").
 * 
 * 	(symbol? 'this-is-a-symbol)     => #t
 * 	(symbol? ':this-is-not)         => ()
 * 
 * Except in obscure cases, the same name always names the same
 * symbol (see xref:"Symbol Hash-tables").
 * 
 * 	(eq? 'abc
 * 	     'abc
 * 	     (string->symbol "abc")
 * 	     (symbol-append "a" 'bc))
 * 
 * 	=> #t
 * 
 * Symbols are read in a case sensitive manner:
 * 
 * 	(eq? 'ABC 'abc) => ()
 * 
 */

/*(c symbol?)
 * (symbol? obj)
 * 
 * Return `#t' if `obj' is a symbol, `#f' otherwise.
 */
SCM_PROC(s_symbol_p, "symbol?", 1, 0, 0, scm_symbol_p);
SCM
scm_symbol_p (SCM x)
{
  SCM_INTS_INDIFFERENT;

  return scm_is_symbol (x) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(menu)
 */

/************************************************************************
 *(h1 "Converting Between Strings and Symbols")
 * 
 */

/*(s symbol->string)
 * (symbol->string symbol)
 * 
 * Return a new string which is `string=?' to `symbol'.
 */
SCM_PROC(s_symbol_to_string, "symbol->string", 1, 0, 0, scm_symbol_to_string);
SCM
scm_symbol_to_string (SCM s)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_symbol (s), s, scm_arg1, s_symbol_to_string);
  return scm_makfromstr (SCM_RO_CHARS(s), (size_t)SCM_LENGTH(s));
}


/*(c string->symbol)
 * (string->symbol string)
 * 
 * Return a symbol which is `string=?' to `string'.
 */
SCM_PROC(s_string_to_symbol, "string->symbol", 1, 0, 0, scm_string_to_symbol);
SCM
scm_string_to_symbol (SCM s)
{
  SCM_INTS_ENABLED;

  SCM vcell;
  SCM answer;

  SCM_ASSERT(scm_is_ro_string(s), s, scm_arg1, s_string_to_symbol);
  vcell = scm_intern(SCM_RO_CHARS(s), (size_t)SCM_LENGTH(s));
  answer = SCM_CAR (vcell);
  return answer;
}


/************************************************************************
 *(h1 "Symbol Constructors")
 * 
 * These procedures construct symbols.
 * 
 */
/*(c symbol)
 * (symbol . args)
 * 
 * Simply:
 * 
 * 	(string->symbol (apply string args))
 */
/*(c symbol-append)
 * (symbol-append . args)
 * 
 * Simply:
 * 
 * 	(string->symbol (apply string-append args))
 */
/*(c list->symbol)
 * (list->symbol . args)
 * 
 * Simply:
 * 
 * 	(string->symbol (apply list->string args))
 */

/*(c gensym)
 * (gensym . name-parts)
 * 
 * Construct a new, ordinary symbol.
 * 
 * If any `name-parts' are specified, concatenate them 
 * using `string-append' to form the root of the name of
 * the new symbol, adding additional characters as necessary
 * to generate a new symbol name.
 */


/****************************************************************
 *(h1 "Syntax for Unusual Symbol Names")
 * 
 * Symbol names containing unusual characters can be written as "#s",
 * followed by a quoted string.
 * 
 * 	'#s"this is one
 *	    really unusual
 * 	    symbol name."
 * 	=> #s"this is one\n    really unusual\n    symbol name."
 * 
 * 	(symbol->string '#s"strange symbol") 
 *	=> "strange symbol"
 * 
 * 	(string->symbol "strange symbol")
 *	=> #s"strange symbol"
 * 
 * 	(symbol? '#s":not-a-keyword")
 *	=> #t
 */

/****************************************************************
 *(h1 "Symbol Hash-tables")
 * 
 * At the time they are created, symbols are ordinarily recorded in a
 * built-in hash-table that is used to hold top-level definitions.
 * This process is called "interning" the symbol and the built-in
 * hash-table is called the "symhash" table.  If the same name s
 * interned in the symhash table more than once, the same symbol is
 * returned each time.
 * 
 * Using the procedures in this section, it is possible to create
 * symbols that are interned in an aribtrary hash-table.  By using a
 * hash table which is not the symhash table, distinct symbols can be
 * created which have the same name.  No two symbols interned in the
 * same hash-table have the same name, but different symbols, in
 * different hash-tables, can have the same name.
 * 
 * It is also possible to create symbols which are not interned in any
 * hash table.  This is another way in which distinct but same-named
 * symbols can arise.
 * 
 */

/*(c string->hash-table-symbol)
 * (string->hash-table-symbol hash-table string :optional soft?)
 * 
 * Return a symbol which is `string=?' to `string' and is interned in
 * `hash-table'.  By default, create such a symbol if none already
 * exists.
 *
 * If `soft?' is true, then do not create a new symbol -- instead,
 * return `#f' if the symbol does not already exist.
 *
 * If `hash-table' is `#t', create an uninterned symbol regardless of
 * the value of `soft?'.
 *
 * If `hash-table' is `#f', use the built-in hash-table.
 */
SCM_PROC(s_string_to_hash_table_symbol,
	 "string->hash-table-symbol", 2, 1, 0, scm_string_to_hash_table_symbol);
SCM
scm_string_to_hash_table_symbol (SCM o, SCM s, SCM softp)
{
  SCM_INTS_ENABLED;

  SCM vcell;
  SCM answer;
  int softness;

  SCM_ASSERT(scm_is_ro_string(s), s, scm_arg2, s_string_to_hash_table_symbol);
  SCM_ASSERT((o == SCM_BOOL_F) || (o == SCM_BOOL_T) || (scm_is_vector(o)),
	 o, scm_arg1, s_string_to_hash_table_symbol);

  softness = ((softp != SCM_UNDEFINED) && (softp != SCM_BOOL_F));

  if (o == SCM_BOOL_T)
    o = scm_symhash;
  else if (o == SCM_BOOL_F)
    o = SCM_BOOL_F;
    
  vcell = scm_hash_table_intern (o, SCM_RO_CHARS(s), (size_t)SCM_RO_LENGTH(s), !softness, 0);
  if (vcell == SCM_BOOL_F)
    return vcell;
  answer = SCM_CAR (vcell);
  return answer;
}

/*(c hash-table-symbol-append)
 * (hash-table-symbol-append hash-table . args)
 * 
 * Simply:
 * 
 *     (define (hash-table-symbol-append ob . args)
 *       (string->hash-table-symbol ob (apply string-append args)))
 */

/*(c hash-table-gensym)
 * (hash-table-gensym hash-table . name-parts)
 * 
 * Construct a new symbol in the specified `hash-table'.
 * 
 * If any `name-parts' are specified, concatenate them 
 * using `string-append' to form the root of the name of
 * the new symbol.
 * 
 */


/*(c intern-symbol)
 * (intern-symbol hash-table symbol)
 * 
 * Add `symbol' to the symbols in `hash-table', if it is not already
 * among them.
 * 
 * Using this procedure, it is possible to create a single symbol
 * which is interned in multiple hash tables.
 *
 * If `hash-table' is `#f', use the system's default hash-table.
 */
SCM_PROC(s_intern_symbol, "intern-symbol", 2, 0, 0, scm_intern_symbol);
SCM
scm_intern_symbol (SCM o, SCM s)
{
  SCM_INTS_ENABLED;
  size_t hval;

  SCM_ASSERT(scm_is_symbol (s), s, scm_arg2, s_intern_symbol);
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  SCM_ASSERT(scm_is_vector(o), o, scm_arg1, s_intern_symbol);

  hval = scm_strhash (SCM_RO_UCHARS (s), SCM_RO_LENGTH (s), SCM_LENGTH(o));

  SCM_REDEFER_INTS;
  {
    SCM lsym;
    SCM sym;
    int need_to_intern;
    
    need_to_intern = 1;
    for (lsym = SCM_VECTOR_ELTS (o)[hval];
	 !SCM_IS_IMMEDIATE (lsym);
	 lsym = SCM_CDR (lsym))
      {
	sym = SCM_CAR (lsym);
	if (SCM_CAR (sym) == s)
	  {
	    need_to_intern = 0;
	    break;
	  }
      }
    if (need_to_intern)
      SCM_VECTOR_ELTS (o)[hval] = scm_acons (s, SCM_UNDEFINED, SCM_VECTOR_ELTS (o)[hval]);
  }
  SCM_REALLOW_INTS;
  return s;
}


/*(c unintern-symbol)
 * (unintern-symbol hash-table symbol)
 * 
 * Remove `symbol' from `hash-table'.
 *
 * If `hash-table' is `#f', use the built-in top-level hash-table.
 */
SCM_PROC(s_unintern_symbol, "unintern-symbol", 2, 0, 0, scm_unintern_symbol);
SCM
scm_unintern_symbol (SCM o, SCM s)
{
  SCM_INTS_ENABLED;

  size_t hval;
  SCM answer;

  SCM_ASSERT(scm_is_symbol (s), s, scm_arg2, s_unintern_symbol);
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  SCM_ASSERT(scm_is_vector(o), o, scm_arg1, s_unintern_symbol);
  hval = scm_strhash (SCM_RO_UCHARS (s), SCM_RO_LENGTH (s), SCM_LENGTH(o));

  SCM_DEFER_INTS;
  {
    SCM lsym_follow;
    SCM lsym;
    SCM sym;
	  
    answer = SCM_BOOL_F;

    for (lsym = SCM_VECTOR_ELTS (o)[hval], lsym_follow = SCM_BOOL_F;
	 !SCM_IS_IMMEDIATE (lsym);
	 lsym_follow = lsym, lsym = SCM_CDR (lsym))
      {
	sym = SCM_CAR (lsym);
	if (SCM_CAR (sym) == s)
	  {
	    /* Found the symbol to unintern. */
	    if (lsym_follow == SCM_BOOL_F)
	      SCM_VECTOR_ELTS(o)[hval] = lsym;
	    else
	      SCM_CDR(lsym_follow) = SCM_CDR(lsym);
	    answer = SCM_BOOL_T;
	    break;
	  }
      }
  }
  SCM_ALLOW_INTS;
  return answer;
}


/*(c symbol-interned?)
 * (symbol-interned? hash-table symbol)
 * 
 * Return `#t' if `symbol' occurs in `hash-table'.
 *
 * If `hash-table' is `#f', the built-in hash table is searched.
 */
SCM_PROC(s_symbol_interned_p, "symbol-interned?", 2, 0, 0, scm_symbol_interned_p);
SCM
scm_symbol_interned_p (SCM o, SCM s)
{
  SCM_INTS_ENABLED;
  SCM vcell;

  SCM_ASSERT (scm_is_symbol (s), s, scm_arg2, s_symbol_interned_p);
  if (o == SCM_BOOL_F)
    o = scm_symhash;
  SCM_ASSERT(scm_is_vector(o), o, scm_arg1, s_symbol_interned_p);

  vcell = scm_maybe_symbol_to_hash_table_vcell (o, s);
  if (SCM_IS_IMMEDIATE(vcell) && (o == scm_symhash))
    vcell = scm_maybe_symbol_to_hash_table_vcell (scm_weak_symhash, s);
  return (!SCM_IS_IMMEDIATE(vcell) ? SCM_BOOL_T : SCM_BOOL_F);
}


/************************************************************************
 *h1 "The C Interface to Symbols"
 * 
 * This is a mess.  Sorry.
 * 
 */

/*c scm_is_symbol
 * int scm_is_symbol (SCM obj);
 * 
 * Return 1 of `obj' is a symbol, 0 otherwise.
 */
int
scm_is_symbol (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && SCM_SYMBOLP (obj);
}



/*c scm_symbol_to_vcell
 * SCM scm_symbol_to_vcell (SCM symbol, SCM proc, SCM definep);
 * 
 * Find (or optionally create) the value-binding cell ("vcell")
 * denoted by `symbol'.
 *
 * This function is used by eval to resolve references to free
 * variables.
 *
 *   symbol	- the symbol
 *   proc	- #f or a procedure, explained below
 *   definep	- #t if the symbol is being defined, #f otherwise
 * 
 * If `definep' is true, and no vcell yet exists, this procedure will
 * create one.  If `definep' is `#f' and no vcell exists, this
 * procedure returns `#f'.
 *
 * If `definep' is not `#f' and the symbol is found in
 * `scm_weak_symhash', the table of uninterned symbols, this function
 * moves the symbol to `scm_symhash' to protect the symbol from
 * garbage collection.
 *
 * If proc is not `#f', then it should be a Scheme procedure which
 * will do most of the work.  It is invoked:
 *
 *		(proc symbol define?)
 *
 * and should return either `#f' or a variable.  If it returns a
 * variable, the vcell of that variable becomes the return value of
 * this function.  If it returns `#f', so does this function.
 *
 * If `proc' is `#f', the new symbol is created in the symhash table.
 */
SCM 
scm_symbol_to_vcell (SCM sym, SCM proc, SCM definep)
{
  SCM_INTS_ENABLED;

  if (proc != SCM_BOOL_F)
    {
      SCM var;

      var = scm_apply3 (proc, sym, scm_cons(definep, scm_listofnull), 0);

      if (var == SCM_BOOL_F)
	return SCM_BOOL_F;
      else
	{
	  if (   SCM_IS_IMMEDIATE(var)
	      || !SCM_VARIABLEP (var)
	      || (scm_global_variable != SCM_VARIABLE_TYPE (var)))
	    scm_wta (sym, scm_strange_symbol, s_internal_intern_symbol);
	  return SCM_VARIABLE_VCELL (var);
	}
    }
  else
    {
      SCM lsym;
      SCM * lsymp;
      SCM z;
      size_t scm_hash;
      int got_it;

      scm_hash = scm_strhash (SCM_RO_UCHARS (sym), (size_t) SCM_RO_LENGTH (sym),
			      (unsigned long) scm_symhash_size);

      SCM_DEFER_INTS;
      got_it = 0;

      for (lsym = SCM_VECTOR_ELTS (scm_symhash)[scm_hash]; !SCM_IS_IMMEDIATE (lsym); lsym = SCM_CDR (lsym))
	{
	  z = SCM_CAR (lsym);
	  if (SCM_CAR (z) == sym)
	    {
	      got_it = 1;
	      goto done;
	    }
	}

      for (lsym = *(lsymp = &SCM_VECTOR_ELTS (scm_weak_symhash)[scm_hash]);
	   !SCM_IS_IMMEDIATE (lsym);
	   lsym = *(lsymp = &SCM_CDR (lsym)))
	{
	  z = SCM_CAR (lsym);
	  if (SCM_CAR (z) == sym)
	    {
	      if (definep != SCM_BOOL_F)
		{
		  *lsymp = SCM_CDR (lsym);
		  SCM_CDR (lsym) = SCM_VECTOR_ELTS(scm_symhash)[scm_hash];
		  SCM_VECTOR_ELTS(scm_symhash)[scm_hash] = lsym;
		}
	      got_it = 1;
	      goto done;
	    }
	}
    done:
      SCM_ALLOW_INTS;
      if (got_it)
	return z;
      scm_wta (sym, scm_uninterned_symbol, s_internal_intern_symbol);
      return SCM_UNSPECIFIED;	/* not reached */
    }
}


/*c scm_maybe_symbol_to_hash_table_vcell
 * SCM scm_maybe_symbol_to_hash_table_vcell (SCM hash_table, SCM sym);
 *
 * Return the vcell for `sym' in `hash_table' or `#f' if there is
 * none.
 *
 * If `hash_table' is `#f' or `SCM_UNDEFINED', `scm_symhash' is used.
 * 
 */
SCM 
scm_maybe_symbol_to_hash_table_vcell (SCM hash_table, SCM sym)
{
  SCM_INTS_NESTED;

  SCM lsym, z;
  size_t scm_hash;
  SCM answer;

  if ((hash_table == SCM_BOOL_F) || SCM_UNBNDP (hash_table))
    hash_table = scm_symhash;

  scm_hash = scm_strhash (SCM_RO_UCHARS (sym),
			  (size_t) SCM_RO_LENGTH (sym),
			  SCM_LENGTH (hash_table));
  SCM_REDEFER_INTS;
  answer = SCM_BOOL_F;
  for (lsym = SCM_VECTOR_ELTS (hash_table)[scm_hash];
       !SCM_IS_IMMEDIATE (lsym);
       lsym = SCM_CDR (lsym))
    {
      z = SCM_CAR (lsym);
      if (SCM_CAR (z) == sym)
	{
	  answer = z;
	  break;
	}
    }

  SCM_REALLOW_INTS;
  return answer;
}



/*c scm_symbol_to_hash_table_vcell
 * SCM scm_symbol_to_hash_table_vcell (SCM hash_table, SCM sym);
 * 
 * Return the vcell for `sym' in `hash_table' or signal an error if
 * there is none.
 *
 * If `hash_table' is `#f' or `SCM_UNDEFINED', `scm_symhash' is used.
 */
SCM 
scm_symbol_to_hash_table_vcell (SCM hash_table, SCM sym)
{
  SCM_INTS_ENABLED;

  SCM answer;
  answer = scm_maybe_symbol_to_hash_table_vcell (hash_table, sym);
  if (answer != SCM_BOOL_F)
    return answer;
  scm_wta (sym, scm_uninterned_symbol, s_internal_intern_symbol);
  return SCM_UNSPECIFIED;		/* not reached */
}


/*c scm_hash_table_intern
 * SCM scm_hash_table_intern (SCM hash_table,
 *                            t_uchar *name,
 *                            size_t len,
 *                            int create_it,
 *                            int define_it);
 * 
 * Find (or optionally create) a symbol of the specified name in
 * hash_table.  Return the vcell for that symbol.
 *
 * hash_table may be:
 * 
 *	#f		- create an uninterned symbol 
 *	SCM_UNDEFINED 	- use scm_symhash
 *	a hash table	- use that hash table
 *
 * If hash_table is scm_symhash, scm_weak_symhash is also checked.
 *
 * create_it may be:
 *	0		- don't create the symbol if it doesn't exist
 *	1		- create the symbol if it doesn't exist
 *
 *
 * define_it may be:
 *
 *	0		- possibly return a symbol subject to GC
 *	1		- return a symbol that is protected from GC
 *
 * define_it only matters if hash_table is scm_symhash or 
 * scm_weak_symhash.  In that case, the symbol is interned
 * on scm_symhash.
 */
SCM 
scm_hash_table_intern (SCM hash_table,
		       t_uchar *name,
		       size_t len,
		       int create_it,
		       int define_it)
{
  SCM_INTS_NESTED;
  SCM lsym;
  SCM * lsymp;
  SCM z;
  size_t i;
  t_uchar *tmp;
  size_t scm_hash;

  SCM_REDEFER_INTS;
  i = len;
  tmp = (t_uchar *) name;

  if (SCM_UNBNDP (hash_table))
    hash_table = scm_symhash;
  else if (hash_table == SCM_BOOL_F)
    goto uninterned_symbol;

  scm_hash = scm_strhash (tmp, i, SCM_LENGTH(hash_table));

 retry_new_hash_table:
  for (lsymp = &SCM_VECTOR_ELTS (hash_table)[scm_hash], lsym = *lsymp;
       !SCM_IS_IMMEDIATE (lsym);
       lsymp = &SCM_CDR (lsym), lsym = *lsymp)
    {
      z = SCM_CAR (lsym);
      z = SCM_CAR (z);
      tmp = SCM_RO_UCHARS (z);
      if (   (SCM_RO_LENGTH (z) != len)
	  || (str_cmp_n (name, len, tmp, len)))
	continue;
      {
	SCM a;
	a = SCM_CAR (lsym);
	if (define_it && (hash_table == scm_weak_symhash))
	  {
	    *lsymp = SCM_CDR (lsym);
	    SCM_CDR (lsym) = SCM_VECTOR_ELTS(scm_symhash)[scm_hash];
	    SCM_VECTOR_ELTS(scm_symhash)[scm_hash] = lsym;
	  }
	SCM_REALLOW_INTS;
	return a;
      }
    }

  if (hash_table == scm_symhash)
    {
      hash_table = scm_weak_symhash;
      goto retry_new_hash_table;
    }
  
  if (!create_it)
    {
      SCM_REALLOW_INTS;
      return SCM_BOOL_F;
    }

  if (define_it && (hash_table == scm_weak_symhash))
    hash_table =  scm_symhash;

 uninterned_symbol:

  {
    SCM_INTS_NESTED;
    SCM s;
    t_uchar * dst;

    SCM_NEWCELL (s);
    SCM_REDEFER_INTS;
    dst = (t_uchar *)scm_must_malloc (len + 1);
    
    SCM_CDR (s) = (SCM)dst;
    SCM_SET_LENGTH (s, len, scm_tc7_symbol);
    while (len--)
      *dst++ = *name++;
    *dst = 0;
    SCM_REALLOW_INTS;
    lsym = s;
  }


  if (hash_table == SCM_BOOL_F)
    {
      SCM answer;
      SCM_REALLOW_INTS;
      SCM_NEWCELL (answer);
      SCM_DEFER_INTS;
      SCM_CAR (answer) = lsym;
      SCM_CDR (answer) = SCM_UNDEFINED;
      SCM_REALLOW_INTS;
      return answer;
    }
  else
    {
      SCM a;
      SCM b;

      SCM_NEWCELL (a);
      SCM_NEWCELL (b);
      SCM_CAR (a) = lsym;
      SCM_CDR (a) = SCM_UNDEFINED;
      SCM_CAR (b) = a;
      SCM_CDR (b) = SCM_VECTOR_ELTS(hash_table)[scm_hash];
      SCM_VECTOR_ELTS(hash_table)[scm_hash] = b;
      SCM_REALLOW_INTS;
      return SCM_CAR (b);
    }
}


/* scm_intern
 * 
 * Intern a symbol in the scm_symhash table.  Return the vcell.
 */
SCM 
scm_intern (char *name, size_t len)
{
  SCM_INTS_NESTED;
  return scm_hash_table_intern (scm_symhash, name, len, 1, 0);
}


/* scm_intern0
 * 
 * Intern a symbol in the scm_symhash table.  Return the vcell.
 */
SCM
scm_intern0 (char * name)
{
  SCM_INTS_NESTED;
  return scm_intern (name, strlen (name));
}


/* scm_intern_symhash
 * 
 * Intern a symbol in the scm_symhash table and initialize the
 * value of its vcell.  Return the vcell.
 *
 * If `val' is SCM_UNDEFINED and the variable already has a value,
 * the existing value is not overridden.
 *
 * This function does not allocate a private copy of `name' --
 * it presumes that `name' will continue to exist indefinately.
 * The symbol created is of type scm_tc7_symbol.  This is the only
 * function to create symbols of that type.
 */
SCM 
scm_intern_symhash (char *name, SCM val)
{
  SCM_INTS_NESTED;
  SCM easy_answer;
  SCM answer;

  SCM_REDEFER_INTS;
  easy_answer = scm_hash_table_intern (scm_symhash, name, strlen (name), 0, 1);
  if (!SCM_IS_IMMEDIATE (easy_answer))
    {
      if (val != SCM_UNDEFINED)
	SCM_CDR (easy_answer) = val;
      answer = easy_answer;
    }
  else
    {
      SCM lsym;
      size_t len = strlen (name);
      t_uchar *tmp = (t_uchar *) name;
      size_t scm_hash = scm_strhash (tmp, len, (unsigned long) scm_symhash_size);
      SCM_NEWCELL (lsym);
      SCM_SET_LENGTH (lsym, (long) len, scm_tc7_static_symbol);
      SCM_CDR (lsym) = (SCM)name;
      lsym = scm_cons (lsym, val);
      SCM_VECTOR_ELTS (scm_symhash)[scm_hash] = scm_cons (lsym, SCM_VECTOR_ELTS (scm_symhash)[scm_hash]);
      answer = lsym;
    }
  SCM_REALLOW_INTS;
  return answer;
}




void
scm_init_symbols (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/symbols.x"
}



/************************************************************************
 *h1 "Symbol Internals"
 *
 * All symbols are represented by non-immediate object:
 * 
 *  ..length (24 bits)...tc7_?symbol  ......t_uchar * name......
 *
 * There are two kinds of symbol:
 *
 *	scm_tc7_symbol		-- symbol whose name was allocated by 
 * 				   malloc and will be freed if the symbol
 *				   is GCed
 *	scm_tc7_static_symbol	-- symbol whose name was not allocated by
 *				   this code and will not be freed even
 *				   if the symbol is GCed
 *
 * Those two tags differ only in the "S" option bit and that
 * difference is ignored by macros and functions like `SCM_TYP7S' and
 * `scm_is_symbol'.
 *
 * Symbol tags are also related to string tags.  `scm_tc7_symbol' and
 * `scm_tc7_string' differ only the "R" option bit.
 * `scm_tc7_static_symbol' and `scm_tc7_static_string' also differ
 * only in the "R" option bit.  The test `scm_is_ro_string' is true for
 * both kinds of symbols.
 *
 * Symbols may be created:
 *
 *	in the symhash table	  -- the built-in top-level hash table
 *	in the weak symhash table -- the built-in top-level hash table
 *	in another hash table	  -- a separate namespace of symbols
 * 	uninterned		  -- not in any hash table at all
 *
 * The symhash table holds top-level variables that are bound.
 * The weak symhash table holds unbound top-level variables
 * which are therefore subject to garbage collection.
 *
 * The system avoids creating more than one symbol with the same
 * name in a given hash table.
 *
 * Closely related to symbols are "vcells" (value cells).  A vcell
 * is a cons pair used as storage for a global variable.  The CDR of
 * the cell holds the variable's value.  The CAR holds a name for the
 * variable (not necessarily the only name).
 */

/****************************************************************
 *(h1 "Rationale -- Symbols")
 *
 * Standard Scheme does not provide same-named symbols which are not
 * `eq?'.  We provide them because they are a natural (and useful)
 * generalization
 * 
 * We regard symbols as immutable strings.  See xref:"Strings".
 */
