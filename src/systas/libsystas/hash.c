/* hash.c - scheme hash functions
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
#include <math.h>
#include "systas/libsystas/hash.h"
#include "systas/libsystas/hash.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/vectors.h"


/* __STDC__ prototypes for static functions */
static unsigned long scm_hasher(SCM obj, unsigned long n, size_t depth);


/************************************************************************
 *(h0 "Hash Values")
 *
 * Systas scheme has built-in rules for computing hash values.  There
 * are three hash functions, corresponding to the three types of
 * equality (`eq?', `eqv?', and `equal?').
 */


/*(c hashq)
 * (hashq obj n)
 * 
 * Compute a hash value modulo `n' for `obj'.  `eq?' objects return
 * equal hash values.
 */
SCM_PROC(s_hashq, "hashq", 2, 0, 0, scm_hashq);
SCM
scm_hashq(SCM obj, SCM n)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(SCM_INUMP(n) && 0 <= n, n, scm_arg2, s_hashq);
  return SCM_MAKINUM(scm_ihashq (obj, SCM_INUM (n)));
}


/*(c hashv)
 * (hashv obj n)
 * 
 * Compute a hash value modulo `n' for `obj'.  `eqv?' objects return
 * equal hash values.
 */
SCM_PROC(s_hashv, "hashv", 2, 0, 0, scm_hashv);
SCM
scm_hashv(SCM obj, SCM n)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT(SCM_INUMP(n) && 0 <= n, n, scm_arg2, s_hashv);
  return SCM_MAKINUM(scm_ihashv (obj, SCM_INUM (n)));
}


/*(c hash)
 * (hash obj n)
 * 
 * Compute a hash value modulo `n' for `obj'.  `equal?' objects return
 * equal hash values.
 */
SCM_PROC(s_hash, "hash", 2, 0, 0, scm_hash);
SCM
scm_hash(SCM obj, SCM n)
{
  SCM_ASSERT(SCM_INUMP(n) && 0 <= n, n, scm_arg2, s_hash);
  return SCM_MAKINUM(scm_ihash(obj, SCM_INUM(n)));
}


/*c scm_strhash)
 * unsigned long scm_strhash (t_uchar *str, size_t len, unsigned long n);
 * 
 * Compute a hash value modulo `n' for a string.
 */
unsigned long 
scm_strhash (t_uchar *str, size_t len, unsigned long n)
{
  SCM_INTS_INDIFFERENT;

  if (len > 5)
    {
      size_t i = 5;
      unsigned long h = 264 % n;
      while (i--)
	h = ((h << 8) + ((unsigned) (str[h % len]))) % n;
      return h;
    }
  else
    {
      size_t i = len;
      unsigned long h = 0;
      while (i)
	h = ((h << 8) + ((unsigned) (str[--i]))) % n;
      return h;
    }
}


/*c scm_ihashq)
 * unsigned int scm_ihashq (SCM obj, unsigned int n);
 * 
 * Compute a hash value for a Scheme object, module `n'.
 * eq? objects return equal hash values.
 */
unsigned int
scm_ihashq (SCM obj, unsigned int n)
{
  SCM_INTS_UNKNOWN;

  if (SCM_IS_IMMEDIATE (obj))
    return ((unsigned int) (obj >> 7)) % n;
  else
    return ((unsigned int) (obj >> 3)) % n;
}


/*c scm_ihashv)
 * unsigned int scm_ihashv (SCM obj, unsigned int n);
 * 
 * Compute a hash value for a Scheme object, module `n'.
 * eqv? objects return equal hash values.
 */
unsigned int
scm_ihashv (SCM obj, unsigned int n)
{
  SCM_INTS_ENABLED;

  if (!SCM_IS_IMMEDIATE(obj) && SCM_NUMP(obj))
    return (unsigned int) scm_hasher(obj, n, 10);
  else
    return ((unsigned int)obj) % n;
}


/*c scm_ihash)
 * unsigned int scm_ihash (SCM obj, unsigned int n);
 * 
 * Compute a hash value for a Scheme object, module `n'.
 * equal? objects return equal hash values.
 */
unsigned int
scm_ihash (SCM obj, unsigned int n)
{
  return (unsigned int)scm_hasher (obj, n, 10);
}



/* scm_hasher
 * 
 * Compute a hash value for a Scheme object.
 * equal? objects have equal hash values.
 *
 * The value returned is between 0 and `n'.
 * `depth' limits the amount of recursion.
 *
 * WARNING: There is no record that the quality of this hash
 * function has been tested.
 */
static unsigned long
scm_hasher(SCM obj, unsigned long n, size_t depth)
{
  SCM_INTS_NESTED;

  switch (7 & (int) obj)
    {
    default:
      return 4194319L % n;

    case 2: case 4: case 6:	/* immediate value */
    immediates:
      return ((unsigned long) obj) % n;

    case 0:			/* non-immediate values */
      switch (SCM_TYP7(obj))
	{
	default:
	  return 4194329L % n;

	case scm_tcs_closures:
	case scm_tc7_contin:
	case scm_tcs_subrs:
	  return ((unsigned long)obj) % n;

	case scm_tcs_symbols:
	case scm_tc7_string:
	case scm_tc7_substring:
	case scm_tc7_subsymbol:
	case scm_tc7_static_string:
	case scm_tc7_static_substring:
	case scm_tc7_static_subsymbol:
	strings:
	  return scm_strhash (SCM_RO_UCHARS(obj), (size_t) SCM_RO_LENGTH(obj), n);


	case scm_tcs_cons_imcar:
	case scm_tcs_cons_nimcar:
	  if (depth)
	    return ((  scm_hasher (SCM_CAR(obj), n, depth/2)
		     + scm_hasher (SCM_CDR(obj), n, depth - 1))
		    % n);
	  else
	    return 1;

	case scm_tc7_wvect:
	case scm_tc7_vector:
	  {
	    size_t len;
	    SCM * data;

	    len = SCM_LENGTH(obj);
	    data = SCM_VECTOR_ELTS(obj);
	    if (len > 5)
	      {
		size_t i;
		unsigned long h;
		i = depth / 2;
		h = 1;
		while (i--)
		  h = ((h<<8) + (scm_hasher(data[h % len], n, 2))) % n;
		return h;
	      }
	    else
	      {
		size_t i;
		unsigned long h;
		i = len;
		h = (n)-1;
		while (i--)
		  h = ((h<<8) + (scm_hasher(data[i], n, depth/len))) % n;
		return h;
	      }
	  }

	case scm_tc7_smob:
	  switch SCM_TYP16(obj)
	    {
	    default:
	      return (4194389L % n);

	    case scm_tcs_bignums:
	    bighash:
	      return SCM_INUM(scm_modulo(obj, SCM_MAKINUM(n)));

#ifdef SCM_FLOATS
	    case scm_tc16_flo:
	      if (SCM_REALP(obj))
		{
		  double r;
		  double f;
		  r = SCM_REALPART(obj);
		  SCM_REDEFER_INTS;
		  f = floor(r);
		  SCM_REALLOW_INTS;
		  if (f == r)
		    {
		      obj = scm_inexact_to_exact (obj);
		      if (SCM_IS_IMMEDIATE(obj))
			goto immediates;
		      goto bighash;
		    }
		}

	      {
		obj = scm_number_to_string (obj, SCM_MAKINUM(10));
		goto strings;
	      }
#endif
	    }
	}
    }
}



void
scm_init_hash (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/hash.x"
}



/************************************************************************
 *(h1 "Rationale -- Hash Values")
 * 
 * 
 * For obvious reasons, the hash value functions mirror the equality
 * functions (see xref:"Equality").
 */
