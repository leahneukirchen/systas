/* smob.c - small oject extension types
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include <stdlib.h>
#include "systas/libsystas/smob.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/numbers.h"


/************************************************************************
 * Small Object Internals
 *
 * "smobs" -- small objects -- are the way that several other types
 * are represented.   Smobs are non-immediate objects with 16 bits
 * of tag information:
 *
 *  .type-data..subtype.scm_tc7_smob  ...........type-data............
 *
 *
 * scm_tc7_smob, together with the GC mark bit, take up 8 bits.
 *
 * The subtype is an 8-bit tag.
 *
 * The meaning of the remaining bits is determined by the particular
 * smob subtype.
 *
 * Every smob type is associated with a structure:
 *
 *	typedef struct scm_small_object_functions
 *	{
 *	  SCM (*mark) (SCM);
 *	  size_t (*free) (SCM);
 *	  int (*print) (SCM exp, SCM port, int writing);
 *	  SCM (*equalp) (SCM, SCM);
 *	} scm_small_object_functions;
 *	
 *
 * mark	- mark the object during the mark phase of garbage collection.
 *	  Objects can be recursively marked by calling scm_gc_mark
 *	  or tail-recursively marked by returning them from `mark'.
 *	  The smob itself is usually marked this way:
 *
 *	  	if (SCM_GC8MARKP (ptr))
 *	    	  return SCM_BOOL_F; 		// already marked 
 *
 *	  	SCM_SETGC8MARK (ptr);
 *
 * free - free a smob that has been collected by GC.  This should
 * 	  free all malloced storage associated with the smob and return
 *	  the number of bytes freed.
 *
 * print - print a smob.  If `writing' is 1, print it for `write', otherwise,
 *	   print it for `display'.  If the print function is 0 or returns 0,
 *	   an unknown object representation is printed for the smob.
 *
 * equalp - return SCM_BOOL_T if the arguments (both smobs of the type
 *	   associated with this structure) are "equal?".  Otherwise,
 *	   return SCM_BOOL_F.
 */


/* This table is indexed by smob sub-type tags:
 */
scm_small_object_functions *scm_smobs = 0;
int scm_numsmob = 0;

/* scm_newsmob
 * 
 * Add a new smob type to the type table.  Return the sub-type number
 * of the new type.
 */
long 
scm_newsmob (scm_small_object_functions *smob)
{
  SCM_INTS_DISABLED;
  char *tmp;

  if (255 <= scm_numsmob)
    goto smoberr;

  if (scm_numsmob)
    tmp = (char *) realloc ((char *) scm_smobs,
			    (1 + scm_numsmob) * sizeof (scm_small_object_functions));
  else
    tmp = (char *) malloc (sizeof (scm_small_object_functions));
      
  if (tmp)
    {
      scm_smobs = (scm_small_object_functions *) tmp;
      scm_smobs[scm_numsmob].mark = smob->mark;
      scm_smobs[scm_numsmob].free = smob->free;
      scm_smobs[scm_numsmob].print = smob->print;
      scm_smobs[scm_numsmob].equalp = smob->equalp;
      scm_numsmob++;
    }
  if (!tmp)
    {
    smoberr:
      scm_panic ("allocation failure in scm_newsmob");
    }
  return scm_tc7_smob + (scm_numsmob - 1) * 256;
}


/****************************************************************
 * Initialization for float, bignum, free cells
 *
 */

static scm_small_object_functions freecell =
{
  scm_mark0,
  scm_free0,
  0,
  0
};

static scm_small_object_functions flob =
{
  scm_mark0,
  /*flofree*/ 0,
  scm_floprint,
  scm_floequal
};

static scm_small_object_functions bigob =
{
  scm_mark0,
  /*bigfree*/ 0,
  scm_bigprint,
  scm_bigequal
};

void
scm_smob_prehistory (void)
{
  SCM_INTS_DISABLED;

  /* WARNING: These scm_newsmob calls must be done in this order.
   * They must preceed all other calls to scm_newsmob.
   */
  scm_newsmob (&freecell);
  scm_newsmob (&flob);
  scm_newsmob (&bigob);
  scm_newsmob (&bigob);		/* n.b.: two smobs, one smobfuns */
}

