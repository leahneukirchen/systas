/* struct.c - user defined data types
 *
 ****************************************************************
 * Copyright (C) 1998 Free Software Foundation,Inc.
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#include <stddef.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/smob.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/hashtab.h"
#include "systas/libsystas/struct.h"
#include "systas/libsystas/read-print.h"


static long scm_tc16_structure;

/* __STDC__ prototypes for static functions */
static int prin_struct (SCM exp, SCM port, int writing);



/************************************************************************
 *(h0 "Structures")
 *
 * "Structures" are user defined data types.  Using structures,
 * programmers can implement types which are disjoint from other
 * Scheme types, which have private state, and which have a
 * programmer-defined representation.
 *
 * A structure has two parts: its "type" and its "data".
 *
 * The data of a structure is an arbitrary value.
 *
 * The type of a structure is a cons pair.  The value stored in the
 * car of that pair is called the "public type" of the structure.  The
 * value stored in the cdr of the pair is called the "private type".
 * 
 * A structure is created using `make-structure' by supplying a type
 * and data.
 *
 * Given only a structure, it is impossible to retrieve the private
 * type of the structure or the type cons pair.  Only the public type
 * may be retrieved by using `structure-public-type'.
 *
 * Given only a structure, it is impossible to retrieve the data of
 * the the structure, but given a structure and its private type, the
 * data can be retrieved using `structure-data'.
 * 
 */
/*(menu)
 */


/************************************************************************
 *(h1 "Structure Procedures")
 * 
 * 
 * 
 */


/*(c structure?)
 * (structure? obj)
 * 
 * Return `#t' if `obj' is a structure, `#f' otherwise.
 */
SCM_PROC (s_structure_p, "structure?", 1, 0, 0, scm_structure_p);
SCM
scm_structure_p (SCM obj)
{
  if (scm_is_struct (obj))
    return SCM_BOOL_T;
  else
    return SCM_BOOL_F;
}


/*(c make-structure)
 * (make-structure type data)
 * 
 * Construct a new structure of the given type containing the given
 * data.
 * 
 * `type' must be a cons pair.  The car of that pair is the public
 * type of the new object; the cdr of that pair is the private type.
 */
SCM_PROC (s_make_structure, "make-structure", 2, 0, 0, scm_make_structure);
SCM
scm_make_structure (SCM type, SCM data)
{
  SCM x;
  SCM answer;

  SCM_ASSERT (!SCM_IS_IMMEDIATE (type) && SCM_CONSP (type), type, scm_arg1, s_make_structure);
  SCM_NEWCELL (answer);
  SCM_NEWCELL (x);
  SCM_DEFER_INTS;
  SCM_CAR (x) = type;
  SCM_CDR (x) = data;
  SCM_CAR (answer) = scm_tc16_structure;
  SCM_CDR (answer) = x;
  SCM_ALLOW_INTS;
  return answer;
}


/*(c structure-data)
 * (structure-data private_type structure)
 * 
 * If `obj' is a structure and `private-type' is its private type,
 * return the structure's data.  Otherwise, return `#f'.
 */
SCM_PROC (s_structure_data, "structure-data", 2, 0, 0, scm_structure_data);
SCM
scm_structure_data (SCM private_type, SCM structure)
{
  SCM_ASSERT (!SCM_IS_IMMEDIATE (structure) && (scm_tc16_structure == SCM_TYP16 (structure)),
	      structure, scm_arg2, s_structure_data);

  if (private_type == SCM_CDADR (structure))
    return SCM_CDDR (structure);
  else
    return SCM_BOOL_F;
}


/*(c structure-public-type)
 * (structure-public-type structure)
 * 
 * Return the public type of `structure'.
 */
SCM_PROC (s_structure_public_type, "structure-public-type", 1, 0, 0, scm_structure_public_type);
SCM
scm_structure_public_type (SCM structure)
{
  SCM_ASSERT (!SCM_IS_IMMEDIATE (structure) && (scm_tc16_structure == SCM_TYP16 (structure)),
	      structure, scm_arg1, s_structure_public_type);

  return SCM_CAADR (structure);
}


/*(c set-structure-print-function)
 * (set-structure-print-function type print_function)
 * 
 * Establish `print-function' as the way of printing structures having
 * the given type.
 *
 * `type' is the cons-pair passed to `make-struct'.
 *
 * `print-function' is called:
 *
 *		(print-function structure port writing?)
 *
 * where `writing?' is `#f' if the structure should be printed in the
 * manner of `display', and #t otherwise.
 *
 * `print-function' may be `#f' in which case the default printing
 * rule is used.
 */
SCM_PROC (s_set_structure_print_function, "set-structure-print-function",
	  2, 0, 0, scm_set_structure_print_function);
SCM
scm_set_structure_print_function (SCM type, SCM print_function)
{
  SCM_INTS_ENABLED;
  return scm_hashq_set_x (scm_print_struct_functions, type, print_function);
}





/******************************************************
 * scm_small_object_functions for structures
 * 
 */
static int
prin_struct (SCM exp, SCM port, int writing)
{
  SCM_INTS_DISABLED;
  SCM printfn;
  int printed;
  int errn;

  SCM_ALLOW_INTS;
  printfn = scm_hashq_ref (scm_print_struct_functions, SCM_CADR (exp), SCM_BOOL_F);
  printed = 0;
  if (printfn != SCM_BOOL_F)
    {
      SCM got;
      SCM p;
      if ((port != SCM_UNDEFINED) && (port != SCM_BOOL_F))
	p = port;
      else
	p = scm_cur_outp;
      got = scm_apply3 (printfn,
			scm_listify (exp, port, scm_int_to_bool (writing), SCM_UNDEFINED),
			SCM_EOL,
			0);
      printed = (SCM_BOOL_F != got);
    }
  SCM_DEFER_INTS;
  if (!printed)
    {
      scm_port_puts (&errn, port, "#<structure ");
      scm_intprint (exp, 16, port);
      scm_port_putc (&errn, port, '>');
    }
  return 1;
}

static struct scm_small_object_functions structure_smob = {scm_markcdr, scm_free0, prin_struct, 0};

int
scm_is_struct (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_TYP16 (obj) == scm_tc16_structure);
}



void
scm_init_struct (void)
{
  scm_tc16_structure = scm_newsmob (&structure_smob);
#include "systas/libsystas/struct.x"
  scm_print_struct_functions = scm_make_weak_key_hash_table (SCM_MAKINUM (63));
}


/************************************************************************
 *h1 "Structure Internals")
 *
 * Structure are non-immediate objects:
 *
 *  ..............scm_tc16_structure  ...........SCM handle2..........
 *
 * `handle2' is a cons pair:
 *
 *  ............SCM type............  ...........SCM data.............
 *
 * `type' is a cons pair:
 *
 *  .........SCM public_type........  .........SCM private_type.......
 *
 * `data', `public_type', and `private_type' are all arbitrary values.
 *
 *
 */

/************************************************************************
 *(h1 "Rationale -- Structures")
 *
 * Structures are a lightweight and simple mechanism that is nevertheless
 * adequate for implementing more complex forms of user-defined type.
 *
 * Implementations of user-defined types require four basic facilities
 * from the underlying language implementation:
 *
 *	1. A facility for creating types that are disjoint from all
 *	   other types.  All our structures satisfy `structure?', but
 *	   no other built-in type predicate.
 *
 *	2. A facility for dispatching genericly on the type of an 
 *	   object of user defined type.  The "public type" of our 
 *	   structures provides this.
 *
 *	3. A facility for specifying the internal representation of 
 *	   objects of user defined types.  The "data" of our structures
 *	   is nearly arbitrary: programmers can choose its representation
 *	   freely.  ("data" should not be #f, but that is the only 
 *	   restriction).
 *
 *	4. A facility for protecting the internal representation of
 *	   objects of user defined types from malicious or incorrect
 *	   code.   The "private type" of our structures and its use
 *	   as an "access key" for a structures data provides this.
 *
 * From these four basic facilities, any of the familiar record or
 * object systems can easily be synthesized.
 *
 * The implementation of structures is thus very simple -- there
 * doesn't seem to be anything extraneous.
 */

