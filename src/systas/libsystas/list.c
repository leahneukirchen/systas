/* list.c - scheme lists
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
#include "systas/libsystas/list.h"
#include "systas/libsystas/eq.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/eval.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/numbers.h"
#include "systas/libsystas/vectors.h"

#include <stdarg.h>
#define var_start(x, y) va_start(x, y)


/************************************************************************
 *(h0 "Lists")
 *
 * A "list", or "proper list" is considered to be any chain of
 * cons-pairs, the cdr of each pair pointing to the next pair in the
 * chain, and the cdr of the last pair containing `()'.
 * 
 * A list is written as a left-parenthesis, followed by a white-space
 * separated enumeration of the car slots of the cells in the list,
 * followed by a right-parenthesis.
 * 
 *	;; Note that lists typed-in have to be quoted or else
 *	;; they will be interpreted as an expression and
 *	;; evaluated accordingly:
 *	;;
 *	'(a b c) => (a b c)     ; a three element list
 * 
 * An "improper list" is similar, except that the cdr slot of the last
 * pair is filled with some object other than `()'.  Dotted-pair notation
 * is used to write an improper list (see xref:"Cons Pairs"):
 * 
 *	'(a b . c) => (a b . c)     ; a two element improper list
 *	                            ; with the symbol c in the last cdr
 *	                            ; slot.  It is the c that makes
 *	                            ; the list "improper"
 */


/*(c null?)
 * (null? obj)
 * 
 * Return `#t' if `obj' is the empty list, `#f' otherwise.
 */
SCM_PROC(s_null_p, "null?", 1, 0, 0, scm_null_p);
SCM
scm_null_p(SCM x)
{
  SCM_INTS_INDIFFERENT;

 return (SCM_EOL == x) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c list?)
 * (list? obj)
 * 
 * Return `#t' if `obj' is a proper list, `#f' otherwise.
 */
SCM_PROC(s_list_p, "list?", 1, 0, 0, scm_list_p);
SCM
scm_list_p(SCM x)
{
  SCM_INTS_INDIFFERENT;

  if (scm_ilength (x) < 0)
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}


/************************************************************************
 *(h1 "Constructing Lists")
 * 
 */

/*(c list)
 * (list . args)
 * 
 * Return a newly constructed list of the arguments.
 */
SCM_PROC(s_list, "list", 0, 0, 1, scm_list);
SCM
scm_list(SCM objs)
{
  return objs;
}

/*(c make-list)
 * (make-list n :optional init)
 * 
 * Construct a new list of `n' elements, initially `#f' or `init'.
 */

/*(c range) 
 * (range x :optional y)
 * 
 * Return a new `x' element list of the integers `0 .. x-1'.
 * With two arguments, return a list `x..y-1'.
 */

/*(c reverse-range) 
 * (reverse-range x :optional y)
 * 
 * Return a new `x' element list of the integers `x-1 .. 0'.
 * With two arguments, return a list `y-1..x'.
 */


/*(c list-append)
 * (list-append . arguments)
 * 
 * Append the elements of the argument lists to form a mostly new
 * list.  The new list may share state with the last argument.
 */
SCM_PROC (s_append, "append", 0, 0, 1, scm_list_append);
SCM_PROC (s_list_append, "list-append", 0, 0, 1, scm_list_append);
SCM
scm_list_append(SCM args)
{
  SCM_INTS_ENABLED;
  SCM res;
  SCM *lloc;

  res = SCM_EOL;
  lloc = &res;
  if (SCM_IS_IMMEDIATE(args))
    {
      SCM_ASSERT((SCM_EOL == args), args, scm_argn, s_list_append);
      return res;
    }
  SCM_ASSERT(SCM_CONSP(args), args, scm_argn, s_list_append);
  while (1)
    {
      SCM arg;

      arg = SCM_CAR(args);
      args = SCM_CDR(args);

      if (SCM_IS_IMMEDIATE(args))
	{
	  *lloc = arg;
	  SCM_ASSERT((SCM_EOL == args), args, scm_argn, s_list_append);
	  return res;
	}

      SCM_ASSERT(SCM_CONSP(args), args, scm_argn, s_list_append);

      for(; !SCM_IS_IMMEDIATE(arg); arg = SCM_CDR(arg))
	{
	  SCM_ASSERT(SCM_CONSP(arg), arg, scm_argn, s_list_append);
	  *lloc = scm_cons(SCM_CAR(arg), SCM_EOL);
	  lloc = &SCM_CDR(*lloc);
	}

      SCM_ASSERT((SCM_EOL == arg), arg, scm_argn, s_list_append);
    }
}


/*(c append)
 * (append . arguments)
 * 
 * A synonym for `list-append'.
 */
SCM_PROC (s_append, "append", 0, 0, 1, scm_append);
SCM 
scm_append (SCM arguments)
{
  return scm_list_append (arguments);
}


/*(c list-append!)
 * (list-append! . arguments)
 * 
 * Append the elements of the argument lists to form a list.  The
 * arguments are modified: the cdr of the last cons pair of each
 * argument but the last is modified to point to the next argument.
 *
 * The arguments may be improper lists.
 */
SCM_PROC (s_append_x, "append!", 0, 0, 1, scm_list_append_x);
SCM_PROC (s_list_append_x, "list-append!", 0, 0, 1, scm_list_append_x);
SCM
scm_list_append_x(SCM args)
{
  SCM_INTS_ENABLED;
  SCM answer;
  SCM lp;

  if (SCM_EOL == args)
    return SCM_EOL;

  while (SCM_EOL == SCM_CAR (args))
    {
      args = SCM_CDR (args);
      if (SCM_EOL == args)
	return SCM_EOL;
    }

  answer = SCM_CAR (args);
  lp = scm_last_pair (answer);
  args = SCM_CDR (args);
  while (SCM_EOL != args)
    {
      if (SCM_EOL != SCM_CAR (args))
	{
	  SCM nlp;

	  nlp = scm_last_pair (SCM_CAR (args));
	  SCM_CDR (lp) = SCM_CAR (args);
	  lp = nlp;
	}
      args = SCM_CDR (args);
    }

  return answer;
}

/*(c append!)
 * (append! . arguments)
 * 
 * A synonym for `list-append!'.
 */
SCM_PROC (s_append_x, "append!", 0, 0, 1, scm_append_x);
SCM
scm_append_x (SCM arguments)
{
  return scm_list_append_x (arguments);
}


/*(c list-copy)
 * (list-copy list)
 * 
 * Return a new list with the same elements as `list'.
 *
 * If `list' is an improper list, the new list and `list'
 * have the same element in the cdr of the last pair.
 */
SCM_PROC (s_list_copy, "list-copy", 1, 0, 0, scm_list_copy);
SCM 
scm_list_copy (SCM lst)
{
  SCM_INTS_INDIFFERENT;
  SCM newlst;
  SCM * fill_here;
  SCM from_here;

  newlst = SCM_EOL;
  fill_here = &newlst;
  from_here = lst;

  while (!SCM_IS_IMMEDIATE (from_here) && SCM_CONSP (from_here))
    {
      SCM c;
      c = scm_cons (SCM_CAR (from_here), SCM_CDR (from_here));
      *fill_here = c;
      fill_here = &SCM_CDR (c);
      from_here = SCM_CDR (from_here);
    }
  return newlst;
}


/*(c copy-tree)
 * (copy-tree obj)
 * 
 * Recursively copy the list and vector structure of `obj'.  Return
 * the copy.
 */
SCM_PROC(s_copy_tree, "copy-tree", 1, 0, 0, scm_copy_tree);
SCM 
scm_copy_tree (SCM obj)
{
  SCM_INTS_ENABLED;
  SCM ans;
  SCM tl;

  if (SCM_IS_IMMEDIATE (obj))
    return obj;

  if (scm_is_vector (obj))
    {
      size_t i;
      i = SCM_LENGTH (obj);
      ans = scm_make_vector (SCM_MAKINUM (i), SCM_UNSPECIFIED, SCM_UNDEFINED);
      while (i--)
	SCM_VECTOR_ELTS (ans)[i] = scm_copy_tree (SCM_VECTOR_ELTS (obj)[i]);
      return ans;
    }

  if (SCM_NCONSP (obj))
    return obj;

  ans = tl = scm_cons (scm_copy_tree (SCM_CAR (obj)), SCM_UNSPECIFIED);

  while (!SCM_IS_IMMEDIATE (obj = SCM_CDR (obj)) && SCM_CONSP (obj))
    tl = (SCM_CDR (tl) = scm_cons (scm_copy_tree (SCM_CAR (obj)), SCM_UNSPECIFIED));

  SCM_CDR (tl) = obj;
  return ans;
}



/************************************************************************
 *(h1 "List Length")
 * 
 */


/*(c list-length)
 * (list-length obj)
 * 
 * Return the length of `obj' if it is a list, otherwise
 * signal an error.
 */
SCM_PROC(s_list_length, "list-length", 1, 0, 0, scm_list_length);
SCM_PROC(s_length, "length", 1, 0, 0, scm_list_length);
SCM
scm_list_length(SCM x)
{
  SCM_INTS_ENABLED;
  int i;

  i = scm_ilength (x);
  SCM_ASSERT (i >= 0, x, scm_arg1, s_list_length);
  return SCM_MAKINUM (i);
}


/*(c list-length+)
 * (list-length+ obj)
 * 
 * Like `list-length', but return `#f' for circular lists and the
 * number of elements for improper lists.
 */
SCM_PROC (s_list_length_plus, "list-length+", 1, 0, 0, scm_list_length_plus);
SCM
scm_list_length_plus (SCM x)
{
  SCM_INTS_ENABLED;
  int i;

  i = scm_sloppy_ilength (x);
  if (i < 0)
    return SCM_BOOL_F;
  else
    return SCM_MAKINUM (i);
}

/*(c soft-list-length)
 * (soft-list-length obj)
 * 
 * Return the length of `obj' if it is a proper list, otherwise
 * return `#f'.
 */
SCM_PROC(s_soft_list_length, "soft-list-length", 1, 0, 0, scm_soft_list_length);
SCM
scm_soft_list_length(SCM x)
{
  SCM_INTS_ENABLED;
  int i;

  i = scm_ilength(x);
  if (i >= 0)
    return SCM_MAKINUM (i);
  else
    return SCM_BOOL_F;
}



/************************************************************************
 *(h1 "Reversing Lists")
 * 
 */

/*(c list-reverse)
 * (list-reverse list :optional new-tail)
 * 
 * Return a list having the same elements as `list', but 
 * in reverse order.
 * 
 * If `new-tail' is provided, it is stored in the cdr of the
 * last pair of the returned list.  If `list' is `()', `new-tail'
 * is returned.
 */
SCM_PROC (s_reverse, "reverse", 1, 1, 0, scm_list_reverse);
SCM_PROC (s_list_reverse, "list-reverse", 1, 1, 0, scm_list_reverse);
SCM
scm_list_reverse (SCM lst, SCM res)
{
  SCM_INTS_ENABLED;
  SCM p;

  if (res == SCM_UNDEFINED)
    res = SCM_EOL;
  p = lst;
  
  for(; !SCM_IS_IMMEDIATE (p); p = SCM_CDR(p))
    {
      SCM_ASSERT (SCM_CONSP (p), lst, scm_arg1, s_list_reverse);
      res = scm_cons (SCM_CAR (p), res);
    }
  SCM_ASSERT ((SCM_EOL == p), lst, scm_arg1, s_list_reverse);
  return res;
}


/*(c list-reverse!)
 * (list-reverse! list :optional new-tail)
 * 
 * Return a list having the same elements as `list', but 
 * in reverse order. `list' is modified -- pairs form `list'
 * are re-used to form the new list.
 *
 * If `new-tail' is provided, it is stored in the cdr of the
 * last pair of the returned list.
 */
SCM_PROC (s_list_reverse_x, "list-reverse!", 1, 1, 0, scm_list_reverse_x);
SCM
scm_list_reverse_x (SCM lst, SCM newtail)
{
  SCM_INTS_INDIFFERENT;
  SCM old_tail;

  if (newtail == SCM_UNDEFINED)
    newtail = SCM_EOL;

 loop:
  if (!(!SCM_IS_IMMEDIATE (lst) && SCM_CONSP (lst)))
    {
      SCM_ASSERT (SCM_EOL == lst, lst, scm_arg1, s_list_reverse_x);
      return lst;
    }

  old_tail = SCM_CDR (lst);
  SCM_CDR (lst) = newtail;
  if (SCM_EOL == old_tail)
    return lst;

  newtail = lst;
  lst = old_tail;
  goto loop;
}

/*(c reverse!)
 * (reverse! list :optional new-tail)
 * 
 * A synonym for `list-reverse!'.
 */
SCM_PROC (s_reverse_x, "reverse!", 1, 1, 0, scm_reverse_x);
SCM
scm_reverse_x (SCM list, SCM newtail)
{
  return scm_list_reverse_x (list, newtail);
}


/************************************************************************
 *(h1 "Partitioning Lists")
 * 
 */

/*(c list-head)
 * (list-head list n)
 * 
 * Return a new list containing the first `n' elements of `list'.
 */
SCM_PROC(s_list_head, "list-head", 2, 0, 0, scm_list_head);
SCM
scm_list_head(SCM lst, SCM k)
{
  SCM_INTS_ENABLED;

  SCM answer;
  SCM * pos;
  long i;

  SCM_ASSERT(SCM_INUMP(k), k, scm_arg2, s_list_head);
  answer = SCM_EOL;
  pos = &answer;
  i = SCM_INUM(k);
  while (i-- > 0)
    {
      SCM_ASSERT (!SCM_IS_IMMEDIATE(lst) && SCM_CONSP(lst), lst, scm_arg1, s_list_head);
      *pos = scm_cons (SCM_CAR (lst), SCM_EOL);
      pos = &SCM_CDR (*pos);
      lst = SCM_CDR(lst);
    }
  return answer;
}


/*(c list-tail)
 * (list-tail list n)
 * 
 * Return the cdr of pair `n' (counting from 0) of `list'.
 */
SCM_PROC(s_list_tail, "list-tail", 2, 0, 0, scm_list_tail);
SCM
scm_list_tail(SCM lst, SCM k)
{
  SCM_INTS_ENABLED;
  long i;

  SCM_ASSERT (SCM_INUMP(k), k, scm_arg2, s_list_tail);
  i = SCM_INUM(k);
  SCM_ASSERT (i >= 0, k, scm_arg2, s_list_tail);
  while (i-- > 0)
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE(lst) && SCM_CONSP(lst), lst, scm_arg1, s_list_tail);
      lst = SCM_CDR(lst);
    }
  return lst;
}


/*(c last-pair)
 * (last-pair list)
 * 
 * Return the last pair in a (possibly improper) list.
 */
SCM_PROC(s_last_pair, "last-pair", 1, 0, 0, scm_last_pair);
SCM
scm_last_pair(SCM sx)
{
  SCM_INTS_ENABLED;
  SCM res;
  SCM x;

  res = sx;

  SCM_ASSERT(!SCM_IS_IMMEDIATE(res) && SCM_CONSP(res), res, scm_arg1, s_last_pair);
  while (!0)
    {
      x = SCM_CDR(res);
      if (SCM_IS_IMMEDIATE(x) || SCM_NCONSP(x))
	return res;
      res = x;
      x = SCM_CDR(res);
      if (SCM_IS_IMMEDIATE(x) || SCM_NCONSP(x))
	return res;
      res = x;
      sx = SCM_CDR(sx);
      SCM_ASSERT(x != sx, sx, scm_arg1, s_last_pair);
    }
}



/************************************************************************
 *(h1 "Indexed Access to Lists")
 * 
 */

/*(c list-ref)
 * (list-ref list n)
 * 
 * Return element `n' (counting from 0) of `list'.
 */
SCM_PROC(s_list_ref, "list-ref", 2, 0, 0, scm_list_ref);
SCM
scm_list_ref(SCM lst, SCM k)
{
  SCM_INTS_ENABLED;
  long i;

  SCM_ASSERT(SCM_INUMP(k), k, scm_arg2, s_list_ref);
  i = SCM_INUM(k);
  SCM_ASSERT(i >= 0, k, scm_arg2, s_list_ref);
  while (i-- > 0)
    {
      if (SCM_IS_IMMEDIATE(lst) || !SCM_CONSP(lst))
	goto erout;
      lst = SCM_CDR(lst);
    }
 erout:
  SCM_ASSERT(!SCM_IS_IMMEDIATE(lst) && SCM_CONSP(lst),
	     (SCM_EOL == lst)?k:lst, (SCM_EOL == lst)?scm_outofrange:scm_arg1, s_list_ref);
  return SCM_CAR(lst);
}


/*(c list-set!)
 * (list-set! list n value)
 * 
 * Set element `n' (counting from 0) of `list'.  Return `list'.
 */
SCM_PROC(s_list_set_x, "list-set!", 3, 0, 0, scm_list_set_x);
SCM
scm_list_set_x(SCM lst, SCM k, SCM val)
{
  SCM_INTS_ENABLED;
  long i;
  SCM pos;

  SCM_ASSERT(SCM_INUMP(k), k, scm_arg2, s_list_set_x);
  i = SCM_INUM(k);
  SCM_ASSERT(i >= 0, k, scm_arg2, s_list_set_x);

  pos = lst;
  while (i-- > 0)
    {
      if (SCM_IS_IMMEDIATE(pos) || !SCM_CONSP(pos))
	goto erout;
      pos = SCM_CDR(pos);
    }
 erout:
  SCM_ASSERT(!SCM_IS_IMMEDIATE(pos) && SCM_CONSP(pos),
	     (SCM_EOL == pos) ? k : pos, (SCM_EOL == pos) ? scm_outofrange : scm_arg1, s_list_set_x);
  SCM_CAR (pos) = val;
  return lst;
}


/*(c list-cdr-ref)
 * (list-cdr-ref list n)
 * 
 * Return the cdr of pair `n' (counting from 0) of `list'.
 * 
 */
SCM_PROC(s_list_cdr_ref, "list-cdr-ref", 2, 0, 0, scm_list_cdr_ref);
SCM
scm_list_cdr_ref (SCM list, SCM k)
{
  SCM_INTS_ENABLED;
  long i;

  SCM_ASSERT (SCM_INUMP(k), k, scm_arg2, s_list_cdr_ref);
  i = SCM_INUM (k);
  SCM_ASSERT (i >= 0, k, scm_arg2, s_list_cdr_ref);
  while (i-- > 0)
    {
      SCM_ASSERT(!SCM_IS_IMMEDIATE (list) && SCM_CONSP (list), list, scm_arg1, s_list_cdr_ref);
      list = SCM_CDR (list);
    }
  SCM_ASSERT (!SCM_IS_IMMEDIATE (list) && SCM_CONSP (list), list, scm_arg1, s_list_cdr_ref);
  return SCM_CDR (list);
}


/*(c list-cdr-set!)
 * (list-cdr-set! list n value)
 * 
 * Set the cdr of pair `n' (counting from 0) of `list'.  Return
 * `list'.
 */
SCM_PROC(s_list_cdr_set_x, "list-cdr-set!", 3, 0, 0, scm_list_cdr_set_x);
SCM
scm_list_cdr_set_x(SCM lst, SCM k, SCM val)
{
  SCM_INTS_ENABLED;
  long i;
  SCM pos;

  SCM_ASSERT(SCM_INUMP(k), k, scm_arg2, s_list_cdr_set_x);
  i = SCM_INUM(k);
  SCM_ASSERT(i >= 0, k, scm_arg2, s_list_cdr_set_x);

  pos = lst;
  while (i-- > 0)
    {
      if (SCM_IS_IMMEDIATE(pos) || !SCM_CONSP(pos))
	goto erout;
      pos = SCM_CDR(pos);
    }
 erout:
  SCM_ASSERT(!SCM_IS_IMMEDIATE(pos) && SCM_CONSP(pos),
	     (SCM_EOL == pos) ? k : lst,
	     (SCM_EOL == pos) ? scm_outofrange : scm_arg1,
	     s_list_cdr_set_x);
  SCM_CDR (pos) = val;
  return lst;
}


/************************************************************************
 *(h1 "Searching for List Elements")
 * 
 */

/*(c memq)
 * (memq obj list)
 * 
 * If `obj' occurs in `list', return the first sublist whose car is
 * `eq?' to `obj'.
 *
 * `list' may be an improper list.
 */
SCM_PROC (s_memq, "memq", 2, 0, 0, scm_memq);
SCM
scm_memq(SCM x, SCM lst)
{
  SCM_INTS_INDIFFERENT;

  for(;  !SCM_IS_IMMEDIATE(lst) && SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_CAR(lst)==x)
	return lst;
    }
  return SCM_BOOL_F;
}


/*(c memv)
 * (memv obj list)
 * 
 * If `obj' occurs in `list', return the first sublist whose car is
 * `eqv?' to `obj'.
 *
 * `list' may be an improper list.
 */
SCM_PROC (s_memv, "memv", 2, 0, 0, scm_memv);
SCM
scm_memv(SCM x, SCM lst)
{
  SCM_INTS_INDIFFERENT;

  for(;  !SCM_IS_IMMEDIATE(lst) && SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_BOOL_F != scm_eqv_p (SCM_CAR(lst), x))
	return lst;
    }
  return SCM_BOOL_F;
}


/*(c member)
 * (member obj list :optional =)
 * 
 * If `obj' occurs in `list', return the first sublist whose car is
 * `equal?' to `obj'.
 *
 * `list' may be an improper list.
 * 
 * If `=' is supplied, it is used to compare `obj' to elements of
 * `list' in this way:
 *
 *	(compare obj elt-of-list)
 *
 */
SCM_PROC (s_member, "member", 2, 1, 0, scm_member);
SCM
scm_member (SCM x, SCM lst, SCM pred)
{
  SCM_INTS_ENABLED;

  for(;  !SCM_IS_IMMEDIATE(lst) && SCM_CONSP (lst);  lst = SCM_CDR(lst))
    {
      if (SCM_BOOL_F != scm_generalized_equal_p (pred, x, SCM_CAR(lst)))
	return lst;
    }
  return SCM_BOOL_F;
}

/*(c list-elt-indexq)
 * (list-elt-indexq l k)
 * 
 * Return the index (0 based) of the first element in list `l'
 * that matches (`eq?') the value `k'.
 */

/*(c list-elt-indexv)
 * (list-elt-indexv l k)
 * 
 * Return the index (0 based) of the first element in list `l'
 * that matches (`eqv?') the value `k'.
 */

/*(c list-elt-index)
 * (list-elt-index l k)
 * 
 * Return the index (0 based) of the first element in list `l'
 * that matches (`equal?') the value `k'.
 */



/************************************************************************
 *(h1 "Removing List Elements")
 */

/*(c delq!)
 * (delq! obj list)
 * 
 * Modify `list' by removing the all pairs whose car is `eq?' to obj.
 */
SCM_PROC(s_delq_x, "delq!", 2, 0, 0, scm_delq_x);
SCM
scm_delq_x (SCM item, SCM lst)
{
  SCM_INTS_INDIFFERENT;
  SCM start;
  SCM place;

  if (SCM_IS_IMMEDIATE (lst) || SCM_NCONSP (lst))
    return lst;

  while (!SCM_IS_IMMEDIATE (lst) && SCM_CONSP (lst) && (SCM_CAR (lst) == item))
    lst = SCM_CDR (lst);

  if (!(!SCM_IS_IMMEDIATE (lst) && SCM_CONSP (lst)))
    return lst;

  start = lst;
  place = start;

  while (1)
    {
      SCM next;
      next = SCM_CDR (place);
      if (!(!SCM_IS_IMMEDIATE (next) && SCM_CONSP (next)))
	break;
      if (SCM_CAR (next) == item)
	SCM_CDR (place) = SCM_CDR (next);
      else
	place = next;
    }
  return start;
}


/*(c delq)
 * (delq obj list)
 * 
 * Return a new copy of `list' omitting all pairs whose car is `eq?'
 * to `obj'.
 */
SCM_PROC (s_delq, "delq", 2, 0, 0, scm_delq);
SCM
scm_delq (SCM item, SCM lst)
{
  SCM copy;

  copy = scm_list_copy (lst);
  return scm_delq_x (item, copy);
}


/*(c delv!)
 * (delv! obj list)
 * 
 * Modify `list' by removing the all pairs whose car is `eqv?' to obj.
 */
SCM_PROC(s_delv_x, "delv!", 2, 0, 0, scm_delv_x);
SCM
scm_delv_x (SCM item, SCM lst)
{
  SCM_INTS_INDIFFERENT;
  SCM start;
  SCM place;

  if (SCM_IS_IMMEDIATE (lst) || SCM_NCONSP (lst))
    return lst;

  while (!SCM_IS_IMMEDIATE (lst) && SCM_CONSP (lst) && (SCM_BOOL_F != scm_eqv_p (SCM_CAR (lst), item)))
    lst = SCM_CDR (lst);

  if (!(!SCM_IS_IMMEDIATE (lst) && SCM_CONSP (lst)))
    return lst;

  start = lst;
  place = start;

  while (1)
    {
      SCM next;
      next = SCM_CDR (place);
      if (!(!SCM_IS_IMMEDIATE (next) && SCM_CONSP (next)))
	break;
      if (SCM_BOOL_F != scm_eqv_p (SCM_CAR (next), item))
	SCM_CDR (place) = SCM_CDR (next);
      else
	place = next;
    }
  return start;
}


/*(c delv)
 * (delv obj list)
 * 
 * Return a new copy of `list' omitting all pairs whose car is `eqv?'
 * to obj.
 */
SCM_PROC (s_delv, "delv", 2, 0, 0, scm_delv);
SCM
scm_delv (SCM item, SCM lst)
{
  SCM copy;

  copy = scm_list_copy (lst);
  return scm_delv_x (item, copy);
}


/*(c delete!)
 * (delete! obj list :optional =)
 * 
 * Modify `list' by removing the all pairs whose car is `equal?' to
 * obj.
 * 
 * If `=' is supplied, it is used to compare `obj' to elements of
 * `list' in this way:
 *
 *	(compare obj elt-of-list)
 *
 */
SCM_PROC(s_delete_x, "delete!", 2, 1, 0, scm_delete_x);
SCM
scm_delete_x (SCM item, SCM lst, SCM compare)
{
  SCM_INTS_INDIFFERENT;
  SCM start;
  SCM place;

  if (SCM_IS_IMMEDIATE (lst) || SCM_NCONSP (lst))
    return lst;

  while (!SCM_IS_IMMEDIATE (lst) && SCM_CONSP (lst) && (SCM_BOOL_F != scm_generalized_equal_p (compare, item, SCM_CAR (lst))))
    lst = SCM_CDR (lst);

  if (!(!SCM_IS_IMMEDIATE (lst) && SCM_CONSP (lst)))
    return lst;

  start = lst;
  place = start;

  while (1)
    {
      SCM next;
      next = SCM_CDR (place);
      if (!(!SCM_IS_IMMEDIATE (next) && SCM_CONSP (next)))
	break;
      if (SCM_BOOL_F != scm_generalized_equal_p (compare, item, SCM_CAR (next)))
	SCM_CDR (place) = SCM_CDR (next);
      else
	place = next;
    }
  return start;
}


/*(c delete)
 * (delete obj list :optional =)
 * 
 * Return a new copy of `list' omitting all pairs whose car is
 * `equal?' to obj.
 * 
 * If `=' is supplied, it is used to compare `obj' to elements of
 * `list' in this way:
 *
 *	(compare obj elt-of-list)
 */
SCM_PROC (s_delete, "delete", 2, 1, 0, scm_delete);
SCM
scm_delete (SCM item, SCM lst, SCM compare)
{
  SCM copy;

  copy = scm_list_copy (lst);
  return scm_delete_x (item, copy, compare);
}


/************************************************************************
 *(h1 "List-based Iterators")
 * 
 */

/*(c map)
 * (map procedure . arguments)
 * 
 * Construct a new list by applying `procedure' to each element of the
 * list arguments.
 * 
 * For example:
 * 
 * 	(map - '(1 2 3)) => (-1 -2 -3)
 * 	
 * 	(map + '(1 2 3) '(10 10 11))
 * 	 => (11 12 14)
 * 
 * The list arguments to map need not be proper lists, but if they
 * improper, the contents of the last cdr are ignored.
 * 
 * The list elements may differ in length, but `map' stops when the
 * shortest list is exhausted.
 * 
 * `map' is applied to the elements of its arguments in order.
 */
SCM_PROC(s_map, "map", 2, 0, 1, scm_map);
SCM 
scm_map (SCM proc, SCM arg1, SCM args)
{
  SCM_INTS_ENABLED;
  long i;
  SCM res;
  SCM *pres;
  SCM *ve;			/* Keep args from being optimized away. */

  res = SCM_EOL;
  pres = &res;
  ve = &args;

  /* If this procedure is changed, "map-in-order" may also have to be
   * updated.
   */

  if (SCM_EOL == arg1)
    return res;
  SCM_ASSERT (!SCM_IS_IMMEDIATE (arg1), arg1, scm_arg2, s_map);
  if (SCM_EOL == args)
    {
      while (!SCM_IS_IMMEDIATE (arg1))
	{
	  SCM_ASSERT (SCM_CONSP (arg1), arg1, scm_arg2, s_map);
	  *pres = scm_cons (scm_apply3 (proc, SCM_CAR (arg1), scm_listofnull, 0), SCM_EOL);
	  pres = &SCM_CDR (*pres);
	  arg1 = SCM_CDR (arg1);
	}
      return res;
    }
  args = scm_vector (scm_cons (arg1, args));
  ve = SCM_VECTOR_ELTS (args);
  for (i = SCM_LENGTH (args) - 1; i >= 0; i--)
    {
      if (SCM_EOL == ve[i])
	return res;
      SCM_ASSERT (!SCM_IS_IMMEDIATE (ve[i]) && SCM_CONSP (ve[i]), ve[i], scm_arg2, s_map);
    }

  while (1)
    {
      arg1 = SCM_EOL;
      for (i = SCM_LENGTH (args) - 1; i >= 0; i--)
	{
	  if (SCM_IS_IMMEDIATE (ve[i]))
	    return res;
	  arg1 = scm_cons (SCM_CAR (ve[i]), arg1);
	  ve[i] = SCM_CDR (ve[i]);
	}
      *pres = scm_cons (scm_apply3 (proc, arg1, SCM_EOL, 0), SCM_EOL);
      pres = &SCM_CDR (*pres);
    }
}


/*(c for-each)
 * (for-each procedure . arguments)
 * 
 * Apply function to the list arguments, element-wise, for side
 * effects.
 *
 * `for-each' is similar to `map', but `for-each' does not return a
 * useful value - the results of applying `function' are ignored.
 */
SCM_PROC(s_for_each, "for-each", 2, 0, 1, scm_for_each);
SCM 
scm_for_each (SCM proc, SCM arg1, SCM args)
{
  SCM_INTS_ENABLED;
  SCM *ve;			/* Keep args from being optimized away. */
  long i;

  ve = &args;			/* Keep args from being optimized away. */

  if (SCM_EOL == arg1)
    return SCM_UNSPECIFIED;
  SCM_ASSERT (!SCM_IS_IMMEDIATE (arg1), arg1, scm_arg2, s_for_each);
  if (SCM_EOL == args)
    {
      while (!SCM_IS_IMMEDIATE (arg1))
	{
	  SCM_ASSERT (SCM_CONSP (arg1), arg1, scm_arg2, s_for_each);
	  scm_apply3 (proc, SCM_CAR (arg1), scm_listofnull, 0);
	  arg1 = SCM_CDR (arg1);
	}
      return SCM_UNSPECIFIED;
    }
  args = scm_vector (scm_cons (arg1, args));
  ve = SCM_VECTOR_ELTS (args);

  for (i = SCM_LENGTH (args) - 1; i >= 0; i--)
    {
      if (SCM_EOL == ve[i])
	return SCM_UNSPECIFIED;
      SCM_ASSERT (!SCM_IS_IMMEDIATE (ve[i]) && SCM_CONSP (ve[i]), args, scm_arg2, s_for_each);
    }

  while (1)
    {
      arg1 = SCM_EOL;
      for (i = SCM_LENGTH (args) - 1; i >= 0; i--)
	{
	  if (SCM_IS_IMMEDIATE (ve[i]))
	    return SCM_UNSPECIFIED;
	  arg1 = scm_cons (SCM_CAR (ve[i]), arg1);
	  ve[i] = SCM_CDR (ve[i]);
	}
      scm_apply3 (proc, arg1, SCM_EOL, 0);
    }
}

/*(c filter)
 * (filter pred list)
 * 
 * Return all the elements of list that satisfy predicate `pred'. The
 * list is not disordered -- elements that appear in the result list
 * occur in the same order as they occur in the argument list. The
 * returned list may share a common tail with the argument list. The
 * dynamic order in which the various applications of pred are made is
 * not specified.
 * 
 *  	(filter even? '(0 7 8 8 43 -4)) => (0 8 8 -4)
 */

/*(c and-map)
 * (and-map f l)
 * 
 * Apply `f' to successive elements of `l' until the end of list or `f' 
 * returns `#f'.
 * 
 * If returning early, return `#f'.  Otherwise, return the last value returned
 * by `f'.  If `f' has never been called because `l' is empty, return `#t'.
 */

/*(c or-map)
 * (or-map f l)
 * 
 * Apply `f' to successive elements of `l' until end of list or until `f'
 * returns a value other than `#f'.
 * 
 * If returning early, return the return value of the last call to `f'.
 * Otherwise, return `#f'.
 */



/************************************************************************
 *h1 "The C Interface to Lists")
 * 
 */

/*c scm_listify)
 * SCM scm_listify (SCM elt, ...);
 * 
 * Return a list of the arguments.  The last argument (not included in
 * the list) must be `SCM_UNDEFINED'.
 */
SCM
scm_listify (SCM elt, ...)
{
  SCM_INTS_INDIFFERENT;
  va_list foo;
  SCM answer;
  SCM *pos;

  var_start (foo, elt);
  answer = SCM_EOL;
  pos = &answer;
  while (elt != SCM_UNDEFINED)
    {
      *pos = scm_cons (elt, SCM_EOL);
      pos = &SCM_CDR (*pos);
      elt = va_arg (foo, SCM);
    }
  return answer;
}



/*c scm_ilength)
 * SCM scm_ilength (SCM obj);
 * 
 * Return the length of `obj' if it is a list, otherwise return -1.
 */
long
scm_ilength (SCM sx)
{
  SCM_INTS_INDIFFERENT;
  long i;
  SCM x;

  i = 0;
  x = sx;
  do
    {
      if (SCM_IS_IMMEDIATE(x))
	return (SCM_EOL == x) ? i : -1;
      if (SCM_NCONSP(x))
	return -1;
      x = SCM_CDR(x);
      i++;
      if (SCM_IS_IMMEDIATE(x))
	return (SCM_EOL == x) ? i : -1;
      if (SCM_NCONSP(x))
	return -1;
      x = SCM_CDR(x);
      i++;
      sx = SCM_CDR(sx);
    } while (x != sx);
  return -1;
}


/*c scm_sloppy_ilength)
 * SCM scm_sloppy_ilength (SCM obj);
 * 
 * Return the length of `obj' if it is a list or improper list.
 * Return -1 if `obj' is a circular list.
 */
long
scm_sloppy_ilength (SCM sx)
{
  SCM_INTS_INDIFFERENT;
  long i;
  SCM x;

  i = 0;
  x = sx;
  do
    {
      if (SCM_IS_IMMEDIATE (x)  || SCM_NCONSP (x))
	return i;
      x = SCM_CDR(x);
      i++;
      if (SCM_IS_IMMEDIATE (x)  || SCM_NCONSP (x))
	return i;
      x = SCM_CDR(x);
      i++;
      sx = SCM_CDR(sx);
    } while (x != sx);
  return -1;
}


/*c scm_eilength)
 * SCM scm_eilength (SCM obj);
 * 
 * Return the length of `obj' if it is a list (including a list with
 * GLOC elements), otherwise return -1.
 */
long
scm_eilength (SCM sx)
{
  SCM_INTS_INDIFFERENT;
  long i;
  SCM x;

  i = 0;
  x = sx;
  do
    {
      if (SCM_IS_IMMEDIATE(x))
	return (SCM_EOL == x) ? i : -1;
      if (SCM_NECONSP(x))
	return -1;
      x = SCM_CDR(x);
      i++;
      if (SCM_IS_IMMEDIATE(x))
	return (SCM_EOL == x) ? i : -1;
      if (SCM_NECONSP(x))
	return -1;
      x = SCM_CDR(x);
      i++;
      sx = SCM_CDR(sx);
    } while (x != sx);
  return -1;
}






void
scm_init_list (void)
{
  SCM_INTS_DISABLED;

#include "systas/libsystas/list.x"
}


/****************************************************************
 *(h1 "Rationale -- Lists"
 *    :category design)
 *
 * Systas follows the Scheme standard with a few obvious extensions.
 * 
 * 
 */
