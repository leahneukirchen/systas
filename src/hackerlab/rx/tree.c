/* tree.c - parse trees for regexps
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/rx/nfa-cache.h"
#include "hackerlab/rx/tree.h"


static unsigned long exps_allocated = 0;
static unsigned long exps_freed = 0;

/****************************************************************
 *(h0 "Regexp Expression Trees" 
 *    :subtitle "rx/rexp.c"
 *    :includes ("rx/tree.h"))
 *
 *
 * Every regexp has a natural representation as a syntax tree.  The
 * type `struct rx_exp_node' is used to represent one node of such a
 * tree.  Functions are provided that construct and manipulate
 * expression trees representing regexps.
 * 
 * Syntax trees can be constructed directly by programs as a way to
 * specify a regexp and strings obeying standard regexp syntax can be
 * parsed to produce syntax trees.  Regexp parsing is explained in
 * xref:"Regexp Parsing".  The topic of this chapter is the output
 * of parsing: the data structure used to represent a regexp syntax
 * tree.
 *
 * Rx is able to compute some analytic (recursively defined)
 * properties of regexp expression tree nodes.  Therefore, the Rx
 * representation of expression tree nodes contains some "extra"
 * fields to hold that data.
 *
 * Rx is able to translate an expression tree into an NFA and
 * indirectly into a DFA.  The performance of this translation is
 * commonly critical and is greatly improved by caching information
 * in still more "extra" fields of expression nodes.
 */
/*(menu)
 */

/*(include-documentation "tree.h")
 */

/****************************************************************
 *(h1 "Regexp Tree Allocation and Initialization")
 */

/*(c rx_exp_node)
 * struct rx_exp_node * rx_exp_node (int type);
 *
 * Allocate a new expression node and initialize its type
 * and reference count.  The reference count is initialized to 1,
 * the type to `type', and all other fields to 0.
 *
 * If the allocation fails, return 0.
 *
 */
struct rx_exp_node *
rx_exp_node (enum rx_exp_node_type type)
{
  struct rx_exp_node *n;

  n = (struct rx_exp_node *) rx_nfa_cache_malloc (sizeof (*n));
  if (!n)
    return 0;
  ++exps_allocated;
  mem_set0 ((char *)n, sizeof (*n));
  n->type = type;
  n->refs = 1;
  return n;
}


/*(c rx_mk_r_cset)
 * struct rx_exp_node * rx_mk_r_cset (enum rx_exp_node_type type,
 *				      int size, bits b);
 *
 * Allocate a new character set expression node.  The reference count
 * is initialized to 1, the type to `type', the `cset' field to a
 * newly allocated copy of `b', the `cset_size' field to `size', and
 * all other fields to 0.
 *
 * If any allocation fails, return 0;
 * 
 * `type' should be `r_cset'.
 */
struct rx_exp_node *
rx_mk_r_cset (enum rx_exp_node_type type, int size, bits b)
{
  struct rx_exp_node * n;

  if (type != r_cset)
    panic ("unreasonable rexp node type in rx_mk_r_cset");

  n = rx_exp_node (type);
  if (!n)
    return 0;
  n->cset = bits_dup (b);
  if (!n->cset)
    {
      rx_free_exp (n);
      return 0;
    }
  n->cset_size = size;
  return n;
}


/*(c rx_mk_r_cset_take)
 * struct rx_exp_node * rx_mk_r_cset_take (enum rx_exp_node_type type,
 *				      int size, bits b);
 *
 * Allocate a new character set expression node.  The reference count
 * is initialized to 1, the type to `type', the `cset' field to a
 * newly allocated copy of `b', the `cset_size' field to `size', and
 * all other fields to 0.
 *
 * If any allocation fails, return 0.
 * 
 * `type' should be `r_cset'.
 */
struct rx_exp_node *
rx_mk_r_cset_take (enum rx_exp_node_type type, int size, bits b)
{
  struct rx_exp_node * n;

  if (type != r_cset)
    panic ("unreasonable rexp node type in rx_mk_r_cset_take");
  n = rx_exp_node (type);
  if (!n)
    return 0;
  n->cset = b;
  n->cset_size = size;
  return n;
}


/*(c rx_mk_r_binop)
 *  struct rx_exp_node * rx_mk_r_binop (enum rx_exp_node_type type,
 *                                      struct rx_exp_node * a,
 *                                      struct rx_exp_node * b);
 *
 * Allocate a regexp expression node with two subexpressions.  The
 * reference count is initialized to 1, the type to `type', the `left'
 * and `right' fields to `a' and `b', and all other fields to 0.
 *
 * If any allocation fails, return 0.
 *
 * The reference counts of `a' and `b' are not modified by this
 * function -- the caller's reference count to those nodes is taken 
 * over by this function.
 * 
 * `type' should be `r_concat' `r_right_concat' or `r_alternate'.
 */
struct rx_exp_node *
rx_mk_r_binop (enum rx_exp_node_type type,
	       struct rx_exp_node * a,
	       struct rx_exp_node * b)
{
  struct rx_exp_node * n;
  if ((type != r_concat) && (type != r_right_concat) && (type != r_alternate))
    panic ("unreasonable rexp node type in rx_mk_r_binop");
  n = rx_exp_node (type);
  if (!n)
    return 0;
  n->left = a;
  n->right = b;
  return n;
}


/*(c rx_mk_r_monop)
 * struct rx_exp_node * rx_mk_r_monop (enum rx_exp_node_type type,
 *                                     struct rx_exp_node * a);
 *
 * Allocate a regexp expression node with one subexpression.  The
 * reference count is initialized to 1, the type to `type', the `left'
 * field to `a', and all other fields to 0.
 *
 * If any allocation fails, return 0.
 *
 * The reference count of `a' is not modified by this function -- the
 * caller's reference count to that node is taken over by this
 * function.
 * 
 * `type' should be `r_star', `r_interval', `r_parens', or `r_context'.
 */
struct rx_exp_node *
rx_mk_r_monop (enum rx_exp_node_type type,
	       struct rx_exp_node * a)
{
  struct rx_exp_node * n;
  if (   (type != r_star)
      && (type != r_interval)
      && (type != r_parens)
      && (type != r_context))
    panic ("unreasonable rexp node type in rx_mk_r_monop");
  n = rx_exp_node (type);
  if (!n)
    return 0;
  n->left = a;
  n->right = 0;
  return n;
}



/*(c rx_mk_r_str)
 * struct rx_exp_node * rx_mk_r_str (enum rx_exp_node_type type,
 *                                   const t_uchar * s,
 *                                   size_t len,
 *				     enum uni_encoding_scheme encoding);
 *
 * Allocate a regexp expression node whose parameter is a string.  The
 * reference count is initialized to 1, the type to `type', the `str'
 * field to a newly allocated copy of the `len'-byte string `s', the
 * `str_len' field is initialized to `len', and all other fields to 0.
 *
 * If any allocation fails, return 0.
 * 
 * `type' should be `r_string'.
 */
struct rx_exp_node *
rx_mk_r_str (enum rx_exp_node_type type,
	     const t_uchar * s,
	     size_t len,
	     enum uni_encoding_scheme encoding)
{
  struct rx_exp_node *n;

  if (type != r_string)
    panic ("unreasonable rexp node type in rx_mk_r_str_c");
  n = rx_exp_node (type);
  if (!n)
    return 0;
  n->str = (t_uchar *)rx_nfa_cache_malloc (len);
  if (len && !n->str)
    {
      rx_free_exp (n);
      return 0;
    }
  mem_move (n->str, s, len);
  n->str_len = len;
  n->encoding = encoding;
  return n;
}


/*(c rx_mk_r_int)
 * struct rx_exp_node * rx_mk_r_int (enum rx_exp_node_type type,
 *                                   int intval);
 *
 * Allocate a regexp expression node whose parameter is an integer.
 * The reference count is initialized to 1, the type to `type', the
 * `intval' field is initialized to `intval', and all other fields to
 * 0.
 *
 * If any allocation fails, return 0.
 * 
 * `type' should be `r_cut', `r_parens', or `r_context'.
 */
struct rx_exp_node *
rx_mk_r_int (enum rx_exp_node_type type,
	     int intval)
{
  struct rx_exp_node * n;
  if (   (type != r_cut)
      && (type != r_parens)
      && (type != r_context))
    panic ("unreasonable rexp node type in rx_mk_r_int");
  n = rx_exp_node (type);
  if (!n)
    return 0;
  n->intval = intval;
  return n;
}


/*(c rx_mk_r_subexp_int)
 * struct rx_exp_node * rx_mk_r_subexp_int (enum rx_exp_node_type type,
 *				            struct rx_exp_node * subexp,
 *                                          int intval);
 *
 * Allocate a regexp expression node whose parameters are a
 * subexpression and an integer.  The reference count is initialized
 * to 1, the type to `type', the `left' field is initialized to
 * `subexp', the `intval' field is initialized to `intval', and all
 * other fields to 0.
 *
 * The reference count of `subexp' is not modified by this function --
 * the caller's reference count to that node is taken over by this
 * function.
 *
 * If any allocation fails, return 0.
 * 
 * `type' should be `r_parens'.  `intval' is the subexpression number
 * for this parenthesized expression (the number used for
 * backreferences).  If `intval' is 0, this is an anonymous
 * subexpression (it can not be backreferenced).
 */
struct rx_exp_node *
rx_mk_r_subexp_int (enum rx_exp_node_type type,
		    struct rx_exp_node * subexp,
		    int intval)
{
  struct rx_exp_node * n;
  if (type != r_parens)
    panic ("unreasonable rexp node type in rx_mk_r_subexp_int");
  n = rx_exp_node (type);
  if (!n)
    return 0;
  n->left = subexp;
  n->intval = intval;
  return n;
}


/*(c rx_mk_r_int2)
 * struct rx_exp_node * rx_mk_r_int2 (enum rx_exp_node_type type,
 *                                    int intval,
 *                                    int intval2);
 *
 * Allocate a regexp expression node whose parameters are two
 * integers.  The reference count is initialized to 1, the type to
 * `type', the `intval' and `intval2' fields are initialized to `intval' and `intval2', and all
 * other fields to 0.
 *
 * If any allocation fails, return 0.
 * 
 * `type' should be `r_interval'.
 */
struct rx_exp_node *
rx_mk_r_int2 (enum rx_exp_node_type type,
	      int intval,
	      int intval2)
{
  struct rx_exp_node * n;
  if (type != r_interval)
    panic ("unreasonable rexp node type in rx_mk_r_int2");
  n = rx_exp_node (type);
  if (!n)
    return 0;
  n->intval = intval;
  n->intval2 = intval2;
  return n;
}


/*(c rx_mk_r_subexp_int2)
 * struct rx_exp_node * rx_mk_r_subexp_int2 (enum rx_exp_node_type type,
 * 					     struct rx_exp_node * subexp,
 *                                           int intval,
 *                                           int intval2);
 *
 * Allocate a regexp expression node whose parameters are a
 * subexpression and two integers.  The reference count is initialized
 * to 1, the type to `type', the `left' field is initialized to
 * `subexp', the `intval' and `intval2' fields are initialized to
 * `intval' and `intval2', and all other fields to 0.
 *
 * The reference count of `subexp' is not modified by this function --
 * the caller's reference count to that node is taken over by this
 * function.
 * 
 * If any allocation fails, return 0.
 * 
 * `type' should be `r_interval'.  The two integer parameters are the
 * lower and upper bound of the iteration.
 */
struct rx_exp_node *
rx_mk_r_subexp_int2 (enum rx_exp_node_type type,
		     struct rx_exp_node * subexp,
		     int intval,
		     int intval2)
{
  struct rx_exp_node * n;
  if (type != r_interval)
    panic ("unreasonable rexp node type in rx_mk_r_int2");
  n = rx_exp_node (type);
  if (!n)
    return 0;
  n->left = subexp;
  n->intval = intval;
  n->intval2 = intval2;
  return n;
}


/****************************************************************
 *(h1 "Regexp Tree Reference Counting and Copying.")
 *
 */

/*(c rx_save_exp)
 * void rx_save_exp (struct rx_exp_node * node);
 * 
 * Increment the reference count of `node'.
 */
void
rx_save_exp (struct rx_exp_node * node)
{
  if (node)
    ++node->refs;
}


/*(c rx_free_exp)
 * void rx_free_exp (struct rx_exp_node * node);
 * 
 * Decrement the reference count of `node'.  If it drops to 0, free
 * the node, free all allocated data attached to it, and recursively
 * `rx_free_exp' the sub-expressions of `node'.
 */
void
rx_free_exp (struct rx_exp_node * node)
{
  if (node && !--node->refs)
    {
      if (node->cr)
	{
	  node->next_same_nfa->prev_same_nfa = node->prev_same_nfa;
	  node->prev_same_nfa->next_same_nfa = node->next_same_nfa;
	}
      if (node->cset)
	bits_free (node->cset);
      if (node->str)
	rx_nfa_cache_free (node->str);
      rx_free_exp (node->left);
      rx_free_exp (node->right);
      rx_free_exp (node->simplified);
      rx_nfa_cache_free ((char *)node);
      ++exps_freed;
      /* safe_printfmt (1, "free %lx\n", (unsigned long)node); */
    }
}


/****************************************************************
 *(h1 "Regexp Tree Hashing and Equality")
 *
 */

/*(c rx_exp_equal)
 * int rx_exp_equal (struct rx_exp_node * a, struct rx_exp_node * b);
 * 
 * Return 1 if two regexp expression trees are "equal", 0 otherwise.
 *
 * "Equal" means that the two trees have identical structure which is
 * a sufficient but not necessary condition of the two patterns
 * matching the same set of strings with identical results.
 */
int
rx_exp_equal (struct rx_exp_node * a, struct rx_exp_node * b)
{
  int ret;

  if (a == b)
    return 1;

  if ((a == 0) || (b == 0))
    return 0;

  if (a->type != b->type)
    return 0;

  switch (a->type)
    {
    case r_cset:
      ret = (   (a->cset_size == b->cset_size)
	     && bits_is_equal (a->cset, b->cset));
      break;

    case r_string:
      ret = (   (a->str_len == b->str_len)
	     && !str_cmp_n (a->str, a->str_len, b->str, a->str_len));
      break;

    case r_cut:
      ret = (a->intval == b->intval);
      break;

    case r_concat:
    case r_right_concat:
    case r_alternate:
      ret = (   rx_exp_equal (a->left, b->left)
	     && rx_exp_equal (a->right, b->right));
      break;
    case r_star:
      ret = rx_exp_equal (a->left, b->left);
      break;
    case r_interval:
      ret = (   (a->intval == b->intval)
	     && (a->intval2 == b->intval2)
	     && rx_exp_equal (a->left, b->left));
      break;
    case r_parens:
      ret = (   (a->intval == b->intval)
	     && rx_exp_equal (a->left, b->left));
      break;

    case r_context:
      ret = (a->intval == b->intval);
      break;
    default:
      return 0;
    }
  return ret;
}


static unsigned long
exp_hash (struct rx_exp_node * node, unsigned long seed)
{
  unsigned long contribution;

  if (!node)
    return seed;

  /* This is just made up and should be checked out. */

  contribution = (  node->type
		  ^ str_hash_n (node->str, node->str_len)
		  ^ ((seed << 3) + node->intval)
		  ^ ((seed << 3) + node->intval2));

  seed = contribution ^ (seed << 11) ^ (seed >> (8 * sizeof (seed) - 11));
  seed = exp_hash (node->left, seed);
  seed = exp_hash (node->right, seed);
  return seed;
}


/*(c rx_exp_hash)
 * unsigned long rx_exp_hash (struct rx_exp_node * node);
 * 
 * Compute a word-sized hash value for an expression tree.  Regexps
 * which are equal (in the sense of `rx_exp_equal') have equal hash
 * values (in the sense of `==').
 */
unsigned long
rx_exp_hash (struct rx_exp_node * node)
{
  return exp_hash (node, 0);
}


