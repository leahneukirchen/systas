/* tag: Tom Lord Tue Dec  4 14:41:34 2001 (bitset-tree-print.c)
 */
/* bitset-tree-print.c - 
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/vu/safe.h"
#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/bitsets/bitset-print.h"
#include "hackerlab/bitsets/bitset-tree-print.h"


/************************************************************************
 *(h2 "Printing Bitset Trees"
 *    :includes ("hackerlab/bitsets/bitset-tree-print.h"))
 * 
 * 
 * 
 */

/*(c bits_tree_print)
 * void bits_tree_print (int fd,
 *                       alloc_limits lim,
 *                       struct bits_tree_rule * rule,
 *                       bits_tree bt,
 *                       t_uchar * name,
 *                       t_uchar * stub,
 *                       int is_static,
 *                       int decls_only,
 *                       int is_nested);
 * 
 * Print C code which compiles to an initialized bitset tree.
 * 
 * `fd' is the descriptor on which to emit code.
 * 
 * `lim', if not 0, describes allocation limits that apply to this
 * bitset tree.  For more information about allocation limits, see
 * xref:"Allocation With Limitations".
 * 
 * `rule' describes the branching structure of the bitset tree.
 * See xref:"Bitset Tree Rules".
 * 
 * `bt' is the bitset tree to print.
 * 
 * `name' is the name for the bitset tree.  
 * 
 * This function calls `panic' if an error occurs.
 */
void
bits_tree_print (int fd,
		 alloc_limits lim,
		 struct bits_tree_rule * rule,
		 bits_tree bt,
		 t_uchar * name,
		 t_uchar * stub,
		 int is_static,
		 int decls_only,
		 int is_nested)
{
#define MAX_NAME 2048
  int x;
  int name_len;
  t_uchar row_name[2 * MAX_NAME];
  t_uchar stub_name[2 * MAX_NAME];
  int name_end;
  bits_tree * set;
  
  name_len = str_length (name);
  if (name_len > MAX_NAME)
    panic ("name too long in bits_tree_print");

  if (decls_only)
    {
      safe_printfmt (fd, "%s bits_tree %s;\n", (is_static ? "static" : "extern"),  name);
      return;
    }

  bits_tree_compact (lim, rule, bt);

  set = (bits_tree *)bt;

  str_cpy (row_name, stub);

  str_cpy (stub_name, stub);
  str_cat (stub_name, "_");
  name_end = str_length (stub_name);

  if (rule->fanout == 0)
    {
      bitset_print (fd, rule->subset_size, (bitset)set, row_name, 1, 0);
    }
  else
    {
      for (x = 0; x < rule->fanout; ++x)
	{
	  if (   (set[x] == bits_tree_empty_bitset)
	      || (set[x] == bits_tree_full_bitset))
	    continue;

	  cvt_long_to_decimal (stub_name + name_end, (long)x);
	  bits_tree_print (fd, lim, rule + 1, set[x], stub_name, stub_name, 1, 0, 1);
	}
      
      row_name[name_end] = 0;
      safe_printfmt (fd, "\n\n");
      safe_printfmt (fd, "static bits_tree * %s[%d] =\n", row_name, rule->fanout);
      safe_printfmt (fd, "{\n");
      for (x = 0; x < rule->fanout; ++x)
	{
	  if (set[x] == bits_tree_empty_bitset)
	    safe_printfmt (fd, "  (bits_tree *)0,\n");
	  else if (set[x] == bits_tree_full_bitset)
	    safe_printfmt (fd, "  (bits_tree *)-1L,\n");
	  else
	    {
	      cvt_long_to_decimal (stub_name + name_end, (long)x);
	      safe_printfmt (fd, "  (bits_tree *)%s,\n", stub_name);
	    }
	}
      safe_printfmt (fd, "};\n\n");
    }

  if (!is_nested)
    {
      safe_printfmt (fd, "\n\n");
      safe_printfmt (fd, "%sbits_tree * %s = (bits_tree *)%s\n\n", (is_static ? "static " : ""), name, row_name);
    }
}

