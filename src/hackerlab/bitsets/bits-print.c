/* tag: Tom Lord Tue Dec  4 14:41:35 2001 (bits-print.c)
 */
/* bits-print.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/vu/safe.h"
#include "hackerlab/bitsets/bitset-tree-print.h"
#include "hackerlab/bitsets/bits-print.h"


/************************************************************************
 *(h2 "Printing Shared Bitset Trees"
 *    :includes ("hackerlab/bitsets/bits-print.h"))
 * 
 * |bitset printing (shared bitset trees)|
 * |printing (shared bitset trees)|
 * 
 * 
 */


/*(c bits_print)
 * void bits_print (int fd,
 *                  bits set,
 *                  t_uchar * name,
 *                  t_uchar * stub,
 *                  int is_static,
 *                  int decls_only,
 *                  int as_macro);
 * 
 * Print C code for an initialized shared bitset tree.
 * 
 * `fd' -- print output on descriptor `fd'.
 * 
 * `set' -- the bitset to print.
 * 
 * `name' -- the variable name for the bitset.
 * 
 * `stub' -- a variable name prefix for components of the bitset.
 * 
 * `is_static' -- print a static declaration.
 * 
 * `decls_only' -- print only a declaration, not the bitset itself.
 * 
 * `as_macro' -- define `name' as a pre-processor name, not a variable.
 * 
 * This function calls `panic' if an error occurs.
 */
void
bits_print (int fd,
	    bits set,
	    t_uchar * name,
	    t_uchar * stub,
	    int is_static,
	    int decls_only,
	    int as_macro)
{
#define MAX_NAME 2048
  int name_len;
  t_uchar struct_name[2 * MAX_NAME];
  t_uchar rule_name[2 * MAX_NAME];
  t_uchar stree_name[2 * MAX_NAME];
  
  name_len = str_length (name);
  if (name_len > MAX_NAME)
    panic ("name too long in bits_tree_print");

  if (decls_only)
    {
      safe_printfmt (fd, "%s bits %s;\n", (is_static ? "static" : "extern"),  name);
      return;
    }

  bits_compact (set);

  bits_tree_print (fd, set->lim, set->rule, set->stree->tree, stub, stub, 1, 0, 1);

  str_cpy (rule_name, stub);
  str_cat (rule_name, "_rule");
  safe_printfmt (fd, "static struct bits_tree_rule %s[] =\n", rule_name);
  safe_printfmt (fd, "{\n");
  {
    int x;
    x = 0;
    do
      {
	safe_printfmt (fd, "  {%d, %lu, %lu, %lu},\n",
		       set->rule[x].fanout,
		       (unsigned long)set->rule[x].subset_size,
		       (unsigned long)set->rule[x].subset_shift,
		       (unsigned long)set->rule[x].subset_mask);
      }
    while (set->rule[x++].fanout);
  }
  safe_printfmt (fd, "};\n\n");

  str_cpy (stree_name, stub);
  str_cat (stree_name, "_stree");
  safe_printfmt (fd, "static struct bits_tree_shared %s = { %d, (bits_tree)&%s };\n\n", stree_name, 1, stub);

  str_cpy (struct_name, stub);
  str_cat (struct_name, "_bits");
  safe_printfmt (fd, "static struct bits %s = \n", struct_name);
  safe_printfmt (fd, "{\n");
  safe_printfmt (fd, "  0,\n");
  safe_printfmt (fd, "  %s,\n", rule_name);
  safe_printfmt (fd, "  &%s\n", stree_name);
  safe_printfmt (fd, "};\n\n");
  if (!as_macro)
    safe_printfmt (fd, "%sbits %s = &%s;\n\n", (is_static ? "static " : ""), name, struct_name);
  else
    safe_printfmt (fd, "#define %s  (&%s)\n\n", name, struct_name);
}

