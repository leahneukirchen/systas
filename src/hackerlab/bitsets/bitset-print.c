/* bitset-print.c - print a bitset as C source code.
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */




#include "hackerlab/vu/safe.h"
#include "hackerlab/bitsets/bitset-print.h"


/************************************************************************
 *(h2 "Printing Flat Bitsets"
 *    :includes ("hackerlab/bitsets/bitset-print.h"))
 * 
 * |bitset printing (flat bitsets)|
 * |printing (flat bitsets)|
 * 
 */



/*(c bitset_print)
 * void bitset_print (int fd,
 *                    bit_t size,
 *                    bitset set,
 *                    t_uchar * name,
 *                    int is_static,
 *                    int decls_only);
 * 
 * Print C code for an initialized bitset.
 * 
 * `fd' -- print output on descriptor `fd'.
 * 
 * `size' -- the number of bits in the set.
 * 
 * `set' -- the bitset to print.
 * 
 * `name' -- the variable name for the bitset.
 * 
 * `is_static' -- 1 for a static declaration, 0 otherwise.
 * 
 * `decls_only' -- print only an `extern' declaration.
 * 
 * This function calls `panic' if an error occurs.
 */
void
bitset_print (int fd,
	      bit_t size,
	      bitset set,
	      t_uchar * name,
	      int is_static,
	      int decls_only)
{
  int x;
  int n_subsets;
  
  if (decls_only)
    {
      safe_printfmt (fd, "extern biset_subset %s[];\n", name);
      return;
    }

  n_subsets = bitset_numb_subsets (size);

  safe_printfmt (fd, "%sbitset_subset %s[%d] =\n", (is_static ? "static " : ""), name, n_subsets);
  safe_printfmt (fd, "{\n");
  for (x = 0; x < n_subsets - 1; ++x)
    {
      safe_printfmt (fd, "  0x%lx,\n", set[x]);
    }
  if (bitset_bit_mask (size))
    safe_printfmt (fd, "  0x%lx,\n", set[x] & bitset_bit_mask (size));
  else
    safe_printfmt (fd, "  0x%lx,\n", set[x]);
  
  safe_printfmt (fd, "};\n");
}

