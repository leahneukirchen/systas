/* tag: Tom Lord Tue Dec  4 14:41:51 2001 (pow2-array-print.c)
 */
/* pow2-array-print.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/panic.h"
#include "hackerlab/mem/alloc-limits.h"
#include "hackerlab/char/str.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/vu/safe.h"
#include "hackerlab/hash/hashtree.h"
#include "hackerlab/hash/hash-utils.h"
#include "hackerlab/arrays/ar.h"
#include "hackerlab/arrays/pow2-array-print.h"


/************************************************************************
 *(h2 "Generating C Code for a Sparse Array")
 * 
 * 
 * 
 */




static void
pow2_array_print_node (int fd,
		       struct hashtree * table,
		       t_uchar * name,
		       void * node,
		       void ** default_tree_nodes,
		       t_uchar ** default_tree_names,
		       struct pow2_array_level_rule * level,
		       size_t elt_size,
		       t_uchar * elt_type,
		       void (*print_elt_initializer) (int fd, void * elt))
{
  if (!node)
    {
      safe_printfmt (fd, "#define %s 0\n\n", name);
      return;
    }
  else if (default_tree_nodes && (*default_tree_nodes == node))
    {
      safe_printfmt (fd, "#define %s %s\n\n", name, *default_tree_names);
      return;
    }

  if (table)
    {
      struct hashtree_item * item;

      item = hashtree_store (table, hash_ul ((unsigned long)node), node, 0);
      if (item->binding)
	{
	  safe_printfmt (fd, "#define %s %s\n\n", name, (t_uchar *)item->binding);
	  return;
	}
      else
	{
	  item->binding = (void *)str_save (lim_use_must_malloc, name);
	}
    }

  if (!level->addr_shift)
    {
      size_t n_elts;
      size_t x;

      safe_printfmt (fd, "static %s %s[] =\n", elt_type, name);
      safe_printfmt (fd, "{");
      n_elts = level->addr_mask + 1;
      for (x = 0; x < n_elts; ++x)
	{
	  print_elt_initializer (fd, (void *)((t_uchar *)node + elt_size * x));
	  safe_printfmt (fd, ",\n");
	}
      safe_printfmt (fd, "};\n\n");
    }
  else
    {
      size_t n_elts;
      size_t x;
      t_uchar * page_name;
      t_uchar page_suffix[100];

      n_elts = level->addr_mask + 1;
      page_suffix[0] = '_';
      for (x = 0; x < n_elts; ++x)
	{
	  cvt_ulong_to_decimal (page_suffix + 1, (t_ulong)x);
	  page_name = str_alloc_cat (lim_use_must_malloc, name, page_suffix);
	  pow2_array_print_node (fd,
				 table,
				 page_name,
				 ((void **)node)[x],
				 (default_tree_nodes ? (default_tree_nodes + 1) : 0),
				 (default_tree_names ? (default_tree_names + 1) : 0),
				 level + 1,
				 elt_size,
				 elt_type,
				 print_elt_initializer);
	  lim_free (lim_use_must_malloc, (void *)page_name);
	}
      safe_printfmt (fd, "static void * %s[] =\n", name);
      safe_printfmt (fd, "{");
      for (x = 0; x < n_elts; ++x)
	{
	  cvt_ulong_to_decimal (page_suffix + 1, (t_ulong)x);
	  page_name = str_alloc_cat (lim_use_must_malloc, name, page_suffix);
	  safe_printfmt (fd, "  (void *)%s,\n", page_name);
	  lim_free (lim_use_must_malloc, (void *)page_name);
	}
      safe_printfmt (fd, "};\n\n");
    }
}


static void
pow2_array_print_default_node (int fd,
			       t_uchar * name,
			       t_uchar * subtree_name,
			       struct pow2_array_level_rule * level)
{
  size_t elts;
  size_t x;

  elts = level->addr_mask + 1;
  safe_printfmt (fd, "static void * %s[] =\n", name);
  safe_printfmt (fd, "{\n");
  for (x = 0; x < elts; ++x)
    safe_printfmt (fd, "  (void *)%s,\n", subtree_name);
  safe_printfmt (fd, "};\n\n");
}


static void
pow2_array_rules_print (t_uchar *** default_tree_names_p,
			int fd,
			t_uchar * name,
			struct pow2_array_rules * rules,
			t_uchar * elt_type,
			void (*print_elt_initializer) (int fd, void * elt))
{
  t_uchar * levels_name;
  t_uchar * defaults_name;
  t_uchar ** default_tree_names;
  t_uchar * default_page_name;

  levels_name = 0;
  defaults_name = 0,
  default_tree_names = 0;
  default_page_name = 0;

  if (!rules->defaults)
    defaults_name = str_save (lim_use_must_malloc, "0");
  else
    {
      int level;

      for (level = 0; rules->levels[level].addr_shift; ++level)
	;

      default_page_name = str_alloc_cat (lim_use_must_malloc, name, "_dflt_page");
      pow2_array_print_node (fd, 0, default_page_name, rules->defaults[level], 0, 0, rules->levels + level, rules->elt_size, elt_type, print_elt_initializer);
      *(t_uchar **)ar_ref ((void **)&default_tree_names, lim_use_must_malloc, level, sizeof (t_uchar *))
	= str_save (lim_use_must_malloc, default_page_name);

      while (level--)
	{
	  static t_uchar suffix[100];
	  static t_uchar number[100];

	  cvt_long_to_decimal (number, (long)level);
	  str_cpy (suffix, "_dflt_");
	  str_cat (suffix, number);
	  default_tree_names[level] = str_alloc_cat (lim_use_must_malloc, name, suffix);
	  pow2_array_print_default_node (fd, default_tree_names[level], default_tree_names[level + 1], rules->levels + level);
	}

      defaults_name = str_alloc_cat (lim_use_must_malloc, name, "_defaults");
      safe_printfmt (fd, "static void * %s[] =\n", defaults_name);
      safe_printfmt (fd, "{\n");
      {
	int x;
	x = 0;
	while (1)
	  {
	    safe_printfmt (fd, "  (void *)%s,\n", default_tree_names[x]);
	    if (!rules->levels[x].addr_shift)
	      break;
	    ++x;
	  }
      }
      safe_printfmt (fd, "};\n\n");
    }

  {
    int x;
    levels_name = str_alloc_cat (lim_use_must_malloc, name, "_levels");
    safe_printfmt (fd, "static struct pow2_array_level_rule %s[] =\n", levels_name);
    safe_printfmt (fd, "{\n");
    x = 0;
    while (1)
      {
	safe_printfmt (fd, "  { %d, 0x%lx },\n", rules->levels[x].addr_shift, (t_ulong)rules->levels[x].addr_mask);
	if (!rules->levels[x].addr_shift)
	  break;
	++x;
      }
    safe_printfmt (fd, "};\n\n");
  }

  safe_printfmt (fd, "static struct pow2_array_rules %s =\n", name);
  safe_printfmt (fd, "{\n");
  safe_printfmt (fd, "  %s,\n", defaults_name);
  safe_printfmt (fd, "  %s,\n", levels_name);
  safe_printfmt (fd, "  %d,\n", rules->elt_size);
  safe_printfmt (fd, "};\n\n");

  lim_free (lim_use_must_malloc, levels_name);
  lim_free (lim_use_must_malloc, defaults_name);
  lim_free (lim_use_must_malloc, default_page_name);
  *default_tree_names_p = default_tree_names;
}



static void
pow2_array_print_macro_exp (int fd, pow2_array array, int level)
{
  if (!level)
    {
      safe_printfmt (fd, "((void **)(ARRAY)->root)");
    }
  else
    {
      safe_printfmt (fd, "((void **)(");
      pow2_array_print_macro_exp (fd, array, level - 1);
      safe_printfmt (fd, "[((INDEX) >> %d) & 0x%lx]))", array->rules->levels[level - 1].addr_shift, (t_ulong)array->rules->levels[level - 1].addr_mask);
    }
}




static void
free_item (struct hashtree_item * it, struct hashtree_rules * ign)
{
  lim_free (lim_use_must_malloc, it->binding);
}




/*(c pow2_array_print)
 * void pow2_array_print (int fd,
 *                        struct pow2_array * array,
 *                        t_uchar * name,
 *                        t_uchar * stub,
 *                        int decls_only,
 *                        t_uchar * ref_macro_name,
 *                        int is_static,
 *                        t_uchar * elt_type,
 *                        void (*print_elt_initializer) (int fd, void * elt));
 * 
 * On descriptor `fd', print C code which compiles to a staticly initialized
 * sparse array equal to `array'.
 * 
 * `name' is the variable name to use.
 * 
 * `stub' is a variable name prefix to use when printing internal
 * data structures of `array'.
 * 
 * `decls_only', if not 0, means to print only a declaration for the
 * array -- not the static initializer.
 * 
 * `ref_macro_name', if not 0, is a name for a CPP macro.  The macro
 * will be of the form:
 * 
 * 	#define REF_MACRO(ARRAY, ELT) ((ELT_TYPE *)(....))
 * 
 * The macro is a version of `pow2_array_rref', optimized for access to
 * the staticly initialized array.
 * 
 * `elt_type' is a type name for elements of the array.
 * 
 * `print_elt_initializer' prints a static initializer for particular
 * elements.
 * 
 * This function exits by calling `panic' if an error occurs.
 */
void
pow2_array_print (int fd,
		  struct pow2_array * array,
		  t_uchar * name,
		  t_uchar * stub,
		  int decls_only,
		  t_uchar * ref_macro_name,
		  int is_static,
		  t_uchar * elt_type,
		  void (*print_elt_initializer) (int fd, void * elt))
{
  struct hashtree * table;
  t_uchar * rules_name;
  t_uchar * root_name;
  t_uchar ** default_tree_names;
  t_uchar * struct_name;

  if (decls_only)
    {
      safe_printfmt (fd, "%s struct pow2_array * %s;\n", (is_static ? "static " : "extern "), name);
    }


  if (ref_macro_name)
    {
      int n_levels;

      n_levels = 0;
      for (n_levels = 0; array->rules->levels[n_levels].addr_shift; ++n_levels)
	;
      ++n_levels;

      if (!array->rules->defaults)
	panic ("asked to print pow2_array ref macro for array without defaults");

      safe_printfmt (fd, "#define %s(ARRAY, INDEX)  (((%s *)", ref_macro_name, elt_type);
      pow2_array_print_macro_exp (fd, array, n_levels - 1);
      safe_printfmt (fd, ")[(INDEX) & 0x%lx])\n\n", (t_ulong)array->rules->levels[n_levels - 1].addr_mask);
    }

  if (decls_only)
    return;

  table = hashtree_alloc (0);
  rules_name = str_alloc_cat (lim_use_must_malloc, stub, "_rules");
  pow2_array_rules_print (&default_tree_names, fd, rules_name, array->rules, elt_type, print_elt_initializer);

  root_name = str_alloc_cat (lim_use_must_malloc, stub, "_0");
  pow2_array_print_node
    (fd, table, root_name, array->root, array->rules->defaults, default_tree_names, array->rules->levels, array->rules->elt_size, elt_type, print_elt_initializer);

  struct_name = str_alloc_cat (lim_use_must_malloc, name, "_struct");

  safe_printfmt (fd, "static struct pow2_array %s =\n", struct_name);
  safe_printfmt (fd, "{\n");
  safe_printfmt (fd, "  lim_no_allocations,\n");
  safe_printfmt (fd, "  &%s,\n", rules_name);
  safe_printfmt (fd, "  (void *)%s\n", root_name);
  safe_printfmt (fd, "};\n\n");
  safe_printfmt (fd, "%spow2_array %s = &%s;\n", (is_static ? "static " : ""), name, struct_name);
  safe_printfmt (fd, "\n\n");

  lim_free (lim_use_must_malloc, (void *)rules_name);
  lim_free (lim_use_must_malloc, (void *)root_name);
  lim_free (lim_use_must_malloc, (void *)struct_name);
  hashtree_free (table, free_item, 0);
  {
    int x;

    x = (int)ar_size ((void *)default_tree_names, lim_use_must_malloc, sizeof (*default_tree_names));
    while (x--)
      {
	lim_free (lim_use_must_malloc, default_tree_names[x]);
      }
    ar_free ((void **)&default_tree_names, lim_use_must_malloc);
  }
}


