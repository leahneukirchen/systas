/* tag: Tom Lord Tue Dec  4 14:41:37 2001 (unidata.c)
 */
/* unidata.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/char/str.h"
#include "hackerlab/uni/unidata.h"


/*(c uni_bidi_category)
 * enum uni_bidi_categories;
 * 
 * An enumeration of the bi-directional classifications of 
 * of Unicode characters.
 * 
 * (See the *Unicode 3.0* specification for more information
 * on the meaning of these categories.)
 * 
 * Each category name is formed by prefixing `uni_bidi_' to
 * a category name (`L', `LRE', etc.):
 * 
 * 	uni_bidi_L
 * 	uni_bidi_LRE
 * 	etc.
 * 
 * The macro `UNI_BIDI_CATEGORIES' may also be handy:
 * 
 */


#undef UNI_BIDI_CATEGORY
#define UNI_BIDI_CATEGORY(n,ign)	{ #n, uni_bidi_ ## n },
const struct uni_bidi_category_name uni_bidi_category_names[] =
{
  UNI_BIDI_CATEGORIES
    {0, 0}
};

const int n_uni_bidi_categories = sizeof (uni_bidi_category_names) / sizeof (struct uni_bidi_category_name);


/*(c uni_bidi_category_lookup_n)
 * int uni_bidi_category_lookup_n (t_uchar * name, size_t len);
 * 
 * Given the name of a bidirectional category (e.g. "l"), return the
 * corresponding `enum uni_bidi_category'.  If `name' is not valid,
 * return -1.
 *
 * `name' -- the bidi category name to look up.
 * `len' -- the length of `name' in bytes.
 */
int
uni_bidi_category_lookup_n (t_uchar * name, size_t len)
{
  int x;

  for (x = 0; x < n_uni_bidi_categories; ++x)
    {
      if (!str_casecmp_n (uni_bidi_category_names[x].name, str_length (uni_bidi_category_names[x].name), name, len))
	return uni_bidi_category_names[x].category;
    }

  return -1;
}



/*(c uni_bidi_category_lookup)
 * int uni_bidi_category_lookup (t_uchar * name);
 * 
 * Given the name of a bidirectional category (e.g. "l"), return the
 * corresponding `enum uni_bidi_category'.  IF `name' is not valid,
 * return -1.
 *
 * `name' -- the bidi category name to look up (0-terminated).
 */
int
uni_bidi_category_lookup (t_uchar * name)
{
  return uni_bidi_category_lookup_n (name, str_length (name));
}




const struct uni_general_category_name uni_general_category_names[] = 
{
#define UNI_GENERAL_CATEGORY_NAME(NAME, ALT_NAME, DESC) \
	{ #NAME, uni_general_category_##NAME },
#define UNI_SYNTH_CATEGORY_NAME(NAME, ALT_NAME, DESC) \
	{ #NAME, uni_general_category_##NAME },

  UNI_GENERAL_CATEGORIES_LIST(_NAME)
  UNI_SYNTHETIC_CATEGORIES_LIST(_NAME)
    {0, 0}
};

const enum uni_general_category uni_first_synthetic_category = uni_general_category_L;
const int uni_n_categories = (uni_general_category_symbol + 1);


int
uni_general_category_lookup_n (t_uchar * name, size_t len)
{
  int x;
  for (x = 0; uni_general_category_names[x].name; ++x)
    if (!str_casecmp_n (name, len, uni_general_category_names[x].name, str_length (uni_general_category_names[x].name)))
      return uni_general_category_names[x].category;

  return -1;
}


int
uni_general_category_lookup (t_uchar * name)
{
  return uni_general_category_lookup_n (name, str_length (name));
}



#undef UNI_DECOMPOSITION_TYPE
#define UNI_DECOMPOSITION_TYPE(n)	{ #n, uni_decomposition_ ## n },
const struct uni_decomposition_type_name uni_decomposition_type_names[] =
{
  UNI_DECOMPOSITION_TYPES
    {0, 0}
};

const int n_uni_decomposition_types = sizeof (uni_decomposition_type_names) / sizeof (struct uni_decomposition_type_name);


/*(c uni_decomposition_type_lookup_n)
 * int uni_decomposition_type_lookup_n (t_uchar * name, size_t len);
 * 
 * Given the name of a decomposition mapping type (e.g. "font"), return the
 * corresponding `enum uni_decomposition_type'.  If `name' is not valid,
 * return -1.
 *
 * `name' -- the bidi category name to look up.
 * `len' -- the length of `name' in bytes.
 */
int
uni_decomposition_type_lookup_n (t_uchar * name, size_t len)
{
  int x;

  for (x = 0; x < n_uni_bidi_categories; ++x)
    {
      if (!str_casecmp_n (uni_decomposition_type_names[x].name, str_length (uni_decomposition_type_names[x].name), name, len))
	return uni_decomposition_type_names[x].category;
    }

  return -1;
}



/*(c uni_decomposition_type_lookup)
 * int uni_decomposition_type_lookup (t_uchar * name);
 * 
 * Given the name of a bidirectional category (e.g. "l"), return the
 * corresponding `enum uni_decomposition_type'.  If `name' is not valid,
 * return -1.
 *
 * `name' -- the bidi category name to look up (0-terminated).
 */
int
uni_decomposition_type_lookup (t_uchar * name)
{
  return uni_decomposition_type_lookup_n (name, str_length (name));
}
