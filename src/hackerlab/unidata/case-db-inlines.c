/* tag: Tom Lord Tue Dec  4 14:41:49 2001 (case-db-inlines.c)
 */
/* case-db-inlines.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/uni/unidata.h"
#include "hackerlab/unidata/case-db.h"
#include "hackerlab/unidata/case-db-macros.h"





#ifndef UNIDATA_INLINE_QUALIFIERS
#define UNIDATA_INLINE_QUALIFIERS
#endif

#ifndef __GNUC__
#undef __attribute__
#define __attribute__(X)
#endif


/************************************************************************
 *(h2 "Simple Unicode Case Conversions")
 * 
 * These functions use the case mappings in `unidata.txt'.
 * 
 */

/*(c unidata_to_upper)
 * t_unicode unidata_to_upper (t_unicode c);
 * 
 * If `c' has a default uppercase mapping, return that mapping.
 * Otherwise, return `c'.
 */
UNIDATA_INLINE_QUALIFIERS t_unicode __attribute__((unused))
unidata_to_upper (t_unicode c)
{
  t_unicode mapping;

  mapping = unidata__case_db_ref (unidata__case_db, c).upper;
  return (mapping ? mapping : c);
}


/*(c unidata_to_lower)
 * t_unicode unidata_to_lower (t_unicode c);
 * 
 * If `c' has a default lowercase mapping, return that mapping.
 * Otherwise, return `c'.
 */
UNIDATA_INLINE_QUALIFIERS t_unicode __attribute__((unused))
unidata_to_lower (t_unicode c)
{
  t_unicode mapping;

  mapping = unidata__case_db_ref (unidata__case_db, c).lower;
  return (mapping ? mapping : c);
}


/*(c unidata_to_title)
 * t_unicode unidata_to_title (t_unicode c);
 * 
 * If `c' has a default titlecase mapping, return that mapping.
 * Otherwise, return `c'.
 */
UNIDATA_INLINE_QUALIFIERS t_unicode __attribute__((unused))
unidata_to_title (t_unicode c)
{
  t_unicode mapping;

  mapping = unidata__case_db_ref (unidata__case_db, c).title;
  return (mapping ? mapping : c);
}
