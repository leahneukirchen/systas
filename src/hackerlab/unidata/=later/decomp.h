/* tag: Tom Lord Tue Dec  4 14:41:44 2001 (decomp.h)
 */
/* decomp.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNI__DECOMP_H
#define INCLUDE__UNI__DECOMP_H


#define UNI_DECOMPOSITION_TYPES \
  UNI_DECOMPOSITION_TYPE(font, 2,"font") \
  UNI_DECOMPOSITION_TYPE(nobreak, 3,"noBreak") \
  UNI_DECOMPOSITION_TYPE(initial, 4,"initial") \
  UNI_DECOMPOSITION_TYPE(medial, 5,"medial") \
  UNI_DECOMPOSITION_TYPE(final, 6,"final") \
  UNI_DECOMPOSITION_TYPE(isolated, 7,"isolated") \
  UNI_DECOMPOSITION_TYPE(circle, 8,"circle") \
  UNI_DECOMPOSITION_TYPE(super, 9,"super") \
  UNI_DECOMPOSITION_TYPE(sub, 10,"sub") \
  UNI_DECOMPOSITION_TYPE(vertical, 11,"vertical") \
  UNI_DECOMPOSITION_TYPE(wide, 12,"wide") \
  UNI_DECOMPOSITION_TYPE(narrow, 13,"narrow") \
  UNI_DECOMPOSITION_TYPE(small, 14,"small") \
  UNI_DECOMPOSITION_TYPE(square, 15,"square") \
  UNI_DECOMPOSITION_TYPE(fraction, 16,"fraction") \
  UNI_DECOMPOSITION_TYPE(compat, 17, "compat")

enum uni_decomposition_type
{
#undef UNI_DECOMPOSITION_TYPE
#define UNI_DECOMPOSITION_TYPE(SYM, NUM, NAME) \
  uni_ ## SYM ## _decomposition = NUM,

  uni_canonical_decomposition = 1,
  UNI_DECOMPOSITION_TYPES
};

struct uni_decomposition_mapping
{
  enum uni_decomposition_type type;
  t_unicode * decomposition;	/* ar_free */
};



/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__UNI__DECOMP_H */
