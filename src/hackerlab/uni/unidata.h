/* tag: Tom Lord Tue Dec  4 14:41:36 2001 (unidata.h)
 */
/* unidata.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__UNI__UNIDATA_H
#define INCLUDE__UNI__UNIDATA_H



#include "hackerlab/machine/types.h"



#define UNI_BIDI_CATEGORIES \
  UNI_BIDI_CATEGORY(L, "Left-to-Right") \
  UNI_BIDI_CATEGORY(LRE, "Left-to-Right Embedding") \
  UNI_BIDI_CATEGORY(LRO, "Left-to-Right Override") \
  UNI_BIDI_CATEGORY(R, "Right-to-Left") \
  UNI_BIDI_CATEGORY(AL, "Right-to-Left Arabic") \
  UNI_BIDI_CATEGORY(RLE, "Right-to-Left Embedding") \
  UNI_BIDI_CATEGORY(RLO, "Right-to-Left Override") \
  UNI_BIDI_CATEGORY(PDF, "Pop Directional Format") \
  UNI_BIDI_CATEGORY(EN, "European Number") \
  UNI_BIDI_CATEGORY(ES, "European Number Separator") \
  UNI_BIDI_CATEGORY(ET, "European Number Terminator") \
  UNI_BIDI_CATEGORY(AN, "Arabic Number") \
  UNI_BIDI_CATEGORY(CS, "Common Number Separator") \
  UNI_BIDI_CATEGORY(NSM, "Non-Spacing Mark") \
  UNI_BIDI_CATEGORY(BN, "Boundary Neutral") \
  UNI_BIDI_CATEGORY(B, "Paragraph Separator") \
  UNI_BIDI_CATEGORY(S, "Segment Separator") \
  UNI_BIDI_CATEGORY(WS, "Whitspace") \
  UNI_BIDI_CATEGORY(ON, "Other Neutrals")

enum uni_bidi_category
{
#undef UNI_BIDI_CATEGORY
#define UNI_BIDI_CATEGORY(NAME, IGN) uni_bidi_ ## NAME,

  UNI_BIDI_CATEGORIES
};

struct uni_bidi_category_name
{
  t_uchar * name;
  enum uni_bidi_category category;
};

extern const struct uni_bidi_category_name uni_bidi_category_names[];
extern const int n_uni_bidi_categories;



#define UNI_GENERAL_CATEGORIES_LIST(MACRO) \
  UNI_GENERAL_CATEGORY##MACRO (Lu, letter_uppercase, "Letter, uppercase") \
  UNI_GENERAL_CATEGORY##MACRO (Ll, letter_lowercase, "Letter, lowercase") \
  UNI_GENERAL_CATEGORY##MACRO (Lt, letter_titlecase, "Letter, titlecase") \
  UNI_GENERAL_CATEGORY##MACRO (Lm, letter_modifier, "Letter, modifier") \
  UNI_GENERAL_CATEGORY##MACRO (Lo, letter_other, "Letter, other") \
  \
  UNI_GENERAL_CATEGORY##MACRO (Mn, mark_nonspacing, "Mark, nonspacing") \
  UNI_GENERAL_CATEGORY##MACRO (Mc, mark_spacing_combining, "Mark, spacing combining") \
  UNI_GENERAL_CATEGORY##MACRO (Me, mark_enclosing, "Mark, enclosing") \
  \
  UNI_GENERAL_CATEGORY##MACRO (Nd, number_decimal_digit, "Number, decimal digit") \
  UNI_GENERAL_CATEGORY##MACRO (Nl, number_letter, "Number, letter") \
  UNI_GENERAL_CATEGORY##MACRO (No, number_other, "Number, other") \
  \
  UNI_GENERAL_CATEGORY##MACRO (Zs, separator_space, "Separator, space") \
  UNI_GENERAL_CATEGORY##MACRO (Zl, separator_line, "Separator, line") \
  UNI_GENERAL_CATEGORY##MACRO (Zp, separator_paragraph, "Separator, paragraph") \
  \
  UNI_GENERAL_CATEGORY##MACRO (Cc, other_control, "Other, control") \
  UNI_GENERAL_CATEGORY##MACRO (Cf, other_format, "Other, format") \
  UNI_GENERAL_CATEGORY##MACRO (Cs, other_surrogate, "Other, surrogate") \
  UNI_GENERAL_CATEGORY##MACRO (Co, other_private_use, "Other, private use") \
  UNI_GENERAL_CATEGORY##MACRO (Cn, other_not_assigned, "Other, not assigned") \
  \
  UNI_GENERAL_CATEGORY##MACRO (Pc, punctuation_connector, "Punctuation, connector") \
  UNI_GENERAL_CATEGORY##MACRO (Pd, punctuation_dash, "Punctuation, dash") \
  UNI_GENERAL_CATEGORY##MACRO (Ps, punctuation_open, "Punctuation, open") \
  UNI_GENERAL_CATEGORY##MACRO (Pe, punctuation_close, "Punctuation, close") \
  UNI_GENERAL_CATEGORY##MACRO (Pi, punctuation_initial_quote, "Punctuation, initial quote") \
  UNI_GENERAL_CATEGORY##MACRO (Pf, punctuation_final_quote, "Punctuation, final quote") \
  UNI_GENERAL_CATEGORY##MACRO (Po, punctuation_other, "Punctuation, other") \
  \
  UNI_GENERAL_CATEGORY##MACRO (Sm, symbol_math, "Symbol, math") \
  UNI_GENERAL_CATEGORY##MACRO (Sc, symbol_currency, "Symbol, currency") \
  UNI_GENERAL_CATEGORY##MACRO (Sk, symbol_modifier, "Symbol, modifier") \
  UNI_GENERAL_CATEGORY##MACRO (So, symbol_other, "Symbol, other")

#define UNI_SYNTHETIC_CATEGORIES_LIST(MACRO) \
  UNI_SYNTH_CATEGORY##MACRO (L, letter, "Letter") \
  UNI_SYNTH_CATEGORY##MACRO (M, mark, "Mark") \
  UNI_SYNTH_CATEGORY##MACRO (N, number, "Number") \
  UNI_SYNTH_CATEGORY##MACRO (Z, separator, "Separator") \
  UNI_SYNTH_CATEGORY##MACRO (C, other, "Other") \
  UNI_SYNTH_CATEGORY##MACRO (P, punctuation, "Punctuation") \
  UNI_SYNTH_CATEGORY##MACRO (S, symbol, "Symbol")


enum uni_general_category
{
#define UNI_GENERAL_CATEGORY_ENUM(NAME, ALT_NAME, DESC) \
	uni_general_category_##NAME,  uni_general_category_##ALT_NAME = uni_general_category_##NAME,
#define UNI_SYNTH_CATEGORY_ENUM(NAME, ALT_NAME, DESC) \
	uni_general_category_##NAME,  uni_general_category_##ALT_NAME = uni_general_category_##NAME,

  UNI_GENERAL_CATEGORIES_LIST(_ENUM)
  UNI_SYNTHETIC_CATEGORIES_LIST(_ENUM)
};


struct uni_general_category_name
{
  t_uchar * name;
  enum uni_general_category category;
};

extern const struct uni_general_category_name uni_general_category_names[];
extern const enum uni_general_category uni_first_synthetic_category;
extern const int uni_n_categories;



typedef t_uchar uni_canonical_combining_class;



struct uni_numeric_value
{
  t_uint numerator;
  t_uint denominator;
};



#define UNI_DECOMPOSITION_TYPES \
  UNI_DECOMPOSITION_TYPE(none) \
  UNI_DECOMPOSITION_TYPE(canonical) \
  UNI_DECOMPOSITION_TYPE(font) \
  UNI_DECOMPOSITION_TYPE(noBreak) \
  UNI_DECOMPOSITION_TYPE(initial) \
  UNI_DECOMPOSITION_TYPE(medial) \
  UNI_DECOMPOSITION_TYPE(final) \
  UNI_DECOMPOSITION_TYPE(isolated) \
  UNI_DECOMPOSITION_TYPE(circle) \
  UNI_DECOMPOSITION_TYPE(super) \
  UNI_DECOMPOSITION_TYPE(sub) \
  UNI_DECOMPOSITION_TYPE(vertical) \
  UNI_DECOMPOSITION_TYPE(wide) \
  UNI_DECOMPOSITION_TYPE(narrow) \
  UNI_DECOMPOSITION_TYPE(small) \
  UNI_DECOMPOSITION_TYPE(square) \
  UNI_DECOMPOSITION_TYPE(fraction) \
  UNI_DECOMPOSITION_TYPE(compat)

enum uni_decomposition_type
{
#undef UNI_DECOMPOSITION_TYPE
#define UNI_DECOMPOSITION_TYPE(NAME) \
  uni_decomposition_ ## NAME,

  UNI_DECOMPOSITION_TYPES
};

struct uni_decomposition_mapping
{
  enum uni_decomposition_type type;
  t_unicode * decomposition;
};

struct uni_decomposition_type_name
{
  t_uchar * name;
  enum uni_decomposition_type category;
};

extern const struct uni_decomposition_type_name uni_decomposition_type_names[];
extern const int n_uni_decomposition_types;


struct uni_case_mapping
{
  t_unicode upper;
  t_unicode lower;
  t_unicode title;
};


/* automatically generated __STDC__ prototypes */
extern int uni_bidi_category_lookup_n (t_uchar * name, size_t len);
extern int uni_bidi_category_lookup (t_uchar * name);
extern int uni_general_category_lookup_n (t_uchar * name, size_t len);
extern int uni_general_category_lookup (t_uchar * name);
extern int uni_decomposition_type_lookup_n (t_uchar * name, size_t len);
extern int uni_decomposition_type_lookup (t_uchar * name);
#endif  /* INCLUDE__UNI__UNIDATA_H */
