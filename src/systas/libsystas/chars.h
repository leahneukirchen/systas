/* chars.h - decls for scheme characters
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__CHARS_H
#define INCLUDE__LIBSYSTAS__CHARS_H

#include "systas/libsystas/scm.h"



struct scm_char_name
{
  char * name;
  int char_value;
};

/* scm_char_names[0..32] correspond to characters
 * 0..32.  Other entries are unordered.
 */
extern struct scm_char_name scm_char_names[];
extern int scm_n_char_names;


/* automatically generated __STDC__ prototypes */
extern SCM scm_char_p(SCM x);
extern SCM scm_char_eq_p(SCM x, SCM y);
extern SCM scm_char_lt_p(SCM x, SCM y);
extern SCM scm_char_le_p(SCM x, SCM y);
extern SCM scm_char_gt_p(SCM x, SCM y);
extern SCM scm_char_ge_p(SCM x, SCM y);
extern SCM scm_char_ci_eq_p(SCM x, SCM y);
extern SCM scm_char_ci_lt_p(SCM x, SCM y);
extern SCM scm_char_ci_le_p(SCM x, SCM y);
extern SCM scm_char_ci_gt_p(SCM x, SCM y);
extern SCM scm_char_ci_ge_p(SCM x, SCM y);
extern SCM scm_char_alpha_p(SCM chr);
extern SCM scm_char_digit_p(SCM chr);
extern SCM scm_char_space_p(SCM chr);
extern SCM scm_char_upper_p(SCM chr);
extern SCM scm_char_lower_p(SCM chr);
extern SCM scm_char_alphanumeric_p (SCM chr);
extern SCM scm_char_graph_p (SCM chr);
extern SCM scm_char_printable_p (SCM chr);
extern SCM scm_char_blank_p (SCM chr);
extern SCM scm_char_control_p (SCM chr);
extern SCM scm_char_punct_p (SCM chr);
extern SCM scm_char_xdigit_p (SCM chr);
extern SCM scm_char_ascii_p (SCM chr);
extern SCM scm_char_upcase(SCM chr);
extern SCM scm_char_downcase(SCM chr);
extern SCM scm_char_to_integer(SCM chr);
extern SCM scm_integer_to_char(SCM n);
extern SCM scm_write_char (SCM chr, SCM port);
extern int scm_exact_is_char (SCM x);
extern int scm_is_char (SCM x);
extern int scm_char_to_int (SCM x);
extern SCM scm_int_to_char (int x);
extern void scm_init_chars (void);
#endif  /* INCLUDE__LIBSYSTAS__CHARS_H */
