/* strings.h - strings
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__STRINGS_H
#define INCLUDE__LIBSYSTAS__STRINGS_H

#include "systas/libsystas/scm.h"



/* automatically generated __STDC__ prototypes */
extern SCM scm_string_p (SCM x);
extern SCM scm_string (SCM chrs);
extern SCM scm_make_string (SCM k, SCM chr);
extern SCM scm_substring (SCM str, SCM start, SCM end);
extern SCM scm_shared_substring_p (SCM str);
extern SCM scm_make_shared_substring (SCM str, SCM frm, SCM to);
extern SCM scm_shared_substring_parts (SCM str);
extern SCM scm_read_only_string_p (SCM x);
extern SCM scm_writable_string_p (SCM x);
extern SCM scm_basic_string_p (SCM x);
extern SCM scm_string_length (SCM str);
extern SCM scm_string_null_p (SCM str);
extern SCM scm_string_ref (SCM str, SCM k);
extern SCM scm_string_set_x (SCM str, SCM k, SCM chr);
extern SCM scm_string_index (SCM str, SCM chr, SCM frm, SCM to);
extern SCM scm_string_rindex (SCM str, SCM chr, SCM frm, SCM to);
extern SCM scm_substring_move_left_x (SCM str1, SCM start1, SCM end1, SCM str2, SCM start2);
extern SCM scm_substring_move_right_x (SCM str1, SCM start1, SCM end1, SCM str2, SCM start2);
extern SCM scm_substring_fill_x (SCM str, SCM start, SCM end, SCM chr);
extern SCM scm_string_fill_x (SCM str, SCM chr);
extern SCM scm_string_equal_p (SCM s1, SCM s2);
extern SCM scm_string_lt_p (SCM s1, SCM s2);
extern SCM scm_string_le_p (SCM s1, SCM s2);
extern SCM scm_string_gt_p (SCM s1, SCM s2);
extern SCM scm_string_ge_p (SCM s1, SCM s2);
extern SCM scm_string_ci_equal_p (SCM s1, SCM s2);
extern SCM scm_string_ci_lt_p (SCM s1, SCM s2);
extern SCM scm_string_ci_le_p (SCM s1, SCM s2);
extern SCM scm_string_ci_gt_p (SCM s1, SCM s2);
extern SCM scm_string_ci_ge_p (SCM s1, SCM s2);
extern SCM scm_string_upcase_x (SCM v);
extern SCM scm_string_downcase_x (SCM v);
extern SCM scm_list_to_string (SCM l);
extern SCM scm_string_append (SCM args);
extern SCM scm_string_to_list (SCM str);
extern int scm_is_basic_string (SCM obj);
extern int scm_is_ro_string (SCM obj);
extern int scm_is_string (SCM obj);
extern int scm_is_substr (SCM obj);
extern int scm_is_writable_substr (SCM obj);
extern int scm_is_ro_substr (SCM obj);
extern SCM scm_makstr (long len);
extern SCM scm_makfromstr (t_uchar *src, long len);
extern SCM scm_makfromstr0 (t_uchar *src);
extern SCM scm_take_str0 (char * it);
extern SCM scm_take_str (char * it, int len);
extern SCM scm_take_str0_static (char * it);
extern SCM scm_take_string_static (char * it, int len);
extern SCM scm_argv2scm (int argc, t_uchar **argv);
extern t_uchar ** scm_scm2argv (int * n_args, SCM args, SCM pos, SCM subr);
extern int scm_i_index (SCM * str,
	     SCM chr,
	     SCM sub_start,
	     SCM sub_end,
	     SCM pos,
	     SCM pos2,
	     SCM pos3,
	     SCM pos4,
	     SCM why);
extern int scm_i_rindex (SCM * str,
	      SCM chr,
	      SCM sub_start,
	      SCM sub_end,
	      SCM pos,
	      SCM pos2,
	      SCM pos3,
	      SCM pos4,
	      SCM why);
extern void scm_init_strings (void);
#endif  /* INCLUDE__LIBSYSTAS__STRINGS_H */
