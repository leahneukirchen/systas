/* re8-parse.h - decls for compiling regexp syntax to rx_exp_node trees
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__RX_POSIX__RE8_PARSE_H
#define INCLUDE__RX_POSIX__RE8_PARSE_H



#include "hackerlab/machine/types.h"
#include "hackerlab/rx/tree.h"
#include "hackerlab/rx-posix/errnorx.h"



/* The interface for translating a regexp string to 
 * a regexp syntax tree.
 */

extern const t_uchar rx_case_fold_translation_table[];
extern const t_uchar rx_id_translation_table[];



/* automatically generated __STDC__ prototypes */
extern int rx_parse (struct rx_exp_node ** rx_exp_p,
		     int * nsub,
		     const t_uchar * pattern,
		     size_t size,
		     int extended_p,
		     int newline_separates_lines,
		     int dfa_only,
		     int cset_size,
		     const t_uchar * translate);
#endif  /* INCLUDE__RX_POSIX__RE8_PARSE_H */
