/* symbols.h - decls for scheme symbols
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__SYMBOLS_H
#define INCLUDE__LIBSYSTAS__SYMBOLS_H

#include "systas/libsystas/scm.h"



extern int scm_symhash_size;


/* automatically generated __STDC__ prototypes */
extern SCM scm_symbol_p (SCM x);
extern SCM scm_symbol_to_string (SCM s);
extern SCM scm_string_to_symbol (SCM s);
extern SCM scm_string_to_hash_table_symbol (SCM o, SCM s, SCM softp);
extern SCM scm_intern_symbol (SCM o, SCM s);
extern SCM scm_unintern_symbol (SCM o, SCM s);
extern SCM scm_symbol_interned_p (SCM o, SCM s);
extern int scm_is_symbol (SCM obj);
extern SCM scm_symbol_to_vcell (SCM sym, SCM proc, SCM definep);
extern SCM scm_maybe_symbol_to_hash_table_vcell (SCM hash_table, SCM sym);
extern SCM scm_symbol_to_hash_table_vcell (SCM hash_table, SCM sym);
extern SCM scm_hash_table_intern (SCM hash_table,
				  t_uchar *name,
				  size_t len,
				  int create_it,
				  int define_it);
extern SCM scm_intern (char *name, size_t len);
extern SCM scm_intern0 (char * name);
extern SCM scm_intern_symhash (char *name, SCM val);
extern void scm_init_symbols (void);
#endif  /* INCLUDE__LIBSYSTAS__SYMBOLS_H */
