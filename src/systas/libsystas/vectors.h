/* vectors.h - decls for scheme vectors (arrays)
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */


#ifndef INCLUDE__LIBSYSTAS__VECTORS_H
#define INCLUDE__LIBSYSTAS__VECTORS_H

#include "systas/libsystas/scm.h"



/* Return a pointer to the array of SCM values contained
 * by a vector (or weak vector).
 */
#define SCM_VECTOR_ELTS(x) 		((SCM *)SCM_CDR(x))


/* automatically generated __STDC__ prototypes */
extern SCM scm_vector_p(SCM x);
extern SCM scm_vector_length(SCM v);
extern SCM scm_vector_ref(SCM v, SCM k);
extern SCM scm_vector_set_x(SCM v, SCM k, SCM obj);
extern SCM scm_vector_equal_p (SCM x, SCM y, SCM eq);
extern SCM scm_make_vector (SCM k, SCM fill, SCM multip);
extern SCM scm_vector (SCM l);
extern SCM scm_vector_fill_x (SCM v, SCM fill_x);
extern SCM scm_vector_move_left_x (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2);
extern SCM scm_vector_move_right_x (SCM vec1, SCM start1, SCM end1, SCM vec2, SCM start2);
extern SCM scm_vector_to_list(SCM v);
extern SCM scm_list_to_vector (SCM elts);
extern int scm_is_vector (SCM x);
extern SCM scm_makvector (int i, SCM fill, int multi, SCM head);
extern void scm_init_vectors (void);
#endif  /* INCLUDE__LIBSYSTAS__VECTORS_H */
