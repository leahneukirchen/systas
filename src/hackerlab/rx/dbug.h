/* dbug.h - debugging decls for rx
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__RX__DBUG_H
#define INCLUDE__RX__DBUG_H

#include "hackerlab/rx/tree.h"
#include "hackerlab/rx/nfa.h"
#include "hackerlab/rx/super.h"



/* automatically generated __STDC__ prototypes */
extern void rx_print_rexp (int fd,
			   int cset_size,
			   int indent,
			   struct rx_exp_node * rexp);
extern void rx_unparse_print_rexp (int fd,
				   int cset_size,
				   struct rx_exp_node * rexp);
extern void rx_print_nfa (int fd, struct rx_nfa * rx);
extern void rx_print_superstate (int fd, struct rx_superstate * state);
#endif  /* INCLUDE__RX__DBUG_H */
