/* panic.h - decls for fatal errors
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__BUGS__PANIC_H
#define INCLUDE__BUGS__PANIC_H


#include "hackerlab/machine/types.h"
#include "hackerlab/bugs/panic-exit.h"


#define invariant_4(CONDITION, MSG, FILE, LINE) \
	invariant_test (CONDITION, MSG, FILE, LINE)

#define invariant_2(CONDITION, MSG) \
	invariant_4 (CONDITION, MSG, __FILE__, __LINE__)

#define invariant__x(CONDITION) \
	invariant_4 (CONDITION, #CONDITION, __FILE__, __LINE__)

#define invariant(CONDITION) \
	invariant__x(CONDITION)

#define invariant_at_file_linex(CONDITION, FILE, LINE) \
	invariant_test (CONDITION, #CONDITION, FILE, LINE)

#define invariant_at_file_line(CONDITION, FILE, LINE) \
	invariant_at_file_linex(CONDITION, FILE, LINE)



/* automatically generated __STDC__ prototypes */
extern void panic (char * str);
extern void panic_msg (char * str);
extern void invariant_test (int condition, char * str, char * file, int line);
#endif  /* INCLUDE__BUGS__PANIC_H */
