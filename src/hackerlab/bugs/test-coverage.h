/* tag: Tom Lord Tue Dec  4 14:41:26 2001 (test-coverage.h)
 */
/* test-coverage.h -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__BUGS__TEST_COVERAGE_H
#define INCLUDE__BUGS__TEST_COVERAGE_H


#include "hackerlab/bugs/panic.h"



/* example:
 *
 * #define TEST_COVERAGE_LIST(MACRO) \
 *   TEST_COVERAGE_ ## MACRO (name1); \
 *   TEST_COVERAGE_ ## MACRO (name2); \
 *   ...			      \
 *   TEST_COVERAGE_ ## MACRO (name2) \
 *
 *
 * TEST_COVERAGE_LIST(DECL);
 *
 *
 * {
 * 	...
 *   TEST_COVERED(name_n);
 * 
 * }
 * 
 * 
 *
 * {
 *    TEST_COVERAGE_LIST(CHECK);
 *    exit (0);
 * }
 */

#define TEST_COVERAGE_DECL(NAME) 	static int NAME = 0
#define TEST_COVERAGE_CHECK(NAME)	if (!NAME) panic ("test coverage failure: " #NAME)
#define TEST_COVERED(NAME)  NAME = 1



/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__BUGS__TEST_COVERAGE_H */
