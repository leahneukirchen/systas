/* test-decls.h - test decls.h
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef RX_TESTS_TEST_DECLSH
#define RX_TESTS_TEST_DECLSH

#include "hackerlab/rx-posix/regexps.h"

struct regmatch_answer
{
  regoff_t rm_so;
  regoff_t rm_eo;
};

struct rx_test
{
  t_uchar * name;
  t_uchar * pattern;
  int cflags;
  int compile_error;

  t_uchar * string;
  int eflags;

  int is_match;
  size_t n_match;
  struct regmatch_answer pmatch[11];
};

#include "hackerlab/tests/rx-posix-tests/test-cases.h"


/* automatically generated __STDC__ prototypes */
#endif  /* RX_TESTS_TEST_DECLSH */
