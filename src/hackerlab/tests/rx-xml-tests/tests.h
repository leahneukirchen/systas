/* tests.h  - declarations for testing rx-xml
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */

#ifndef INCLUDE__XML_TESTS__TESTS_H
#define INCLUDE__XML_TESTS__TESTS_H



#include "hackerlab/rx-xml/re.h"



struct xml_re_test_case
{
  t_uchar * test_name;
  t_uchar * re;
  enum rx_xml_recomp_errno comp_error;
  t_uchar * str;
  enum rx_xml_rematch_errno match_error;
  int is_match;
};

extern struct xml_re_test_case xml_re_test_cases[];
extern struct xml_re_test_case xml_re_test_cases_alternative_syntax[];




/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__XML_TESTS__TESTS_H */
