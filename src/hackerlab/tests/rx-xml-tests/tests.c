/* tests.c - test cases for rx-xml
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/tests/rx-xml-tests/tests.h"



#define TEST(name,re,comperr,str,matcherr,ismatch) \
  { name, re, comperr, str, matcherr, ismatch }

#define TEST_LIST \
											                                \
  /* 			ATOM TESTS 							                                \
   */											                                \
											                                \
											                                \
  /* normal characters */								                                \
  TEST("atom test 0", "x", 0, "x", 0, 1),						                                \
											                                \
  /* 	character classes */								                                \
											                                \
  /* 		single character escape */						                                \
  TEST("atom test 1", "\\n", 0, "\n", 0, 1),						                                \
  TEST("atom test 2", "\\r", 0, "\r", 0, 1),						                                \
  TEST("atom test 3", "\\t", 0, "\t", 0, 1),						                                \
  TEST("atom test 4", "\\\\", 0, "\\", 0, 1),						                                \
  TEST("atom test 5", "\\.", 0, ".", 0, 1),						                                \
  TEST("atom test 6", "\\.", 0, "x", 0, 0),						                                \
											                                \
  TEST("atom test 7", "\\-", 0, "-", 0, 1),						                                \
  TEST("atom test 8", "\\^", 0, "^", 0, 1),						                                \
  TEST("atom test 9", "\\?", 0, "?", 0, 1),						                                \
  TEST("atom test 10", "\\*", 0, "*", 0, 1),						                                \
  TEST("atom test 11", "\\+", 0, "+", 0, 1),						                                \
  TEST("atom test 12", "\\{", 0, "{", 0, 1),						                                \
  TEST("atom test 13", "\\}", 0, "}", 0, 1),						                                \
  TEST("atom test 14", "\\(", 0, "(", 0, 1),						                                \
  TEST("atom test 15", "\\)", 0, ")", 0, 1),						                                \
  TEST("atom test 16", "\\[", 0, "[", 0, 1),						                                \
											                                \
  TEST("atom test 17", "\\a", rx_xml_recomp_BOGUS_CHAR, "a", 0, 0),			                                \
											                                \
  /* category escapes */												\
  TEST("category test 0", "\\p{isGreek}", 0, "\\u0370", 0, 1),				                                \
  TEST("category test 1", "\\p{isBasicLatin}", 0, "\\u0370", 0, 0),			                                \
  TEST("category test 2", "\\p{isbasiclatin}", 0, "\\u0370", 0, 0),			                                \
  TEST("category test 3", "\\p{isBasic Latin}", 0, "\\u0370", 0, 0),			                                \
  TEST("category test 4", "\\p{isbasic latin}", 0, "\\u0370", 0, 0),			                                \
  TEST("category test 5", "\\p{isGreek}", 0, "a", 0, 0),				                                \
  TEST("category test 6", "\\p{isBasicLatin}", 0, "a", 0, 1),			                                	\
  TEST("category test 7", "\\p{isbasiclatin}", 0, "a", 0, 1),			                                	\
  TEST("category test 8", "\\p{isBasic Latin}", 0, "a", 0, 1),			                                	\
  TEST("category test 9", "\\p{isbasic latin}", 0, "a", 0, 1),			                                	\
															\
  /* multi-character escapes */						                                		\
  TEST("mce test 0", ".", 0, "a", 0, 1),						                                \
  TEST("mce test 1", ".", 0, "b", 0, 1),						                                \
  TEST("mce test 2", ".", 0, "\n", 0, 0),						                                \
  TEST("mce test 3", ".", 0, "\r", 0, 0),						                                \
  TEST("mce test 4", ".", 0, " ", 0, 1),						                                \
  TEST("mce test 5", ".", 0, "\\u2200", 0, 1),					                                	\
											                                \
  TEST("mce test 6", "\\s", 0, " ", 0, 1),						                                \
  TEST("mce test 7", "\\s", 0, "\t", 0, 1),						                                \
  TEST("mce test 8", "\\s", 0, "\n", 0, 1),						                                \
  TEST("mce test 9", "\\s", 0, "\r", 0, 1),						                                \
  TEST("mce test 10", "\\s", 0, "x", 0, 0),						                                \
											                                \
  TEST("mce test 11", "\\S", 0, " ", 0, 0),						                                \
  TEST("mce test 12", "\\S", 0, "\t", 0, 0),						                                \
  TEST("mce test 13", "\\S", 0, "\n", 0, 0),						                                \
  TEST("mce test 14", "\\S", 0, "\r", 0, 0),						                                \
  TEST("mce test 15", "\\S", 0, "x", 0, 1),						                                \
											                                \
											                                \
  TEST("mce test 16", "\\i", 0, "a", 0, 1),						                                \
  TEST("mce test 17", "\\i", 0, "A", 0, 1),						                                \
  TEST("mce test 18", "\\i", 0, "\\u4e00", 0, 1),					                                \
  TEST("mce test 19", "\\i", 0, ":", 0, 1),						                                \
  TEST("mce test 20", "\\i", 0, "_", 0, 1),						                                \
  TEST("mce test 21", "\\i", 0, "2", 0, 0),						                                \
  TEST("mce test 22", "\\i", 0, ".", 0, 0),						                                \
  TEST("mce test 23", "\\i", 0, "\\u0300", 0, 0),					                                \
  TEST("mce test 24", "\\i", 0, " ", 0, 0),						                                \
  TEST("mce test 25", "\\i", 0, "\n", 0, 0),						                                \
											                                \
  TEST("mce test 26", "\\I", 0, "a", 0, 0),						                                \
  TEST("mce test 27", "\\I", 0, "A", 0, 0),						                                \
  TEST("mce test 28", "\\I", 0, "\\u4e00", 0, 0),					                                \
  TEST("mce test 29", "\\I", 0, ":", 0, 0),						                                \
  TEST("mce test 30", "\\I", 0, "_", 0, 0),						                                \
  TEST("mce test 31", "\\I", 0, "2", 0, 1),						                                \
  TEST("mce test 32", "\\I", 0, ".", 0, 1),						                                \
  TEST("mce test 33", "\\I", 0, "\\u0300", 0, 1),					                                \
  TEST("mce test 34", "\\I", 0, " ", 0, 1),						                                \
  TEST("mce test 35", "\\I", 0, "\n", 0, 1),						                                \
											                                \
  TEST("mce test 36", "\\c", 0, "a", 0, 1),						                                \
  TEST("mce test 37", "\\c", 0, "A", 0, 1),						                                \
  TEST("mce test 38", "\\c", 0, "\\u4e00", 0, 1),					                                \
  TEST("mce test 39", "\\c", 0, ":", 0, 1),						                                \
  TEST("mce test 40", "\\c", 0, "_", 0, 1),						                                \
  TEST("mce test 41", "\\c", 0, "2", 0, 1),						                                \
  TEST("mce test 42", "\\c", 0, ".", 0, 1),						                                \
  TEST("mce test 43", "\\c", 0, "\\u0300", 0, 1),					                                \
  TEST("mce test 44", "\\c", 0, " ", 0, 0),						                                \
  TEST("mce test 45", "\\c", 0, "\n", 0, 0),						                                \
											                                \
  TEST("mce test 46", "\\C", 0, "a", 0, 0),						                                \
  TEST("mce test 47", "\\C", 0, "A", 0, 0),						                                \
  TEST("mce test 48", "\\C", 0, "\\u4e00", 0, 0),					                                \
  TEST("mce test 49", "\\C", 0, ":", 0, 0),						                                \
  TEST("mce test 50", "\\C", 0, "_", 0, 0),						                                \
  TEST("mce test 51", "\\C", 0, "2", 0, 0),						                                \
  TEST("mce test 52", "\\C", 0, ".", 0, 0),						                                \
  TEST("mce test 53", "\\C", 0, "\\u0300", 0, 0),					                                \
  TEST("mce test 54", "\\C", 0, " ", 0, 1),						                                \
  TEST("mce test 55", "\\C", 0, "\n", 0, 1),						                                \
											                                \
  TEST("mce test 56", "\\d", 0, "0", 0, 1),						                                \
  TEST("mce test 57", "\\d", 0, "9", 0, 1),						                                \
  TEST("mce test 58", "\\d", 0, "\\u0660", 0, 1),					                                \
  TEST("mce test 59", "\\d", 0, "\\u0966", 0, 1),					                                \
  TEST("mce test 60", "\\d", 0, "x", 0, 0),						                                \
											                                \
  TEST("mce test 61", "\\D", 0, "0", 0, 0),						                                \
  TEST("mce test 62", "\\D", 0, "9", 0, 0),						                                \
  TEST("mce test 63", "\\D", 0, "\\u0660", 0, 0),					                                \
  TEST("mce test 64", "\\D", 0, "\\u0966", 0, 0),					                                \
  TEST("mce test 65", "\\D", 0, "x", 0, 1),						                                \
											                                \
  TEST("mce test 66", "\\w", 0, "a", 0, 1),						                                \
  TEST("mce test 67", "\\w", 0, "\\u2200", 0, 1),					                                \
  TEST("mce test 68", "\\w", 0, "\\u4e00", 0, 1),					                                \
  TEST("mce test 69", "\\w", 0, " ", 0, 0),						                                \
  TEST("mce test 70", "\\w", 0, "\t", 0, 0),						                                \
											                                \
  TEST("mce test 71", "\\W", 0, "a", 0, 0),						                                \
  TEST("mce test 72", "\\W", 0, "\\u2200", 0, 0),					                                \
  TEST("mce test 73", "\\W", 0, "\\u4e00", 0, 0),					                                \
  TEST("mce test 74", "\\W", 0, " ", 0, 1),						                                \
  TEST("mce test 75", "\\W", 0, "\t", 0, 1),						                                \
											                                \
											                                \
  /* 		character class expression */						                                \
  TEST("class test 0", "[ABC]", 0, "A", 0, 1),						                                \
  TEST("class test 1", "[a" RANGE "z]", 0, "m", 0, 1),					                                \
  TEST("class test 2", "[a" RANGE "z]", 0, "z", 0, 1),					                                \
  TEST("class test 3", "[012]", 0, "1", 0, 1),						                                \
  TEST("class test 4", "[7" RANGE "9]", 0, "8", 0, 1),					                                \
  TEST("class test 5", "[\\p{Sm}]", 0, "\\u2200", 0, 1),				                                \
  TEST("class test 6", "[\\p{isGreek}]", 0, "\\u0370", 0, 1),				                                \
  TEST("class test 7", "[\\p{isGreek}]", 0, "\\u03ff", 0, 1),				                                \
  TEST("class test 8", "[\\p{isGreek}]", 0, "\\u036f", 0, 0),				                                \
  TEST("class test 9", "[\\p{isGreek}]", 0, "\\u0400", 0, 0),				                                \
											                                \
  TEST("class test 10", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "A", 0, 1),				\
  TEST("class test 11", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "m", 0, 1),				\
  TEST("class test 12", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "1", 0, 1),				\
  TEST("class test 13", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "8", 0, 1),				\
  TEST("class test 14", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "\\u2200", 0, 1),			\
  TEST("class test 15", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "\\u0370", 0, 1),			\
  TEST("class test 16", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "\\u0400", 0, 0),			\
  TEST("class test 17", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "M", 0, 0),				\
															\
  TEST("class test 18", "[^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "A", 0, 0),				\
  TEST("class test 19", "[^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "z", 0, 0),				\
  TEST("class test 20", "[^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "m", 0, 0),				\
  TEST("class test 21", "[^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "1", 0, 0),				\
  TEST("class test 22", "[^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "8", 0, 0),				\
  TEST("class test 23", "[^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "\\u2200", 0, 0),			\
  TEST("class test 24", "[^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "\\u0370", 0, 0),			\
  TEST("class test 25", "[^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "\\u0400", 0, 1),			\
  TEST("class test 26", "[^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}]", 0, "M", 0, 1),				\
															\
  TEST("class test 27", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}" MINUS "[aeiou8" RANGE "9]]", 0, "A", 0, 1),	\
  TEST("class test 28", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}" MINUS "[aeiou8" RANGE "9]]", 0, "e", 0, 0),	\
  TEST("class test 29", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}" MINUS "[aeiou8" RANGE "9]]", 0, "1", 0, 1),	\
  TEST("class test 30", "[ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}" MINUS "[aeiou8" RANGE "9]]", 0, "8", 0, 0),	\
															\
  /* simple concatenation */												\
  TEST("concat test 187", "abcd", 0, "abcd", 0, 1),									\
  TEST("concat test 188", "abcd", 0, "abcde", 0, 0),									\
  TEST("concat test 189", "abcd", 0, "abc", 0, 0),									\
															\
  /* parenthesized subexpressions */											\
  TEST("paren test 0", "(\\n)", 0, "\n", 0, 1),										\
  TEST("paren test 1", "(\\t)", 0, "\t", 0, 1),										\
  TEST("paren test 2", "(\\.)", 0, ".", 0, 1),										\
  TEST("paren test 3", "(\\-)", 0, "-", 0, 1),										\
  TEST("paren test 4", "(\\?)", 0, "?", 0, 1),										\
  TEST("paren test 5", "(\\+)", 0, "+", 0, 1),										\
  TEST("paren test 6", "(\\})", 0, "}", 0, 1),										\
  TEST("paren test 7", "(\\))", 0, ")", 0, 1),										\
  TEST("paren test 8", "(\\a)", rx_xml_recomp_BOGUS_CHAR, "a", 0, 0),							\
  TEST("paren test 9", "(.)", 0, "b", 0, 1),										\
  TEST("paren test 10", "(.)", 0, "\r", 0, 0),										\
  TEST("paren test 11", "(.)", 0, "\\u2200", 0, 1),									\
  TEST("paren test 12", "(\\s)", 0, "\t", 0, 1),									\
  TEST("paren test 13", "(\\s)", 0, "\r", 0, 1),									\
  TEST("paren test 14", "(\\S)", 0, " ", 0, 0),										\
  TEST("paren test 15", "(\\S)", 0, "\n", 0, 0),									\
  TEST("paren test 16", "(\\S)", 0, "x", 0, 1),										\
  TEST("paren test 17", "(\\i)", 0, "A", 0, 1),										\
  TEST("paren test 18", "(\\i)", 0, ":", 0, 1),										\
  TEST("paren test 19", "(\\i)", 0, "2", 0, 0),										\
  TEST("paren test 20", "(\\i)", 0, "\\u0300", 0, 0),									\
  TEST("paren test 21", "(\\i)", 0, "\n", 0, 0),									\
  TEST("paren test 22", "(\\I)", 0, "A", 0, 0),										\
  TEST("paren test 23", "(\\I)", 0, ":", 0, 0),										\
  TEST("paren test 24", "(\\I)", 0, "2", 0, 1),										\
  TEST("paren test 25", "(\\I)", 0, "\\u0300", 0, 1),									\
  TEST("paren test 26", "(\\I)", 0, "\n", 0, 1),									\
  TEST("paren test 27", "(\\c)", 0, "A", 0, 1),										\
  TEST("paren test 28", "(\\c)", 0, ":", 0, 1),										\
  TEST("paren test 29", "(\\c)", 0, "2", 0, 1),										\
  TEST("paren test 30", "(\\c)", 0, "\\u0300", 0, 1),									\
  TEST("paren test 31", "(\\c)", 0, "\n", 0, 0),									\
  TEST("paren test 32", "(\\C)", 0, "A", 0, 0),										\
  TEST("paren test 33", "(\\C)", 0, ":", 0, 0),										\
  TEST("paren test 34", "(\\C)", 0, "2", 0, 0),										\
  TEST("paren test 35", "(\\C)", 0, "\\u0300", 0, 0),									\
  TEST("paren test 36", "(\\C)", 0, "\n", 0, 1),									\
  TEST("paren test 37", "(\\d)", 0, "9", 0, 1),										\
  TEST("paren test 38", "(\\d)", 0, "\\u0966", 0, 1),									\
  TEST("paren test 39", "(\\D)", 0, "0", 0, 0),										\
  TEST("paren test 40", "(\\D)", 0, "\\u0660", 0, 0),									\
  TEST("paren test 41", "(\\D)", 0, "x", 0, 1),										\
  TEST("paren test 42", "(\\w)", 0, "\\u2200", 0, 1),									\
  TEST("paren test 43", "(\\w)", 0, " ", 0, 0),										\
  TEST("paren test 44", "(\\W)", 0, "a", 0, 0),										\
  TEST("paren test 45", "(\\W)", 0, "\\u4e00", 0, 0),									\
  TEST("paren test 46", "(\\W)", 0, "\t", 0, 1),									\
  TEST("paren test 47", "([a" RANGE "z])", 0, "m", 0, 1),								\
  TEST("paren test 48", "([012])", 0, "1", 0, 1),									\
  TEST("paren test 49", "([\\p{Sm}])", 0, "\\u2200", 0, 1),								\
  TEST("paren test 50", "([\\p{isGreek}])", 0, "\\u03ff", 0, 1),							\
  TEST("paren test 51", "([\\p{isGreek}])", 0, "\\u0400", 0, 0),							\
  TEST("paren test 52", "([ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}])", 0, "m", 0, 1),				\
  TEST("paren test 53", "([ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}])", 0, "8", 0, 1),				\
  TEST("paren test 54", "([ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}])", 0, "\\u0370", 0, 1),			\
  TEST("paren test 55", "([ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}])", 0, "M", 0, 0),				\
  TEST("paren test 56", "([^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}])", 0, "z", 0, 0),				\
  TEST("paren test 57", "([^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}])", 0, "1", 0, 0),				\
  TEST("paren test 58", "([^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}])", 0, "\\u2200", 0, 0),			\
  TEST("paren test 59", "([^ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}])", 0, "\\u0400", 0, 1),			\
  TEST("paren test 60", "([ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}" MINUS "[aeiou8" RANGE "9]])", 0, "A", 0, 1),\
  TEST("paren test 61", "([ABCa" RANGE "z0127" RANGE "9\\p{Sm}\\p{isGreek}" MINUS "[aeiou8" RANGE "9]])", 0, "1", 0, 1),\
															\
  TEST("paren test 62", "(abcd)", 0, "abcd", 0, 1),									\
  TEST("paren test 63", "(abcd)", 0, "abcde", 0, 0),									\
  TEST("paren test 64", "(abcd)", 0, "abc", 0, 0),									\
															\
															\
															\
  /* 			PIECE TESTS 											\
   */															\
															\
  TEST("piece test 0", "x?", 0, "", 0, 1),										\
  TEST("piece test 1", "x?", 0, "x", 0, 1),										\
  TEST("piece test 2", "x?", 0, "y", 0, 0),										\
  TEST("piece test 3", "abcx?", 0, "abc", 0, 1),									\
  TEST("piece test 4", "abcx?", 0, "abcx", 0, 1),									\
  TEST("piece test 5", "abcx?", 0, "abcy", 0, 0),									\
															\
  TEST("piece test 6", "x*", 0, "", 0, 1),										\
  TEST("piece test 7", "x*", 0, "x", 0, 1),										\
  TEST("piece test 8", "x*", 0, "xx", 0, 1),										\
  TEST("piece test 9", "x*", 0, "xxx", 0, 1),										\
  TEST("piece test 10", "x*", 0, "y", 0, 0),										\
  TEST("piece test 11", "x*", 0, "xxy", 0, 0),										\
															\
  TEST("piece test 12", "abcx*", 0, "abc", 0, 1),									\
  TEST("piece test 13", "abcx*", 0, "abcx", 0, 1),									\
  TEST("piece test 14", "abcx*", 0, "abcxx", 0, 1),									\
  TEST("piece test 15", "abcx*", 0, "abcxxx", 0, 1),									\
  TEST("piece test 16", "abcx*", 0, "abcy", 0, 0),									\
  TEST("piece test 17", "abcx*", 0, "abcxxy", 0, 0),									\
															\
															\
  TEST("piece test 18", "x+", 0, "", 0, 0),										\
  TEST("piece test 19", "x+", 0, "x", 0, 1),										\
  TEST("piece test 20", "x+", 0, "xx", 0, 1),										\
  TEST("piece test 21", "x+", 0, "xxx", 0, 1),										\
  TEST("piece test 22", "x+", 0, "y", 0, 0),										\
  TEST("piece test 23", "x+", 0, "xxy", 0, 0),										\
															\
  TEST("piece test 24", "abcx+", 0, "abc", 0, 0),									\
  TEST("piece test 25", "abcx+", 0, "abcx", 0, 1),									\
  TEST("piece test 26", "abcx+", 0, "abcxx", 0, 1),									\
  TEST("piece test 27", "abcx+", 0, "abcxxx", 0, 1),									\
  TEST("piece test 28", "abcx+", 0, "abcy", 0, 0),									\
  TEST("piece test 29", "abcx+", 0, "abcxxy", 0, 0),									\
															\
  TEST("piece test 30", "x{0,0}", 0, "", 0, 1),										\
  TEST("piece test 31", "x{0,0}", 0, "x", 0, 0),									\
  TEST("piece test 32", "x{0,1}", 0, "", 0, 1),										\
  TEST("piece test 33", "x{0,1}", 0, "x", 0, 1),									\
  TEST("piece test 34", "x{0,1}", 0, "xx", 0, 0),									\
  TEST("piece test 35", "x{0,3}", 0, "", 0, 1),										\
  TEST("piece test 36", "x{0,3}", 0, "x", 0, 1),									\
  TEST("piece test 37", "x{0,3}", 0, "xx", 0, 1),									\
  TEST("piece test 38", "x{0,3}", 0, "xxx", 0, 1),									\
  TEST("piece test 39", "x{0,3}", 0, "xxxx", 0, 0),									\
															\
  TEST("piece test 40", "x{1,0}", rx_xml_recomp_BAD_DUPLICATION_RANGE, "", 0, 1),						\
  TEST("piece test 41", "x{1,0}", rx_xml_recomp_BAD_DUPLICATION_RANGE, "x", 0, 0),						\
  TEST("piece test 42", "x{1,1}", 0, "", 0, 0),										\
  TEST("piece test 43", "x{1,1}", 0, "x", 0, 1),									\
  TEST("piece test 44", "x{1,1}", 0, "xx", 0, 0),									\
  TEST("piece test 45", "x{1,3}", 0, "", 0, 0),										\
  TEST("piece test 46", "x{1,3}", 0, "x", 0, 1),									\
  TEST("piece test 47", "x{1,3}", 0, "xx", 0, 1),									\
  TEST("piece test 48", "x{1,3}", 0, "xxx", 0, 1),									\
  TEST("piece test 49", "x{1,3}", 0, "xxxx", 0, 0),									\
															\
															\
  TEST("piece test 50", "abcx{0,0}lmn", 0, "abclmn", 0, 1),								\
  TEST("piece test 51", "abcx{0,0}lmn", 0, "abcxlmn", 0, 0),								\
  TEST("piece test 52", "abcx{0,1}lmn", 0, "abclmn", 0, 1),								\
  TEST("piece test 53", "abcx{0,1}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 54", "abcx{0,1}lmn", 0, "abcxxlmn", 0, 0),								\
  TEST("piece test 55", "abcx{0,3}lmn", 0, "abclmn", 0, 1),								\
  TEST("piece test 56", "abcx{0,3}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 57", "abcx{0,3}lmn", 0, "abcxxlmn", 0, 1),								\
  TEST("piece test 58", "abcx{0,3}lmn", 0, "abcxxxlmn", 0, 1),								\
  TEST("piece test 59", "abcx{0,3}lmn", 0, "abcxxxxlmn", 0, 0),								\
															\
  TEST("piece test 60", "abcx{1,0}lmn", rx_xml_recomp_BAD_DUPLICATION_RANGE, "abclmn", 0, 1),				\
  TEST("piece test 61", "abcx{1,0}lmn", rx_xml_recomp_BAD_DUPLICATION_RANGE, "abcxlmn", 0, 0),				\
  TEST("piece test 62", "abcx{1,1}lmn", 0, "abclmn", 0, 0),								\
  TEST("piece test 63", "abcx{1,1}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 64", "abcx{1,1}lmn", 0, "abcxxlmn", 0, 0),								\
  TEST("piece test 65", "abcx{1,3}lmn", 0, "abclmn", 0, 0),								\
  TEST("piece test 66", "abcx{1,3}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 67", "abcx{1,3}lmn", 0, "abcxxlmn", 0, 1),								\
  TEST("piece test 68", "abcx{1,3}lmn", 0, "abcxxxlmn", 0, 1),								\
  TEST("piece test 69", "abcx{1,3}lmn", 0, "abcxxxxlmn", 0, 0),								\
															\
  TEST("piece test 70", "abcx{0,}lmn", 0, "abclmn", 0, 1),								\
  TEST("piece test 71", "abcx{0,}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 72", "abcx{0,}lmn", 0, "abclmn", 0, 1),								\
  TEST("piece test 73", "abcx{0,}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 74", "abcx{0,}lmn", 0, "abcxxlmn", 0, 1),								\
  TEST("piece test 75", "abcx{0,}lmn", 0, "abclmn", 0, 1),								\
  TEST("piece test 76", "abcx{0,}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 77", "abcx{0,}lmn", 0, "abcxxlmn", 0, 1),								\
  TEST("piece test 78", "abcx{0,}lmn", 0, "abcxxxlmn", 0, 1),								\
  TEST("piece test 79", "abcx{0,}lmn", 0, "abcxxxxlmn", 0, 1),								\
  TEST("piece test 80", "abcx{0,}lmn", 0, "abcxxxxxxxxxxxxxxlmn", 0, 1),						\
															\
  TEST("piece test 81", "abcx{1,}lmn", 0, "abclmn", 0, 0),								\
  TEST("piece test 82", "abcx{1,}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 83", "abcx{1,}lmn", 0, "abclmn", 0, 0),								\
  TEST("piece test 84", "abcx{1,}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 85", "abcx{1,}lmn", 0, "abcxxlmn", 0, 1),								\
  TEST("piece test 86", "abcx{1,}lmn", 0, "abclmn", 0, 0),								\
  TEST("piece test 87", "abcx{1,}lmn", 0, "abcxlmn", 0, 1),								\
  TEST("piece test 88", "abcx{1,}lmn", 0, "abcxxlmn", 0, 1),								\
  TEST("piece test 89", "abcx{1,}lmn", 0, "abcxxxlmn", 0, 1),								\
  TEST("piece test 90", "abcx{1,}lmn", 0, "abcxxxxlmn", 0, 1),								\
  TEST("piece test 91", "abcx{1,}lmn", 0, "abcxxxxxxxxxxxxxxlmn", 0, 1),						\
															\
  TEST("piece test 92", "\\p{Ll}{4}-\\p{Nd}{5}", 0, "lmnop-09876", 0, 0),						\
  TEST("piece test 93", "\\p{Ll}{4}-\\p{Nd}{5}", 0, "\\u03ac\\u03ad\\u03ae\\u03af-12345", 0, 1),			\
  TEST("piece test 94", "\\p{Ll}{4}-\\p{Nd}{5}", 0, "\\u0255\\u0256\\u0257\\u0258-67890", 0, 1),			\
  TEST("piece test 95", "\\p{Ll}{4}-\\p{Nd}{5}", 0, "\\u0561\\u0562\\u0563\\u0564-24680", 0, 1),			\
  TEST("piece test 96", "\\p{Ll}{4}-\\p{Nd}{5}", 0, "abcd-13579", 0, 1),						\
  TEST("piece test 97", "\\p{Ll}{4}-\\p{Nd}{5}", 0, "\\u00e0\\u00e1\\u00e2\\u00e3-\\u0966\\u0967\\u0968\\u0969\\u096a", 0, 1), \
															\
  /*			BRANCH TESTS											\
   */															\
															\
  TEST("alternative test 0", "(wee|week)(night|knights)", 0, "weeknights", 0, 1),					\
  TEST("alternative test 1", "(week|wee)(night|knights)", 0, "weeknights", 0, 1),					\
  TEST("alternative test 2", "(wee|week)(knights|night)", 0, "weeknights", 0, 1),					\
  TEST("alternative test 3", "(week|wee)(knights|night)", 0, "weeknights", 0, 1),					\
															\
															\
															\
  TEST("tutorial example 0", "Chapter \\d", 0, "Chapter 0", 0, 1),							\
  TEST("tutorial example 1", "Chapter \\d", 0, "Chapter 1", 0, 1),							\
  TEST("tutorial example 2", "Chapter \\d", 0, "Chapter 9", 0, 1),							\
  TEST("tutorial example 3", "Chapter \\d", 0, "Chapter x", 0, 0),							\
  TEST("tutorial example 4", "Chapter \\d", 0, "Chapter 19", 0, 0),							\
  TEST("tutorial example 5", "Chapter \\d", 0, "Chapter ", 0, 0),							\
															\
															\
  TEST("tutorial example 6", "Chapter\\s\\d", 0, "Chapter 0", 0, 1),							\
  TEST("tutorial example 7", "Chapter\\s\\d", 0, "Chapter.0", 0, 0),							\
  TEST("tutorial example 8", "Chapter\\s\\d", 0, "Chapter	1", 0, 1),						\
  TEST("tutorial example 9", "Chapter\\s\\d", 0, "Chapter\n9", 0, 1),							\
  TEST("tutorial example 10", "Chapter\\s\\d", 0, "Chapter\rx", 0, 0),							\
  TEST("tutorial example 11", "Chapter\\s\\d", 0, "Chapter 19", 0, 0),							\
  TEST("tutorial example 12", "Chapter\\s\\d", 0, "Chapter ", 0, 0),							\
															\
  /* complex expressions */												\
															\
  TEST("complex 0", "((\\p{Ll}{4}-\\p{Nd}*)|([\\p{isGreek}]+))?", 0, "", 0, 1),						\
  TEST("complex 1", "((\\p{Ll}{4}-\\p{Nd}*)|([\\p{isGreek}]+))?", 0, "abcd-1234", 0, 1),				\
  TEST("complex 2", "((\\p{Ll}{4}-\\p{Nd}*)|([\\p{isGreek}]+))?", 0, "abcdx-1234", 0, 0),				\
  TEST("complex 3", "((\\p{Ll}{4}-\\p{Nd}*)|([\\p{isGreek}]+))?", 0, "\\u03ac\\u03ad\\u03ae\\u03af", 0, 1),		\
  TEST("complex 4", "((\\p{Ll}{4}-\\p{Nd}*)|([\\p{isGreek}]+))?", 0, "\\u03ac\\u03ad\\u03ae\\u03af-", 0, 1),		\
  TEST("complex 5", "((\\p{Ll}{4}-\\p{Nd}*)|([\\p{isGreek}]+))?", 0, "\\u03ac\\u03ad\\u03ae\\u03af-234", 0, 1),		\
  TEST("complex 6", "((\\p{Ll}{4}-\\p{Nd}*)|([\\p{isGreek}]+))?", 0, "\\u03acx\\u03ae\\u03af-234", 0, 1),		\
															\
  /* invalid expressions */												\
  TEST("error 0", "a{1x}", rx_xml_recomp_MISSING_COMMA, 0, 0, 0),							\
  TEST("error 1", "a{1,", rx_xml_recomp_MISSING_BRACE, 0, 0, 0),							\
  TEST("error 2", "a{3,2}", rx_xml_recomp_BAD_DUPLICATION_RANGE, 0, 0, 0),						\
  TEST("error 3", "(a", rx_xml_recomp_MISSING_RPAREN, 0, 0, 0),								\
  TEST("error 4", "[abc", rx_xml_recomp_BOGUS_CHARACTER_CLASS, 0, 0, 0),						\
  TEST("error 5", "\\px", rx_xml_recomp_BOGUS_CATEGORY_ESCAPE, 0, 0, 0),						\
  TEST("error 6", "\\P", rx_xml_recomp_BOGUS_CATEGORY_ESCAPE, 0, 0, 0),							\
  TEST("error 7", "[z" RANGE "a]", rx_xml_recomp_BOGUS_CHARACTER_CLASS, 0, 0, 0),					\
  TEST("error 8", "[z" RANGE "]", rx_xml_recomp_BOGUS_CHARACTER_CLASS, 0, 0, 0),					\
  TEST(0, 0, 0, 0, 0, 0)



struct xml_re_test_case xml_re_test_cases[] =
{
#define RANGE	"-"
#define MINUS   "-"

   TEST_LIST
};

struct xml_re_test_case xml_re_test_cases_alternative_syntax[] =
{
#undef RANGE
#undef MINUS
#define RANGE	".."
#define MINUS   "^"

   TEST_LIST
};

