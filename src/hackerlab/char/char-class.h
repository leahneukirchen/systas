/* char-class.h - decls for character classifications
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#ifndef INCLUDE__CHAR__CHAR_CLASS_H
#define INCLUDE__CHAR__CHAR_CLASS_H


#include "hackerlab/machine/types.h"



extern t_uchar char__to_lower_table[];
extern t_uchar char__to_upper_table[];
extern short char__digit_value_table [];

#define char_to_lower(X)	(char__to_lower_table[(t_uchar)(X)])
#define char_to_upper(X)	(char__to_upper_table[(t_uchar)(X)])
#define char_digit_value(X)	(char__digit_value_table[(t_uchar)(X)])

enum char_classes
{
  char_class_upper = (1 << 0),
  char_class_lower = (1 << 1),
  char_class_alpha = (1 << 2),
  char_class_digit = (1 << 3),
  char_class_alnum = (1 << 4),
  char_class_control = (1 << 5),
  char_class_printable = (1 << 6),
  char_class_space = (1 << 7),
  char_class_graph = (1 << 8),
  char_class_c_id = (1 << 9),
  char_class_xdigit = (1 << 10),
  char_class_odigit = (1 << 11),
  char_class_punct = (1 << 12),
  char_class_blank = (1 << 13)
};

extern unsigned short char__class_table[];

#define char_is_ascii(X)	((0 <= (X)) && ((X) < 128))
#define char_is_class(X, C)	((char__class_table[(t_uchar)(X)] & (C)) == (C))
#define char_is_upper(X)	char_is_class ((X), char_class_upper)
#define char_is_lower(X)	char_is_class ((X), char_class_lower)
#define char_is_alpha(X)	char_is_class ((X), char_class_alpha)
#define char_is_digit(X)	char_is_class ((X), char_class_digit)
#define char_is_alnum(X)	char_is_class ((X), char_class_alnum)
#define char_is_control(X)	char_is_class ((X), char_class_control)
#define char_is_printable(X)	char_is_class ((X), char_class_printable)
#define char_is_space(X)	char_is_class ((X), char_class_space)
#define char_is_graph(X)	char_is_class ((X), char_class_graph)
#define char_is_c_id(X)		char_is_class ((X), char_class_c_id)
#define char_is_xdigit(X)	char_is_class ((X), char_class_xdigit)
#define char_is_odigit(X)	char_is_class ((X), char_class_odigit)
#define char_is_punct(X)	char_is_class ((X), char_class_punct)
#define char_is_blank(X)	char_is_class ((X), char_class_blank)

#define char_is_alphanumeric(X) (char_is_alpha((X)) || char_is_digit((X)))

/* automatically generated __STDC__ prototypes */
#endif  /* INCLUDE__CHAR__CHAR_CLASS_H */
