/* printfmt-va.c - varargs formatted printing
 *
 ****************************************************************
 * Copyright (C) 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/os/stdarg.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/str.h"
#include "hackerlab/mem/mem.h"
#include "hackerlab/vu/vu.h"
#include "hackerlab/fmt/cvt.h"
#include "hackerlab/vu/printfmt.h"
#include "hackerlab/vu/printfmt-va.h"



/*c
 * int print_padding (int * errn, int fd, int padding_character, int amt);
 * 
 * Print `amt' copies of `padding_character' on descriptor `fd'.
 */
static int
print_padding (int * errn, int fd, int padding_character, int amt)
{
  static char spaces[] = "                ";

  mem_set ((t_uchar *)spaces, padding_character, sizeof (spaces));

  while (amt > sizeof (spaces) - 1)
    {
      if (0 > vu_write_retry (errn, fd, spaces, sizeof (spaces) - 1))
	return -1;
      amt -= sizeof (spaces) - 1;
    }

  if (0 > vu_write_retry (errn, fd, spaces, amt))
    return -1;
  return 0;
}


/*c
 * int print_zeros (int * errn, int fd, int amt);
 * 
 * Print `amt' zeros ("0") on descriptor fd.
 */
static int
print_zeros (int * errn, int fd, int amt)
{
  static char spaces[] = "0000000000000000";

  while (amt > sizeof (spaces) - 1)
    {
      if (0 > vu_write_retry (errn, fd, spaces, sizeof (spaces) - 1))
	return -1;
      amt -= sizeof (spaces) - 1;
    }

  if (0 > vu_write_retry (errn, fd, spaces, amt))
    return -1;
  return 0;
}




int
printfmt_va_list (int * errn, int fd, t_uchar * fmt, va_list ap)
{
  int amt;
  int wrote;
  t_uchar cbuf[2];
  t_uchar nbuf[64];

  wrote = 0;
  while (1)
    {
      switch (*fmt)
	{
	case 0:
	  return wrote;
	default:
	  {
	    t_uchar * end;

	    end = str_chr_index (fmt, '%');
	    if (!end)
	      end = fmt + str_length (fmt);
	    amt = vu_write_retry (errn, fd, fmt, end - fmt);
	    if (amt < 0)
	      return -1;
	    wrote += amt;
	    fmt = end;
	    break;
	  }
	case '%':
	  {
	    ++fmt;
	    if (*fmt == '%')
	      {
		amt = vu_write_retry (errn, fd, fmt, 1);
		if (amt < 0)
		  return -1;
		wrote += 1;
		++fmt;
	      }
	    else
	      {
		int padding_character;
		int leave_blank;
		int alternate_form;
		int left_adjusted;
		int print_sign;
		int field_width;
		int precision;
		int is_short;
		int is_long;
		int is_long_double;
		long arg_long;
		unsigned long arg_ulong;
		t_uchar * arg_str;
		void * arg_ptr;
		int * arg_iptr;
		short * arg_sptr;
		long * arg_lptr;

		padding_character = ' ';
		leave_blank = 0;
		alternate_form = 0;
		left_adjusted = 0;
		print_sign = 0;
		field_width = -1;
		precision = -1;
		is_short = 0;
		is_long = 0;
		is_long_double = 1;
		while (1)
		  {
		    switch (*fmt)
		      {
		      default:
			goto get_conversion_specifier;
		      case '&':
			padding_character = va_arg (ap, int);
			++fmt;
			break;
		      case ' ':
			leave_blank = 1;
			++fmt;
			break;
		      case '#':
			alternate_form = 1;
			++fmt;
			break;
		      case '0':
			padding_character = '0';
			++fmt;
			break;
		      case '-':
			left_adjusted = 1;
			++fmt;
			break;
		      case '+':
			print_sign = 1;
			++fmt;
			break;
		      case '1': case '2': case '3': case '4':
		      case '5': case '6': case '7': case '8': case '9':
			field_width = 0;
			while (char_is_digit (*fmt))
			  {
			    field_width = field_width * 10 + (*fmt - '0');
			    ++fmt;
			  }
			break;
		      case '*':
			field_width = va_arg (ap, int);
			++fmt;
			break;
		      case '.':
			precision = 0;
			++fmt;
			if (*fmt == '*')
			  {
			    precision = va_arg (ap, int);
			    ++fmt;
			    break;
			  }
			while (char_is_digit (*fmt))
			  {
			    precision = precision * 10 + (*fmt - '0');
			    ++fmt;
			  }
			break;
		      case 'h':
			is_short = 1;
			++fmt;
			break;
		      case 'l':
			is_long = 1;
			++fmt;
			break;
		      case 'L':
			is_long_double = 1;
			++fmt;
			break;
		      }
		  }

	      get_conversion_specifier:
		switch (*fmt)
		  {
		  default:
		    *errn = EINVAL;
		    return -1;
		  case 'd':
		    if (!is_long)
		      arg_long = (long)va_arg (ap, int);
		    else
		      arg_long = va_arg (ap, long);
		    if (arg_long >= 0)
		      cvt_ulong_to_decimal (nbuf, (unsigned long)arg_long);
		    else
		      {
			nbuf[0] = '-';
			cvt_ulong_to_decimal (nbuf + 1, (unsigned long)-arg_long);
		      }
		    goto integer_common;
		  case 'o':
		    if (!is_long)
		      arg_ulong = (unsigned long)va_arg (ap, unsigned int);
		    else
		      arg_ulong = va_arg (ap, unsigned long);
		    cvt_ulong_to_octal (nbuf, arg_ulong);
		    if (alternate_form && arg_ulong)
		      {
			mem_move (nbuf + 1, nbuf, str_length (nbuf) + 1);
			nbuf[0] = '0';
		      }
		    goto integer_common;
		  case 'u':
		    if (!is_long)
		      arg_ulong = (unsigned long)va_arg (ap, unsigned int);
		    else
		      arg_ulong = va_arg (ap, unsigned long);
		    cvt_ulong_to_decimal (nbuf, arg_ulong);
		    goto integer_common;
		  case 'X':
		    if (!is_long)
		      arg_ulong = (unsigned long)va_arg (ap, unsigned int);
		    else
		      arg_ulong = va_arg (ap, unsigned long);
		    cvt_ulong_to_HEX (nbuf, arg_ulong);
		    goto integer_common;
		  case 'x':
		    if (!is_long)
		      arg_ulong = (unsigned long)va_arg (ap, unsigned int);
		    else
		      arg_ulong = va_arg (ap, unsigned long);
		  x_common:
		    cvt_ulong_to_hex (nbuf, arg_ulong);
		    if (alternate_form && arg_ulong)
		      {
			mem_move (nbuf + 2, nbuf, str_length (nbuf) + 1);
			nbuf[0] = '0';
			if (*fmt == 'X')
			  nbuf[1] = 'X';
			else
			  nbuf[1] = 'x';
		      }
		    goto integer_common;

		  integer_common:
		    {
		      int len;
		      int padding;
		      int has_sign;
		      int zeros;

		      len = str_length (nbuf);
		      has_sign = (nbuf[0] == '-');
		      zeros = 0;
		      if (precision > (len - has_sign))
			zeros = precision - (len - has_sign);
		      if (print_sign)
			{
			  if (!has_sign)
			    {
			      mem_move (nbuf + 1, nbuf, len + 1);
			      nbuf[0] = '+';
			      has_sign = 1;
			      len += 1;
			    }
			}
		      if (field_width > len + zeros)
			padding = field_width - (len + zeros);
		      else
			padding = 0;
		      if (!left_adjusted && padding)
			{
			  if (0 > print_padding (errn, fd, padding_character, padding))
			    return -1;
			  wrote += padding;
			}
		      if (zeros)
			{
			  if (has_sign)
			    {
			      if (0 > vu_write_retry (errn, fd, nbuf, 1))
				return -1;
			      wrote += 1;
			      mem_move (nbuf, nbuf + 1, len);
			      len -= 1;
			    }
			  if (   (char_to_lower (*fmt) == 'x')
			      && (nbuf[0] == '0')
			      && (char_to_lower (nbuf[1]) == 'x'))
			    {
			      if (0 > vu_write_retry (errn, fd, nbuf, 2))
				return -1;
			      wrote += 2;
			      mem_move (nbuf, nbuf + 2, len - 1);
			      len -= 2;
			    }
			  if (0 > print_zeros (errn, fd, zeros))
			    return -1;
			  wrote += zeros;
			}
		      if (0 > vu_write_retry (errn, fd, nbuf, len))
			return -1;
		      wrote += len;
		      if (left_adjusted && padding)
			{
			  if (0 > print_padding (errn, fd, padding_character, padding))
			    return -1;
			  wrote += padding;
			}
		      break;
		    }
		  case 'e':
		    panic ("floating point formatted output not supported");
		  case 'E':
		    panic ("floating point formatted output not supported");
		  case 'f':
		    panic ("floating point formatted output not supported");
		  case 'g':
		    panic ("floating point formatted output not supported");
		  case 'c':
		    {
		      cbuf[0] = (t_uchar)va_arg (ap, int);
		      cbuf[1] = 0;
		      arg_str = cbuf;
		      goto str_common;
		    }
		  case 's':
		    {
		      int len;
		      int padding;
		  
		      arg_str = va_arg (ap, t_uchar *);
		      if (!arg_str)
			arg_str = "(null)";
		    str_common:
		      if (precision < 0)
			len = str_length (arg_str);
		      else
			{
			  len = 0;
			  while ((len < precision) && arg_str[len])
			    ++len;
			}
		      if (field_width > len)
			padding = field_width - len;
		      else
			padding = 0;

		      if (!left_adjusted && padding)
			{
			  if (0 > print_padding (errn, fd, padding_character, padding))
			    return -1;
			  wrote += padding;
			}
		      if (0 > vu_write_retry (errn, fd, arg_str, len))
			return -1;
		      wrote += len;
		      if (left_adjusted && padding)
			{
			  if (0 > print_padding (errn, fd, padding_character, padding))
			    return -1;
			  wrote += padding;
			}
		      break;
		    }
		  case 'p':
		    arg_ptr = va_arg (ap, void *);
		    arg_ulong = (unsigned long) arg_ptr;
		    alternate_form = 1;
		    goto x_common;
		  case 'n':
		    if (is_short)
		      {
			arg_sptr = va_arg (ap, short *);
			*arg_sptr = wrote;
		      }
		    else if (is_long)
		      {
			arg_lptr = va_arg (ap, long *);
			*arg_lptr = wrote;
		      }
		    else
		      {
			arg_iptr = va_arg (ap, int *);
			*arg_iptr = wrote;
		      }
		    break;
		  }
		++fmt;
	      }
	    break;
	  }
	}
    }
  return 0;
}
