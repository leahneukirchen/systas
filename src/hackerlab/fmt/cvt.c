/* cvt.c - converting integers to and from strings
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/os/errno.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/str.h"
#include "hackerlab/fmt/cvt.h"


/************************************************************************
 *(h1 "Converting Integers to and from Strings"
 *    :include ("hackerlab/fmt/cvt.h"))
 *
 * The functions in this chapter convert between integer types and
 * strings.
 */
/*(menu)
 */

/************************************************************************
 *(h2 "Converting Integers to Strings")
 * 
 */

/*(c cvt_ulong_to_decimal)
 * void cvt_ulong_to_decimal (t_uchar * nbuf, unsigned long n);
 * 
 * Convert `n' to a 0-terminated decimal number.
 */
void
cvt_ulong_to_decimal (t_uchar * nbuf, unsigned long n)
{
  int x;
  int y;

  if (!n)
    {
      nbuf[0] = '0';
      nbuf[1] = 0;
      return;
    }

  x = 0;
  while (n)
    {
      nbuf[x] = '0' + n % 10;
      n /= 10;
      x += 1;
    }
  nbuf[x] = 0;
  y = 0;
  --x;
  while (x > y)
    {
      int c;
      c = nbuf[x];
      nbuf[x] = nbuf[y];
      nbuf[y] = c;
      --x;
      ++y;
    }
}


/*(c cvt_long_to_decimal)
 * void cvt_long_to_decimal (t_uchar * nbuf, long n);
 * 
 * Convert `n' to a 0-terminated decimal number.
 */
void
cvt_long_to_decimal (t_uchar * nbuf, long n)
{
  if (n < 0)
    {
      nbuf[0] = '-';
      cvt_ulong_to_decimal (nbuf + 1, -n);
    }
  else
    cvt_ulong_to_decimal (nbuf, n);
}


/*(c cvt_ulong_to_octal)
 * void cvt_ulong_to_octal (t_uchar * nbuf, unsigned long n);
 * 
 * Convert `n' to a 0-terminated octal number.
 */
void
cvt_ulong_to_octal (t_uchar * nbuf, unsigned long n)
{
  int x;
  int y;

  if (!n)
    {
      nbuf[0] = '0';
      nbuf[1] = 0;
      return;
    }

  x = 0;
  while (n)
    {
      nbuf[x] = '0' + n % 8;
      n /= 8;
      x += 1;
    }
  nbuf[x] = 0;
  y = 0;
  --x;
  while (x > y)
    {
      int c;
      c = nbuf[x];
      nbuf[x] = nbuf[y];
      nbuf[y] = c;
      --x;
      ++y;
    }
}


/*(c cvt_long_to_octal)
 * void cvt_long_to_octal (t_uchar * nbuf, long n);
 * 
 * Convert `n' to a 0-terminated octal number.
 */
void
cvt_long_to_octal (t_uchar * nbuf, long n)
{
  if (n < 0)
    {
      nbuf[0] = '-';
      cvt_ulong_to_octal (nbuf + 1, -n);
    }
  else
    cvt_ulong_to_octal (nbuf, n);
}


/*(c cvt_ulong_to_HEX)
 * void cvt_ulong_to_HEX (t_uchar * nbuf, unsigned long n);
 * 
 * Convert `n' to a 0-terminated hexadecimal number using upper-case
 * hex digits "A..F".
 */
void
cvt_ulong_to_HEX (t_uchar * nbuf, unsigned long n)
{
  static const t_uchar * HEX = "0123456789ABCDEF";
  int x;
  int y;

  if (!n)
    {
      nbuf[0] = '0';
      nbuf[1] = 0;
      return;
    }

  x = 0;
  while (n)
    {
      nbuf[x] = HEX[n % 16];
      n /= 16;
      x += 1;
    }
  nbuf[x] = 0;
  y = 0;
  --x;
  while (x > y)
    {
      int c;
      c = nbuf[x];
      nbuf[x] = nbuf[y];
      nbuf[y] = c;
      --x;
      ++y;
    }
}


/*(c cvt_long_to_HEX)
 * void cvt_long_to_HEX (t_uchar * nbuf, long n);
 * 
 * Convert `n' to a 0-terminated hexadecimal number using upper-case
 * hex digits "A..F".
 */
void
cvt_long_to_HEX (t_uchar * nbuf, long n)
{
  if (n < 0)
    {
      nbuf[0] = '-';
      cvt_ulong_to_HEX (nbuf + 1, -n);
    }
  else
    cvt_ulong_to_HEX (nbuf, n);
}


/*(c cvt_ulong_to_hex)
 * void cvt_ulong_to_hex (t_uchar * nbuf, unsigned long n);
 * 
 * Convert `n' to a 0-terminated hexadecimal number using lower-case
 * hex digits "a..f".
 */
void
cvt_ulong_to_hex (t_uchar * nbuf, unsigned long n)
{
  static const t_uchar * hex = "0123456789abcdef";
  int x;
  int y;

  if (!n)
    {
      nbuf[0] = '0';
      nbuf[1] = 0;
      return;
    }

  x = 0;
  while (n)
    {
      nbuf[x] = hex[n % 16];
      n /= 16;
      x += 1;
    }
  nbuf[x] = 0;
  y = 0;
  --x;
  while (x > y)
    {
      int c;
      c = nbuf[x];
      nbuf[x] = nbuf[y];
      nbuf[y] = c;
      --x;
      ++y;
    }
}


/*(c cvt_long_to_hex)
 * void cvt_long_to_hex (t_uchar * nbuf, long n);
 * 
 * Convert `n' to a 0-terminated hexadecimal number using lower-case
 * hex digits "a..f".
 */
void
cvt_long_to_hex (t_uchar * nbuf, long n)
{
  if (n < 0)
    {
      nbuf[0] = '-';
      cvt_ulong_to_hex (nbuf + 1, -n);
    }
  else
    cvt_ulong_to_hex (nbuf, n);
}

/************************************************************************
 *(h2 "Converting Decimal Strings to Integers")
 * 
 */


/*(c cvt_decimal_to_ulong)
 * int cvt_decimal_to_ulong (int * errn,
 *			     unsigned long * answerp,
 *			     const t_uchar * text,
 *			     size_t len);
 * 
 * Convert the decimal number `text' to an unsigned long integer.
 *
 * Return 0 upon success, -1 on error.
 * 
 * On error, `*errn' may be:
 *
 *	ERANGE	the decimal number will not fit in an `unsigned long'.
 *	EINVAL	`text' is not a valid decimal number.
 */
int
cvt_decimal_to_ulong (int * errn, unsigned long * answerp, const t_uchar * text, size_t len)
{
  unsigned long answer;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '+'))
    {
      ++text;
      --len;
    }

  while (len)
    if (!char_is_digit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	unsigned long next;
	digit = *text - '0';
	next = answer * 10 + digit;
	if (answer && (next <= answer))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/*(c cvt_decimal_to_uint)
 * int cvt_decimal_to_uint (int * errn,
 *			    unsigned int * answerp,
 *			    const t_uchar * text,
 *			    size_t len);
 * 
 * Convert the decimal number `text' to an unsigned integer.
 *
 * Return 0 upon success, -1 on error.
 * 
 * On error, `*errn' may be:
 *
 *	ERANGE	the decimal number will not fit in an `unsigned int'.
 *	EINVAL	`text' is not a valid decimal number.
 */
int
cvt_decimal_to_uint (int * errn, unsigned int * answerp, const t_uchar * text, size_t len)
{
  unsigned int answer;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '+'))
    {
      ++text;
      --len;
    }

  while (len)
    if (!char_is_digit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	unsigned int next;
	digit = *text - '0';
	next = answer * 10 + digit;
	if (answer && (next <= answer))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/*(c cvt_decimal_to_long)
 * int cvt_decimal_to_long (int * errn,
 *			    long * answerp,
 *			    const t_uchar * text,
 *			    size_t len);
 * 
 * Convert the decimal number `text' to a long integer.
 *
 * Return 0 upon success, -1 on error.
 * 
 * On error, `*errn' may be:
 *
 *	ERANGE		the decimal number will not fit in a `long'.
 *	EINVAL		`text' is not a valid decimal number.
 */
int
cvt_decimal_to_long (int * errn, long * answerp, const t_uchar * text, size_t len)
{
  long answer;
  int sign;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '-'))
    {
      sign = -1;
      ++text;
      --len;
    }
  else
    {
      sign = 1;
      if (len && (*text == '+'))
	{
	  ++text;
	  --len;
	}
    }

  while (len)
    if (!char_is_digit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	long next;
	digit = *text - '0';
	next = answer * 10 + sign * digit;
	if (   answer
	    && ((sign > 0)
		? (next <= answer)
		: (answer <= next)))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/*(c cvt_decimal_to_int)
 * int cvt_decimal_to_int (int * errn,
 *			   int * answerp,
 *			   const t_uchar * text,
 *			   size_t len);
 * 
 * Convert the decimal number `text' to an integer.
 *
 * Return 0 upon success, -1 on error.
 *
 * On error, `*errn' may be:
 *
 *	ERANGE		the decimal number will not fit in an `int'.
 *	EINVAL		`text' is not a valid decimal number.
 */
int
cvt_decimal_to_int (int * errn, int * answerp, const t_uchar * text, size_t len)
{
  int answer;
  int sign;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '-'))
    {
      sign = -1;
      ++text;
      --len;
    }
  else
    {
      sign = 1;
      if (len && (*text == '+'))
	{
	  ++text;
	  --len;
	}
    }

  while (len)
    if (!char_is_digit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	int next;
	digit = *text - '0';
	next = answer * 10 + sign * digit;
	if (answer &&
	    ((sign > 0)
	     ? (next <= answer)
	     : (answer <= next)))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/************************************************************************
 *(h2 "Converting Hexadecimal Strings to Integers")
 * 
 */


/*(c cvt_hex_to_ulong)
 * int cvt_hex_to_ulong (int * errn,
 *			 unsigned long * answerp,
 *			 const t_uchar * text,
 *			 size_t len);
 * 
 * Convert the hexadecimal number `text' to an unsigned long integer.
 *
 * Return 0 upon success, -1 on error.
 * 
 * On error, `*errn' may be:
 *
 *	ERANGE	the hexadecimal number will not fit in an `unsigned long'.
 *	EINVAL	`text' is not a valid hexadecimal number.
 */
int
cvt_hex_to_ulong (int * errn, unsigned long * answerp, const t_uchar * text, size_t len)
{
  unsigned long answer;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '+'))
    {
      ++text;
      --len;
    }

  while (len)
    if (!char_is_xdigit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	unsigned long next;
	digit = char_digit_value (*text);
	next = answer * 16 + digit;
	if (answer && (next <= answer))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/*(c cvt_hex_to_uint)
 * int cvt_hex_to_uint (int * errn,
 *			unsigned int * answerp,
 *			const t_uchar * text,
 *			size_t len);
 * 
 * Convert the hexadecimal number `text' to an unsigned integer.
 *
 * Return 0 upon success, -1 on error.
 * 
 * On error, `*errn' may be:
 *
 *	ERANGE	the hexadecimal number will not fit in an `unsigned int'.
 *	EINVAL	`text' is not a valid hexadecimal number.
 */
int
cvt_hex_to_uint (int * errn, unsigned int * answerp, const t_uchar * text, size_t len)
{
  unsigned int answer;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '+'))
    {
      ++text;
      --len;
    }

  while (len)
    if (!char_is_xdigit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	unsigned int next;
	digit = char_digit_value (*text);
	next = answer * 16 + digit;
	if (answer && (next <= answer))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/*(c cvt_hex_to_long)
 * int cvt_hex_to_long (int * errn,
 *			long * answerp,
 *			const t_uchar * text,
 *			size_t len);
 * 
 * Convert the hexadecimal number `text' to a long integer.
 *
 * Return 0 upon success, -1 on error.
 * 
 * On error, `*errn' may be:
 *
 *	ERANGE		the hexadecimal number will not fit in a `long'.
 *	EINVAL		`text' is not a valid hexadecimal number.
 */
int
cvt_hex_to_long (int * errn, long * answerp, const t_uchar * text, size_t len)
{
  long answer;
  int sign;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '-'))
    {
      sign = -1;
      ++text;
      --len;
    }
  else
    {
      sign = 1;
      if (len && (*text == '+'))
	{
	  ++text;
	  --len;
	}
    }

  while (len)
    if (!char_is_xdigit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	long next;
	digit = char_digit_value (*text);
	next = answer * 16 + sign * digit;
	if (answer &&
	    ((sign > 0)
	     ? (next <= answer)
	     : (answer <= next)))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/*(c cvt_hex_to_int)
 * int cvt_hex_to_int (int * errn,
 *		       int * answerp,
 *		       const t_uchar * text,
 *		       size_t len);
 * 
 * Convert the hexadecimal number `text' to an integer.
 *
 * Return 0 upon success, -1 on error.
 *
 * On error, `*errn' may be:
 *
 *	ERANGE		the hexadecimal number will not fit in an `int'.
 *	EINVAL		`text' is not a valid hexadecimal number.
 */
int
cvt_hex_to_int (int * errn, int * answerp, const t_uchar * text, size_t len)
{
  int answer;
  int sign;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '-'))
    {
      sign = -1;
      ++text;
      --len;
    }
  else
    {
      sign = 1;
      if (len && (*text == '+'))
	{
	  ++text;
	  --len;
	}
    }

  while (len)
    if (!char_is_xdigit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	int next;
	digit = char_digit_value (*text);
	next = answer * 16 + sign * digit;
	if (answer &&
	    ((sign > 0)
	     ? (next <= answer)
	     : (answer <= next)))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/************************************************************************
 *(h2 "Converting Octal Strings to Integers")
 * 
 */

/*(c cvt_octal_to_ulong)
 * int cvt_octal_to_ulong (int * errn,
 *			   unsigned long * answerp,
 *			   const t_uchar * text,
 *			   size_t len);
 * 
 * Convert the octal number `text' to an unsigned long integer.
 *
 * Return 0 upon success, -1 on error.
 * 
 * On error, `*errn' may be:
 *
 *	ERANGE	the octal number will not fit in an `unsigned long'.
 *	EINVAL	`text' is not a valid octal number.
 */
int
cvt_octal_to_ulong (int * errn,
		    unsigned long * answerp,
		    const t_uchar * text,
		    size_t len)
{
  unsigned long answer;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '+'))
    {
      ++text;
      --len;
    }

  while (len)
    if (!char_is_digit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	unsigned long next;
	digit = *text - '0';
	next = answer * 8 + digit;
	if (answer && (next <= answer))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/*(c cvt_octal_to_uint)
 * int cvt_octal_to_uint (int * errn,
 *			    unsigned int * answerp,
 *			    const t_uchar * text,
 *			    size_t len);
 * 
 * Convert the octal number `text' to an unsigned integer.
 *
 * Return 0 upon success, -1 on error.
 * 
 * On error, `*errn' may be:
 *
 *	ERANGE	the octal number will not fit in an `unsigned int'.
 *	EINVAL	`text' is not a valid octal number.
 */
int
cvt_octal_to_uint (int * errn, unsigned int * answerp, const t_uchar * text, size_t len)
{
  unsigned int answer;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '+'))
    {
      ++text;
      --len;
    }

  while (len)
    if (!char_is_digit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	unsigned int next;
	digit = *text - '0';
	next = answer * 8 + digit;
	if (answer && (next <= answer))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/*(c cvt_octal_to_long)
 * int cvt_octal_to_long (int * errn,
 *			    long * answerp,
 *			    const t_uchar * text,
 *			    size_t len);
 * 
 * Convert the octal number `text' to a long integer.
 *
 * Return 0 upon success, -1 on error.
 * 
 * On error, `*errn' may be:
 *
 *	ERANGE		the octal number will not fit in a `long'.
 *	EINVAL		`text' is not a valid octal number.
 */
int
cvt_octal_to_long (int * errn, long * answerp, const t_uchar * text, size_t len)
{
  long answer;
  int sign;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '-'))
    {
      sign = -1;
      ++text;
      --len;
    }
  else
    {
      sign = 1;
      if (len && (*text == '+'))
	{
	  ++text;
	  --len;
	}
    }

  while (len)
    if (!char_is_digit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	long next;
	digit = *text - '0';
	next = answer * 8 + sign * digit;
	if (answer &&
	    ((sign > 0)
	     ? (next <= answer)
	     : (answer <= next)))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}


/*(c cvt_octal_to_int)
 * int cvt_octal_to_int (int * errn,
 *			   int * answerp,
 *			   const t_uchar * text,
 *			   size_t len);
 * 
 * Convert the octal number `text' to an integer.
 *
 * Return 0 upon success, -1 on error.
 *
 * On error, `*errn' may be:
 *
 *	ERANGE		the octal number will not fit in an `int'.
 *	EINVAL		`text' is not a valid octal number.
 */
int
cvt_octal_to_int (int * errn, int * answerp, const t_uchar * text, size_t len)
{
  int answer;
  int sign;

  answer = 0;
  if (len < 0)
    len = str_length (text);

  if (len == 0)
    {
      *errn = EINVAL;
      return -1;
    }

  if (len && (*text == '-'))
    {
      sign = -1;
      ++text;
      --len;
    }
  else
    {
      sign = 1;
      if (len && (*text == '+'))
	{
	  ++text;
	  --len;
	}
    }

  while (len)
    if (!char_is_digit (*text))
      {
	*errn = EINVAL;
	return -1;
      }
    else
      {
	int digit;
	int next;
	digit = *text - '0';
	next = answer * 8 + sign * digit;
	if (answer &&
	    ((sign > 0)
	     ? (next <= answer)
	     : (answer <= next)))
	  {
	    *errn = ERANGE;
	    return -1;
	  }
	answer = next;
	++text;
	--len;
      }
  *answerp = answer;
  return 0;
}
