/* uni-str-inlines.c - inline unicode string functions
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#define UNI_CODING_INLINES

#include "hackerlab/machine/types.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/uni/invariant.h"
#include "hackerlab/uni/coding.h"
#include "hackerlab/unicode/uni-str.h"



#ifndef UNI_INLINE_QUALIFIERS
#define UNI_INLINE_QUALIFIERS
#endif

#ifndef __GNUC__
#undef __attribute__
#define __attribute__(X)
#endif



UNI_INLINE_QUALIFIERS uni_string  __attribute__((unused))
uni_str_offset (enum uni_encoding_scheme encoding,
		uni_string string,
		ssize_t x)
{
  return (uni_string)((t_uchar *)string + (x * uni_code_unit_width (encoding)));
}




UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused))
uni_str_iscan (enum uni_encoding_scheme encoding,
	       uni_string str,
	       size_t * pos,
	       size_t len)
{
  t_unicode c0;
  t_unicode c1;

  switch (encoding)
    {
    default:
      panic ("unhandled encoding scheme in uni_str_iscan");

    case uni_iso8859_1:

      uni_invariant (*pos < len);
      return (t_unicode)(((t_uchar *)str)[(*pos)++]);

    case uni_utf16:

      uni_invariant (*pos < len);
      c0 = (t_unicode)(((t_unichar **)str)[(*pos)++]);

      if (uni_is_low_surrogate (c0))
	{
	  --(*pos);
	  return 0xfffe;
	}

      if (!uni_is_high_surrogate (c0))
	return c0;

      if (((*pos) + 1) < len)
	{
	  --(*pos);
	  return 0xffff;
	}

      c1 = (t_unicode)(((t_unichar **)str)[(*pos)++]);

      if (!uni_is_low_surrogate (c1))
	{
	  (*pos) -= 2;
	  return 0xfffe;
	}

      return uni_assemble_surrogates (c0, c1);

    case uni_utf8:

      return uni_utf8_iscan ((t_uchar *)str, pos, len);
    }
}



UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused))
uni_str_scan (enum uni_encoding_scheme encoding,
	      uni_string * str)
{
  t_unicode c0;
  t_unicode c1;

  switch (encoding)
    {
    default:
      panic ("unhandled encoding scheme in uni_str_scan");

    case uni_iso8859_1:

      return (t_unicode)*((*(t_uchar **)str)++);

    case uni_utf16:

      c0 = (t_unicode)*((*(t_unichar **)str)++);

      if (uni_is_low_surrogate (c0))
	{
	  --(*((t_unichar **)str));
	  return 0xfffe;
	}

      if (!uni_is_high_surrogate (c0))
	return c0;

      c1 = (t_unicode)*((*(t_unichar **)str)++);

      if (!uni_is_low_surrogate (c1))
	{
	  (*(t_unichar **)str) -= 2;
	  return 0xfffe;
	}
      
      return uni_assemble_surrogates (c0, c1);

    case uni_utf8:
      {
	t_unicode c0;
	t_unicode c1;
	t_unicode c2;
	t_unicode c3;
	t_unicode c;

	c0 = (t_unicode)*((*(t_uchar **)str)++);
	switch ((c0 >> 3) & 0x1f)
	  {
	  case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
	  case 8: case 9: case 10: case 11: case 12: case 13: case 14: case 15:
	    return c0;

	  case 24: case 25: case 26: case 27:

	    c1 = (t_unicode)*((*(t_uchar **)str)++);

	    if ((c1 >> 6) != 2)
	      {
		(*(t_uchar **)str) -= 2;
		return (c1 ? 0xfffe : 0xffff);
	      }

	    c = (((c0 & 0x1f) << 6) | (c1 & 0x3f));

	    if (c < 128)
	      {
		(*(t_uchar **)str) -= 2;
		return 0xfffe;
	      }

	    return c;

	  case 28: case 29:

	    c1 = (t_unicode)*((*(t_uchar **)str)++);

	    if (!c1)
	      {
		(*(t_uchar **)str) -= 2;
		return 0xffff;
	      }

	    c2 = (t_unicode)*((*(t_uchar **)str)++);

	    if (((c1 >> 6) != 2) || ((c2 >> 6) != 2))
	      {
		(*(t_uchar **)str) -= 3;
		return (c2 ? 0xfffe : 0xffff);
	      }

	    c = ((c0 & 0xf) << 12) | ((c1 & 0x3f) << 6) | (c2 & 0x3f);

	    if (uni_is_surrogate (c) || (c < (1 << 11)))
	      {
		(*(t_uchar **)str) -= 3;
		return 0xfffe;
	      }

	    return c;

	  case 30:
	    c1 = (t_unicode)*((*(t_uchar **)str)++);

	    if (!c1)
	      {
		(*(t_uchar **)str) -= 2;
		return 0xffff;
	      }

	    c2 = (t_unicode)*((*(t_uchar **)str)++);
	    if (!c2)
	      {
		(*(t_uchar **)str) -= 3;
		return 0xffff;
	      }

	    c3 = (t_unicode)*((*(t_uchar **)str)++);

	    if (((c1 >> 6) != 2) || ((c2 >> 6) != 2) || ((c3 >> 6) != 2))
	      {
		(*(t_uchar **)str) -= 4;
		return (c3 ? 0xfffe : 0xffff);
	      }

	    c = (((c0 & 7) << 18) | ((c1 & 0x3f) << 12) | ((c2 & 0x3f) << 6) | (c3 & 0x3f));

	    if  ((c < (1 << 16)) || (c > 0x10ffff))
	      {
		(*(t_uchar **)str) -= 4;
		return 0xfffe;
	      }

	    return c;
	    
	  default:		/* 16 and 32 */
	    return 0xfffe;
	  }  
      }
    }
}




UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_str_iput (enum uni_encoding_scheme encoding,
	      uni_string str,
	      size_t * pos,
	      size_t len,
	      t_unicode c)
{
  switch (encoding)
    {
    default:
      panic ("unhandled encoding scheme in uni_str_iput");

    case uni_iso8859_1:
      return uni_iso8859_1_iput ((t_uchar *)str, pos, len, c);

    case uni_utf8:
      return uni_utf8_iput ((t_uchar *)str, pos, len, c);

    case uni_utf16:
      uni_invariant (!uni_is_noncharacter (c));
      uni_invariant (!uni_is_surrogate (c));

      if (c <= 0xffff)
	{
	  if ((*pos) >= len)
	    return -1;

	  ((t_unichar *)str)[*pos] = c;
	  ++(*pos);
	  return 1;
	}
      else
	{
	  t_unicode hi;
	  t_unicode lo;

	  if ((*pos + 1) >= len)
	    return -2;

	  hi = (c - 0x10000) / 0x400 + 0xd800;
	  lo = (c - 0x10000) + 0xdc00;
      
	  ((t_unichar *)str)[*pos] = hi;
	  ++(*pos);
	  ((t_unichar *)str)[*pos] = lo;
	  ++(*pos);
	  return 2;
	}
    }
}


UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_str_put (enum uni_encoding_scheme encoding,
	     uni_string * str,
	     t_unicode c)
{
  switch (encoding)
    {
    default:
      panic ("unhandled encoding scheme in uni_str_iput");

    case uni_iso8859_1:
      uni_invariant (c < 256);
      *((*((t_uchar **)str))++) = c;
      return 1;

    case uni_utf8:
      {
	uni_invariant (!uni_is_noncharacter (c));
	uni_invariant (!uni_is_surrogate (c));

	if (c < 0x80)
	  {
	    *((*((t_uchar **)str))++) = c;
	    return 1;
	  }
	else if (c < 0x800)
	  {
	    *((*((t_uchar **)str))++) = (0xc0 | (c >> 6));
	    *((*((t_uchar **)str))++) = (0x80 | (c & 0x3f));
	    return 2;
	  }
	else if (c < 0x10000)
	  {
	    *((*((t_uchar **)str))++) = (0xe0 | (c >> 12));
	    *((*((t_uchar **)str))++) = (0x80 | ((c >> 6) & 0x3f));
	    *((*((t_uchar **)str))++) = (0x80 | (c & 0x3f));
	    return 3;
	  }
	else if (c < 0x110000)
	  {
	    t_unicode hi_bits;

	    hi_bits = (c >> 16);
	    c &= 0xffff;

	    *((*((t_uchar **)str))++) = (0xf0 | (hi_bits >> 2));
	    *((*((t_uchar **)str))++) = (0x80 | ((hi_bits & 3) << 4) | ((c >> 12) & 0xf));
	    *((*((t_uchar **)str))++) = (0x80 | ((c >> 6) & 0x3f));
	    *((*((t_uchar **)str))++) = (0x80 | (c & 0x3f));
	    return 4;
	  }
	else
	  panic ("out of range character in uni_str_put");
      }

    case uni_utf16:
      {
	uni_invariant (!uni_is_noncharacter (c));
	uni_invariant (!uni_is_surrogate (c));

	if (c < 0x10000)
	  {
	    *((*((t_unichar **)str))++) = c;
	    return 1;
	  }
	else
	  {
	    t_unicode hi;
	    t_unicode lo;

	    hi = (c - 0x10000) / 0x400 + 0xd800;
	    lo = (c - 0x10000) + 0xdc00;

	    *((*((t_unichar **)str))++) = hi;
	    *((*((t_unichar **)str))++) = lo;

	    return 2;
	  }
      }
    }
}
