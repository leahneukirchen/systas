/* tag: Tom Lord Tue Dec  4 14:41:36 2001 (coding-inlines.c)
 */
/* coding-inlines.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/machine/endian.h"
#include "hackerlab/machine/types.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/uni/invariant.h"
#include "hackerlab/uni/coding.h"



#ifndef UNI_INLINE_QUALIFIERS
#define UNI_INLINE_QUALIFIERS
#endif

#ifndef __GNUC__
#undef __attribute__
#define __attribute__(X)
#endif



UNI_INLINE_QUALIFIERS size_t __attribute__((unused))
uni_cset_size (enum uni_encoding_scheme encoding)
{
  switch (encoding)
    {
    default:
      panic ("unrecognized string encoding scheme in uni_cset_size");
      return 0;

    case uni_iso8859_1:
      return 256;

    case uni_utf8:
    case uni_utf16:
    case uni_utf16be:
    case uni_utf16le:
      return (1 << 21);
    }
}


UNI_INLINE_QUALIFIERS size_t __attribute__((unused))
uni_code_unit_size (enum uni_encoding_scheme encoding)
{
  switch (encoding)
    {
    default:
      panic ("unrecognized string encoding scheme in uni_cset_size");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return 1;

    case uni_utf16:
    case uni_utf16be:
    case uni_utf16le:
      return 2;
    }
}



UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_is_noncharacter (t_unicode c)
{
  return (c & 0x1ffffe) == 0xfffe;
}

/*(c uni_is_high_surrogate)
 * int uni_is_high_surrogate (t_unicode c);
 * 
 * Return 1 if `c' is a high surrogate, 0 otherwise.
 * 
 */
UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_is_high_surrogate (t_unicode c)
{
  return (((c) >= 0xdb80) && ((c) <= 0xdbff));
}


/*(c uni_is_low_surrogate)
 * int uni_is_low_surrogate (t_unicode c);
 * 
 * Return 1 if `c' is a low surrogate, 0 otherwise.
 * 
 */
UNI_INLINE_QUALIFIERS int  __attribute__((unused))
uni_is_low_surrogate (t_unicode c)
{
  return (((c) >= 0xdc00) && ((c) <= 0xdfff));
}

UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_is_surrogate (t_unicode c)
{
  return (c >= 0xdb80) && (c <= 0xdfff);
}


UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused))
uni_assemble_surrogates (t_unichar hi, t_unichar lo)
{
  return ((hi - 0xd800) * 0x400) + (lo - 0xdc00) + 0x10000;
}




UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_length_in_iso8859_1 (t_unicode c)
{
  if (c >= 256)
    return -1;

  return 1;
}


UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_length_in_utf8 (t_unicode c)
{
  if (c < 128)
    return 1;
  else if (c < (1 << 11))
    return 2;
  else if (c < (1 << 16))
    return 3;
  else
    return 4;
}


UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_length_in_utf16 (t_unicode c)
{
  if (c < (1 << 16))
    return 2;
  else
    return 4;
}


UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_length_in_encoding (enum uni_encoding_scheme enc, t_unicode c)
{
  switch (enc)
    {
    default:
      panic ("unhandled encoding in uni_length_in_encoding");
      return -1;

    case uni_iso8859_1:
      return uni_length_in_iso8859_1 (c);

    case uni_utf8:
      return uni_length_in_utf8 (c);

    case uni_utf16:
    case uni_utf16be:
    case uni_utf16le:
      return uni_length_in_utf16 (c);
    }
}





UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused))
uni_iso8859_1_iscan (t_uchar * str, size_t * pos, size_t len)
{
  uni_invariant (*pos < len);
  return str [(*pos)++];
}




UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused))
uni_utf8_iscan (t_uchar * str, size_t * pos, size_t len)
{
  t_unicode c0;
  t_unicode c1;
  t_unicode c2;
  t_unicode c3;
  t_unicode c;

  uni_invariant (*pos < len);

  c0 = str [(*pos)++];
  switch ((c0 >> 3) & 0x1f)
    {
    case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
    case 8: case 9: case 10: case 11: case 12: case 13: case 14: case 15:
      return c0;

    case 24: case 25: case 26: case 27:

      if ((*pos) >= len)
	{
	  --(*pos);
	  return 0xffff;
	}

      c1 = str [(*pos)++];

      if ((c1 >> 6) != 2)
	{
	  (*pos) -= 2;
	  return 0xfffe;
	}

      c = (((c0 & 0x1f) << 6) | (c1 & 0x3f));

      if (c < 128)
	{
	  (*pos) -= 2;
	  return 0xfffe;
	}

      return c;

    case 28: case 29:
      if (((*pos) + 1) >= len)
	{
	  --(*pos);
	  return 0xffff;
	}

      c1 = str [(*pos)++];
      c2 = str [(*pos)++];

      if (((c1 >> 6) != 2) || ((c2 >> 6) != 2))
	{
	  (*pos) -= 3;
	  return 0xfffe;
	}

      c = ((c0 & 0xf) << 12) | ((c1 & 0x3f) << 6) | (c2 & 0x3f);
      if (uni_is_surrogate (c) || (c < (1 << 11)))
	{
	  (*pos) -= 3;
	  return 0xfffe;
	}
      
      return c;

    case 30:
      if (((*pos) + 2) >= len)
	{
	  --(*pos);
	  return 0xffff;
	}

      c1 = str [(*pos)++];
      c2 = str [(*pos)++];
      c3 = str [(*pos)++];

      if (((c1 >> 6) != 2) || ((c2 >> 6) != 2) || ((c3 >> 6) != 2))
	{
	  (*pos) -= 4;
	  return 0xfffe;
	}

      c = (((c0 & 7) << 18) | ((c1 & 0x3f) << 12) | ((c2 & 0x3f) << 6) | (c3 & 0x3f));

      if ((c < (1 << 16)) || (c > 0x10ffff))
	{
	  (*pos) -= 4;
	  return 0xfffe;
	}

      return c;

    default:			/* 16 and 31 */
      return 0xfffe;
    }
}



UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused))
uni_utf16be_iscan (t_uchar * str, size_t * pos, size_t len)
{
  t_unicode hi;
  t_unicode lo;
  t_unicode first;
  t_unicode second;

  uni_invariant (((*pos) + 1) < len);
  
  hi = str [(*pos)++];
  lo = str [(*pos)++];
  first = (hi << 8) | lo;

  if (uni_is_low_surrogate (first))
    {
      (*pos) -= 2;
      return 0xfffe;
    }

  if (!uni_is_high_surrogate (first))
    return first;

  if (((*pos) + 1) >= len)
    {
      (*pos) -= 2;
      return 0xffff;
    }

  hi = str [(*pos)++];
  lo = str [(*pos)++];
  second = (hi << 8) | lo;

  if (!uni_is_low_surrogate (second))
    {
      (*pos) -= 2;
      return 0xfffe;
    }

  return uni_assemble_surrogates (first, second);
}


UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused))
uni_utf16le_iscan (t_uchar * str, size_t * pos, size_t len)
{
  t_unicode hi;
  t_unicode lo;
  t_unicode first;
  t_unicode second;

  uni_invariant (((*pos) + 1) < len);
  
  lo = str [(*pos)++];
  hi = str [(*pos)++];
  first = (hi << 8) | lo;

  if (uni_is_low_surrogate (first))
    {
      (*pos) -= 2;
      return 0xfffe;
    }

  if (!uni_is_high_surrogate (first))
    return first;

  if (((*pos) + 1) >= len)
    {
      (*pos) -= 2;
      return 0xffff;
    }

  lo = str [(*pos)++];
  hi = str [(*pos)++];
  second = (hi << 8) | lo;

  if (!uni_is_low_surrogate (second))
    {
      (*pos) -= 2;
      return 0xfffe;
    }

  return uni_assemble_surrogates (first, second);
}


UNI_INLINE_QUALIFIERS t_unicode __attribute__((unused))
uni_utf16_iscan (t_uchar * str, size_t * pos, size_t len)
{
#if MACHINE_IS_BIGENDIAN
  return uni_utf16be_iscan (str, pos, len);
#else
  return uni_utf16le_iscan (str, pos, len);
#endif
}




UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_iso8859_1_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c)
{
  uni_invariant (c < 256);

  if ((*pos) >= len)
    {
      return -1;
    }
  str[(*pos)++] = (t_uchar)c;
  return 1;
}



UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_utf8_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c)
{
  uni_invariant (!uni_is_noncharacter (c));
  uni_invariant (!uni_is_surrogate (c));

  if (c < 0x80)
    {
      if (*pos >= len)
	return -1;
      str[(*pos)++] = c;
      return 1;
    }
  else if (c < 0x800)
    {
      if ((*pos + 1) >= len)
	return -2;

      str[(*pos)++] = (0xc0 | (c >> 6));
      str[(*pos)++] = (0x80 | (c & 0x3f));
      return 2;
    }
  else if (c <= 0xffff)
    {
      if ((*pos + 2) >= len)
	return -3;

      str[(*pos)++] = (0xe0 | (c >> 12));
      str[(*pos)++] = (0x80 | ((c >> 6) & 0x3f));
      str[(*pos)++] = (0x80 | (c & 0x3f));
      return 3;
    }
  else if (c < 0x110000)
    {
      t_unicode hi_bits;

      if ((*pos + 3) >= len)
	return -4;

      hi_bits = (c >> 16);
      c &= 0xffff;

      str[(*pos)++] = (0xf0 | (hi_bits >> 2));
      str[(*pos)++] = (0x80 | ((hi_bits & 3) << 4) | ((c >> 12) & 0xf));
      str[(*pos)++] = (0x80 | ((c >> 6) & 0x3f));
      str[(*pos)++] = (0x80 | (c & 0x3f));
      return 4;
    }
  else
    while (1)
      panic ("out of range character in uni_utf8_iput");
}



UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_utf16be_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c)
{
  uni_invariant (!uni_is_noncharacter (c));
  uni_invariant (!uni_is_surrogate (c));

  if (c <= 0xffff)
    {
      if ((*pos + 1) >= len)
	return -2;

      str[(*pos)++] = ((c >> 8) & 0xff);
      str[(*pos)++] = (c & 0xff);
      return 2;
    }
  else
    {
      t_unicode hi;
      t_unicode lo;

      if ((*pos + 3) >= len)
	return -4;

      hi = (c - 0x10000) / 0x400 + 0xd800;
      lo = (c - 0x10000) + 0xdc00;
      
      str[(*pos)++] = ((hi >> 8) & 0xff);
      str[(*pos)++] = (hi & 0xff);

      str[(*pos)++] = ((lo >> 8) & 0xff);
      str[(*pos)++] = (lo & 0xff);
      return 4;
    }
}


UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_utf16le_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c)
{
  uni_invariant (!uni_is_noncharacter (c));
  uni_invariant (!uni_is_surrogate (c));

  if (c <= 0xffff)
    {
      if ((*pos + 1) >= len)
	return -2;

      str[(*pos)++] = (c & 0xff);
      str[(*pos)++] = ((c >> 8) & 0xff);
      return 2;
    }
  else
    {
      t_unicode hi;
      t_unicode lo;

      if ((*pos + 3) >= len)
	return -4;

      hi = (c - 0x10000) / 0x400 + 0xd800;
      lo = (c - 0x10000) + 0xdc00;
      
      str[(*pos)++] = (hi & 0xff);
      str[(*pos)++] = ((hi >> 8) & 0xff);

      str[(*pos)++] = (lo & 0xff);
      str[(*pos)++] = ((lo >> 8) & 0xff);
      return 4;
    }
}


UNI_INLINE_QUALIFIERS int __attribute__((unused))
uni_utf16_iput (t_uchar * str, size_t * pos, size_t len, t_unicode c)
{
#if MACHINE_IS_BIGENDIAN
  return uni_utf16be_iput (str, pos, len, c);
#else
  return uni_utf16le_iput (str, pos, len, c);
#endif
}


