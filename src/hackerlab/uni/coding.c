/* tag: Tom Lord Tue Dec  4 14:41:36 2001 (coding.c)
 */
/* coding.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#define UNI_CODING_INLINES

#include <stddef.h>
#include "hackerlab/machine/types.h"
#include "hackerlab/machine/endian.h"
#include "hackerlab/bugs/panic.h"
#include "hackerlab/uni/coding.h"


/************************************************************************
 *(h1 "Unaligned Encoding Forms")
 * 
 * 
 * 
 */




int
uni_utf8_to_utf16be (t_uchar * out,
		     size_t * outp, 
		     size_t out_len,
		     t_uchar * in,
		     size_t * inp,
		     size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf8_iscan (in, inp, in_len);

      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf16be_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}


int
uni_utf8_to_utf16le (t_uchar * out,
		     size_t * outp, 
		     size_t out_len,
		     t_uchar * in,
		     size_t * inp,
		     size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf8_iscan (in, inp, in_len);

      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf16le_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}

int
uni_utf8_to_utf16 (t_uchar * out,
		   size_t * outp, 
		   size_t out_len,
		   t_uchar * in,
		   size_t * inp,
		   size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf8_iscan (in, inp, in_len);
      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf16_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}



int
uni_utf16be_to_utf8 (t_uchar * out,
		     size_t * outp, 
		     size_t out_len,
		     t_uchar * in,
		     size_t * inp,
		     size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf16be_iscan (in, inp, in_len);

      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf8_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}


int
uni_utf16be_to_utf16le (t_uchar * out,
			size_t * outp, 
			size_t out_len,
			t_uchar * in,
			size_t * inp,
			size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf16be_iscan (in, inp, in_len);
      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf16le_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}

int
uni_utf16be_to_utf16 (t_uchar * out,
		      size_t * outp, 
		      size_t out_len,
		      t_uchar * in,
		      size_t * inp,
		      size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf16be_iscan (in, inp, in_len);
      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf16_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}



int
uni_utf16le_to_utf8 (t_uchar * out,
		     size_t * outp, 
		     size_t out_len,
		     t_uchar * in,
		     size_t * inp,
		     size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf16le_iscan (in, inp, in_len);
      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf8_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}


int
uni_utf16le_to_utf16be (t_uchar * out,
			size_t * outp, 
			size_t out_len,
			t_uchar * in,
			size_t * inp,
			size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf16le_iscan (in, inp, in_len);
      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf16be_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}

int
uni_utf16le_to_utf16 (t_uchar * out,
		      size_t * outp, 
		      size_t out_len,
		      t_uchar * in,
		      size_t * inp,
		      size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf16le_iscan (in, inp, in_len);
      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf16_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}





int
uni_utf16_to_utf8 (t_uchar * out,
		   size_t * outp, 
		   size_t out_len,
		   t_uchar * in,
		   size_t * inp,
		   size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf16_iscan (in, inp, in_len);
      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf8_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}


int
uni_utf16_to_utf16be (t_uchar * out,
		      size_t * outp, 
		      size_t out_len,
		      t_uchar * in,
		      size_t * inp,
		      size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf16_iscan (in, inp, in_len);
      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf16be_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}

int
uni_utf16_to_utf16le (t_uchar * out,
		      size_t * outp, 
		      size_t out_len,
		      t_uchar * in,
		      size_t * inp,
		      size_t in_len)
{
  t_unicode c;

  while (1)
    {
      if (*inp >= in_len)
	return 0;

      c = uni_utf16_iscan (in, inp, in_len);
      if (c == 0xffff)
	return uni_input_truncated;
      else if (c == 0xfffe)
	return uni_input_invalid;
      else
	{
	  int amt;
	  amt = uni_utf16le_iput (out, outp, out_len, c);
	  if (amt < 0)
	    return -amt;
	}
    }
}




uni_iscan_fn
uni_encoding_iscan_fn (enum uni_encoding_scheme encoding)
{
  switch (encoding)
    {
    case uni_iso8859_1:
      return uni_iso8859_1_iscan;

    case uni_utf8:
      return uni_utf8_iscan;

    case uni_utf16:
      return uni_utf16_iscan;

    case uni_utf16be:
      return uni_utf16be_iscan;

    case uni_utf16le:
      return uni_utf16le_iscan;
    }

  while (1)
    panic ("not reached in uni_encoding_iscan_fn");
}



uni_iput_fn
uni_encoding_iput_fn (enum uni_encoding_scheme encoding)
{
  switch (encoding)
    {
    case uni_iso8859_1:
      return uni_iso8859_1_iput;

    case uni_utf8:
      return uni_utf8_iput;

    case uni_utf16:
      return uni_utf16_iput;

    case uni_utf16be:
      return uni_utf16be_iput;

    case uni_utf16le:
      return uni_utf16le_iput;
    }

  while (1)
    panic ("not reached in uni_encoding_iput_fn");
}

