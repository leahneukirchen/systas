/* tag: Tom Lord Tue Dec  4 14:41:42 2001 (uni-str.c)
 */
/* uni-str.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/bugs/panic.h"
#include "hackerlab/char/str.h"
#include "hackerlab/hash/hash-utils.h"
#include "hackerlab/uni/invariant.h"
#include "hackerlab/unicode/unidata.h"
#include "hackerlab/unicode/unichar-str.h"
#include "hackerlab/unicode/uni-str.h"


/************************************************************************
 *(h1 "Unicode String Processing")
 * 
 * 
 * 
 */



/*(c uni_string :category type)
 * typedef struct uni_undefined_struct * uni_string;
 * 
 * The opaque pointer type `uni_string' is used to represent a
 * string of Unicode characters in any of the encodings:
 * 
 *	uni_iso8859_1
 * 	uni_utf8
 * 	uni_utf16
 * 
 * By using this type to represent all Unicode strings, programs can
 * be written without regard to what encoding scheme is being used for
 * internal processing.  (See xref:"Naming Unicode Coding Forms".)
 * 
 * Pointers of type `uni_string' are created by a simple cast:
 * 
 * 	t_uchar * an_iso8859_1_string;
 * 	t_uchar * a_utf8_string;
 * 	t_unichar * a_utf16_string;
 * 	uni_string a_unicode_string;
 * 
 * 	a_unicode_string = (uni_string)a_iso8859_1_string;
 * 
 * or
 * 
 * 	a_unicode_string = (uni_string)a_utf8_string;
 * 
 * or
 * 
 * 	a_unicode_string = (uni_string)a_utf16_string;
 * 
 * \Note:/ A UTF-16 Unicode string must be properly aligned for
 * the type `t_unichar' (an unsigned 16-bit integer).  
 * 
 * Functions that operate on Unicode strings generally accept two
 * arguments for each string: an encoding type, and a `uni_string'
 * value.  By convention, the order of those arguments is always:
 * 
 * 		encoding, string
 * 
 * for example: 
 * 
 * 		uni_str_length (encoding, string)
 * 
 * When you write new functions that process Unicode strings, 
 * you should ordinarily follow that same convention.
 * 
 * In almost all cases, when a function accepts or returns the
 * length of a Unicode string, that length is measured in code
 * units (not code points).  For example, a UTF-16 string containing
 * only the code point `U+000A' has length 1;  a UTF-16 string
 * containing only the code point `U+10000A' has length 2 (because
 * it is made up of two code units, a high and a low surrogate).
 */


/*(menu)
 */


/************************************************************************
 *(h2 "Unicode String Length")
 * 
 * 
 * 
 */


/*(c uni_str_length)
 * size_t uni_str_length (enum uni_encoding_scheme encoding,
 *                        uni_string s);
 * 
 * Return the length of a 0-terminated Unicode string, measured in
 * code units.
 * 
 * If `encoding' is not one of `uni_iso8859_1', `uni_utf8', or
 * `uni_utf16', this function exits the process by calling `panic'.
 */
size_t
uni_str_length (enum uni_encoding_scheme encoding,
		uni_string s)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_length");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return str_length ((t_uchar *)s);

    case uni_utf16:
      return unichar_str_length ((t_unichar *)s);
    }
}


/*(c uni_str_length_for_encoding)
 * size_t
 * uni_str_length_for_encoding (enum uni_encoding_scheme out_enc,
 *                              enum uni_encoding_scheme encoding,
 *                              uni_string str);
 * 
 * Return the length of a 0-terminated Unicode string, measured
 * in code units of a specified encoding.
 * 
 * `out_enc' is the encoding to use when reporting the length 
 * of the string.
 * 
 * `encoding' is the encoding of the string as it is stored in memory.
 * 
 * For example, to find out the UTF-8 length of a UTF-16 string:
 * 
 * 	t_unichar a_utf16_string;
 * 	size_t utf8_length;
 * 
 * 	utf8_length
 *	 = uni_str_length_for_encoding (uni_utf8,
 *				        uni_utf16,
 *					(uni_string)a_utf16_string);
 */
size_t
uni_str_length_for_encoding (enum uni_encoding_scheme out_enc,
			     enum uni_encoding_scheme encoding,
			     uni_string str)
{
  if (out_enc == encoding)
    return uni_str_length (encoding, str);
  else
    {
      size_t l;

      l = 0;

      while (1)
	{
	  t_unicode c;

	  c = uni_str_scan (encoding, &str);

	  if (!c)
	    return l;

	  l += uni_length_in_encoding (out_enc, c);
	}
    }
}


/*(c uni_str_length_for_encoding_n)
 * size_t uni_str_length_for_encoding_n (enum uni_encoding_scheme out_enc,
 *                                       enum uni_encoding_scheme encoding,
 *                                       uni_string str,
 *                                       size_t len);
 * 
 * Return the length of a Unicode string, measured in code units of a
 * specified encoding.
 * 
 * `out_enc' is the encoding to use when reporting the length 
 * of the string.
 * 
 * `encoding' is the encoding of the string as it is stored in memory.
 * 
 * `len' is the length of `str' in `encoding', measured in code units.
 * 
 * This function does not stop at a `U+0000' but instead, counts
 * such characters normally.  This may not be the Right behavior and
 * may change in future releases: programs should not rely on this 
 * behavior.
 * 
 */
size_t
uni_str_length_for_encoding_n (enum uni_encoding_scheme out_enc,
			       enum uni_encoding_scheme encoding,
			       uni_string str,
			       size_t len)
{
  if (out_enc == encoding)
    return len;
  else
    {
      size_t pos;
      size_t l;

      l = 0;
      pos = 0;

      while (pos < len)
	{
	  t_unicode c;

	  c = uni_str_iscan (encoding, str, &pos, len);
	  l += uni_length_in_encoding (out_enc, c);
	}

      return l;
    }
}



/************************************************************************
 *(h2 "Unicode String Copying")
 * 
 * 
 * 
 */

/*(c uni_str_cpy)
 * uni_string uni_str_cpy (enum uni_encoding_scheme encoding,
 *                         uni_string to,
 *                         uni_string from);
 * 
 * Copy a 0-terminated Unicode string, preserving its encoding.
 * 
 * `encoding' is the encoding scheme of both the source string
 * and its copy.
 * 
 * `to' is the destination for the copied string.
 * 
 * `from' is the string to copy.
 */
uni_string
uni_str_cpy (enum uni_encoding_scheme encoding,
	     uni_string to,
	     uni_string from)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_cpy");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return (uni_string)str_cpy ((t_uchar *)to, (t_uchar *)from);

    case uni_utf16:
      return (uni_string)unichar_str_cpy ((t_unichar *)to, (t_unichar *)from);
    }
}


/*(c uni_str_cpy_n)
 * uni_string uni_str_cpy_n (enum uni_encoding_scheme encoding,
 *                           uni_string to,
 *                           uni_string from,
 *                           size_t n);
 * 
 * Copy a Unicode string, preserving its encoding.
 * 
 * `encoding' is the encoding scheme of both the source string
 * and its copy.
 * 
 * `to' is the destination for the copied string.
 * 
 * `from' is the string to copy.
 * 
 * `n' is the length of the string to copy, measured in code units.
 * 
 * If a 0 character is encountered, copying stops.
 * 
 * In all cases, a 0 character is added to the copied string.
 * 
 * \Warning:/ This function is different from `strncpy'.  `strncpy'
 * always stores exactly `n' characters in `to', padding the result
 * with 0 if a 0 character is encountered in `from' before `n'
 * characters are written.  This function stores up to `n+1' characters:
 * up to `n' non-0 characters from `from', plus a final 0.  Also,
 * this function only ever adds a single 0 to `to' -- it does not 
 * pad `to' with 0 characters.
 * 
 */
uni_string
uni_str_cpy_n (enum uni_encoding_scheme encoding,
	       uni_string to,
	       uni_string from,
	       size_t n)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_cpy_n");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return (uni_string)str_cpy_n ((t_uchar *)to, (t_uchar *)from, n);

    case uni_utf16:
      return (uni_string)unichar_str_cpy_n ((t_unichar *)to, (t_unichar *)from, n);
    }
}



/*(c uni_str_cpy_xfrm)
 * uni_string uni_str_cpy_xfrm (enum uni_encoding_scheme to_enc,
 *                              uni_string to,
 *                              enum uni_encoding_scheme from_enc,
 *                              uni_string from);
 * 
 * Copy a 0-terminated Unicode string, possibly changing encoding
 * schemes.
 * 
 * `to_enc' is the encoding scheme for the copy.
 * 
 * `to' is the destination of the copied string.
 * 
 * `from_enc' is the encoding scheme for the source string.
 * 
 * `from' is the string to copy.
 */
uni_string
uni_str_cpy_xfrm (enum uni_encoding_scheme to_enc,
		  uni_string to,
		  enum uni_encoding_scheme from_enc,
		  uni_string from)
{
  if (to_enc == from_enc)
    return uni_str_cpy (to_enc, to, from);
  else
    {
      uni_string saved_to;

      saved_to = to;

      while (1)
	{
	  t_unicode c;

	  c = uni_str_scan (from_enc, &from);
	  uni_str_put (to_enc, &to, c);

	  if (!c)
	    return saved_to;
	}
    }
}


/*(c uni_str_cpy_xfrm_n)
 * uni_string uni_str_cpy_xfrm_n (enum uni_encoding_scheme to_enc,
 *                                uni_string to,
 *                                enum uni_encoding_scheme from_enc,
 *                                uni_string from,
 *                                size_t n);
 * 
 * Copy a Unicode string, possibly changing encoding schemes.
 * 
 * `to_enc' is the encoding scheme for the copy.
 * 
 * `to' is the destination of the copied string.
 * 
 * `from_enc' is the encoding scheme for the source string.
 * 
 * `from' is the string to copy.
 *
 * `n' is the length of the string to copy, measured in code units.
 * 
 * If a 0 character is encountered, copying stops.
 * 
 * In all cases, a 0 character is added to the copied string.
 * 
 * \Warning:/ This function is different from `strncpy'.  `strncpy'
 * always stores exactly `n' characters in `to', padding the result
 * with 0 if a 0 character is encountered in `from' before `n'
 * characters are written.  This function stores up to `n+1' characters:
 * up to `n' non-0 characters from `from', plus a final 0.
 * 
 */
uni_string
uni_str_cpy_xfrm_n (enum uni_encoding_scheme to_enc,
		    uni_string to,
		    enum uni_encoding_scheme from_enc,
		    uni_string from,
		    size_t n)
{
  if (to_enc == from_enc)
    return uni_str_cpy_n (to_enc, to, from, n);
  else
    {
      uni_string saved_to;
      size_t from_pos;

      saved_to = to;
      from_pos = 0;

      while (from_pos < n)
	{
	  t_unicode c;

	  c = uni_str_iscan (from_enc, from, &from_pos, n);
	  uni_str_put (to_enc, &to, c);
	}
      uni_str_put (to_enc, &to, 0);
      return to;
    }
}



/************************************************************************
 *(h2 "String Concatenation")
 * 
 * 
 * 
 */


/*(c uni_str_cat)
 * uni_string uni_str_cat (enum uni_encoding_scheme encoding,
 *                         uni_string to,
 *                         uni_string from);
 * 
 * Concatenate two 0-terminated Unicode strings which use the
 * same encoding scheme.
 * 
 * Characters are append to `to', beginning from the position
 * of the terminating `U+0000' of `to'.
 * 
 * Characters are copied from `from'.
 * 
 * A final `U+0000' is included in the concatenated string.
 * 
 */
uni_string
uni_str_cat (enum uni_encoding_scheme encoding,
	     uni_string to,
	     uni_string from)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_cat");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return (uni_string)str_cat ((t_uchar *)to, (t_uchar *)from);

    case uni_utf16:
      return (uni_string)unichar_str_cat ((t_unichar *)to, (t_unichar *)from);
    }
}


/*(c uni_str_cat_n)
 * uni_string uni_str_cat_n (enum uni_encoding_scheme encoding,
 *                           uni_string to,
 *                           uni_string from,
 *                           size_t n);
 * 
 * Append to a 0-terminated Unicode string from a string of
 * the same encoding.
 * 
 * Characters are append to `to', beginning from the position
 * of the terminating `U+0000' of `to'.
 * 
 * Up to `n' characters are copied from `from'.  If a 0 character
 * is encountered before `n' characters have been copied, copying
 * stops.
 * 
 * A final `U+0000' is included in the concatenated string.
 * 
 */
uni_string
uni_str_cat_n (enum uni_encoding_scheme encoding,
	       uni_string to,
	       uni_string from,
	       size_t n)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_cat_n");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return (uni_string)str_cat_n ((t_uchar *)to, (t_uchar *)from, n);

    case uni_utf16:
      return (uni_string)unichar_str_cat_n ((t_unichar *)to, (t_unichar *)from, n);
    }
}


/*(c uni_str_cat_xfrm)
 * uni_string uni_str_cat_xfrm (enum uni_encoding_scheme to_enc,
 *                              uni_string to,
 *                              enum uni_encoding_scheme from_enc,
 *                              uni_string from);
 * 
 * Concatenate two 0-terminated Unicode strings which might use 
 * different encoding schemes.
 * 
 * `to_enc' is the encoding scheme for `to' (and for the concatenated
 * string)
 * 
 * Characters are append to `to', beginning from the position
 * of the terminating `U+0000' of `to'.
 * 
 * `from_enc' is the encoding scheme for `from'.
 * 
 * Characters are copied from `from'.
 * 
 * A final `U+0000' is included in the concatenated string.
 * 
 * 
 */
uni_string
uni_str_cat_xfrm (enum uni_encoding_scheme to_enc,
		  uni_string to,
		  enum uni_encoding_scheme from_enc,
		  uni_string from)
{
  if (to_enc == from_enc)
    return uni_str_cat (to_enc, to, from);
  else
    {
      uni_string dest;

      dest = uni_str_chr_index (to_enc, to, 0);
      return uni_str_cpy_xfrm (to_enc, dest, from_enc, from);
    }
}


/*(c uni_str_cat_xfrm_n)
 * uni_string uni_str_cat_xfrm_n (enum uni_encoding_scheme to_enc,
 *                                uni_string to,
 *                                enum uni_encoding_scheme from_enc,
 *                                uni_string from,
 *                                size_t n);
 * 
 * Append to a 0-terminated Unicode string from a string which
 * might use a different encoding.
 * 
 * `to_enc' is the encoding scheme for `to' (and for the concatenated
 * string).
 * 
 * Characters are append to `to', beginning from the position
 * of the terminating `U+0000' of `to'.
 * 
 * `from_enc' is the encoding scheme for `from'.
 * 
 * Up to `n' characters are copied from `from'.  If a 0 character
 * is encountered before `n' characters have been copied, copying
 * stops.
 * 
 * A final `U+0000' is included in the concatenated string.
 * 
 */
uni_string
uni_str_cat_xfrm_n (enum uni_encoding_scheme to_enc,
		    uni_string to,
		    enum uni_encoding_scheme from_enc,
		    uni_string from,
		    size_t n)
{
  if (to_enc == from_enc)
    return uni_str_cat_n (to_enc, to, from, n);
  else
    {
      uni_string dest;

      dest = uni_str_chr_index (to_enc, to, 0);
      return uni_str_cpy_xfrm_n (to_enc, dest, from_enc, from, n);
    }
}



/************************************************************************
 *(h1 "Unicode String Hashing")
 * 
 * 
 * 
 */


/*(c uni_str_hash_n)
 * unsigned long uni_str_hash_n (enum uni_encoding_scheme encoding,
 *                               uni_string s,
 *                               size_t len);
 * 
 * Compute a hash value for a Unicode string of length `len' (measured
 * in code units).
 * 
 * This function does not stop at a `U+0000' but instead, always
 * processes `len' characters.
 * 
 * This function will return different hash values for the same string
 * in two different encodings.  See also
 * xref:"uni_str_hash_codepoints_n".
 */
unsigned long
uni_str_hash_n (enum uni_encoding_scheme encoding,
		uni_string s,
		size_t len)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_hash_n");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return str_hash_n ((t_uchar *)s, len);

    case uni_utf16:
      return unichar_str_hash_n ((t_unichar *)s, len);
    }
}


/*(c uni_str_hash_codepoints_n)
 * unsigned long 
 * uni_str_hash_codepoints_n (enum uni_encoding_scheme encoding,
 *                            uni_string s,
 *                            size_t len);
 * 
 * Compute a hash value for a Unicode string of length `len' (measured
 * in code units).
 * 
 * This function does not stop at a `U+0000' but instead, always
 * processes `len' characters.
 * 
 * This function will always return the same hash values for a given
 * string of code points, regardless of what encoding scheme is used.
 * See also xref:"uni_str_hash_n".
 * 
 */
unsigned long
uni_str_hash_codepoints_n (enum uni_encoding_scheme encoding,
			   uni_string s,
			   size_t len)
{
  size_t pos;
  unsigned long h;

  pos = 0;
  h = 0xbeda221e;

  while (pos < len)
    {
      t_unicode c;

      c = uni_str_iscan (encoding, s, &pos, len);

      uni_invariant (!uni_is_noncharacter (c));
	
      h ^= hash_ul ((unsigned long)c);
    }
  return h;
}


/************************************************************************
 *(h2 "Finding a Code Point in a Unicode String")
 * 
 * 
 * 
 */


/*(c uni_str_chr_index)
 * uni_string 
 * uni_str_chr_index (enum uni_encoding_scheme encoding,
 *                    uni_string s,
 *                    t_unicode c);
 * 
 * Return the position of the first occurence of code point `c' in a
 * 0-terminated Unicode string.  If `c' is not found, return 0.
 * 
 * \Note:/ This function looks for code points, not code units.
 * It can not be used, for example, to find the position of
 * an unpaired surrogate in a UTF-16 string.
 */
uni_string
uni_str_chr_index (enum uni_encoding_scheme encoding,
		   uni_string s,
		   t_unicode c)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_index");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return (uni_string)str_chr_index ((t_uchar *)s, c);

    case uni_utf16:
      return (uni_string)unichar_str_chr_index ((t_unichar *)s, c);
    }
}


/*(c uni_str_chr_rindex)
 * uni_string uni_str_chr_rindex (enum uni_encoding_scheme encoding,
 *                                uni_string s,
 *                                t_unicode c);
 * 
 * Return the position of the last occurence of code point `c' in a
 * 0-terminated Unicode string.  If `c' is not found, return 0.
 * 
 * \Note:/ This function looks for code points, not code units.
 * It can not be used, for example, to find the position of
 * an unpaired surrogate in a UTF-16 string.
 * 
 */
uni_string
uni_str_chr_rindex (enum uni_encoding_scheme encoding,
		    uni_string s,
		    t_unicode c)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_rindex");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return (uni_string)str_chr_rindex ((t_uchar *)s, c);

    case uni_utf16:
      return (uni_string)unichar_str_chr_rindex ((t_unichar *)s, c);
    }
}


/*(c uni_str_chr_index_n)
 * uni_string uni_str_chr_index_n (enum uni_encoding_scheme encoding,
 *                                 uni_string s,
 *                                 size_t n,
 *                                 t_unicode c);
 * 
 * Return the position of the first occurence of code point `c' in a
 * Unicode string.  If `c' is not found, return 0.
 * 
 * No more than `n' code units of `s' are searched.  
 * 
 * \Note:/ If `U+0000' is encountered before `n' code units are
 * processed, searching stops.  This may not be the Right behavior and
 * may change in future releases: programs should not rely on this
 * behavior.
 * 
 * \Note:/ This function looks for code points, not code units.
 * It can not be used, for example, to find the position of
 * an unpaired surrogate in a UTF-16 string.
 * 
 * 
 */
uni_string
uni_str_chr_index_n (enum uni_encoding_scheme encoding,
		     uni_string s,
		     size_t n,
		     t_unicode c)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_index_n");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return (uni_string)str_chr_index_n ((t_uchar *)s, n, c);

    case uni_utf16:
      return (uni_string)unichar_str_chr_index_n ((t_unichar *)s, n, c);
    }
}


/*(c uni_str_chr_rindex_n)
 * uni_string uni_str_chr_rindex_n (enum uni_encoding_scheme encoding,
 *                                  uni_string s,
 *                                  size_t n,
 *                                  t_unicode c);
 * 
 * Return the position of the last occurence of code point `c' in a
 * Unicode string.  If `c' is not found, return 0.
 * 
 * `n' is the length of `s' measured in code units.
 * 
 * \Note:/ This function looks for code points, not code units.
 * It can not be used, for example, to find the position of
 * an unpaired surrogate in a UTF-16 string.
 * 
 */
uni_string
uni_str_chr_rindex_n (enum uni_encoding_scheme encoding,
		      uni_string s,
		      size_t n,
		      t_unicode c)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_rindex_n");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return (uni_string)str_chr_rindex_n ((t_uchar *)s, n, c);

    case uni_utf16:
      return (uni_string)unichar_str_chr_rindex_n ((t_unichar *)s, n, c);
    }
}


/************************************************************************
 *(h2 "Simple Unicode String Comparisons")
 * 
 * 
 * The functions in this section compare Unicode strings by code
 * points.  They are not sensative to locale;  they are not sensative
 * to canonical equivalences of any kind.
 */

/*(c uni_str_cmp)
 * int uni_str_cmp (enum uni_encoding_scheme encoding,
 *                  uni_string a,
 *                  uni_string b);
 * 
 * Compare the code points of 0-terminated strings `a' and `b'
 * to determine their lexical ordering.
 * 
 * If `a' is less, return -1.
 * 
 * If `a' and `b' are identical, return 0.
 * 
 * If `a' is greater, return 1.
 */
int
uni_str_cmp (enum uni_encoding_scheme encoding,
	     uni_string a,
	     uni_string b)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_rindex_n");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return str_cmp ((t_uchar *)a, (t_uchar *)b);

    case uni_utf16:
      return unichar_str_cmp ((t_unichar *)a, (t_unichar *)b);
    }
}


/*(c uni_str_cmp_n)
 * int uni_str_cmp_n (enum uni_encoding_scheme encoding,
 *                    uni_string a, size_t a_l,
 *                    uni_string b, size_t b_l);
 * 
 * Compare the code points of strings `a' and `b'
 * to determine their lexical ordering.
 * 
 * `a_l' and `b_l' are the lengths of `a' and `b', measured
 * in code units.
 * 
 * If `a' is less, return -1.
 * 
 * If `a' and `b' are identical, return 0.
 * 
 * If `a' is greater, return 1.
 * 
 * \Note:/ Both `a' and `b' are considered to be 0-terminated.  If
 * `U+0000' is encountered before `a_l' or `b_l' code units have been
 * processed, comparison stops.  This may not be the Right behavior
 * and may change in future releases: programs should not rely on this
 * behavior.
 */
int
uni_str_cmp_n (enum uni_encoding_scheme encoding,
	       uni_string a, size_t a_l,
	       uni_string b, size_t b_l)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_rindex_n");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return str_cmp_n ((t_uchar *)a, a_l, (t_uchar *)b, b_l);

    case uni_utf16:
      return unichar_str_cmp_n ((t_unichar *)a, a_l, (t_unichar *)b, b_l);
    }
}


/*(c uni_str_cmp_prefix)
 * int uni_str_cmp_prefix (enum uni_encoding_scheme encoding,
 *                         uni_string a,
 *                         uni_string b);
 * 
 * Compare `b' to a prefix of `a'.  Both strings are 0-terminated.
 * 
 * If a prefix of `a' is equal to `b', return 0.
 * 
 * If `a' is less than `b', return -1.
 * 
 * If `a' is greater than `b', return -1.
 */
int
uni_str_cmp_prefix (enum uni_encoding_scheme encoding,
		    uni_string a,
		    uni_string b)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_rindex_n");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return str_cmp_prefix ((t_uchar *)a, (t_uchar *)b);

    case uni_utf16:
      return unichar_str_cmp_prefix ((t_unichar *)a, (t_unichar *)b);
    }
}


int
uni_str_casecmp (enum uni_encoding_scheme encoding,
		 uni_string a,
		 uni_string b)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_rindex_n");
      return 0;

    case uni_iso8859_1:
    case uni_utf8:
      return str_casecmp ((t_uchar *)a, (t_uchar *)b);

    case uni_utf16:
      return unichar_str_casecmp ((t_unichar *)a, (t_unichar *)b);
    }
}


/*(c uni_str_casecmp_n)
 * int uni_str_casecmp_n (enum uni_encoding_scheme encoding,
 *                        uni_string a, size_t a_l,
 *                        uni_string b, size_t b_l);
 * 
 * Compare the code points of 0-terminated strings `a' and `b'
 * to determine their lexical ordering, disregarding simple
 * case distinctions.
 * 
 * If `a' is less, return -1.
 * 
 * If `a' and `b' are identical, return 0.
 * 
 * If `a' is greater, return 1.
 * 
 * The strings are compared as if all characters in both 
 * strings were passed through `unidata_to_
 * 
 */
int
uni_str_casecmp_n (enum uni_encoding_scheme encoding,
		   uni_string a, size_t a_l,
		   uni_string b, size_t b_l)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_rindex_n");
      return 0;

    case uni_iso8859_1:
      return str_casecmp_n ((t_uchar *)a, a_l, (t_uchar *)b, b_l);

    case uni_utf8:
      broken;

    case uni_utf16:
      return unichar_str_casecmp_n ((t_unichar *)a, a_l, (t_unichar *)b, b_l);
    }
}


int
uni_str_casecmp_prefix (enum uni_encoding_scheme encoding,
			uni_string a,
			uni_string b)
{
  switch (encoding)
    {
    default:
      panic ("illegal encoding in uni_str_chr_rindex_n");
      return 0;

    case uni_iso8859_1:
      return str_casecmp_prefix ((t_uchar *)a, (t_uchar *)b);

    case uni_utf8:
      broken;

    case uni_utf16:
      return unichar_str_casecmp_prefix ((t_unichar *)a, (t_unichar *)b);
    }
}


int
uni_str_cmp_xfrm (enum uni_encoding_scheme a_enc,
		  uni_string a,
		  enum uni_encoding_scheme b_enc,
		  uni_string b)
{
  if (a_enc == b_enc)
    return uni_str_cmp (a_enc, a, b);
  else
    {
      while (1)
	{
	  t_unicode from_a;
	  t_unicode from_b;

	  from_a = uni_str_scan (a_enc, &a);
	  from_b = uni_str_scan (b_enc, &b);

	  uni_invariant (!uni_is_noncharacter (from_a));
	  uni_invariant (!uni_is_noncharacter (from_b));

	  if (from_a < from_b)
	    return -1;
	  else if (from_b < from_a)
	    return 1;
	  else if (!from_a)
	    return 0;
	}
    }
}


int
uni_str_cmp_xfrm_n (enum uni_encoding_scheme a_enc,
		    uni_string a, size_t a_l,
		    enum uni_encoding_scheme b_enc,
		    uni_string b, size_t b_l)
{
  if (a_enc == b_enc)
    return uni_str_cmp_n (a_enc, a, a_l, b, b_l);
  else
    {
      size_t a_pos;
      size_t b_pos;
      size_t l;

      a_pos = 0;
      b_pos = 0;
      l = ((a_l < b_l) ? a_l : b_l);

      while (a_pos < l)
	{
	  t_unicode from_a;
	  t_unicode from_b;

	  from_a = uni_str_iscan (a_enc, a, &a_pos, a_l);
	  from_b = uni_str_iscan (b_enc, b, &b_pos, b_l);

	  uni_invariant (!uni_is_noncharacter (from_a));
	  uni_invariant (!uni_is_noncharacter (from_b));

	  if (from_a < from_b)
	    return -1;
	  else if (from_b < from_a)
	    return 1;
	  else if (!from_a)
	    return 0;
	}

      if (a_l < b_l)
	return -1;
      else if (b_l < a_l)
	return 1;
      else
	return 0;
    }
}


int
uni_str_cmp_prefix_xfrm (enum uni_encoding_scheme a_enc,
			 uni_string a,
			 enum uni_encoding_scheme b_enc,
			 uni_string b)
{
  if (a_enc == b_enc)
    return uni_str_cmp_prefix (a_enc, a, b);
  else
    {
      while (1)
	{
	  t_unicode from_a;
	  t_unicode from_b;

	  from_a = uni_str_scan (a_enc, &a);

	  if (!from_a)
	    return 0;

	  from_b = uni_str_scan (b_enc, &b);

	  uni_invariant (!uni_is_noncharacter (from_a));
	  uni_invariant (!uni_is_noncharacter (from_b));

	  if (from_a < from_b)
	    return -1;
	  else if (from_b < from_a)
	    return 1;
	}
    }
}


int
uni_str_casecmp_xfrm (enum uni_encoding_scheme a_enc,
		      uni_string a,
		      enum uni_encoding_scheme b_enc,
		      uni_string b)
{
  if (a_enc == b_enc)
    return uni_str_casecmp (a_enc, a, b);
  else
    {
      while (1)
	{
	  t_unicode from_a;
	  t_unicode from_b;

	  from_a = uni_str_scan (a_enc, &a);
	  from_b = uni_str_scan (b_enc, &b);

	  uni_invariant (!uni_is_noncharacter (from_a));
	  uni_invariant (!uni_is_noncharacter (from_b));

	  from_a = unidata_to_lower (from_a);
	  from_b = unidata_to_lower (from_b);

	  if (from_a < from_b)
	    return -1;
	  else if (from_b < from_a)
	    return 1;
	  else if (!from_a)
	    return 0;
	}
    }
}


int
uni_str_casecmp_xfrm_n (enum uni_encoding_scheme a_enc,
			uni_string a, size_t a_l,
			enum uni_encoding_scheme b_enc,
			uni_string b, size_t b_l)
{
  if (a_enc == b_enc)
    return uni_str_casecmp_n (a_enc, a, a_l, b, b_l);
  else
    {
      size_t a_pos;
      size_t b_pos;
      size_t l;

      a_pos = 0;
      b_pos = 0;
      l = ((a_l < b_l) ? a_l : b_l);

      while (a_pos < l)
	{
	  t_unicode from_a;
	  t_unicode from_b;

	  from_a = uni_str_iscan (a_enc, a, &a_pos, a_l);
	  from_b = uni_str_iscan (b_enc, b, &b_pos, b_l);

	  uni_invariant (!uni_is_noncharacter (from_a));
	  uni_invariant (!uni_is_noncharacter (from_b));

	  from_a = unidata_to_lower (from_a);
	  from_b = unidata_to_lower (from_b);

	  if (from_a < from_b)
	    return -1;
	  else if (from_b < from_a)
	    return 1;
	  else if (!from_a)
	    return 0;
	}

      if (a_l < b_l)
	return -1;
      else if (b_l < a_l)
	return 1;
      else
	return 0;
    }
}


int
uni_str_casecmp_prefix_xfrm (enum uni_encoding_scheme a_enc,
			     uni_string a,
			     enum uni_encoding_scheme b_enc,
			     uni_string b)
{
  if (a_enc == b_enc)
    return uni_str_casecmp_prefix (a_enc, a, b);
  else
    {
      while (1)
	{
	  t_unicode from_a;
	  t_unicode from_b;

	  from_a = uni_str_scan (a_enc, &a);

	  if (!from_a)
	    return 0;

	  from_b = uni_str_scan (b_enc, &b);

	  uni_invariant (!uni_is_noncharacter (from_a));
	  uni_invariant (!uni_is_noncharacter (from_b));

	  from_a = unidata_to_lower (from_a);
	  from_b = unidata_to_lower (from_b);

	  if (from_a < from_b)
	    return -1;
	  else if (from_b < from_a)
	    return 1;
	}
    }
}



uni_string
uni_str_save_xfrm (alloc_limits limits,
		   enum uni_encoding_scheme out_enc,
		   enum uni_encoding_scheme str_enc,
		   uni_string str)
{
  if (out_enc == str_enc)
    {
      switch (str_enc)
	{
	default:
	  goto hard_case;

	case uni_iso8859_1:
	case uni_utf8:
	  return (uni_string)str_save (limits, (t_uchar *)str);

	case uni_utf16:
	  return (uni_string)unichar_str_save (limits, (t_unichar *)str);
	}
    }
  else
    {
      size_t out_len;
      uni_string answer;

    hard_case:

      out_len = uni_str_length_for_encoding (out_enc, str_enc, str) + 1;
      answer = (uni_string)lim_malloc (limits, out_len * uni_code_unit_width (out_enc));
      uni_str_cpy_xfrm (out_enc, answer, str_enc, str);
      return answer;
    }
}


uni_string
uni_str_save_xfrm_n (alloc_limits limits,
		     enum uni_encoding_scheme out_enc,
		     enum uni_encoding_scheme str_enc,
		     uni_string str,
		     size_t len)
{
  if (out_enc == str_enc)
    {
      switch (str_enc)
	{
	default:
	  goto hard_case;

	case uni_iso8859_1:
	case uni_utf8:
	  return (uni_string)str_save_n (limits, (t_uchar *)str, len);

	case uni_utf16:
	  return (uni_string)unichar_str_save_n (limits, (t_unichar *)str, len);
	}
    }
  else
    {
      size_t out_len;
      uni_string answer;

    hard_case:

      out_len = uni_str_length_for_encoding_n (out_enc, str_enc, str, len) + 1;
      answer = (uni_string)lim_malloc (limits, out_len * uni_code_unit_width (out_enc));
      uni_str_cpy_xfrm_n (out_enc, answer, str_enc, str, len);
      return answer;
    }
}


uni_string
uni_str_alloc_cat_xfrm (alloc_limits limits,
			enum uni_encoding_scheme out_enc,
			enum uni_encoding_scheme str1_enc,
			uni_string str1,
			enum uni_encoding_scheme str2_enc,
			uni_string str2)
{
  if ((out_enc == str1_enc) && (str1_enc == str2_enc))
    {
      switch (out_enc)
	{
	default:
	  goto hard_case;

	case uni_iso8859_1:
	case uni_utf8:
	  return (uni_string)str_alloc_cat (limits, (t_uchar *)str1, (t_uchar *)str2);

	case uni_utf16:
	  return (uni_string)unichar_str_alloc_cat (limits, (t_unichar *)str1, (t_unichar *)str2);
	}
    }
  else
    {
      size_t str1_size;
      size_t str2_size;
      size_t total_size;
      uni_string answer;

    hard_case:

      str1_size = uni_str_length_for_encoding (out_enc, str1_enc, str1);
      str2_size = uni_str_length_for_encoding (out_enc, str2_enc, str2);
      total_size = str1_size + str2_size + 1;

      answer = (uni_string)lim_malloc (limits, total_size * uni_code_unit_width (out_enc));
      uni_str_cpy_xfrm (out_enc, answer, str1_enc, str1);
      uni_str_cpy_xfrm (out_enc, uni_str_offset (out_enc, answer, str1_size), str2_enc, str2);
      return answer;
    }
}



alloc_cat_n_xfrm
