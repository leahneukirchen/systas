/* tag: Tom Lord Tue Dec  4 14:41:42 2001 (unichar-str.c)
 */
/* unichar-str.c -
 *
 ****************************************************************
 * Copyright (C) 2000 Tom Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */


#include "hackerlab/mem/mem.h"
#include "hackerlab/hash/hash-utils.h"
#include "hackerlab/unidata/unidata.h"
#include "hackerlab/unicode/unichar-str.h"


/************************************************************************
 *(h1 "UTF-16 String Processing")
 * 
 * 
 * 
 */


/************************************************************************
 *(h2 "String Length")
 * 
 * 
 * 
 */

/*(c unichar_str_length)
 * size_t unichar_str_length (const t_unichar * x);
 * 
 * Return the length of the 0-terminated string `x'.  The length does
 * not include the 0-byte itself.
 */
size_t
unichar_str_length (const t_unichar * x)
{
  size_t q;

  if (!x)
    return 0;
  q = 0;
  while (*x)
    ++x, ++q;

  return q; 
}


/*(c unichar_str_length_n)
 * size_t unichar_str_length_n (const t_unichar * x, size_t n);
 * 
 * Return the length of the 0-terminated string `x' but not more than
 * `n'.  The length does not include the 0-byte itself.
 * 
 * If `x' is longer than `n' code units, return `n'.
 */
size_t
unichar_str_length_n (const t_unichar * x, size_t n)
{
  size_t q;

  if (!x)
    return 0;
  q = 0;
  while ((q < n) && *x)
    ++x, ++q;

  return q; 
}


/************************************************************************
 *(h2 "Computing Hash Values From Strings")
 * 
 */

/*(c unichar_str_hash_n)
 * unsigned long unichar_str_hash_n (const t_unichar * chr, size_t len);
 * 
 * Compute an `unsigned long' hash value for a string.
 */
unsigned long
unichar_str_hash_n (const t_unichar * chr, size_t len)
{
  return hash_mem ((t_uchar *)chr, len * sizeof (t_unichar));
}



/************************************************************************
 *(h2 "Simple String Searching")
 * 
 */

/*(c unichar_str_chr_index)
 * t_unichar * unichar_str_chr_index (const t_unichar * s, int c);
 * 
 * Return the position in 0-terminated string `s' of the first
 * occurence of `c'.  Return 0 if `c' does not occur in `s'.
 */
t_unichar *
unichar_str_chr_index (const t_unichar * s, fix this -- should be t_unicode int c)
{
  while (*s)
    {
      broken -- should look for code points!
      if (*s == c)
	return (t_unichar *)s;
      ++s;
    }
  if (c == 0)
    return (t_unichar *)s;
  return 0;
}


/*(c unichar_str_chr_rindex)
 * t_unichar * unichar_str_chr_rindex (const t_unichar * s, int c);
 * 
 * Return the position in 0-terminated string `s' of the last
 * occurence of `c'.  Return 0 if `c' does not occur in `s'.
 */
t_unichar *
unichar_str_chr_rindex (const t_unichar * s, int c)
{
  const t_unichar * best;

  best = 0;
  while (*s)
    {
      if (*s == c)
	best = s;
      ++s;
    }
  if (c == 0)
    return (t_unichar *)s;
  return (t_unichar *)best;
}


/*(c unichar_str_chr_index_n)
 * t_unichar * unichar_str_chr_index_n (const t_unichar * s, size_t n, int c);
 * 
 * Return the position in length `n' string `s' of the first occurence
 * of `c'.  Return 0 if `c' does not occur in `s'.
 */
t_unichar *
unichar_str_chr_index_n (const t_unichar * s, size_t n, int c)
{
  while (n)
    {
      if (*s == c)
	return (t_unichar *)s;
      if (!*s)
	return 0;
      ++s;
      --n;
    }
  return 0;
}


/*(c unichar_str_chr_rindex_n)
 * t_unichar * unichar_str_chr_rindex_n (const t_unichar * s, size_t n, int c);
 * 
 * Return the position in length `n' string `s' of the last occurence
 * of `c'.  Return 0 if `c' does not occur in `s'.
 */
t_unichar *
unichar_str_chr_rindex_n (const t_unichar * s, size_t n, int c)
{
  s += n - 1;
  while (n)
    {
      if (*s == c)
	return (t_unichar *)s;
      --s;
      --n;
    }
  return 0;
}



/************************************************************************
 *(h2 "String Comparisons")
 * 
 * 
 * 
 */

/*(c unichar_str_cmp)
 * int unichar_str_cmp (const t_unichar * a, const t_unichar * b);
 * 
 * Compare strings `a' and `b' returning -1 if `a' is lexically first,
 * 0 if the two strings are equal, 1 if `b' is lexically first.
 */
int
unichar_str_cmp (const t_unichar * a, const t_unichar * b)
{
  if ((!a || !*a) && (!b || !*b))
    return 0;

  if (!b || !*b)
    return 1;

  if (!a || !a)
    return -1;

  while ((*a == *b) && *a)
    {
      ++a;
      ++b;
    }

  if (*a == *b)
    return 0;
  else if (*a < *b)
    return -1;
  else
    return 1;
}


/*(c unichar_str_cmp_n)
 * int unichar_str_cmp_n (const t_unichar * a, size_t a_l, const t_unichar * b, size_t b_l);
 * 
 * Compare strings `a' (length `a_l') and `b' (length `b_l') returning
 * -1 if `a' is lexically first, 0 if the two strings are equal, 1 if
 * `b' is lexically first.
 *
 */
int
unichar_str_cmp_n (const t_unichar * a, size_t a_l, const t_unichar * b, size_t b_l)
{
  size_t l;

  l = ((a_l < b_l) ? a_l : b_l);
  while (l)
    {
      t_unichar ac;
      t_unichar bc;

      ac = *a++;
      bc = *b++;
      if (ac < bc)
	return -1;
      else if (ac > bc)
	return 1;
      else if (!ac)
	return 0;
      --l;
    }
  if (a_l < b_l)
    return -1;
  else if (b_l < a_l)
    return 1;
  else
    return 0;
}


/*(c unichar_str_cmp_prefix)
 * int unichar_str_cmp_prefix (const t_unichar * prefix, const t_unichar * s);
 * 
 * Compare strings `prefix' and `s'.  If `prefix' is a prefix of `s',
 * return 0.  Otherwise, if `prefix' is lexically first, return -1;
 * if `s' is lexically first, return 1.
 */
int
unichar_str_cmp_prefix (const t_unichar * prefix, const t_unichar * s)
{
  if (!prefix)
    return 0;

  if (!s)
    {
      if (!*prefix)
	return 0;
      else
	return 1;
    }

  while (*prefix)
    {
      if (*s < *prefix)
	return 1;
      else if (*s > *prefix)
	return -1;
      else
	++prefix, ++s;
    }
  return 0;
}


/************************************************************************
 *(h2 "String Comparisons Ignoring Case")
 * 
 */

/*(c unichar_str_casecmp)
 * int unichar_str_casecmp (const t_unichar * a, const t_unichar * b);
 * 
 * Compare strings `a' and `b' ignoring case, returning -1 if `a' is
 * lexically first, 0 if the two strings are equal, 1 if `b' is
 * lexically first.
 */
int
unichar_str_casecmp (const t_unichar * a, const t_unichar * b)
{
  if ((!a || !*a) && (!b || !*b))
    return 0;

  if (!b || !*b)
    return 1;

  if (!a || !*a)
    return -1;

  while ((unidata_to_lower(*a) == unidata_to_lower(*b)) && *a)
    {
      ++a;
      ++b;
    }

  if (unidata_to_lower(*a) == unidata_to_lower(*b))
    return 0;
  else if (unidata_to_lower(*a) < unidata_to_lower(*b))
    return -1;
  else
    return 1;
}


/*(c unichar_str_casecmp_n)
 * int unichar_str_casecmp_n (const t_unichar * a, size_t a_l,
 *		      const t_unichar * b, size_t b_l);
 * 
 * Compare strings `a' (length `a_l') and `b' (length `b_l') ignoring
 * case returning -1 if `a' is lexically first, 0 if the two strings
 * are equal, 1 if `b' is lexically first.
 */
int
unichar_str_casecmp_n (const t_unichar * a, size_t a_l,
	       const t_unichar * b, size_t b_l)
{
  size_t l;
  l = ((a_l < b_l) ? a_l : b_l);
  while (l)
    {
      t_unichar ac;
      t_unichar bc;

      ac = unidata_to_lower(*a++);
      bc = unidata_to_lower(*b++);
      if (ac < bc)
	return -1;
      else if (ac > bc)
	return 1;
      else if (!ac)
	return 0;
      --l;
    }
  if (a_l < b_l)
    return -1;
  else if (b_l < a_l)
    return 1;
  else
    return 0;
}


/*(c unichar_str_casecmp_prefix)
 * int unichar_str_casecmp_prefix (const t_unichar * prefix, const t_unichar * s);
 * 
 * Compare strings `prefix' and `s', ignoring case.  If `prefix' is a
 * prefix of `s', return 0.  Otherwise, if `prefix' is lexically
 * first, return -1; if `s' is lexically first, return 1.
 */
int
unichar_str_casecmp_prefix (const t_unichar * prefix, const t_unichar * s)
{
  if (!prefix)
    return 0;

  if (!s)
    {
      if (!*prefix)
	return 0;
      else
	return 1;
    }
  while (*prefix)
    {
      if (unidata_to_lower(*s) < unidata_to_lower(*prefix))
	return 1;
      else if (unidata_to_lower(*s) > unidata_to_lower(*prefix))
	return -1;
      else
	++prefix, ++s;
    }
  return 0;
}


/************************************************************************
 *(h2 "String Copying")
 * 
 */

/*(c unichar_str_cpy)
 * t_unichar * unichar_str_cpy (t_unichar * to, const t_unichar * from);
 * 
 * Copy the 0-terminated string `from' to `to'.  `from' and `to'
 * should not overlap.
 *
 * Returns `to'.
 */
size_t fix this
unichar_str_cpy (t_unichar * to, const t_unichar * from)
{
  t_unichar * answer;
  answer = to;
  if (from)
    {
      while (*from)
	*to++ = *from++;
    }
  *to = 0;
  return answer;
}


/*(c unichar_str_cpy_n)
 * t_unichar * unichar_str_cpy_n (t_unichar * to,
 *			const t_unichar * from,
 *			size_t n);
 * 
 * Copy up-to `n' characters from `from' to `to'.  
 *
 * \Warning:/ This function is different from `strncpy'.  `strncpy'
 * always stores exactly `n' characters in `to', padding the result
 * with 0 if a 0 character is encountered in `from' before `n'
 * characters are written.  This function stores up to `n+1' characters:
 * up to `n' non-0 characters from `from', plus a final 0.
 * 
 * Returns `to'.
 */
void
unichar_str_cpy_n (t_unichar * to,
	   const t_unichar * from,
	   size_t n)
{
  t_unichar * answer;

  answer = to;
  if (from)
    {
      while (n && *from)
	{
	  *to++ = *from++;
	  --n;
	}
    }
  *to++ = 0;
  return answer;
}


/************************************************************************
 *(h2 "String Concatenation")
 * 
 * 
 * 
 */

/*(c unichar_str_cat)
 * t_unichar * unichar_str_cat (t_unichar * to, const t_unichar * from);
 * 
 * Append the 0-terminated string `from' to the 0-terminated string
 * `to'.  The strings should not overlap.  
 *
 * Returns `to'.
 */
size_t fix this
unichar_str_cat (t_unichar * to, const t_unichar * from)
{
  t_unichar * answer;

  answer = to;

  if (from)
    {
      while (*to)
	++to;

      while (*from)
	*to++ = *from++;

      *to = 0;
    }

  return answer;
}


/*(c unichar_str_cat_n)
 * t_unichar * unichar_str_cat_n (t_unichar * to,
 *			const t_unichar * from,
 *			size_t n);
 * 
 * Append at most `n' characters of the 0-terminated string `from' to
 * the 0-terminated string `to'.  The strings should not overlap.
 * Add a final 0 (thus writing up to `n + 1' characters in `to',
 * starting from the position of the final 0 in `to' on entry).
 *
 * Returns `to'.
 */
t_unichar * 
unichar_str_cat_n (t_unichar * to,
	   const t_unichar * from,
	   size_t n)
{
  t_unichar * answer;

  answer = to;

  while (*to)
    ++to;

  while (n && *from)
    {
      *to++ = *from++;
      --n;
    }

  *to = 0;
  return answer;
}



/************************************************************************
 *(h2 "String Allocators")
 * 
 * 
 */


/*(c unichar_str_save)
 * t_unichar * strsav (alloc_limits limits, const t_unichar * str);
 * 
 * Allocate a copy of the 0-terminated string `str'.
 *
 * Allocate storage according to `limits'.  (See xref:"Allocation
 * With Limitations".)
 */
t_unichar *
unichar_str_save (alloc_limits limits, const t_unichar * str)
{
  t_unichar * it;
  size_t len;

  len = unichar_str_length (str);
  it = (t_unichar *)lim_malloc (limits, sizeof (t_unichar) * (len + 1));
  if (!it)
    return 0;
  unichar_str_cpy (it, str);
  return it;
}


/*(c unichar_str_save_n)
 * t_unichar * unichar_str_save_n (alloc_limits limits,
 *                       	   const t_unichar * str,
 *                       	   size_t len);
 * 
 * Allocate a copy of the n-byte string `str'.
 * Add one byte to the new string and store 0 in that byte.
 *
 * Allocate storage according to `limits'.  (See xref:"Allocation
 * With Limitations".)
 */
t_unichar *
unichar_str_save_n (alloc_limits limits,
		    const t_unichar * str,
		    size_t len)
{
  t_unichar * it;

  it = (t_unichar *)lim_malloc (limits, sizeof (t_unichar) * (len + 1));
  if (!it)
    return 0;
  mem_move ((t_uchar *)it, (t_uchar *)str, len);
  it[len] = 0;
  return it;
}


/*(c unichar_str_alloc_cat)
 * t_unichar * unichar_str_alloc_cat (alloc_limits limits,
 *                          const t_unichar * str1,
 *                          const t_unichar * str2);
 * 
 * Allocate a new string large enough to hold the concatenation of
 * 0-terminated strings `str1' and `str2' (including a final 0).
 * Initialize the new string with the concatenation of `str1' and
 * `str2'.
 *
 * Allocate storage according to `limits'.  (See xref:"Allocation
 * With Limitations".)
 */
t_unichar *
unichar_str_alloc_cat (alloc_limits limits,
	       const t_unichar * str1,
	       const t_unichar * str2)
{
  if (!str1 && !str2)
    return 0;
  if (!str1)
    return unichar_str_save (limits, str2);
  if (!str2)
    return unichar_str_save (limits, str1);
  {
    t_unichar * it;
    size_t len;

    len = unichar_str_length (str1) + unichar_str_length (str2) + 1;
    it = (t_unichar *)lim_malloc (limits, sizeof (t_unichar) * len);
    if (!it)
      return 0;
    unichar_str_cpy (it, str1);
    unichar_str_cat (it, str2);
    return it;
  }
}


/*(c unichar_str_alloc_cat_n)
 * t_unichar * unichar_str_alloc_cat (alloc_limits limits,
 *                          const t_unichar * str1,
 *                          const t_unichar * str2,
 *			    size_t n);
 * 
 * Allocate a new 0-terminated string large enough to hold the
 * concatenation of 0-terminated strings `str1' and `str2',
 * considering at most `n' characters from `str2'.
 * 
 * Initialize the new string with the concatenation of `str1' and
 * up to `n' characters of `str2'.  Append a final 0.
 *
 * Allocate storage according to `limits'.  (See xref:"Allocation
 * With Limitations".)
 */
t_unichar *
unichar_str_alloc_cat_n (alloc_limits limits,
			 const t_unichar * str1,
			 const t_unichar * str2,
			 size_t n)
{
  if (!str1 && !str2)
    return 0;
  if (!str1)
    return unichar_str_save_n (limits, str2, n);
  if (!str2)
    return unichar_str_save (limits, str1);
  {
    t_unichar * it;
    size_t len;

    len = unichar_str_length (str1) + unichar_str_length_n (str2, n) + 1;
    it = (t_unichar *)lim_malloc (limits, sizeof (t_unichar) * len);
    if (!it)
      return 0;
    unichar_str_cpy (it, str1);
    unichar_str_cat_n (it, str2, n);
    return it;
  }
}

