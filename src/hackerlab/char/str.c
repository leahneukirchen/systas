/* str.c - string functions
 *
 ****************************************************************
 * Copyright (C) 1998, 2000 Thomas Lord
 * 
 * See the file "COPYING" for further information about
 * the copyright and warranty status of this work.
 */



#include "hackerlab/mem/mem.h"
#include "hackerlab/char/char-class.h"
#include "hackerlab/char/str.h"


/************************************************************************
 *(h1 "String Functions")
 *
 * These functions operate on `strings', defined to be arrays of
 * 't_uchar' terminated by (and not otherwise containing)
 * the null character (`(t_uchar)0').
 * 
 * The functionality described in this function overlaps with some
 * of the functions in the standard C library defined by Posix,
 * but there are differences.  Read the documentation carefully if
 * you are replacing uses of Posix functions with Hackerlab functions.
 */
/*(menu)
 */

/************************************************************************
 *(h2 "String Length")
 * 
 * 
 * 
 */

/*(c str_length)
 * size_t str_length (const t_uchar * x);
 * 
 * Return the length of the 0-terminated string `x'.  The length does
 * not include the 0-byte itself.
 */
size_t
str_length (const t_uchar * x)
{
  size_t q;

  if (!x)
    return 0;
  q = 0;
  while (*x)
    ++x, ++q;

  return q; 
}


/*(c str_length_n)
 * size_t str_length_n (const t_uchar * x, size_t n);
 * 
 * Return the length of the 0-terminated string `x' but not more than
 * `n'.  The length does not include the 0-byte itself.
 * 
 * If `x' is longer than `n' bytes, return `n'.
 */
size_t
str_length_n (const t_uchar * x, size_t n)
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

/*(c str_hash_n)
 * unsigned long str_hash_n (const t_uchar * chr, size_t len);
 * 
 * Compute an `unsigned long' hash value for a string.
 */
unsigned long
str_hash_n (const t_uchar * chr, size_t len)
{
  unsigned long result;

  result = 0;
  while (len--)
    {
      result = *chr + (result << 3) + (result >> (8 * sizeof (result) - 3));
      ++chr;
    }

  return result;
}



/************************************************************************
 *(h2 "Simple String Searching")
 * 
 */

/*(c str_chr_index)
 * t_uchar * str_chr_index (const t_uchar * s, int c);
 * 
 * Return the position in 0-terminated string `s' of the first
 * occurence of `c'.  Return 0 if `c' does not occur in `s'.
 */
t_uchar *
str_chr_index (const t_uchar * s, int c)
{
  while (*s)
    {
      if (*s == c)
	return (t_uchar *)s;
      ++s;
    }
  if (c == 0)
    return (t_uchar *)s;
  return 0;
}


/*(c str_chr_rindex)
 * t_uchar * str_chr_rindex (const t_uchar * s, int c);
 * 
 * Return the position in 0-terminated string `s' of the last
 * occurence of `c'.  Return 0 if `c' does not occur in `s'.
 */
t_uchar *
str_chr_rindex (const t_uchar * s, int c)
{
  const t_uchar * best;

  best = 0;
  while (*s)
    {
      if (*s == c)
	best = s;
      ++s;
    }
  if (c == 0)
    return (t_uchar *)s;
  return (t_uchar *)best;
}


/*(c str_chr_index_n)
 * t_uchar * str_chr_index_n (const t_uchar * s, size_t n, int c);
 * 
 * Return the position in length `n' string `s' of the first occurence
 * of `c'.  Return 0 if `c' does not occur in `s'.
 */
t_uchar *
str_chr_index_n (const t_uchar * s, size_t n, int c)
{
  while (n)
    {
      if (*s == c)
	return (t_uchar *)s;
      if (!*s)
	return 0;
      ++s;
      --n;
    }
  return 0;
}


/*(c str_chr_rindex_n)
 * t_uchar * str_chr_rindex_n (const t_uchar * s, size_t n, int c);
 * 
 * Return the position in length `n' string `s' of the last occurence
 * of `c'.  Return 0 if `c' does not occur in `s'.
 */
t_uchar *
str_chr_rindex_n (const t_uchar * s, size_t n, int c)
{
  s += n - 1;
  while (n)
    {
      if (*s == c)
	return (t_uchar *)s;
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

/*(c str_cmp)
 * int str_cmp (const t_uchar * a, const t_uchar * b);
 * 
 * Compare strings `a' and `b' returning -1 if `a' is lexically first,
 * 0 if the two strings are equal, 1 if `b' is lexically first.
 */
int
str_cmp (const t_uchar * a, const t_uchar * b)
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


/*(c str_cmp_n)
 * int str_cmp_n (const t_uchar * a,
 *		  size_t a_l,
 *		  const t_uchar * b,
 *		  size_t b_l);
 * 
 * Compare strings `a' (length `a_l') and `b' (length `b_l') returning
 * -1 if `a' is lexically first, 0 if the two strings are equal, 1 if
 * `b' is lexically first.
 *
 */
int
str_cmp_n (const t_uchar * a, size_t a_l, const t_uchar * b, size_t b_l)
{
  size_t l;

  l = ((a_l < b_l) ? a_l : b_l);
  while (l)
    {
      t_uchar ac;
      t_uchar bc;

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


/*(c str_cmp_prefix)
 * int str_cmp_prefix (const t_uchar * prefix, const t_uchar * s);
 * 
 * Compare strings `prefix' and `s'.  If `prefix' is a prefix of `s',
 * return 0.  Otherwise, if `prefix' is lexically first, return -1;
 * if `s' is lexically first, return 1.
 */
int
str_cmp_prefix (const t_uchar * prefix, const t_uchar * s)
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

/*(c str_casecmp)
 * int str_casecmp (const t_uchar * a, const t_uchar * b);
 * 
 * Compare strings `a' and `b' ignoring case, returning -1 if `a' is
 * lexically first, 0 if the two strings are equal, 1 if `b' is
 * lexically first.
 */
int
str_casecmp (const t_uchar * a, const t_uchar * b)
{
  if ((!a || !*a) && (!b || !*b))
    return 0;

  if (!b || !*b)
    return 1;

  if (!a || !*a)
    return -1;

  while ((char_to_lower(*a) == char_to_lower(*b)) && *a)
    {
      ++a;
      ++b;
    }

  if (char_to_lower(*a) == char_to_lower(*b))
    return 0;
  else if (char_to_lower(*a) < char_to_lower(*b))
    return -1;
  else
    return 1;
}


/*(c str_casecmp_n)
 * int str_casecmp_n (const t_uchar * a, size_t a_l,
 *		      const t_uchar * b, size_t b_l);
 * 
 * Compare strings `a' (length `a_l') and `b' (length `b_l') ignoring
 * case returning -1 if `a' is lexically first, 0 if the two strings
 * are equal, 1 if `b' is lexically first.
 */
int
str_casecmp_n (const t_uchar * a, size_t a_l,
	       const t_uchar * b, size_t b_l)
{
  size_t l;
  l = ((a_l < b_l) ? a_l : b_l);
  while (l)
    {
      t_uchar ac;
      t_uchar bc;

      ac = char_to_lower(*a++);
      bc = char_to_lower(*b++);
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


/*(c str_casecmp_prefix)
 * int str_casecmp_prefix (const t_uchar * prefix, const t_uchar * s);
 * 
 * Compare strings `prefix' and `s', ignoring case.  If `prefix' is a
 * prefix of `s', return 0.  Otherwise, if `prefix' is lexically
 * first, return -1; if `s' is lexically first, return 1.
 */
int
str_casecmp_prefix (const t_uchar * prefix, const t_uchar * s)
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
      if (char_to_lower(*s) < char_to_lower(*prefix))
	return 1;
      else if (char_to_lower(*s) > char_to_lower(*prefix))
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

/*(c str_cpy)
 * t_uchar * str_cpy (t_uchar * to, const t_uchar * from);
 * 
 * Copy the 0-terminated string `from' to `to'.  `from' and `to'
 * should not overlap.
 *
 * Returns `to'.
 */
t_uchar *
str_cpy (t_uchar * to, const t_uchar * from)
{
  t_uchar * answer;
  answer = to;
  if (from)
    {
      while (*from)
	*to++ = *from++;
    }
  *to = 0;
  return answer;
}


/*(c str_cpy_n)
 * t_uchar * str_cpy_n (t_uchar * to,
 *			const t_uchar * from,
 *			size_t n);
 * 
 * Copy up-to `n' characters from `from' to `to'.  
 *
 * Add a final 0 to `to'.
 * 
 * \Warning:/ This function is different from `strncpy'.  `strncpy'
 * always stores exactly `n' characters in `to', padding the result
 * with 0 if a 0 character is encountered in `from' before `n'
 * characters are written.  This function stores up to `n+1' characters:
 * up to `n' non-0 characters from `from', plus a final 0.
 * 
 * Returns `to'.
 */
t_uchar *
str_cpy_n (t_uchar * to,
	   const t_uchar * from,
	   size_t n)
{
  t_uchar * answer;

  answer = to;
  if (from)
    {
      while (n && *from)
	{
	  *to++ = *from++;
	  --n;
	}
    }
  while (n--)
    *to++ = 0;
  return answer;
}


/************************************************************************
 *(h2 "String Concatenation")
 * 
 * 
 * 
 */

/*(c str_cat)
 * t_uchar * str_cat (t_uchar * to, const t_uchar * from);
 * 
 * Append the 0-terminated string `from' to the 0-terminated string
 * `to'.  The strings should not overlap.  
 *
 * Returns `to'.
 */
t_uchar *
str_cat (t_uchar * to, const t_uchar * from)
{
  t_uchar * answer;

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


/*(c str_cat_n)
 * t_uchar * str_cat_n (t_uchar * to,
 *			const t_uchar * from,
 *			size_t n);
 * 
 * Append at most `n' characters of the 0-terminated string `from' to
 * the 0-terminated string `to'.  The strings should not overlap.
 * Add a final 0 (thus writing up to `n + 1' characters in `to',
 * starting from the position of the final 0 in `to' on entry).
 *
 * Returns `to'.
 */
t_uchar *
str_cat_n (t_uchar * to,
	   const t_uchar * from,
	   size_t n)
{
  t_uchar * answer;

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


/*(c str_save)
 * t_uchar * strsav (alloc_limits limits, const t_uchar * str);
 * 
 * Allocate a copy of the 0-terminated string `str'.
 *
 * Allocate storage according to `limits'.  (See xref:"Allocation
 * With Limitations".)
 */
t_uchar *
str_save (alloc_limits limits, const t_uchar * str)
{
  t_uchar * it;
  size_t len;

  len = str_length (str);
  it = (t_uchar *)lim_malloc (limits, len + 1);
  if (!it)
    return 0;
  str_cpy (it, str);
  return it;
}


/*(c str_save_n)
 * t_uchar * str_save_n (alloc_limits limits,
 *                       const t_uchar * str,
 *                       size_t len);
 * 
 * Allocate a copy of the n-byte string `str'.
 * Add one byte to the new string and store 0 in that byte.
 *
 * Allocate storage according to `limits'.  (See xref:"Allocation
 * With Limitations".)
 */
t_uchar *
str_save_n (alloc_limits limits,
	    const t_uchar * str,
	    size_t len)
{
  t_uchar * it;

  it = (t_uchar *)lim_malloc (limits, len + 1);
  if (!it)
    return 0;
  mem_move (it, str, len);
  it[len] = 0;
  return it;
}


/*(c str_alloc_cat)
 * t_uchar * str_alloc_cat (alloc_limits limits,
 *                          const t_uchar * str1,
 *                          const t_uchar * str2);
 * 
 * Allocate a new string large enough to hold the concatenation of
 * 0-terminated strings `str1' and `str2' (including a final 0).
 * Initialize the new string with the concatenation of `str1' and
 * `str2'.
 *
 * Allocate storage according to `limits'.  (See xref:"Allocation
 * With Limitations".)
 */
t_uchar *
str_alloc_cat (alloc_limits limits,
	       const t_uchar * str1,
	       const t_uchar * str2)
{
  if (!str1 && !str2)
    return 0;
  if (!str1)
    return str_save (limits, str2);
  if (!str2)
    return str_save (limits, str1);
  {
    t_uchar * it;
    size_t len;

    len = str_length (str1) + str_length (str2) + 1;
    it = (t_uchar *)lim_malloc (limits, len);
    if (!it)
      return 0;
    str_cpy (it, str1);
    str_cat (it, str2);
    return it;
  }
}


/*(c str_alloc_cat_n)
 * t_uchar * str_alloc_cat_n (alloc_limits limits,
 *                            const t_uchar * str1,
 *                            const t_uchar * str2,
 *			      size_t n);
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
t_uchar *
str_alloc_cat_n (alloc_limits limits,
		 const t_uchar * str1,
		 const t_uchar * str2,
		 size_t n)
{
  if (!str1 && !str2)
    return 0;
  if (!str1)
    return str_save_n (limits, str2, n);
  if (!str2)
    return str_save (limits, str1);
  {
    t_uchar * it;
    size_t len;

    len = str_length (str1) + str_length_n (str2, n) + 1;
    it = (t_uchar *)lim_malloc (limits, len);
    if (!it)
      return 0;
    str_cpy (it, str1);
    str_cat_n (it, str2, n);
    return it;
  }
}



/*(c str_realloc_cat)
 * t_uchar * str_realloc_cat (alloc_limits limits,
 *                            t_uchar * str1,
 *                            const t_uchar * str2);
 * 
 * Reallocate `str1`' to be large enough to hold the concatenation of
 * 0-terminated strings `str1' and `str2' (including a final 0).
 * Initialize the new string with the concatenation of `str1' and
 * `str2'.
 *
 * Allocate storage according to `limits'.  (See xref:"Allocation
 * With Limitations".)
 */
t_uchar *
str_realloc_cat (alloc_limits limits,
		 t_uchar * str1,
		 const t_uchar * str2)
{
  if (!str1 && !str2)
    return 0;

  if (!str1)
    return str_save (limits, str2);

  if (!str2)
    return str1;

  {
    t_uchar * it;
    size_t len;

    len = str_length (str1) + str_length (str2) + 1;
    it = (t_uchar *)lim_realloc (limits, str1, len);
    if (!it)
      return 0;
    str_cat (it, str2);
    return it;
  }
}


/*(c str_realloc_cat_n)
 * t_uchar * str_realloc_cat_n (alloc_limits limits,
 *                              t_uchar * str1,
 *                              const t_uchar * str2,
 *			        size_t n);
 * 
 * Reallocate `str' to be large enough to hold the
 * concatenation of 0-terminated strings `str1' and `str2',
 * considering at most `n' characters from `str2'.
 * 
 * Initialize the new string with the concatenation of `str1' and
 * up to `n' characters of `str2'.  Append a final 0.
 *
 * Allocate storage according to `limits'.  (See xref:"Allocation
 * With Limitations".)
 */
t_uchar *
str_realloc_cat_n (alloc_limits limits,
		   t_uchar * str1,
		   const t_uchar * str2,
		   size_t n)
{
  if (!str1 && !str2)
    return 0;

  if (!str1)
    return str_save_n (limits, str2, n);

  if (!str2)
    return str1;

  {
    t_uchar * it;
    size_t len;

    len = str_length (str1) + str_length_n (str2, n) + 1;
    it = (t_uchar *)lim_realloc (limits, str1, len);
    if (!it)
      return 0;
    str_cat_n (it, str2, n);
    return it;
  }
}


