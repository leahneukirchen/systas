/* strings.c - Strings
 *
 ****************************************************************
 * Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.
 * 
 * This software comes with NO WARRANTY.
 * This software is publicly licensed under the terms of
 * the GNU General Public License, version 2, which is included
 * with the source code.
 */



#include <stddef.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "systas/libsystas/strings.h"
#include "systas/libsystas/boolean.h"
#include "systas/libsystas/error.h"
#include "systas/libsystas/list.h"
#include "systas/libsystas/pairs.h"
#include "systas/libsystas/gc.h"
#include "systas/libsystas/ports.h"
#include "systas/libsystas/vectors.h"
#include "systas/libsystas/chars.h"
#include "systas/libsystas/symbols.h"
#include "systas/libsystas/numbers.h"



SCM_SYMBOL (s_scm2argv, "scm2argv"); 
SCM_SYMBOL (s_make_string_internal, "!!!make-string-internal"); 


/************************************************************************
 *(h0 "Strings")
 *
 * Strings are sequences of characters.  Ordinary strings are written
 * using double-quotes:
 * 
 *	"Strings are used for many purposes in Emacs"
 * 
 * Strings can include newlines:
 * 
 *	"Strings are used for many purposes in Emacs, as can be
 *	expected in a text editor;..."
 * 
 * Strings can include special "escape sequences" that represent
 * individual characters:
 * 
 *	"as can be\nexpected in a text editor;..."
 * 
 * The supported escape seqences are:
 * 
 *	Escape sequence:                        Character:
 *      
 *	\0                                      #\null
 *	\f                                      #\ack
 *	\n                                      #\newline
 *	\r                                      #\return
 *	\t                                      #\tab
 *	\a                                      #\bel
 *	\v                                      #\vt
 * 
 * The characters within a string are addressed by integers, 
 * starting with 0.  For example:
 * 
 * 	(string-ref "abc" 1) => #\b
 * 
 * Characters are also addressed by negative integers, counting
 * backwards from the last character in the string, starting with -1:
 * 
 * 	(string-ref "abc" -1) => #\c
 * 
 * Several string functions address substrings of a string.  This is
 * always done with a starting and ending position.  All characters
 * from `[start...end-1]' are included in the substring.  If `end <=
 * start', the substring is empty.
 * 
 */

/*(c string?)
 * (string? obj)
 * 
 * Return `#t' if `obj' is a string, `#f' otherwise.
 */
SCM_PROC(s_string_p, "string?", 1, 0, 0, scm_string_p);
SCM
scm_string_p (SCM x)
{
  SCM_INTS_UNKNOWN;

  return scm_is_string (x) ? SCM_BOOL_T : SCM_BOOL_F;
}

/*(menu)
 */

/************************************************************************
 *(h1 "Symbols: Read-only Strings")
 *
 * If a string function does not modify a string argument, generally a
 * symbol may be passed for that argument.  The print name of the
 * symbol is used as the string.
 *
 * When the documentation for a particular function says that an
 * argument may be a "read-only string", that means that the argument
 * may be an ordinary string, a symbol, or a shared substring of a
 * string or symbol.
 *
 */


/************************************************************************
 *(h1 "Constructing Strings")
 *
 */

/*(c string)
 * (string . args)
 * 
 * Combine the arguments into a string.
 *
 * The arguments may be characters or read-only strings.
 */
SCM_PROC(s_string, "string", 0, 0, 1, scm_string);
SCM
scm_string (SCM chrs)
{
  SCM_INTS_ENABLED;
  SCM res;
  char *data;
  long i;
  long len;
  int type_error;
  SCM errobj;
  SCM errloc;


  SCM_DEFER_INTS;
  type_error = 0;
  i = scm_ilength (chrs);
  if (i < 0)
    {
      type_error = 1;
      errobj = chrs;
      errloc = scm_arg1;
      goto signal_error;
    }
  len = 0;
  {
    SCM s;

    for (len = 0, s = chrs; s != SCM_EOL; s = SCM_CDR (s))
      if (scm_is_char (SCM_CAR (s)))
	len += 1;
      else if (scm_is_ro_string (SCM_CAR (s)))
	len += SCM_RO_LENGTH (SCM_CAR (s));
      else
	{
	  type_error = 1;
	  errobj = s;
	  errloc = scm_arg1;
	  goto signal_error;
	}
  }
  res = scm_makstr (len);
  data = SCM_STRING_CHARS (res);
  for ( ; SCM_EOL != chrs; chrs = SCM_CDR (chrs))
    {
      if (scm_is_char (SCM_CAR (chrs)))
	*data++ = scm_char_to_int (SCM_CAR (chrs));
      else 
	{
	  int l;
	  char * c;
	  l = SCM_RO_LENGTH (SCM_CAR (chrs));
	  c = SCM_RO_CHARS (SCM_CAR (chrs));
	  while (l)
	    {
	      --l;
	      *data++ = *c++;
	    }
	}
    }
 signal_error:
  SCM_ALLOW_INTS;
  if (type_error)
    {
      SCM_ASSERT (0, errobj, errloc, s_string);
    }
  return res;
}

/*(c string-copy)
 * (string-copy str)
 * 
 * Construct a new copy of `str'.
 */


/*(c make-string)
 * (make-string n :optional char)
 * 
 * Return a new string with `n' characters, optionally all initialized
 * to be equal to `char'.
 *
 */
SCM_PROC(s_make_string, "make-string", 1, 1, 0, scm_make_string);
SCM
scm_make_string (SCM k, SCM chr)
{
  SCM_INTS_ENABLED;
  SCM res;
  char *dst;
  long i;

  SCM_ASSERT (SCM_INUMP (k) && (k >= 0), k, scm_arg1, s_make_string);
  i = SCM_INUM (k);
  SCM_ASSERT ((i < SCM_LENGTH_MAX), k, scm_arg1, s_make_string);
  res = scm_makstr (i);
  dst = SCM_STRING_CHARS (res);
  if (scm_is_char (chr))
    {
      char c;

      c = scm_char_to_int (chr);
      for (i--;i >= 0;i--)
	{
	  dst[i] = c;
	}
    }
  return res;
}


/*(c substring)
 * (substring string :optional from to)
 * 
 * Return a new string which is a substring of `string'.  The new
 * string is `(- to from)' characters long and contains characters
 * from `string' beginning at `from'.
 *
 * If `from' is omitted, it defaults to 0.
 * 
 * If `to' is omitted, it defaults to `(string-length string)'.
 * 
 */
SCM_PROC(s_substring, "substring", 1, 2, 0, scm_substring);
SCM
scm_substring (SCM str, SCM start, SCM end)
{
  SCM_INTS_ENABLED;
  long l;
  int s;
  int e;

  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg1, s_substring);

  if ((start == SCM_BOOL_F) || (start == SCM_UNDEFINED))
    s = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (start), start, scm_arg2, s_substring);
      s = SCM_INUM (start);
      if (s < 0)
	s += SCM_RO_LENGTH (str);
    }

  if ((end == SCM_BOOL_F) || (end == SCM_UNDEFINED))
    e = SCM_RO_LENGTH (str);
  else
    {
      SCM_ASSERT (SCM_INUMP (end), end, scm_arg3, s_substring);
      e = SCM_INUM (end);
      if (e < 0)
	e += SCM_RO_LENGTH (str);
    }

  SCM_ASSERT ((0 <= s) && (s <= SCM_RO_LENGTH (str)), start, scm_outofrange, s_substring);
  SCM_ASSERT ((s <= e) && (e <= SCM_RO_LENGTH (str)), end, scm_outofrange, s_substring);
  l = e - s;
  return scm_makfromstr (&SCM_RO_CHARS (str)[s], (size_t)l);
}



/************************************************************************
 *(h1 "Shared Substrings")
 *
 * Whenever you extract a substring using the procedure `substring', a
 * new string is allocated and substring data is copied from the old
 * string to the new string.
 * 
 * Sometimes, in order avoid the expense of copying or in order to
 * specify mutations on just part of a larger string, programmers will
 * write programs that pass around three values at a time: a string, a
 * starting position within the string, and an ending position.  This
 * convention is awkward, especially because many useful string
 * primitives operate only on full strings, not a string with
 * substring indexes.  To make matters worse, when programmers use
 * this convention, all too often some functions wind up operating on
 * string-start-end and other functions operate on
 * string-start-length.
 * 
 * Systas Scheme offers a bettter way: shared substrings.  A shared
 * substring is a string-like object built from an older string
 * without copying any data.  A shared substring is not a copy of the
 * older string, but shares the older string's data directly.
 * Compared to copied substrings, shared substrings are cheap to
 * allocate.
 * 
 * On the other hand, holding on to a small shared substring of a
 * large string prevents the large string from being garbage collected
 * and so may result in wasted space.
 * 
 * It may be helpful to describe the internal representation of shared
 * substrings.  An ordinary string in Systas Scheme is represented by
 * two values: an integer that records the number of characters allocated
 * to the string, and a pointer that points to the null-terminated
 * string data.  A shared substring consists of an integer length, an
 * integer offset, and a pointer to an ordinary string.  There is not
 * much difference between passing shared substrings and passing three
 * parameters (string, offset, end-pos) except that using shared
 * substrings simplifies programming by keeping track of two of the
 * three parameters automatically.
 * 
 * Since a symbol is a kind of read-only string, a shared substring of
 * a symbol is another kind of read-only string.
 */


/*(c shared-substring?)
 * (shared-substring? string)
 * 
 * Return `#t' if `string' is a shared substring, `#f' otherwise.
 */
SCM_PROC (s_shared_substring_p, "shared-substring?", 1, 0, 0, scm_shared_substring_p);
SCM
scm_shared_substring_p (SCM str)
{
  return scm_int_to_bool (scm_is_ro_substr (str));
}


/*(c make-shared-substring)
 * (make-shared-substring string :optional from to)
 * 
 * Return a new shared substring of read-only string, `string'.  The
 * substring will span from `from' (or 0) to `to' (or `(string-length
 * string)').
 *
 * The shared substring and `string' share data -- modifications to
 * one can effect the other.
 */
SCM_PROC(s_make_shared_substring, "make-shared-substring", 1, 2, 0, scm_make_shared_substring);
SCM
scm_make_shared_substring (SCM str, SCM frm, SCM to)
{
  SCM_INTS_ENABLED;
  long f;
  long t;
  SCM answer;
  SCM start_str;

  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg1, s_make_shared_substring);

  if ((frm == SCM_UNDEFINED) || (frm == SCM_BOOL_F))
    frm = SCM_MAKINUM (0);
  else
    SCM_ASSERT (SCM_INUMP (frm), frm, scm_arg2, s_make_shared_substring);

  if ((to == SCM_UNDEFINED) || (to == SCM_BOOL_F))
    to = SCM_MAKINUM (SCM_RO_LENGTH (str));
  else
    SCM_ASSERT (SCM_INUMP (to), to, scm_arg3, s_make_shared_substring);

  f = SCM_INUM (frm);
  t = SCM_INUM (to);
  if (f < 0)
    f = SCM_RO_LENGTH (str) + f;
  if (t < 0)
    t = SCM_RO_LENGTH (str) + t;
  SCM_ASSERT ((f >= 0), frm, scm_outofrange, s_make_shared_substring);
  SCM_ASSERT ((f <= t) && (t <= SCM_RO_LENGTH (str)), to, scm_outofrange, s_make_shared_substring);

  SCM_NEWCELL (answer);
  SCM_NEWCELL (start_str);

  SCM_DEFER_INTS;
  if (scm_is_ro_substr (str))
    {
      long offset;
      offset = SCM_INUM (SCM_SUBSTR_OFFSET (str));
      f += offset;
      t += offset;
      SCM_CAR (start_str) = SCM_MAKINUM (f);
      SCM_CDR (start_str) = SCM_SUBSTR_STR (str);
      SCM_CDR (answer) = start_str;
      SCM_SET_LENGTH (answer, t - f, SCM_TYP7 (str));
    }
  else
    {
      int str_type;
      int bits;
      SCM_CAR (start_str) = SCM_MAKINUM (f);
      SCM_CDR (start_str) = str;
      SCM_CDR (answer) = start_str;
      str_type = SCM_TYP7 (str);
      bits = scm_tc7_shared_substring_flag | (str_type & (scm_tc7_writable_flag | scm_tc7_static_allocation_flag));
      SCM_SET_LENGTH (answer, t - f, scm_tc7_base_string_tag | bits);
    }
  SCM_ALLOW_INTS;
  return answer;
}


/*(c shared-substring-parts)
 * (shared-substring-parts string)
 * 
 * If `string' is a shared substring, return a list:
 *
 *	(original-string from to)
 *
 * which describes what `string' is a substring of.
 *
 * If `string' is not a shared substring, return a list:
 *
 *	(string 0 ,(string-length string))
 */
SCM_PROC (s_shared_substring_parts, "shared-substring-parts", 1, 0, 0, scm_shared_substring_parts);
SCM
scm_shared_substring_parts (SCM str)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg1, s_shared_substring_parts);

  if (!scm_is_ro_substr (str))
    return scm_listify (str, SCM_INUM0, SCM_MAKINUM (SCM_RO_LENGTH (str)), SCM_UNDEFINED);
  else
    {
      int offset;
      int len;

      offset = SCM_INUM (SCM_SUBSTR_OFFSET (str));
      len = SCM_RO_LENGTH (str);
      return scm_listify (SCM_SUBSTR_STR(str),
			  SCM_SUBSTR_OFFSET (str),
			  SCM_MAKINUM (offset + len),
			  SCM_UNDEFINED);
    }
}




/************************************************************************
 *(h1 "The String Lattice")
 *
 * There are, in fact, three string-like types in Systas Scheme,
 * described by these three predicates:
 * 
 * 	read-only-string?
 * 	string? (aka writable-string?)
 *	basic-string?
 * 
 * Each successive line in the list includes the preceeding types:
 * every writable-string or string is also a read-only-string; every
 * basic-string is also a writable-string, string and
 * read-only-string.
 *
 * The reverse is not true: some read-only-strings are *not*
 * writable-strings.  Some writable-strings are not basic-strings.
 *
 * A basic-string is an object created by a procedure such as
 * `list->string' or `string'.  A string (or writable-string) is any
 * of those object, or any shared substring of a string.  A read-only
 * string is any string, or a symbol, or a shared substring of a
 * symbol.
 * 
 * As a general rule, if a string procedure does not modify its
 * argument, then it will accept any read-only string.  If a string
 * procedure does modify its argument, then it won't accept some
 * read-only strings (e.g. symbols), but may accept writable-strings
 * which are not basic-strings.
 * 
 * For example, this code works in Systas Scheme because symbols are
 * read-only-strings:
 * 
 * 	(string-append 'hello " " 'world) => "hello world"
 */

/*(c read-only-string?)
 * (read-only-string? obj)
 * 
 * Return `#t' if `obj' is a read-only string, `#f' otherwise.
 *
 * All string-like types are read-only strings: symbols, strings, and
 * shared substrings of both symbols and strings.
 */
SCM_PROC(s_read_only_string_p, "read-only-string?", 1, 0, 0, scm_read_only_string_p);
SCM
scm_read_only_string_p (SCM x)
{
  SCM_INTS_UNKNOWN;

  return scm_is_ro_string (x) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c writable-string?)
 * (writable-string? obj)
 * 
 * Return `#t' if `obj' is a writable string, `#f' otherwise.
 *
 * Writable strings are either ordinary strings or shared substrings
 * of ordinary strings.
 */
SCM_PROC (s_writable_string_p, "writable-string?", 1, 0, 0, scm_writable_string_p);
SCM
scm_writable_string_p (SCM x)
{
  SCM_INTS_UNKNOWN;

  return scm_is_string (x) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c basic-string?)
 * (basic-string? obj)
 * 
 * Return `#t' if `obj' is a writable string which is not a shared
 * substring, `#f' otherwise.
 */
SCM_PROC (s_basic_string_p, "basic-string?", 1, 0, 0, scm_basic_string_p);
SCM
scm_basic_string_p (SCM x)
{
  SCM_INTS_UNKNOWN;

  return scm_is_basic_string (x) ? SCM_BOOL_T : SCM_BOOL_F;
}


/************************************************************************
 *(h1 "Strings as Arrays of Characters")
 * 
 */

/*(c string-length)
 * (string-length string)
 * 
 * Return the number of characters in a read-only string.
 */
SCM_PROC(s_string_length, "string-length", 1, 0, 0, scm_string_length);
SCM
scm_string_length (SCM str)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg1, s_string_length);
  return SCM_MAKINUM (SCM_RO_LENGTH (str));
}



/*(c string-null?)
 * (string-null? obj)
 * 
 * Return `#t' if `obj' is a read-only string of 0 length.
 */
SCM_PROC(s_string_null_p, "string-null?", 1, 0, 0, scm_string_null_p);
SCM
scm_string_null_p (SCM str)
{
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg1, s_string_null_p);
  return (SCM_RO_LENGTH (str)
	  ? SCM_BOOL_F
	  : SCM_BOOL_T);
}



/*(c string-ref)
 * (string-ref string (:optional n))
 * 
 * Return the character at position `n' in a read-only string.  If
 * omitted, `n' defaults to 0.
 */
SCM_PROC(s_string_ref, "string-ref", 1, 1, 0, scm_string_ref);
SCM
scm_string_ref (SCM str, SCM k)
{
  int x;
  SCM_INTS_ENABLED;

  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg1, s_string_ref);
  if ((k == SCM_UNDEFINED) || (k == SCM_BOOL_F))
    k = SCM_MAKINUM (0);
  SCM_ASSERT (SCM_INUMP (k), k, scm_arg2, s_string_ref);
  x = SCM_INUM (k);
  if (x < 0)
    x = SCM_RO_LENGTH (str) + x;
  SCM_ASSERT (x < SCM_RO_LENGTH (str) && x >= 0, k, scm_outofrange, s_string_ref);
  return scm_int_to_char (SCM_RO_UCHARS (str)[x]);
}


/*(c string-set!)
 * (string-set! string n char)
 * 
 * Set the character at position `n' of a writable string.
 */
SCM_PROC(s_string_set_x, "string-set!", 3, 0, 0, scm_string_set_x);
SCM
scm_string_set_x (SCM str, SCM k, SCM chr)
{
  SCM_INTS_ENABLED;
  int x;

  SCM_ASSERT (scm_is_string (str), str, scm_arg1, s_string_set_x);
  SCM_ASSERT (SCM_INUMP (k), k, scm_arg2, s_string_set_x);
  SCM_ASSERT (scm_is_char (chr), chr, scm_arg3, s_string_set_x);
  x = SCM_INUM (k);
  if (x < 0)
    x = SCM_RO_LENGTH (str) + x;
  SCM_ASSERT (   (x < SCM_STRING_LENGTH (str))
	      && (x >= 0),
	      k, scm_outofrange, s_string_set_x);
  SCM_STRING_CHARS (str)[x] = scm_char_to_int (chr);
  return str;
}

/*(c string-for-each)
 * (string-for-each proc str)
 * 
 * In order, beginning at index 0, for each character `c' in `str',
 * invoke:
 * 
 * 	(proc c)
 * 
 * and discard the result.
 */


/************************************************************************
 *(h1 "Searching for Characters Within Strings")
 * 
 */


/*(c string-index)
 * (string-index string char from to)
 * 
 * Return the offset of the first occurence of `char' in `string'
 * searching `(- to from)' characters beginning at `from'.
 * 
 * The offset returned is relative to the beginning of `string'.
 *
 * Return `#f' if the character is not found.
 */
SCM_PROC(s_string_index, "string-index", 2, 2, 0, scm_string_index);
SCM 
scm_string_index (SCM str, SCM chr, SCM frm, SCM to)
{
  SCM_INTS_ENABLED;

  int pos;
  
  if ((frm == SCM_UNDEFINED) || (frm == SCM_BOOL_F))
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
    to = SCM_BOOL_F;
  pos = scm_i_index (&str, chr, frm, to, scm_arg1, scm_arg2, scm_arg3, scm_arg4, s_string_index);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}


/*(c string-rindex)
 * (string-rindex string char from to)
 * 
 * Return the offset of the last occurence of `char' in `string'
 * searching `(- to from)' characters beginning at `from'.
 * 
 * The offset returned is relative to the beginning of `string'.
 *
 * Return `#f' if the character is not found.
 */
SCM_PROC(s_string_rindex, "string-rindex", 2, 2, 0, scm_string_rindex);
SCM 
scm_string_rindex (SCM str, SCM chr, SCM frm, SCM to)
{
  SCM_INTS_ENABLED;
  int pos;
  
  if (frm == SCM_UNDEFINED)
    frm = SCM_BOOL_F;
  if (to == SCM_UNDEFINED)
    to = SCM_BOOL_F;
  pos = scm_i_rindex (&str, chr, frm, to, scm_arg1, scm_arg2, scm_arg3, scm_arg4, s_string_index);
  return (pos < 0
	  ? SCM_BOOL_F
	  : SCM_MAKINUM (pos));
}


/************************************************************************
 *(h1 "Moving Substrings")
 * 
 */
 
/*(c substring-move-left!)
 * (substring-move-left! source start end dest dest-start)
 * 
 * Copy characters from the read-only string `source' to the writable
 * string `dest'.  `(- end start)' characters are copied taken from
 * `source' beginning at `start' and moved to `dest' beginning at
 * `dest-start'.
 *
 * Characters are moved in order (from `start' to `end-1') which is
 * important in the case the `source' and `dest' are the same and the
 * source and destination regions overlap.
 */
SCM_PROC(s_substring_move_left_x, "substring-move-left!", 5, 0, 0, scm_substring_move_left_x);
SCM
scm_substring_move_left_x (SCM str1, SCM start1, SCM end1, SCM str2, SCM start2)
{
  SCM_INTS_ENABLED;
  long i;
  long j;
  long e;

  SCM_ASSERT (scm_is_ro_string (str1), str1, scm_arg1, s_substring_move_left_x);
  SCM_ASSERT (SCM_INUMP (start1), start1, scm_arg2, s_substring_move_left_x);
  SCM_ASSERT (SCM_INUMP (end1), end1, scm_arg3, s_substring_move_left_x);
  SCM_ASSERT (scm_is_string (str2), str2, scm_arg4, s_substring_move_left_x);
  SCM_ASSERT (SCM_INUMP (start2), start2, scm_arg5, s_substring_move_left_x);

  i = SCM_INUM (start1);
  j = SCM_INUM (start2);
  e = SCM_INUM (end1);

  if (i < 0)
    i = SCM_STRING_LENGTH (str1) + i;
  if (j < 0)
    j = SCM_STRING_LENGTH (str2) + j;
  if (e < 0)
    e = SCM_STRING_LENGTH (str1) + e;

  SCM_ASSERT (i <= SCM_RO_LENGTH (str1) && i >= 0, start1, scm_outofrange, s_substring_move_left_x);
  SCM_ASSERT (j <= SCM_STRING_LENGTH (str2) && j >= 0, start2, scm_outofrange, s_substring_move_left_x);
  SCM_ASSERT (e <= SCM_RO_LENGTH (str1) && e >= 0, end1, scm_outofrange, s_substring_move_left_x);
  SCM_ASSERT (e-i+j <= SCM_STRING_LENGTH (str2), start2, scm_outofrange, s_substring_move_left_x);

  while (i<e)
    SCM_STRING_CHARS (str2)[j++] = SCM_RO_CHARS (str1)[i++];

  return str2;
}


/*(c substring-move-right!)
 * (substring-move-right! source start end dest dest-start)
 * 
 * Copy characters from the read-only string `source' to the writable
 * string `dest'.  `(- end start)' characters are copied taken from
 * `source' beginning at `start' and moved to `dest' beginning at
 * `dest-start'.
 *
 * Characters are moved in reverse order (from `end-1' to `start')
 * which is important in the case the `source' and `dest' are the same
 * and the source and destination regions overlap.
 */
SCM_PROC(s_substring_move_right_x, "substring-move-right!", 5, 0, 0, scm_substring_move_right_x);
SCM
scm_substring_move_right_x (SCM str1, SCM start1, SCM end1, SCM str2, SCM start2)
{
  SCM_INTS_ENABLED;
  long i;
  long j;
  long e;

  SCM_ASSERT (scm_is_ro_string (str1), str1, scm_arg1, s_substring_move_right_x);
  SCM_ASSERT (SCM_INUMP (start1), start1, scm_arg2, s_substring_move_right_x);
  SCM_ASSERT (SCM_INUMP (end1), end1, scm_arg3, s_substring_move_right_x);
  SCM_ASSERT (scm_is_string (str2), str2, scm_arg4, s_substring_move_right_x);
  SCM_ASSERT (SCM_INUMP (start2), start2, scm_arg5, s_substring_move_right_x);

  i = SCM_INUM (start1);
  j = SCM_INUM (start2);
  e = SCM_INUM (end1);

  if (i < 0)
    i = SCM_STRING_LENGTH (str1) + i;
  if (j < 0)
    j = SCM_STRING_LENGTH (str2) + j;
  if (e < 0)
    e = SCM_STRING_LENGTH (str1) + e;

  SCM_ASSERT (i <= SCM_RO_LENGTH (str1) && i >= 0, start1, scm_outofrange, s_substring_move_right_x);
  SCM_ASSERT (j <= SCM_STRING_LENGTH (str2) && j >= 0, start2, scm_outofrange, s_substring_move_right_x);
  SCM_ASSERT (e <= SCM_RO_LENGTH (str1) && e >= 0, end1, scm_outofrange, s_substring_move_right_x);
  SCM_ASSERT ((j = e-i+j) <= SCM_STRING_LENGTH (str2), start2, scm_outofrange, s_substring_move_right_x);

  while (i<e)
    SCM_STRING_CHARS (str2)[--j] = SCM_RO_CHARS (str1)[--e];

  return str2;
}


/************************************************************************
 *(h1 "String Filling")
 * 
 */


/*(c substring-fill!)
 * (substring-fill! string start end char)
 * 
 * Fill `(- end start)' characters of `string' with `char' beginning
 * at position `start'.
 */
SCM_PROC(s_substring_fill_x, "substring-fill!", 4, 0, 0, scm_substring_fill_x);
SCM
scm_substring_fill_x (SCM str, SCM start, SCM end, SCM chr)
{
  SCM_INTS_ENABLED;
  char * chrs;
  long i;
  long e;
  char c;

  SCM_ASSERT (scm_is_string (str), str, scm_arg1, s_substring_fill_x);
  SCM_ASSERT (SCM_INUMP (start), start, scm_arg2, s_substring_fill_x);
  SCM_ASSERT (SCM_INUMP (end), end, scm_arg3, s_substring_fill_x);
  SCM_ASSERT (scm_is_char (chr), chr, scm_arg4, s_substring_fill_x);

  i = SCM_INUM (start);
  e = SCM_INUM (end);
  c = scm_char_to_int (chr);

  if (i < 0)
    i = SCM_STRING_LENGTH (str) + i;
  if (e < 0)
    e = SCM_STRING_LENGTH (str) + e;

  SCM_ASSERT (i <= SCM_STRING_LENGTH (str) && i >= 0, start, scm_outofrange, s_substring_fill_x);
  SCM_ASSERT (e <= SCM_STRING_LENGTH (str) && e >= 0, end, scm_outofrange, s_substring_fill_x);

  chrs = SCM_STRING_CHARS (str);

  while (i < e)
    chrs[i++] = c;

  return str;
}


/*(c string-fill!)
 * (string-fill! string character)
 * 
 * Fill all of `string' which `character'.
 */
SCM_PROC(s_string_fill_x, "string-fill!", 2, 0, 0, scm_string_fill_x);
SCM
scm_string_fill_x (SCM str, SCM chr)
{
  SCM_INTS_ENABLED;
  char *dst;
  char c;
  long k;

  SCM_ASSERT (scm_is_string (str), str, scm_arg1, s_string_fill_x);
  SCM_ASSERT (scm_is_char (chr), chr, scm_arg2, s_string_fill_x);

  c = scm_char_to_int (chr);
  dst = SCM_STRING_CHARS (str);

  for (k = SCM_STRING_LENGTH (str)-1; k >= 0; k--)
    dst[k] = c;

  return str;
}


/************************************************************************
 *(h1 "String Comparison Functions")
 *
 * String comparison functions define a lexical ordering of strings
 * based on the character comparison functions (see
 * xref:"Characters").
 *
 * There are two flavors of comparison functions: the case dependent
 * functions whose names begin `string' and the case independent
 * functions whose names begin `string-ci'.
 *
 * All of these functions operate on all read-only types of strings:
 * regular strings, substrings, and symbols.
 */

/*(c string=?)
 * (string=? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings
 * containing exactly the same characters in the same order.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_equal_p, "string=?", scm_tc7_rpsubr, scm_string_equal_p);
SCM
scm_string_equal_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;
  size_t i;
  char *c1;
  char *c2;

  SCM_ASSERT (scm_is_ro_string (s1), s1, scm_arg1, s_string_equal_p);
  SCM_ASSERT (scm_is_ro_string (s2), s2, scm_arg2, s_string_equal_p);

  i = SCM_RO_LENGTH (s2);
  if (SCM_RO_LENGTH (s1) != i)
    {
      return SCM_BOOL_F;
    }
  c1 = SCM_RO_CHARS (s1);
  c2 = SCM_RO_CHARS (s2);

  while (0 != i--)
    if (*c1++ != *c2++)
      return SCM_BOOL_F;

  return SCM_BOOL_T;
}


/*(c string<?)
 * (string<? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings, each
 * lexically less than the next.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_lt_p, "string<?", scm_tc7_rpsubr, scm_string_lt_p);
SCM
scm_string_lt_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;
  size_t i;
  size_t len;
  size_t s2len;
  t_uchar *c1;
  t_uchar *c2;
  int c;

  SCM_ASSERT (scm_is_ro_string (s1), s1, scm_arg1, s_string_lt_p);
  SCM_ASSERT (scm_is_ro_string (s2), s2, scm_arg2, s_string_lt_p);
  len = SCM_RO_LENGTH (s1);
  s2len = i = SCM_RO_LENGTH (s2);
  if (len>i) i = len;
  c1 = SCM_RO_UCHARS (s1);
  c2 = SCM_RO_UCHARS (s2);

  for (i = 0;i<len;i++)
    {
      c = (*c1++ - *c2++);
      if (c>0)
	return SCM_BOOL_F;
      if (c<0)
	return SCM_BOOL_T;
    }
  {
    SCM answer;
    answer = (s2len != len) ? SCM_BOOL_T : SCM_BOOL_F;
    return answer;
  }
}


/*(c string<=?)
 * (string<=? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings, each
 * lexically less than or equal to the next.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_le_p, "string<=?", scm_tc7_rpsubr, scm_string_le_p);
SCM
scm_string_le_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;

  return scm_not (scm_string_lt_p (s2, s1));
}


/*(c string>?)
 * (string>? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings, each
 * lexically greater than the next.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_gt_p, "string>?", scm_tc7_rpsubr, scm_string_gt_p);
SCM
scm_string_gt_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;

  return scm_string_lt_p (s2, s1);
}


/*(c string>=?)
 * (string>=? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings, each
 * lexically greater than or equal to the next.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_ge_p, "string>=?", scm_tc7_rpsubr, scm_string_ge_p);
SCM
scm_string_ge_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;

  return scm_not (scm_string_lt_p (s1, s2));
}


/*(c string-ci=?)
 * (string-ci=? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings, each
 * lexically equal to the next, disregarding the case of alphabetic
 * characters.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_ci_equal_p, "string-ci=?", scm_tc7_rpsubr, scm_string_ci_equal_p);
SCM
scm_string_ci_equal_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;
  size_t i;
  t_uchar *c1;
  t_uchar *c2;

  SCM_ASSERT (scm_is_ro_string (s1), s1, scm_arg1, s_string_ci_equal_p);
  SCM_ASSERT (scm_is_ro_string (s2), s2, scm_arg2, s_string_ci_equal_p);
  i = SCM_RO_LENGTH (s2);
  if (SCM_RO_LENGTH (s1) != i)
    {
      return SCM_BOOL_F;
    }
  c1 = SCM_RO_UCHARS (s1);
  c2 = SCM_RO_UCHARS (s2);
  while (0 != i--)
    if (toupper(*c1++) != toupper(*c2++))
      return SCM_BOOL_F;
  return SCM_BOOL_T;
}


/*(c string-ci<?)
 * (string-ci<? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings, each
 * lexically less than the next, disregarding the case of alphabetic
 * characters.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_ci_lt_p, "string-ci<?", scm_tc7_rpsubr, scm_string_ci_lt_p);
SCM
scm_string_ci_lt_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;
  size_t i;
  size_t len;
  size_t s2len;
  t_uchar *c1;
  t_uchar *c2;
  int c;

  SCM_ASSERT (scm_is_ro_string (s1), s1, scm_arg1, s_string_ci_lt_p);
  SCM_ASSERT (scm_is_ro_string (s2), s2, scm_arg2, s_string_ci_lt_p);
  len = SCM_RO_LENGTH (s1);
  s2len = i = SCM_RO_LENGTH (s2);
  if (len>i) i=len;
  c1 = SCM_RO_UCHARS (s1);
  c2 = SCM_RO_UCHARS (s2);
  for (i = 0;i<len;i++)
    {
      c = (toupper(*c1++) - toupper(*c2++));
      if (c>0) return SCM_BOOL_F;
      if (c<0) return SCM_BOOL_T;
    }
  return (s2len != len) ? SCM_BOOL_T : SCM_BOOL_F;
}


/*(c string-ci<=?)
 * (string-ci<=? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings, each
 * lexically less than or equal to the next, disregarding the case of
 * alphabetic characters.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_ci_le_p, "string-ci<=?", scm_tc7_rpsubr, scm_string_ci_le_p);
SCM
scm_string_ci_le_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;

  return scm_not (scm_string_ci_lt_p (s2, s1));
}


/*(c string-ci>?)
 * (string-ci>? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings, each
 * lexically greater than the next, disregarding the case of
 * alphabetic characters.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_ci_gt_p, "string-ci>?", scm_tc7_rpsubr, scm_string_ci_gt_p);
SCM
scm_string_ci_gt_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;

  return scm_string_ci_lt_p (s2, s1);
}


/*(c string-ci>=?)
 * (string-ci>=? . args)
 * 
 * Return `#t' if all of the arguments are read-only strings, each
 * lexically greater than or equal to the next, disregarding the case
 * of alphabetic characters.
 *
 * Given no arguments, this procedure returns `#t'.
 */
SCM_PROC1 (s_string_ci_ge_p, "string-ci>=?", scm_tc7_rpsubr, scm_string_ci_ge_p);
SCM
scm_string_ci_ge_p (SCM s1, SCM s2)
{
  SCM_INTS_ENABLED;

  return scm_not (scm_string_ci_lt_p (s1, s2));
}


/************************************************************************
 *(h1 "String Case Conversions")
 * 
 */


/*(c string-upcase!)
 * (string-upcase! string)
 * 
 * Replace all lowercase alphabetic characters in `string' with
 * uppercase characters.
 */
SCM_PROC (s_string_upcase_x, "string-upcase!", 1, 0, 0, scm_string_upcase_x);
SCM
scm_string_upcase_x (SCM v)
{
  SCM_INTS_ENABLED;
  long k;
  t_uchar *cs;

  SCM_ASSERT (scm_is_string (v), v, scm_arg1, s_string_upcase_x);
  k = SCM_STRING_LENGTH (v);
  cs = SCM_STRING_UCHARS (v);
  while (k--)
    cs[k] = toupper(cs[k]);
  return v;
}


/*(c string-downcase!)
 * (string-downcase!)
 * 
 * Replace all uppercase alphabetic characters in `string' with
 * lowercase characters.
 */
SCM_PROC (s_string_downcase_x, "string-downcase!", 1, 0, 0, scm_string_downcase_x);
SCM
scm_string_downcase_x (SCM v)
{
  SCM_INTS_ENABLED;

  long k;
  t_uchar *cs;

  SCM_ASSERT (scm_is_string (v), v, scm_arg1, s_string_downcase_x);
  k = SCM_STRING_LENGTH (v);
  cs = SCM_STRING_UCHARS (v);
  while (k--)
    cs[k] = tolower(cs[k]);
  return v;
}


/************************************************************************
 *(h1 "Converting Between Strings and Lists")
 * 
 */


/*(c list->string)
 * (list->string l)
 * 
 * Combine the elements of list `l' into a string.
 *
 * The elements may be characters or read-only strings.
 */
SCM_PROC(s_list_to_string, "list->string", 1, 0, 0, scm_list_to_string);
SCM
scm_list_to_string (SCM l)
{
  return scm_string (l);
}


/*(c string-append)
 * (string-append . args)
 * 
 * Combine the arguments into a string.
 *
 * The arguments may be characters or read-only strings.
 */
SCM_PROC(s_string_append, "string-append", 0, 0, 1, scm_string_append);
SCM
scm_string_append (SCM args)
{
  return scm_string (args);
}

/*(c string->list)
 * (string->list s)
 * 
 * Return a list of characters in the read-only string `s'.
 */
SCM_PROC(s_string_to_list, "string->list", 1, 0, 0, scm_string_to_list);
SCM
scm_string_to_list (SCM str)
{
  SCM_INTS_ENABLED;
  long i;
  SCM res;
  t_uchar *src;

  res = SCM_EOL;
  SCM_ASSERT (scm_is_ro_string (str), str, scm_arg1, s_string_to_list);
  src = SCM_RO_UCHARS (str);
  for (i = SCM_RO_LENGTH (str)-1; i >= 0; i--)
    res = scm_cons ((SCM)scm_int_to_char (src[i]), res);
  return res;
}


/************************************************************************
 *h1 "Additional C Funcions for Scheme Strings"
 * 
 */

/*c  scm_is_basic_string)
 * int scm_is_basic_string (SCM obj);
 * 
 * Is the non-immediate object X a string?
 */
int
scm_is_basic_string (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_TYP7S (obj) == scm_tc7_string);
}


/*c  scm_is_ro_string)
 * int scm_is_ro_string (SCM obj);
 * 
 * Is the non-immediate object X a read-only string?
 */
int
scm_is_ro_string (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_TYP7RSU (obj) == scm_tc7_symbol);
}


/*c  scm_is_string)
 * int scm_is_string (SCM obj);
 * 
 * Is the non-immediate object X a writable string?
 */
int
scm_is_string (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_TYP7SU (obj) == scm_tc7_string);
}


/*c  scm_is_substr)
 * int scm_is_substr (SCM obj);
 * 
 * Is the non-immediate object X a substring?
 */
int
scm_is_substr (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_TYP7S (obj) == scm_tc7_substring);
}


/*c  scm_is_writable_substr)
 * int scm_is_writable_substr (SCM obj);
 * 
 * Is the non-immediate object X a writable substring?
 */
int
scm_is_writable_substr (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_TYP7S (obj) == scm_tc7_substring);
}

/*c  scm_is_ro_substr)
 * int scm_is_ro_substr (SCM obj);
 * 
 * Is the non-immediate object X a read-only substring?
 */
int
scm_is_ro_substr (SCM obj)
{
  return !SCM_IS_IMMEDIATE (obj) && (SCM_TYP7RS (obj) == scm_tc7_subsymbol);
}


/*c  scm_makstr)
 * SCM scm_makstr (long len);
 * 
 * Return a new string of `len' characters, initialized arbitrarily.
 */
SCM
scm_makstr (long len)
{
  SCM_INTS_NESTED;
  SCM s;
  SCM * mem;

  SCM_NEWCELL (s);
  SCM_REDEFER_INTS;
  mem = (SCM *)scm_must_malloc (len + 1);
  SCM_CDR (s) = (SCM) (mem);
  SCM_SET_LENGTH (s, len, scm_tc7_string);
  SCM_STRING_CHARS (s)[len] = 0;
  SCM_REALLOW_INTS;
  return s;
}


/*c  scm_makfromstr)
 * SCM scm_makfromstr (t_uchar *src, long len);
 * 
 * Construct a new string of `len' characters, copied from `src'.
 */
SCM
scm_makfromstr (t_uchar *src, long len)
{
  SCM s;
  char *dst;

  s = scm_makstr ((long) len);
  dst = SCM_STRING_CHARS (s);
  while (len--)
    *dst++ = *src++;
  return s;
}


/*c  scm_makfromstr0)
 * SCM scm_makfromstr0 (t_uchar *src);
 * 
 * Construct a new string of characters, copied from (0-terminated)
 * `src'.
 */
SCM
scm_makfromstr0 (t_uchar *src)
{
  SCM_INTS_UNKNOWN;

  if (!src)
    return SCM_BOOL_F;
  return scm_makfromstr (src, (size_t) strlen (src));
}


/*c  scm_take_str0)
 * SCM scm_take_str0 (char * it);
 * 
 * Convert a 0-terminated C string into a new Scheme string.  The C
 * string should be allocated by `malloc' and will be automatically
 * freed if the Scheme string is GCed.
 */
SCM
scm_take_str0 (char * it)
{
  SCM_INTS_NESTED;
  int len;
  SCM answer;

  SCM_NEWCELL (answer);

  SCM_REDEFER_INTS;
  len = strlen (it);
  SCM_SET_LENGTH (answer, len, scm_tc7_string);
  SCM_CDR (answer) = (SCM)it;
  scm_mallocated += len + 1;
  SCM_REALLOW_INTS;

  return answer;
}


/*c  scm_take_str)
 * SCM scm_take_str (char * it, int len);
 * 
 * Convert a C string of the specified length into a new Scheme
 * string.  The C string should be allocated by malloc and will be
 * freed if the Scheme string is GCed.
 */
SCM
scm_take_str (char * it, int len)
{
  SCM answer;

  SCM_NEWCELL (answer);
  SCM_REDEFER_INTS;
  SCM_SET_LENGTH (answer, len, scm_tc7_string);
  SCM_CDR (answer) = (SCM)it;
  scm_mallocated += len + 1;
  SCM_REALLOW_INTS;
  return answer;
}


/*c  scm_take_str0_static)
 * SCM scm_take_str0_static (char * it);
 * 
 * Create a new Scheme string that shares data with the given
 * 0-terminated C string.  Even if the Scheme string is GCed, the C
 * string will not be freed.
 */
SCM
scm_take_str0_static (char * it)
{
  SCM_INTS_NESTED;
  SCM answer;

  SCM_NEWCELL (answer);
  SCM_REDEFER_INTS;
  SCM_SET_LENGTH (answer, strlen (it), scm_tc7_static_string);
  SCM_CDR (answer) = (SCM)it;
  SCM_REALLOW_INTS;
  return answer;
}


/*c  scm_take_string_static)
 * SCM scm_take_string_static (char * it, int len);
 * 
 * Create a new string that shares data with the given C string of the
 * specified length.  Even if the Scheme string is GCed, the C string
 * will not be freed.
 *
 * The string must be 0-terminated.  (it[len] == 0)
 */
SCM
scm_take_string_static (char * it, int len)
{
  SCM_INTS_NESTED;
  SCM answer;

  SCM_NEWCELL (answer);
  SCM_REDEFER_INTS;
  SCM_SET_LENGTH (answer, len - 1, scm_tc7_static_string);
  SCM_CDR (answer) = (SCM)it;
  SCM_REALLOW_INTS;
  return answer;
}


/*c  scm_argv2scm)
 * SCM scm_argv2scm (int argc, t_uchar **argv);
 * 
 * Convert `argv' into a list of strings. 
 *
 * If `argc' < 0, `argv' is presumed to be null-terminated.  If `argc
 * >= 0', it is the number of elements in `argv'.
 */
SCM
scm_argv2scm (int argc, t_uchar **argv)
{
  SCM_INTS_DISABLED;
  int i;
  SCM lst;

  if (0 <= argc)
    i = argc;
  else
    for (i = 0; argv[i]; i++)
      ;

  lst = SCM_EOL;
  while (i--)
    lst = scm_cons (scm_makfromstr (argv[i], (size_t) strlen (argv[i])), lst);
  return lst;
}


/*c  scm_scm2argv)
 * t_uchar ** scm_scm2argv (int * n_args, SCM args, SCM pos, SCM subr);
 * 
 * Convert a list of Scheme strings into a null-terminated array of
 * individually malloced C strings.
 *
 * The number of strings is returned in `n_args'.
 */
t_uchar **
scm_scm2argv (int * n_args, SCM args, SCM pos, SCM subr)
{
  SCM_INTS_DISABLED;
  t_uchar **execargv;
  int num_args;
  int i;

  num_args = scm_ilength (args);
  if (num_args < 0)
    {
      scm_delayed_error (args, pos, subr);
      return 0;
    }
  execargv = (t_uchar **) malloc ((num_args + 1) * sizeof (t_uchar *));
  for (i = 0; SCM_EOL != args; args = SCM_CDR (args), ++i)
    {
      size_t len;
      t_uchar *dst;
      t_uchar *src;

      if (!scm_is_ro_string (SCM_CAR (args)))
	{
	  while (--i > 0)
	    free (execargv[i]);
	  free (execargv);
	  scm_delayed_error (args, pos, subr);
	  return 0;
	}
      len = 1 + SCM_RO_LENGTH (SCM_CAR (args));
      dst = (t_uchar *) malloc ((long) len);
      src = SCM_RO_UCHARS (SCM_CAR (args));
      --len;
      dst[len] = 0;
      while (len--) 
	dst[len] = src[len];
      execargv[i] = dst;
    }
  execargv[i] = 0;
  if (n_args)
    *n_args = i;
  return execargv;
}



/*c  scm_i_index)
 * int scm_i_index (SCM * str,
 *                  SCM chr,
 *                  SCM sub_start,
 *                  SCM sub_end,
 *                  SCM pos,
 *                  SCM pos2,
 *                  SCM pos3,
 *                  SCM pos4,
 *                  SCM why);
 * 
 * 
 * Return the integer offset of the first occurence of `chr' within a substring
 * of `str'.  `sub_start' and `sub_end' specify which substring
 * to search.  The offset returned is relative to the beginning of `str'.
 *
 * Return -1 if the character is not found.
 *
 * `pos', `pos2', `pos3', and `pos4' correspond to the preceeding four
 * arguments and are used as the third arguments in argument-checking
 * calls to ASSERT.  They should be passed values such as `scm_arg1',
 * `scm_arg2', etc.
 */
int
scm_i_index (SCM * str,
	     SCM chr,
	     SCM sub_start,
	     SCM sub_end,
	     SCM pos,
	     SCM pos2,
	     SCM pos3,
	     SCM pos4,
	     SCM why)
{
  SCM_INTS_ENABLED;
  t_uchar * p;
  int x;
  int start;
  int bound;
  int ch;

  SCM_ASSERT (scm_is_ro_string (*str), *str, pos, why);
  SCM_ASSERT (scm_is_char (chr), chr, pos2, why);

  if (sub_start == SCM_BOOL_F)
    start = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (sub_start), sub_start, pos3, why);
      start = SCM_INUM (sub_start);
      if (start < 0)
	start += SCM_RO_LENGTH (*str);
      SCM_ASSERT ((0 <= start) && (start <= SCM_RO_LENGTH (*str)),
		  sub_start, pos3, why);
    }

  if (sub_end == SCM_BOOL_F)
    bound = SCM_RO_LENGTH (*str);
  else
    {
      SCM_ASSERT (SCM_INUMP (sub_end), sub_end, pos4, why);
      bound = SCM_INUM (sub_end);
      if (bound < 0)
	bound += SCM_RO_LENGTH (*str);
      SCM_ASSERT ((start <= bound) && (bound <= SCM_RO_LENGTH (*str)),
		  sub_end, pos4, why);
    }
  p = (t_uchar *)SCM_RO_CHARS (*str) + start;
  ch = scm_char_to_int (chr);

  for (x = start; x < bound; ++x, ++p)
    if (*p == ch)
      return x;

  return -1;
}


/*c  scm_i_rindex)
 * int scm_i_rindex (SCM * str,
 *                   SCM chr,
 *                   SCM sub_start,
 *                   SCM sub_end,
 *                   SCM pos,
 *                   SCM pos2,
 *                   SCM pos3,
 *                   SCM pos4,
 *                   SCM why);
 * 
 * Return the integer offset of the last occurence of `chr' within a substring
 * of `str'.  `sub_start' and `sub_end' specify which substring
 * to search.  The offset returned is relative to the beginning of `str'.
 *
 * Return -1 if the character is not found.
 *
 * `pos', `pos2', `pos3', and `pos4' correspond to the preceeding four
 * arguments and are used as the third arguments in argument-checking
 * calls to ASSERT.  They should be passed values such as `scm_arg1',
 * `scm_arg2', etc.
 * 
 */
int
scm_i_rindex (SCM * str,
	      SCM chr,
	      SCM sub_start,
	      SCM sub_end,
	      SCM pos,
	      SCM pos2,
	      SCM pos3,
	      SCM pos4,
	      SCM why)
{
  SCM_INTS_ENABLED;
  t_uchar * p;
  int x;
  int upper_bound;
  int lower_bound;
  int ch;

  SCM_ASSERT (scm_is_ro_string (*str), *str, pos, why);
  SCM_ASSERT (scm_is_char (chr), chr, pos2, why);

  if (sub_start == SCM_BOOL_F)
    lower_bound = 0;
  else
    {
      SCM_ASSERT (SCM_INUMP (sub_start), sub_start, pos3, why);
      lower_bound = SCM_INUM (sub_start);
      if (lower_bound < 0)
	lower_bound += SCM_RO_LENGTH (*str);
      SCM_ASSERT ((0 <= lower_bound) && (lower_bound <= SCM_RO_LENGTH (*str)),
		  sub_start, pos3, why);
    }

  if (sub_end == SCM_BOOL_F)
    upper_bound = SCM_RO_LENGTH (*str);
  else
    {
      SCM_ASSERT (SCM_INUMP (sub_end), sub_end, pos4, why);
      upper_bound = SCM_INUM (sub_end);
      if (upper_bound < 0)
	upper_bound += SCM_RO_LENGTH (*str);
      SCM_ASSERT ((lower_bound <= upper_bound) && (upper_bound <= SCM_RO_LENGTH (*str)),
		  sub_end, pos4, why);
    }

  ch = scm_char_to_int (chr);
  p = upper_bound - 1 + (t_uchar *)SCM_RO_CHARS (*str);
  for (x = upper_bound - 1; x >= lower_bound; --x, --p)
    if (*p == ch)
      return x;

  return -1;
}




void
scm_init_strings (void)
{
  SCM_INTS_UNKNOWN;

#include "systas/libsystas/strings.x"
}


/************************************************************************
 *h1 "String Internals")
 * 
 * \WARNING: This section is not quite up to date./
 *
 * Ordinary strings are represented by a non-immediate object.
 *
 *  ..size (24 bits)..scm_tc7_string  ......t_uchar * data......
 *
 * Normally, data is allocated by malloc and freed if the string
 * is garbage collected. If the tag is scm_tc7_static_string instead
 * of scm_tc7_string, then the data is not freed if the string is
 * garbage collected.
 *
 * There are a total of 8 tags assigned to string-like types.  These are:
 *
 *	scm_tc7_symbol
 *	scm_tc7_string
 *	scm_tc7_subsymbol
 *	scm_tc7_substring
 *
 *	scm_tc7_static_symbol
 *	scm_tc7_static_string
 *	scm_tc7_static_subsymbol
 *	scm_tc7_static_substring
 *
 * These differ only in the "R", "S", and "U" option bits:
 *
 * 	R = 0	-- read-only string (symbol or shared subsymbol)
 * 	R = 1	-- read-write string
 *
 * 	S = 0	-- character data allocated by malloc
 * 	S = 1	-- character data not allocated by malloc
 *
 * 	U = 0	-- complete string
 * 	U = 1	-- shared substring
 *
 * That relation among the tags is exploited by functions like:
 *
 *	scm_is_ro_string	- True for all string-like types.
 *	scm_is_string		- True for all string-like types with
 *				  the "R" option bit equal to 0.
 * Macros like:
 *
 *	SCM_RO_CHARS
 *	SCM_STRING_LENGTH
 *
 * work for several string-like types.  See "libsystas/strings.h".
 *
 */


/************************************************************************
 *(h1 "Strings Rationale")
 *
 * 
 * Shared substrings make possible efficient string manipulation without
 * having to rely on the gymnastics of passing around `start end' 
 * arguments everywhere.
 * 
 * Read-only strings make formal the obvious similarity between symbols
 * and strings.
 * 
 * Together, these extensions to standard Scheme result in concise
 * idioms.
 * 
 */
